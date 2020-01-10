#!/usr/bin/env python
#coding: utf-8

# [Usage]
# cat data.raw | python translate.py > data.json
#
import sys
import json

if __name__ == '__main__':
    all_ids = set()
    ref_ids = set()

    records = {}

    for i, line in enumerate(sys.stdin):
        if i == 0: #skip the first heading line
            continue

        if not line.strip(): #skip blank lines
            continue

        delim = '|'
        (_id, _name, _type, _knownBy, _docs) = line.strip().split(delim)

        all_ids.add(_id)

        if _knownBy.strip():
            comma = ','
            _knownByIds = map(lambda _id: _id.strip(), _knownBy.split(comma))
        else:
            _knownByIds = []

        for i in _knownByIds:
            ref_ids.add(i)

        record = {
            'name': _name,
            'type': _type,
            'knownBy': _knownByIds,
            'docs': _docs,
            'knows': []
        }
        records[_id] = record

    # verify ref_ids all exist in all_ids
    diff = ref_ids - all_ids
    if diff:
        sys.stderr.write('Error: referring to non-existent id(s): ')
        for i in diff:
            sys.stderr.write(i)

        sys.stderr.write('\n')
    else:
        document = {
            'data': records,
            'errors': []
        }

        out = json.dumps(document)
        print out

