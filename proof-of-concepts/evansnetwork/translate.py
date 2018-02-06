#!/usr/bin/env python
#coding: utf-8

# [Usage]
# cat data.raw | python translate.py > data.json
#
import sys
import json

if __name__ == '__main__':
    records = {}

    for i, line in enumerate(sys.stdin):
        if i == 0: #skip the first heading line
            continue

        delim = '|'
        (_id, _name, _type, _knownBy, _docs) = line.strip().split(delim)

        if _knownBy.strip():
            comma = ','
            _knownByIds = map(lambda _id: _id.strip(), _knownBy.split(comma))
        else:
            _knownByIds = []

        record = {
            'name': _name,
            'type': _type,
            'knownBy': _knownByIds,
            'docs': _docs,
            'knows': []
        }
        records[_id] = record

    document = {
        'data': records,
        'errors': []
    }

    out = json.dumps(document)
    print out

