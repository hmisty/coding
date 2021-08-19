#!/usr/bin/env python3
import sys
import fileinput
import re

def dirty_work(lineno, line):
    if lineno < 11: # for the first 10 lines...
        m = re.search(r'title:\s*"(.+)"', line)
        if m:
            title = m.group(1)
            print('#', title) # leave only the title
    else:
        m = re.match(r'\(公众号：刘教链.*\)', line)
        if m:
            print(m.group(0), '\\')
        else:
            print(line, end='')

def fix(fn):
    with fileinput.FileInput(fn, inplace=True, backup='.bak') as file:
        for line in file:
            lineno = file.filelineno()
            dirty_work(lineno, line)

if __name__ == '__main__':
    if len(sys.argv) > 1:
        for fn in sys.argv:
            if fn == sys.argv[0]:
                continue
            else:
                fix(fn)

