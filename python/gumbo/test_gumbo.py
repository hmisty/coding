#!/bin/env python
#encoding:utf-8

# pip install gumbo

from gumbo import gumboc

html = '''
<div>
    <div>
        </p>
        some content here<br>
        中文<br />
    </div>
</div>
'''

dom = gumboc.parse(html)

print repr(dom)
