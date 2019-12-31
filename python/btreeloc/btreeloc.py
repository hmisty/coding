#!/usr/bin/python3

from decimal import Decimal
import sys
import pprint

#################################
def traverse(btree, root):
    btree[root]['level'] = 0
    btree[root]['pos'] = 0
    _traverse(btree, root, 0, 0)
    
def _traverse(btree, node, level, pos):
    left = btree[node]['left']
    right = btree[node]['right']

    if left != None:
        level_left = level + 1
        pos_left = pos << 1 | 0
        btree[left]['level'] = level_left
        btree[left]['pos'] = pos_left
        _traverse(btree, left, level_left, pos_left)

    if right != None:
        level_right = level + 1
        pos_right = pos << 1 | 1
        btree[right]['level'] = level_right
        btree[right]['pos'] = pos_right
        _traverse(btree, right, level_right, pos_right)

#################################

if __name__ == '__main__':
    lines = sys.stdin.readlines()

    btree = {}
    root = None

    for line in lines:
        line = line.strip()
        (t, child, parent) = line.split(',')
        t = Decimal(t)
        print(t, child, parent)

        btree[child] = {'level':-1, 'pos':-1, 't':t, 'parent':parent, 'left':None, 'right':None}

        if parent in btree:
            if btree[parent]['parent'] == None:
                root = parent

            if btree[parent]['left'] == None:
                btree[parent]['left'] = child
            else:
                left = btree[parent]['left']
                t_left = btree[left]['t']
                if t < t_left:
                    btree[parent]['left'] = child
                    btree[parent]['right'] = left
                else:
                    btree[parent]['right'] = child
        else:
            btree[parent] = {'level':-1, 'pos':-1, 't':Decimal(-1), 'parent':None, 'left':child, 'right':None}
            root = parent


    traverse(btree, root)

    #pprint.pprint(btree)
    #print('root: ', root)

    def by_t(e):
        return btree[e]['t']

    nodes = list(btree.keys())
    nodes.sort(key=by_t)

    for node in nodes:
        level = btree[node]['level']
        pos = btree[node]['pos']
        t = btree[node]['t']
        parent = btree[node]['parent']
        left = btree[node]['left']
        right = btree[node]['right']
        print(level, '\t', pos, '\t', t, '\t', node, '\t', parent, '\t', left, '\t', right)

