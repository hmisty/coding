#!/usr/bin/env python
#coding:utf-8

"""
doctest demonstration.
how to run:
    $ python -m doctest -v try-doctest.py
"""
def multiply(a, b):
    """
    Multiply a with b.

    >>> multiply(1, 2)
    2
    >>> multiply('a', 3)
    'aaa'
    """
    return a * b

"""
main
"""
if __name__ == '__main__':
    import doctest
    doctest.testmod(verbose=True)

