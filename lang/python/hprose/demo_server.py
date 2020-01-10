#!/usr/bin/env python
# encoding: utf-8

import hprose

def hello(name):
    return 'Hello %s!' % name

def add(a, b):
    return a + b

def main():
    server = hprose.HttpServer(port = 8181)
    server.addFunction(hello)
    server.addFunction(add)
    server.start()

if __name__ == '__main__':
    main()
