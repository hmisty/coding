# MIT License
#
# Copyright (c) 2017 Evan Liu (hmisty)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

from __future__ import print_function
import socket
import select
import Queue
import zmq.green as zmq
import bson

from . import status

# the global function map that remotely callable
remote_functions = dict()

def rpc(func, name=None):
    """ add a function to remote callable function map.

    use as function

    >>> rpc(lambda s: s, name="echo")

    or use as decorator

    >>> @rpc
        def echo(s):
            return s

    """
    global remote_functions
    remote_functions[name or func.__name__] = func
    return func

def invoke_func(fn, args):
    global remote_functions

    if not remote_functions.has_key(fn):
        return status.function_not_found

    f = remote_functions[fn]
    if not callable(f):
        return status.function_not_callable

    if args == None:
        result = f()
    else:
        result = f(*args)

    return result

def dispatch(doc):
    response = None
    if doc != None:
        if doc.has_key('fn'):
            fn = doc['fn']

            if doc.has_key('args'):
                args = doc['args']
            else:
                args = None

            print("call %s" % fn)
            try:
                result = invoke_func(fn, args)
                response = status.ok
                response['result'] = result
            except Exception as error:
                response = status.invoke_error
                response['error_msg'] = str(error)
        else:
            response = status.function_not_found

    return response

def listen(sock):
    # blocking here
    bson_in = sock.recv()
    doc = bson.loads(bson_in)
    result = dispatch(doc)
    bson_out = bson.dumps(result)
    sock.send(bson_out)

def start_with_str(conn_str):
    ctx = zmq.Context()
    sock = ctx.socket(zmq.REP)
    sock.bind(conn_str)

    try:
        while True: # loop forever
            listen(sock)
    except KeyboardInterrupt:
        sock.close()
        ctx.term()
        exit()

def start(host, port=None):
    if port:
        addr = 'tcp://{}:{}'.format(host, port)
    else:
        addr = host

    start_with_str(addr)

