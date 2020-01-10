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
from functools import partial
import socket
import select
import Queue
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

def route(obj):
    response = None
    # obj is a bson obj received from a socket
    if obj != None:
        if obj.has_key('fn'):
            fn = obj['fn']

            if obj.has_key('args'):
                args = obj['args']
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

class Server:
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.sock = socket.socket()
        self.poll = select.poll()
        self.handlers = {}
        self.fd_events = {}

        bson.patch_socket()

    def start(self):
        sock = self.sock
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.setblocking(0)
        sock.bind((self.host, self.port))
        sock.listen(100)

        handlers = self.handlers
        poll = self.poll
        self.add_handler(sock.fileno(), self.accept, select.POLLIN)

        while True:
            poll_events = poll.poll(1)
            for fd, event in poll_events:
                handler = handlers.get(fd)
                if handler:
                    handler()


    def accept(self):
       for i in range(100):
           try:
               conn, address = self.sock.accept()
           except OSError:
               break
           else:
                conn.setblocking(0)
                fd = conn.fileno()
                self.add_handler(fd, partial(self.read, conn), select.POLLIN)

    def read(self, conn):
        fd = conn.fileno()
        self.remove_handler(fd)
        try:
            obj = conn.recvobj()
            self.response = route(obj)
        except:
            conn.close()
            raise
        else:
            self.add_handler(fd, partial(self.write, conn), select.POLLOUT)

    def write(self, conn):
        fd = conn.fileno()
        self.remove_handler(fd)
        try:
            conn.sendobj(self.response)
        finally:
            conn.close()

    def add_handler(self, fd, handler, event):
        self.handlers[fd] = handler
        self.register(fd, event)

    def remove_handler(self, fd):
        self.handlers.pop(fd, None)
        self.unregister(fd)

    def register(self, fd, event):
        if fd in self.fd_events:
            raise IOError("fd %s already registered" % fd)
        self.poll.register(fd, event)
        self.fd_events[fd] = event

    def unregister(self, fd):
        event = self.fd_events.pop(fd, None)
        if event is not None:
            self._poll.unregister(fd)


def start(host, port):
    server = Server(host, port)

    try:
        server.start()
    except KeyboardInterrupt:
        exit()
