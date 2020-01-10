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
from socket import socket
import zmq.green as zmq
import bson

class Proxy:
    def __init__(self, host=None, port=None):
        if host != None:
            if port != None:
                self.connect(host, port)
            else:
                self.connect_with_str(host)

    def connect(self, host, port):
        addr = 'tcp://{}:{}'.format(host, port)
        self.connect_with_str(addr)

    def connect_with_str(self, conn_str):
        self.ctx = zmq.Context()
        self.sock = self.ctx.socket(zmq.REQ)
        self.sock.connect(conn_str)

    def use_service(self, namelist):
        for name in namelist:
            f = self.invoke_func(name);
            setattr(self, name, f);

    def invoke_func(self, name):
        def invoke_remote_func(*args, **kwargs):
            bson_out = bson.dumps({'fn': name, 'args': list(args)})
            self.sock.send(bson_out)
            # blocking here
            bson_in = self.sock.recv()
            doc = bson.loads(bson_in)
            err = doc['error_code']
            if (err == 0):
                result = doc['result']
            else:
                result = doc['error_msg']
            return err, result

        return invoke_remote_func

    def disconnect(self):
        self.sock.close()
        self.ctx.term()

def connect(host, port=None):
    proxy = Proxy(host, port)
    return proxy

