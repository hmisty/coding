from __future__ import print_function
from gevent import spawn, spawn_later
import zmq.green as zmq

# server
print(zmq.Context)
ctx = zmq.Context()
sock = ctx.socket(zmq.PUSH)
#sock.bind('ipc:///tmp/zmqtest')
sock.bind('tcp://127.0.0.1:5000')

print('bound')

x = spawn(sock.send_pyobj, ('this', 'is', 'a', 'python', 'tuple'))
x.join()
#spawn_later(1, sock.send_pyobj, {'hi': 1234})
#spawn_later(2, sock.send_pyobj, ({'this': ['is a more complicated object', ':)']}, 42, 42, 42))
#spawn_later(3, sock.send_pyobj, 'foobar')
#spawn_later(4, sock.send_pyobj, 'quit')


#sock.send_pyobj(('this', 'is', 'a', 'python', 'tuple'))
#sock.send_pyobj({'hi': 1234})
#sock.send_pyobj({'this': ['is a more complicated object', ':)']})
#sock.send_pyobj('foobar')
#sock.send_pyobj('quit')
