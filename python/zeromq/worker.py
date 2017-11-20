from __future__ import print_function
from gevent import spawn, spawn_later
import zmq.green as zmq
import time

ctx = zmq.Context() # create a new context to kick the wheels
sock = ctx.socket(zmq.PULL)
#sock.connect('ipc:///tmp/zmqtest')
sock.connect('tcp://127.0.0.1:5000')

def get_objs(sock):
    while True:
        o = sock.recv_pyobj()
        #o = sock.recv()
        #print('received python object:', o)
        print('received:', o)
        time.sleep(1)
        if o == 'quit':
            print('exiting.')
            break

#def print_every(s, t=None):
#	print(s)
#	if t:
#		spawn_later(t, print_every, s, t)

#print_every('printing every half second', 0.5)
#spawn(get_objs, sock).join()
get_objs(sock)

