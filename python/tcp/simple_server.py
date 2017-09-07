#coding:utf-8
from socket import *

IP = '127.0.0.1'
PORT = 8181
ip_port = (IP, PORT)
sock = socket(AF_INET, SOCK_STREAM)
sock.bind(ip_port)

MAX_CONN = 10
sock.listen(MAX_CONN)

count = 0
while True:
    count += 1
    conn, addr = sock.accept()
    print 'server {} being connected from {}'.format(count, addr)

    # we will block here... :(
    while True:
        data = conn.recv(1024)
        print data
        conn.sendall(bytes('ok'))

    conn.close()
    print 'server {} shutdown'.format(count)

sock.close()
