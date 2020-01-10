'''
Created on 2012-1-5
The example client program uses some sockets to demonstrate how the server
with select() manages multiple connections at the same time . The client
starts by connecting each TCP/IP socket to the server
@author: peter
'''

import socket

messages = ["This is the message" ,
            "It will be sent" ,
            "in parts "]

print "Connect to the server"

#server_address = ("192.168.1.102",10001)
server_address = ("127.0.0.1",10001)

#Create a TCP/IP sock

socks = []

for i in range(10):
    socks.append(socket.socket(socket.AF_INET,socket.SOCK_STREAM))

for s in socks:
    s.connect(server_address)

counter = 0
for message in messages :
    #Sending message from different sockets
    for s in socks:
        counter+=1
        print "  %s sending %s" % (s.getpeername(),message+" version "+str(counter))
        s.send(message+" version "+str(counter))

    #Read responses on both sockets
    for s in socks:
        data = s.recv(1024)
        print " %s received %s" % (s.getpeername(),data)
        if not data:
            print "closing socket ",s.getpeername()
            s.close()
