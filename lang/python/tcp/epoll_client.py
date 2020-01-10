import socket

flag = 1
s = socket.socket()
s.connect(('127.0.0.1',8888))
while flag:
    input_msg = input('input>>>')
    if input_msg == '0':
        break

    s.sendall(input_msg.encode())
    msg = s.recv(1024)
    print(msg.decode())

s.close()
