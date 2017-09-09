# ref: http://www.jb51.net/article/111899.htm
import socket
import select

s = socket.socket()
s.bind(('127.0.0.1',8888))
s.listen(5)
epoll_obj = select.epoll()
epoll_obj.register(s,select.EPOLLIN)
connections = {}
while True:
    events = epoll_obj.poll()
    for fd, event in events:
        print(fd,event)
        if fd == s.fileno():
            conn, addr = s.accept()
            connections[conn.fileno()] = conn
            epoll_obj.register(conn,select.EPOLLIN)
            msg = conn.recv(200)
            conn.sendall('ok'.encode())
        else:
            try:
                fd_obj = connections[fd]
                msg = fd_obj.recv(200)
                fd_obj.sendall('ok'.encode())
            except BrokenPipeError:
                epoll_obj.unregister(fd)
                connections[fd].close()
                del connections[fd]

s.close()
epoll_obj.close()
