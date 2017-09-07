#coding:utf-8
import SocketServer

class MyServer(SocketServer.BaseRequestHandler):
    def handle(self):
        conn = self.request
        conn.sendall('我是多线程')

        Flag = True
        while Flag:
            data = conn.recv(1024)
            if data != None:
                data = data.strip()
                if data == 'exit':
                    Flag = False
                elif data == '0':
                    conn.sendall('0')
                else:
                    conn.sendall('未知的命令')
            else:
                conn.sendall('请重新输入.')

if __name__ == '__main__':
    server = SocketServer.ThreadingTCPServer(('127.0.0.1',8181), MyServer)
    server.serve_forever()
