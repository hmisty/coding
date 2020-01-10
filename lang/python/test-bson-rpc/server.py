from bson_rpc import rpc, start_server

@rpc
def add(a, b):
    return a + b

def main(host, port):
    start_server(host, port)

if __name__ == '__main__':
    host = '127.0.0.1'
    port = 8181
    main(host, port)
