from bson_rpc.client import connect
import time

if __name__ == '__main__':
    host = '127.0.0.1'
    port = 8181

    conn = connect(host, port)
    print('connected to server %s' % host)

    conn.use_service(['add']);

    time.sleep(30);

    err, res = conn.add(1,2)
    print('result: %s' % str(res))

    conn.disconnect();
    print('disconnected from server %s' % host)
