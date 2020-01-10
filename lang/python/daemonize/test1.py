from time import sleep
from daemonize import Daemonize

pid = "/tmp/test.pid"


def main():
    while True:
        sleep(5)
        print '.'

daemon = Daemonize(app="test_app", pid=pid, action=main)
daemon.start()
