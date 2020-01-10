#encoding:utf-8
import os
import pika
import pickle

# 建立一个实例
connection = pika.BlockingConnection(
    pika.ConnectionParameters('localhost', 5672)  # 默认端口5672，可不写
)
# 声明一个管道，在管道里发消息
channel = connection.channel()

pid = os.fork()

if pid:
    # 父进程
    stat = os.wait()
    print(stat)
    connection.close()  # 队列关闭
else:
    # 子进程
    # 在管道里声明queue
    channel.queue_declare(queue='hello')
    # RabbitMQ a message can never be sent directly to the queue, it always needs to go through an exchange.
    channel.basic_publish(exchange='',
                          routing_key='hello',  # queue名字
                          body='Hello World!')  # 消息内容
    print(" [x] Sent 'Hello World!'")

    channel.queue_declare(queue='echo')
    channel.basic_publish(exchange='',
                          routing_key='echo',  # queue名字
                          body=pickle.dumps({'cmd':'echo', 'text':'Echo!'}))  # 消息内容
    print(" [x] Sent 'Echo!'")
