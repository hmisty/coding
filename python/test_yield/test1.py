#encoding:UTF-8
import time

def yield_test(n):
    for i in range(n):
        yield call(i)
        print("i=",i)

        #做一些其它的事情
        print("do something.")
        print("end.")

def call(i):
    if i % 2 == 0:
        time.sleep(1.0)

    return i*2

#使用for循环
for i in yield_test(5):
    print(i,",")

