import sys
import time
import zmq

context = zmq.Context()
receiver = context.socket(zmq.PULL)
#receiver.bind("tcp://host2:10000")
receiver.bind("tcp://*:10000")

arr = []
while True:
    data = receiver.recv()
    if data == "start":
        print time.time()
        continue
    arr.append(data)
    if data == "end":
        print time.time()
        print len(arr)
        continue
        
