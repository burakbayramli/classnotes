import zmq, time

context = zmq.Context()
sender = context.socket(zmq.PUSH)
sender.connect("tcp://host3:10000")
sender.send("start")
for i in range(100000):
    sender.send("01234567890123456789")
sender.send("end")
