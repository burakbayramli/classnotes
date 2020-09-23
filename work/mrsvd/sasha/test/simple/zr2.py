import zmq
 
context = zmq.Context()
 
sock = context.socket(zmq.SUB)
#sock.connect('tcp://127.0.0.1:2000')
sock.connect('tcp://host2:5000')
 
# This tells the consumer to drop all messages which
# don't begin with the text ARNOLD
sock.setsockopt(zmq.SUBSCRIBE, 'ARNOLD')
 
while True:
    input = sock.recv()
    print input
