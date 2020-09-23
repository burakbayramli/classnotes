import zmq, sys
 
context = zmq.Context()
socket = context.socket(zmq.SUB)
if sys.argv[1] == 'local':
    print "local"
    socket.connect('tcp://*:5000')    
else:
    socket.connect('epgm://wlan0;224.0.0.251:5000')
    
socket.setsockopt(zmq.SUBSCRIBE, "10001")
 
while True:
    data = socket.recv()
    print data
