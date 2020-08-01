import zmq
 
context = zmq.Context()
socket = context.socket(zmq.SUB)
#socket.connect('pgm://224.0.0.1:5000')
#socket.connect('epgm://wlan0;239.192.1.1:5000')
#socket.connect('epgm://wlan0;224.0.0.251:5000')
socket.connect('pgm://wlan0;224.0.0.251:5000')
#socket.connect('pgm://eth1;239.192.1.1:5000')
socket.setsockopt(zmq.SUBSCRIBE, "10001")
 
while True:
    data = socket.recv()
    print data
