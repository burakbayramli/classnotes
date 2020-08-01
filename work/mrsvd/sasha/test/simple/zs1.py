# producer
import zmq, time

context = zmq.Context()
socket = context.socket(zmq.PUB)
#socket.connect('epgm://eth1;239.192.1.1:5000')
#socket.connect('epgm://eth1;224.0.0.251:5000')
#socket.connect('epgm://wlan0;224.0.0.1:5000') 
socket.connect('pgm://eth1;239.192.1.1:5000')
import random
while True:
    socket.send("10001 kjashfkjasdf" + str(random.random()))
    #time.sleep(1)
