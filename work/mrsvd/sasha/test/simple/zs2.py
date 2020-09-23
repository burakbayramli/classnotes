import zmq
import random
from time import sleep
 
context = zmq.Context()
socket = context.socket(zmq.PUB)
#socket.bind('tcp://127.0.0.1:2000')
socket.bind('tcp://host2:5000')
 
while True:
    face = "Facepalmer"
    randNumber = random.randrange(0,999999)
 
    # This guy should show up to the client
    socket.send("ARNOLD %s %d" % (face, randNumber))
 
    # This will be dropped by the client as it's
    # filtering messages where ARNOLD is the first
    # string
    socket.send("FACEPALMER ARNOLD %s %d" % (face,
        randNumber))
 
    # This will also show up in the client as the
    # filter is simply checking to see that the pattern
    # matches the front of the string, you would need
    # to add a space to the consumer's filter to make
    # this message get dropped
    socket.send("ARNOLDFACEPALMER %s %d" % (face,
        randNumber))
 
    sleep(1)
