import zmq, time, sys

context = zmq.Context()
socket = context.socket(zmq.PUB)

if sys.argv[1] == 'local':
    print "local"
    socket.bind('tcp://127.0.0.1:5000')
else:
    socket.bind('epgm://wlan0;239.192.1.1:5000')
    
while True:
    socket.send("10001 lasjflaskdf")
    time.sleep(1)
