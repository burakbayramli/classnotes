import multicast
from multicast import network

#receiver = multicast.MulticastUDPReceiver ("eth0", "238.0.0.1", 1234 )
#receiver = multicast.MulticastUDPReceiver ("lo", "238.0.0.1", 1234 )
#data = receiver.read()
#receiver.close()

config = network.ifconfig()

# arg is lo, eth0, wlan0, etc..

print config[sys.argv[1]].addresses
# ['10.0.0.1']
print config[sys.argv[1]].multicast
#True - eth0 supports multicast
print config[sys.argv[1]].up
#True - eth0 is up

