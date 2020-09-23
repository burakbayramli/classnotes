import numpy as np
import numpy as np, sys
import ctypedbytes
import cStringIO
import struct           # For raw string I/O

print '-----------------------------------------------'
arr = np.array([23.443,88.33,0.333,8.433,3434.01])
sio = cStringIO.StringIO()
myfmt='f'*len(arr)
bin=struct.pack(myfmt,*arr)
print "bin", bin, len(bin)
arr2 = np.array(struct.unpack(myfmt,bin))
print type(arr2)
print "extracted", arr2


print '-----------------------------------------------'
print "text", len(";".join(map(str,arr))), ";".join(map(str,arr))
print '-----------------------------------------------'
sio = cStringIO.StringIO()
output = ctypedbytes.Output(sio)
output.writes(arr)
print sio.getvalue()
