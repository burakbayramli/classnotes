# Based on the x,y,z location for all particles, at all time steps
# construct image snapshots for those time steps
#
# convert -delay 30 /tmp/sim/*.png $HOME/Downloads/sim.gif
#

import csv, numpy as np, re, os
from mayavi import mlab
mlab.options.offscreen = True

BS = 2.0

if (os.path.exists("/tmp/sim") == False):
    os.mkdir("/tmp/sim")
    
csvx = open('coord-x.dat')
rdx = csv.reader(csvx,delimiter=':')
itx = iter(rdx)

csvy = open('coord-z.dat')
rdy = csv.reader(csvy,delimiter=':')
ity = iter(rdy)

csvz = open('coord-y.dat')
rdz = csv.reader(csvz,delimiter=':')
itz = iter(rdz)

fig = mlab.figure(figure=None, fgcolor=(0., 0., 0.), bgcolor=(1, 1, 1), engine=None)
for i in range(100):
    print (i)
    
    rowx = next(itx)
    rowx = [float(x) if re.search('[0-9]*\.?[0-9]', x) else 0 for x in rowx]
    x = np.array(rowx)
    x += 1.0

    rowy = next(ity)
    rowy = [float(x) if re.search('[0-9]*\.?[0-9]', x) else 0 for x in rowy]
    y = np.array(rowy)
    y += 1.0

    rowz = next(itz)
    rowz = [float(x) if re.search('[0-9]*\.?[0-9]', x) else 0 for x in rowz]
    z = np.array(rowz)
    z += 1.0
    
    r = np.ones(len(x))*0.03
    
    color=(0.2, 0.4, 0.5)
    mlab.points3d(x, y, z, r, color=color, colormap = 'gnuplot', scale_factor=1, figure=fig)
            
    mlab.plot3d([0.0,0.0],[0.0, 0.0],[0.0, BS], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([0.0,BS],[0.0, 0.0],[0.0, 0.0], color=(1,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([0.0,0.0],[0.0, BS],[0.0, 0.0], color=(0,1,0), tube_radius=None, figure=fig)
    mlab.plot3d([0.0,0.0],[0.0, BS],[BS, BS], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([0.0,BS],[0.0,0.0],[BS,BS], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([BS,BS],[0.0,BS],[BS,BS], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([BS,0],[BS,BS],[BS,BS], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([0,0],[BS,BS],[BS,0], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([BS,BS],[0.0,0.0],[0.0,BS], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([BS,BS],[0.0,BS],[0.0,0.0], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([BS,0.0],[BS,BS],[0.0,0.0], color=(0,0,0), tube_radius=None, figure=fig)
    mlab.plot3d([BS,BS],[BS,BS],[0.0,BS], color=(0,0,0), tube_radius=None, figure=fig)

    mlab.view(azimuth=50, elevation=80, focalpoint=[1, 0.2, 1.1], distance=10.0, figure=fig)
    mlab.savefig(filename='/tmp/sim/out-%02d.png' % i)
    mlab.clf()
    #exit()
