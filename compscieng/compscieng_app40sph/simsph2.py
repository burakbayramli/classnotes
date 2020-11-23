import csv, numpy as np, re
from mayavi import mlab
mlab.options.offscreen = True

BS = 500.0

csvx = open('/tmp/simsph-x.csv')
rdx = csv.reader(csvx,delimiter=';')
itx = iter(rdx)

csvy = open('/tmp/simsph-y.csv')
rdy = csv.reader(csvy,delimiter=';')
ity = iter(rdy)

csvz = open('/tmp/simsph-z.csv')
rdz = csv.reader(csvz,delimiter=';')
itz = iter(rdz)

for i in range(40):
    print (i)
    
    rowx = next(itx)
    rowx = [float(x) if re.search('[0-9]*\.?[0-9]', x) else 0 for x in rowx]
    x = np.array(rowx)

    rowy = next(ity)
    rowy = [float(x) if re.search('[0-9]*\.?[0-9]', x) else 0 for x in rowy]
    y = np.array(rowy)

    rowz = next(itz)
    rowz = [float(x) if re.search('[0-9]*\.?[0-9]', x) else 0 for x in rowz]
    z = np.array(rowz)

    r = np.ones(len(rowx))*10.0
    
    fig = mlab.figure(figure=None, fgcolor=(0., 0., 0.), bgcolor=(1, 1, 1), engine=None)
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

    mlab.view(azimuth=50, elevation=80, focalpoint=[200, 200, 200], distance=1600.0, figure=fig)
    mlab.savefig(filename='/tmp/simsph/out-%02d.png' % i)

