from pylab import *
import time

xmax = 10.
xmin = -10.
D = 20
x = linspace(xmin, xmax, D)
xlim(-10,10)
ylim(-10,10)

for i,t in enumerate(linspace(-3., 3., 30)):
    if i % 4 == 0: 
        plot (x,((2. - x)/2.))
        plt.hold(True)
        xx=2*cos(t)**2
        yy=sin(t)**2
        quiver(0,0,xx,yy)
        plt.hold(True)
        plot(xx,yy,'rd')
        plt.hold(True)
        plt.savefig('1i3_' + str(i) + '.png')
        plt.hold(False)

