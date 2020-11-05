# Matplotlib, Pylab ve hareketli plot, animasyonlar

Bir kordinat sistemi uzerinde canli olarak bir hesabin sonucunu
seyretmek istersek, Pylab icin faydali bir ornek kod altta. Kod arka
arkaya 10 tane x,y degeri uretiyor, sayilar 0..1 arasi, ve eksenlerin
sabit kalmasi icin set_xlim, set_ylim cagrilarini yapmak lazim.from
pylab import *from random import *from time import *ion()fig =
plt.figure()ax = fig.add_subplot(111)for i in xrange(10): sleep(0.5)
ax.plot([random()], [random()], 'd') ax.set_xlim(0, 1) ax.set_ylim(0,
1) hold(False) draw()




