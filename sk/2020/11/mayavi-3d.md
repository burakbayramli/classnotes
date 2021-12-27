# Üç Boyutlu Grafikler İçin Mayavi

Simulasyon amaçlı pek çok parçacığı ekranda çizmemiz hareketlerini
takip etmemiz gerekebilir. Fakat çok hızlı olarak bilinen bir dilden,
mesela C++'dan bile, OpenGL çağırıp ekranda parçacıkları
`glutSolidSphere` ile çizdirmemiz hızlı bir cevap almamız için yeterli
olmayabilir. Belki arka planda, gözükmeyecek toplar vardır, bunları
göstermeye gerek var mı?

Bilgisayar grafikleme algoritmalarından (ki bilgisayar oyunlarında
yaygın şekilde kullanılır) ışın takip etme (ray tracing) burada
gerekli olabilir, görüntüye bakılma açısından sanal ışınlar hayal edip
bunların sadece ilk çarpıktıklarını çizmek, arka plana kalanları
çizmemek bir hızlandırıcı ilerleme olurdu.

![](mayavi4.png)

O zaman teker teker top çiz demek yerine tüm topların kordinatlarını
bir seferde verip özel bir kütüphanenin görünecek olan objelere karar
vermesi daha mantıklı olabilir. MayaVi ve onun arka planda kullandığı
VTK işte bu işi başarıyor.

Kurmak icin

```
pip install pyqt5 mayavi
```

Birkac tane top cizelim,

```python
from mayavi import mlab
mlab.options.offscreen = True
```

```python
mlab.figure(fgcolor=(0., 0., 0.), bgcolor=(1, 1, 1))
N = 100
c = np.random.rand(N, 3)
r = np.random.rand(N) / 10.
mlab.points3d(c[:, 0], c[:, 1], c[:, 2], r, color=(0.2, 0.4, 0.5))
mlab.outline()
mlab.savefig(filename='mayavi1.png')
```

![](mayavi1.png)


Şimdi 80,000 tane topu, arka arkaya iki grafikte çizdirelim,


```python
mlab.figure(fgcolor=(0., 0., 0.), bgcolor=(1, 1, 1))
N = 80000
c = np.random.rand(N, 3)
r = np.random.rand(N) / 10.
mlab.points3d(c[:, 0], c[:, 1], c[:, 2], r, color=(0.2, 0.4, 0.5))
mlab.outline()
mlab.savefig(filename='mayavi2.png')

mlab.figure(fgcolor=(0., 0., 0.), bgcolor=(1, 1, 1))
N = 80000
c = np.random.rand(N, 3)
r = np.random.rand(N) / 10.
mlab.points3d(c[:, 0], c[:, 1], c[:, 2], r, color=(0.2, 0.4, 0.5))
mlab.outline()
mlab.savefig(filename='mayavi3.png')
```

![](mayavi2.png)
![](mayavi3.png)

Bu iki grafikleme oldukca hızlı şekilde geri döndü. Eğer bir sıvı
dinamiğini simüle ediyorsak ve milyonlarca parçacık varsa bu perfomans
belki anlık ekranda göstermek için yeterli olmayabilir, fakat arka
planda birkaç dakika beklenerek tüm simülasyonun gidişatı belli birkaç
kare yanyana koyularak kabul edilir bir zaman içinde yaratılabilir.

Kordinat Eksenleri

Eğer `outline` kullanmak yerine sabit büyüklükte, x,y,z kordinat
eksenlerini göstermek istersek, bunu teker teker kendimizin yapması
lazım. Altta orijinde duran her kenarı 2 birim büyüklükte bir küp
gösteriyoruz,

```python
BS = 2.0
N = 100
c = np.random.rand(N, 3)*2.0
r = np.ones(N)*0.1

fig = mlab.figure(figure=None, fgcolor=(0., 0., 0.), bgcolor=(1, 1, 1), engine=None)
color=(0.2, 0.4, 0.5)
mlab.points3d(c[:, 0], c[:, 1], c[:, 2], r, color=color, colormap = 'gnuplot', scale_factor=1, figure=fig)
mlab.points3d(0, 0, 0, 0.1, color=(1,0,0), scale_factor=1.0, figure=fig)

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

mlab.view(azimuth=50, elevation=80, focalpoint=[1, 1, 1], distance=8.0, figure=fig)

mlab.savefig(filename='mayavi5.png')
```

![](mayavi5.png)

Problemler

Eğer ardı ardına grafik basıyorsak, döngü dışında `fig = mlab.figure`
komutu kullanıp, her döngü sonunda `mlab.savefiğ` ardından
`mlab.clf()` uygulamak hafıza problemlerini çözecektir. 

Kaynaklar

[1] https://docs.enthought.com/mayavi/mayavi/mlab.html

[2] https://www.toptal.com/data-science/3d-data-visualization-with-open-source-tools-an-example-using-vtk




