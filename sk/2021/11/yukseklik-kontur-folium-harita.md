# Yukseklik Verisini Kontur ile Folium Haritasinda Gostermek


```python
#36.61599, 29.06161
#36.70624, 29.20870
clat,clon=36.64653, 29.13920
```


contours matlab
https://www.tutorialspoint.com/how-to-get-coordinates-from-the-contour-in-matplotlib
https://stackoverflow.com/questions/19418901/get-coordinates-from-the-contour-in-matplotlib


```python
import matplotlib
import numpy as np
import matplotlib.cm as cm
import matplotlib.mlab as mlab
import matplotlib.pyplot as plt

matplotlib.rcParams['xtick.direction'] = 'out'
matplotlib.rcParams['ytick.direction'] = 'out'

def bivariate_normal(X, Y, sigmax=1.0, sigmay=1.0,
                 mux=0.0, muy=0.0, sigmaxy=0.0):
    Xmu = X-mux
    Ymu = Y-muy
    rho = sigmaxy/(sigmax*sigmay)
    z = Xmu**2/sigmax**2 + Ymu**2/sigmay**2 - 2*rho*Xmu*Ymu/(sigmax*sigmay)
    denom = 2*np.pi*sigmax*sigmay*np.sqrt(1-rho**2)
    return np.exp(-z/(2*(1-rho**2))) / denom

delta = 0.025
x = np.arange(29.06161, 29.20870, delta)
y = np.arange(36.61599, 36.70624, delta)
X, Y = np.meshgrid(x, y)
Z = 10*bivariate_normal(X, Y, 0.02, 0.02, 29.13920, 36.64653)

plt.figure()
#CS = plt.contour(X, Y, Z, levels=[0.2, 1.0] )
CS = plt.contour(X, Y, Z )
plt.clabel(CS, inline=1, fontsize=10)
plt.title('Simplest default with labels')
plt.savefig('elev1.png')
```

```python
print (len(CS.allsegs))
print (CS.allsegs[1])
```

```text
9
[array([[29.16850299, 36.61599   ],
       [29.18289603, 36.64099   ],
       [29.17913935, 36.66599   ],
       [29.16161   , 36.68398265],
       [29.13661   , 36.68902999],
       [29.11161   , 36.67980347],
       [29.0986095 , 36.66599   ],
       [29.09359308, 36.64099   ],
       [29.11161   , 36.61651895],
       [29.11231435, 36.61599   ]])]
```


```python
plt.figure()
#CS = plt.contour(X, Y, Z, levels=[0.2, 1.0] )
CS = plt.contour(X, Y, Z)
#print (CS.allsegs[0][0].shape)
li = 0
for i in range(len(CS.allsegs)):
    for li in range(len(CS.allsegs[i])):
        x = CS.allsegs[i][li][:,0]
        y = CS.allsegs[i][li][:,1]
        plt.plot(x,y,'r-')
plt.savefig('elev3.png')
```

```text
(36, 2)
```

```python
import folium

m = folium.Map(location=[clat, clon], zoom_start=11, tiles="Stamen Terrain")
for i in range(len(CS.allsegs)):
    points = []
    for li in range(len(CS.allsegs[i])):
        x = CS.allsegs[i][li][:,0]
        y = CS.allsegs[i][li][:,1]
        for lon,lat in zip(x,y): points.append([lat,lon])
        folium.PolyLine(points, color='red', weight=1.0, opacity=1).add_to(m)

m.save('elev.html')
```

```python
print (CS.levels)
```

```text
[   0.  500. 1000. 1500. 2000. 2500. 3000. 3500. 4000.]
```
















