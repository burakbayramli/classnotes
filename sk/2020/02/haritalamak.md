# Haritalamak

Harita grafiklemek için eski `basemap` artık kullanılmıyor, yeni paket
`cartopy`. Kurmak,

`pip install cartopy`

Dünya haritası üzerinde bir nokta

```python
import cartopy.crs as ccrs
import cartopy
fig = plt.figure(figsize=(5, 2))
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
ax.set_global()
ax.stock_img()
ax.coastlines()
ax.plot(42.0, 21.53, 'ro', transform=ccrs.PlateCarree())

plt.savefig('har1.png')
```

![](har1.png)
![](https://4.bp.blogspot.com/-ewYmsqMhLBA/XlOhT0jOQLI/AAAAAAAAB6c/eASAITC5i_YdiYXLDoBBpFC6LzuE5JOhgCLcBGAsYHQ/s320/har1.png)

Ülke ismi bazlı tüm ülkeyi renklendirmek,

```python
import matplotlib.pyplot as plt
import cartopy.io.shapereader as shpreader
import cartopy.crs as ccrs
import cartopy.feature as cfeature

def area(ax, iso, clr) :
    shp = shpreader.natural_earth(resolution='10m',category='cultural',
                                  name='admin_0_countries')
    reader = shpreader.Reader(shp)
    for n in reader.records() :
        if n.attributes['ADM0_A3'] == iso: 
            ax.add_geometries(n.geometry, ccrs.PlateCarree(), facecolor=clr) 
    return ax

fig = plt.figure(figsize=(5, 2))
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
ax = plt.axes(projection=ccrs.PlateCarree())
ax.stock_img()
ax.coastlines()
area(ax, "USA", "red")
plt.savefig('har2.png')
```

![](har2.png)
![](https://2.bp.blogspot.com/-0uavwSkj25c/XlOhWkqxoqI/AAAAAAAAB6g/UeutLAQFUwo0AUcGzZ-Th6fIEbiqthv6ACLcBGAsYHQ/s320/har2.png)


Kaynaklar

https://rabernat.github.io/research_computing_2018/maps-with-cartopy.html

https://scitools.org.uk/cartopy/docs/latest/gallery/global_map.html#sphx-glr-gallery-global-map-py

https://github.com/SciTools/cartopy/issues/1303

Renk isimleri - https://matplotlib.org/3.1.0/gallery/color/named_colors.html



