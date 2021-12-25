# Haritalama, Ülke Sınırları, SHP Shapefile, PySHP

```python
import shapefile
sf = shapefile.Reader("TM_WORLD_BORDERS-0.3.shp", encoding = "ISO8859-1")
```

```python
r = sf.records()
countries = sf.shapes()
print (len(countries))
idx = 2
country = countries[idx]
name = r[idx]
print (len(country.points))
print (country.parts)
print (name)
```

```text
246
871
[0, 108, 118, 131, 859]
Record #2: ['AJ', 'AZ', 'AZE', 31, 'Azerbaijan', 8260, 8352021, 142, 145, 47.395, 40.43]
```



```python
import shapefile
sf = shapefile.Reader("TM_WORLD_BORDERS-0.3.shp", encoding = "ISO8859-1")
r = sf.records()
countries = sf.shapes()

def plot_country(idx,color='r'):
   country = countries[idx]
   name = r[idx]
   print (name)
   bounds = list(country.parts) + [len(country.points)]
   print (bounds)
   for previous, current in zip(bounds, bounds[1:]):    
       geo = [[x[0],x[1]] for x in country.points[previous:current]]
       if len(geo) < 1: continue
       geo = np.array(geo)
       if geo.shape[0] > 0:
           plt.plot(geo[:,0],geo[:,1],color)

plot_country(2,'r')
plot_country(4,'b')

plt.savefig('harita-az-arm.png')
```

```text
Record #2: ['AJ', 'AZ', 'AZE', 31, 'Azerbaijan', 8260, 8352021, 142, 145, 47.395, 40.43]
[0, 108, 118, 131, 859, 871]
Record #4: ['AM', 'AM', 'ARM', 51, 'Armenia', 2820, 3017661, 142, 145, 44.563, 40.534]
[0, 12, 395, 408, 418]
```

![](harita-az-arm.png)


















Su Bolgeleri (Nehirler, Goller)

```python
import shapefile
sf = shapefile.Reader("EC_Waterbodies/EC_Waterbodies.shp")
r = sf.records()
cs = sf.shapes()
```

```python
idx = 0
print (len(r))
print (r[idx])
c = cs[idx]
print (c.parts)
```

```text
2662
Record #0: ['Grand River', 'Stream or River', '', '', '', 'GISOWNER', datetime.date(2020, 5, 13), 'GISOWNER', datetime.date(2020, 5, 13), '780.593255083315', 'To Webster', 927.38186506, '{32EB1434-EA18-4BE0-AD81-339BC12796E3}', 375.29812566, 12]
[0, 3099, 3108, 3116, 3125, 3139, 3146, 3154, 3164, 3176, 3192, 3212, 3239, 3264, 3278, 3298, 3353, 3392, 3553, 7868, 7885, 7916, 7964, 7988, 8016, 8048, 8096, 8135, 8165, 8344, 9671, 9678, 9692, 9703, 9713, 9728, 9760, 9825, 9925, 10377, 10383, 10392, 10401, 10421, 10445, 10464, 10486, 10520]
```

```python
def plot_water(idx):
   country = countries[idx]
   name = r[idx]
   print (name)
   bounds = list(country.parts) + [len(country.points)]
   print (bounds)
   for previous, current in zip(bounds, bounds[1:]):    
       geo = [[x[0],x[1]] for x in country.points[previous:current]]
       if len(geo) < 1: continue
       geo = np.array(geo)
       if geo.shape[0] > 0:
           plt.fill(geo[:,0],geo[:,1],'lightblue')

plot_water(2)

plt.savefig('out.png')

```

```text
Record #2: ['', 'Open Water', '', '', 'Eaton', 'GISOWNER', datetime.date(2020, 5, 13), 'GISOWNER', datetime.date(2020, 5, 13), '826.402008942686', '', 0.44385531, '{5237FBCC-DB5E-46AE-AD69-65A569847AA1}', 0.17962187, 7]
[0, 97]
```


















Kaynaklar

[1] https://thematicmapping.org/downloads/world_borders.php

[2] Sileika, Pro Python System Administration


