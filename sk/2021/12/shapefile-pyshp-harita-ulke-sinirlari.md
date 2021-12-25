# Ulke Sinirlari, SHP Shapefile, PySHP

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
sh = sf.shapes()[2]
geo = [[x[0],x[1]] for x in sh.points]
geo = np.array(geo)
plt.plot(geo[:,0],geo[:,1],'gd')

sh = sf.shapes()[4]
geo = [[x[0],x[1]] for x in sh.points]
geo = np.array(geo)
plt.plot(geo[:,0],geo[:,1],'rd')

plt.savefig('out.png')
```

```text
246
```





Kaynaklar

[1] https://thematicmapping.org/downloads/world_borders.php

[2] Sileika, Pro Python System Administration