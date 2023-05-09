# NetCDF, İklim Verisi, NOAA

İklim, mesela Berkeley veri tabanından gelen sıcaklıklar, ya da
NOAA'dan gelen günlük, saatlik rüzgar verileri çoğunlukla netCDF adlı
bir formatla paylaşılıyor. Bu formatları işlemenin yöntemlerine, veri
içeriğine bakalım..

## Berkeley

Berkeley'den gelen dünya ısı verisi mesela.. Berkeley pek çok iklim
verisi paylaşır [3] . Bunlardan biri dünyayı belli bölgelere ayırıp ay
bazında sıcaklık verisini kaydeder. Veri için [3]'e gidilir ve Gridded
Data | Monthly Land | Equal Area alınır, `/tmp` altında olduğunu
düşünelim,

```python
import netCDF4

url='/tmp/Complete_TAVG_EqualArea.nc'
nc = netCDF4.Dataset(url)
print (nc)
```

```text
<class 'netCDF4._netCDF4.Dataset'>
root group (NETCDF4 data model, file format HDF5):
    Conventions: Berkeley Earth Internal Convention (based on CF-1.5)
    title: Native Format Berkeley Earth Surface Temperature Anomaly Field
    history: 06-Oct-2021 21:09:04
    institution: Berkeley Earth Surface Temperature Project
    source_file: Complete_TAVG.50592s.20211006T205528.mat
    source_history: 03-Oct-2021 08:32:25
    source_data_version: eead777c589734c11a431a21725e06d3
    comment: This file contains Berkeley Earth surface temperature anomaly field in our native equal-area format.
    dimensions(sizes): map_points(5498), time(3261), month_number(12)
    variables(dimensions): float32 longitude(map_points), float32 latitude(map_points), float64 time(time), float64 land_mask(map_points), float32 temperature(time, map_points), float32 climatology(month_number, map_points)
    groups: 
```

NetCDF değişik bir format, Pandas ya da numpy stiline benzemiyor. Mesela
`climatology`, `temperature` ve `time` öğeleri,

```python
clim = nc['climatology'][:,:]
anom =  nc['temperature'][:,:]
time =  nc['time'][:]

print (clim.shape)
print (anom.shape)
print (time.shape)
```

```text
(12, 5498)
(3261, 5498)
(3261,)
```

Şimdi `clim` içinde bir matris var, `anom` içinde bir tane daha,
`time` bir vektör. İçeriği şöyle,

```python
print (time[:10])
```

```text
[1750.04166667 1750.125      1750.20833333 1750.29166667 1750.375
 1750.45833333 1750.54166667 1750.625      1750.70833333 1750.79166667]
```

Biraz belge okuma sonrası anlaşılıyor ki zaman bir reel sayı olarak
temsil edilmiş, yani 1750 senesi ve 1751 senesi ortalarında bir yer,
aşağı yukarı Haziran ayı, 1750.4583 olarak temsil ediliyor. Bu
tür sene kodlaması genelde bazı grafikleme, hesaplama işlerini kolaylaştırmak
için yapılır. O zaman mesela üstte 1750.125 anında olanı görmek için indis
1 kullanmak lazım (ikinci öğe).

Dünya 5498 bölgeye bölünmüş bu bölgelerin nerede olduğunu anlamak için
Berkeley belgelerine bakılabilir.

Sıcaklık bir baz sıcaklık artı sapma (anormallik) olarak verilmiş.
Eğer 1000'inci bölge 1750.125 senesindeki sıcaklığı görmek istersek, 
alttaki erişimi yapmak gerekir (0.125 Şubat ayı olur herhalde indis 1)


```python
region = 1000
tidx = 1
month = 1
print (clim[month, region])
print (anom[tidx, region])
```

```text
-12.628022
0.026923507
```

Bu iki değer toplanınca nihai sıcaklık elde edilir. Yani baz sıcaklık
bir ay | bölge matrisi içindeydi, sapma bir zaman | bölge matrisi
içinde. Nihai sıcaklık için belli bir zamanın ayına ve bölgeye göre
baz, zaman indisi ve yine bölgeye göre sapma almak gerekti.

### NOAA NCEI

Bu veriyi dosyayı elle indirmeden işleyeceğiz, kodun kendisi gerekli
dosyayı Internet'ten alıp içindeki netCDF bilgisini işleyecek. Mesela
13/3/1993 için rüzgar esme (hız) verisini alalım, bu bilgi dikey ve
yatay bileşenler u,v içinde gelecek,

```python
from datetime import datetime
from netCDF4 import Dataset, num2date
from scipy.ndimage import gaussian_filter
from siphon.catalog import TDSCatalog

year = 1993
month = 3
day = 13
hour = 0

dt = datetime(year, month, day, hour)
base_url = 'https://www.ncei.noaa.gov/thredds/catalog/model-narr-a-files/'
cat = TDSCatalog(f'{base_url}{dt:%Y%m}/{dt:%Y%m%d}/catalog.xml')
ds = cat.datasets.filter_time_nearest(dt)
ncss = ds.subset()
query = ncss.query()
query.lonlat_box(north=60, south=18, east=300, west=225)
query.all_times()
query.add_lonlat()
query.accept('netcdf')
query.variables('Geopotential_height_isobaric',
                'Temperature_isobaric',
                'u-component_of_wind_isobaric',
                'v-component_of_wind_isobaric')

data = ncss.get_data(query)
```

```python
print (data.dimensions)
```

```text
OrderedDict([('time1', <class 'netCDF4._netCDF4.Dimension'>: name = 'time1', size = 1), ('isobaric1', <class 'netCDF4._netCDF4.Dimension'>: name = 'isobaric1', size = 29), ('y', <class 'netCDF4._netCDF4.Dimension'>: name = 'y', size = 119), ('x', <class 'netCDF4._netCDF4.Dimension'>: name = 'x', size = 268)])
```

```python
print (data)
```

```text
<class 'netCDF4._netCDF4.Dataset'>
root group (NETCDF3_CLASSIC data model, file format NETCDF3):
    Originating_or_generating_Center: US National Weather Service, National Centres for Environmental Prediction (NCEP)
    Originating_or_generating_Subcenter: North American Regional Reanalysis Project
    GRIB_table_version: 0,131
    Generating_process_or_model: North American Regional Reanalysis (NARR)
    Conventions: CF-1.6
    history: Read using CDM IOSP GribCollection v3
    featureType: GRID
    History: Translated to CF-1.0 Conventions by Netcdf-Java CDM (CFGridWriter2)
Original Dataset = DatasetScan#narr-a_221_19930313_0000_000.grb; Translation Date = 2022-01-08T09:37:42.079Z
    geospatial_lat_min: 10.753308882144761
    geospatial_lat_max: 46.8308828962289
    geospatial_lon_min: -153.88242040519995
    geospatial_lon_max: -42.666108129242815
    dimensions(sizes): time1(1), isobaric1(29), y(119), x(268)
    variables(dimensions): float32 u-component_of_wind_isobaric(time1, isobaric1, y, x), float64 time1(time1), float32 isobaric1(isobaric1), float32 y(y), float32 x(x), int32 LambertConformal_Projection(), float64 lat(y, x), float64 lon(y, x), float32 Geopotential_height_isobaric(time1, isobaric1, y, x), float32 Temperature_isobaric(time1, isobaric1, y, x), float32 v-component_of_wind_isobaric(time1, isobaric1, y, x)
    groups: 
```

Boyutları, ve bazı örnek veriyi, kullanımı altta görüyoruz,

```python
u = data.variables['u-component_of_wind_isobaric'][0]
v = data.variables['v-component_of_wind_isobaric'][0]
lat = data.variables['lat'][:]
lon = data.variables['lon'][:]
print (u.shape)
print (lat.shape)
print (lat[:4])

u_wind_var = data.variables['u-component_of_wind_isobaric']
u_wind = u_wind_var[0, 0, :, :].squeeze()
print (u_wind.shape)
```

```text
(29, 119, 268)
(119, 268)
[[17.87115668 17.9637901  18.05570545 ... 11.17123674 11.04457979
  10.91753102]
 [18.10795748 18.20108494 18.29349116 ... 11.37401449 11.2467457
  11.11908431]
 [18.34510656 18.4387305  18.53163001 ... 11.57699036 11.44910733
  11.32083096]
 [18.58259681 18.67671966 18.77011491 ... 11.78015756 11.65165792
  11.52276419]]
(119, 268)
```

## THREDDS Verisi

Bu veri [1,2]'den geliyor, 

```python
import netCDF4

url = "/tmp/uwnd.10m.2021.nc"
nc = netCDF4.Dataset(url)
print (nc)
```

```text
<class 'netCDF4._netCDF4.Dataset'>
root group (NETCDF4_CLASSIC data model, file format HDF5):
    Conventions: CF-1.2
    centerlat: 50.0
    centerlon: -107.0
    comments: 
    institution: National Centers for Environmental Prediction
    latcorners: [ 1.000001  0.897945 46.3544   46.63433 ]
    loncorners: [-145.5       -68.32005    -2.569891  148.6418  ]
    platform: Model
    standardpar1: 50.0
    standardpar2: 50.000001
    title: Daily NARR
    history: created Sat Mar 26 05:25:45 MDT 2016 by NOAA/ESRL/PSD
    dataset_title: NCEP North American Regional Reanalysis (NARR)
    references: https://www.esrl.noaa.gov/psd/data/gridded/data.narr.html
    source: http://www.emc.ncep.noaa.gov/mmb/rreanl/index.html
    References: 
    dimensions(sizes): time(334), y(277), x(349), nbnds(2)
    variables(dimensions): float64 time(time), float32 lat(y, x), float32 lon(y, x), float32 y(y), float32 x(x), int32 Lambert_Conformal(), float32 uwnd(time, y, x), float64 time_bnds(time, nbnds)
    groups: 
```

```python
lat = nc['lat'][:]
print (lat.shape)
lon = nc['lon'][:]
print (lon.shape)
uwnd = nc['uwnd'][:,:,:]
print (uwnd.shape)
```

```text
(277, 349)
(277, 349)
(334, 277, 349)
```

Kaynaklar

[1] THREDDS Catalog, https://psl.noaa.gov/thredds/catalog/Datasets/NARR/catalog.html

[2] THREDDS Data Server, https://psl.noaa.gov/thredds/catalog/Datasets/NARR/Dailies/monolevel/catalog.html?dataset=Datasets/NARR/Dailies/monolevel/uwnd.10m.2021.nc

[3] http://berkeleyearth.org/data/





