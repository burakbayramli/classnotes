# Ruzgar Verisi Indirmek


```python
from datetime import datetime

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.pyplot as plt
import numpy as np

from netCDF4 import Dataset, num2date
from scipy.ndimage import gaussian_filter
from siphon.catalog import TDSCatalog

# Case Study Date
year = 1993
month = 3
day = 13
hour = 0

dt = datetime(year, month, day, hour)

# Read NARR Data from THREDDS server
base_url = 'https://www.ncei.noaa.gov/thredds/catalog/model-narr-a-files/'

# Programmatically generate the URL to the day of data we want
cat = TDSCatalog(f'{base_url}{dt:%Y%m}/{dt:%Y%m%d}/catalog.xml')

# Have Siphon find the appropriate dataset
ds = cat.datasets.filter_time_nearest(dt)

# Interface with the data through the NetCDF Subset Service (NCSS)
ncss = ds.subset()

# Create an NCSS query with our desired specifications
query = ncss.query()
query.lonlat_box(north=60, south=18, east=300, west=225)
query.all_times()
query.add_lonlat()
query.accept('netcdf')
query.variables('Geopotential_height_isobaric',
                'Temperature_isobaric',
                'u-component_of_wind_isobaric',
                'v-component_of_wind_isobaric')

# Use the query to obtain our NetCDF data
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
Original Dataset = DatasetScan#narr-a_221_19930313_0000_000.grb; Translation Date = 2021-12-12T17:14:41.842Z
    geospatial_lat_min: 10.753308882144761
    geospatial_lat_max: 46.8308828962289
    geospatial_lon_min: -153.88242040519995
    geospatial_lon_max: -42.666108129242815
    dimensions(sizes): time1(1), isobaric1(29), y(119), x(268)
    variables(dimensions): float32 Temperature_isobaric(time1, isobaric1, y, x), float64 time1(time1), float32 isobaric1(isobaric1), float32 y(y), float32 x(x), int32 LambertConformal_Projection(), float64 lat(y, x), float64 lon(y, x), float32 u-component_of_wind_isobaric(time1, isobaric1, y, x), float32 Geopotential_height_isobaric(time1, isobaric1, y, x), float32 v-component_of_wind_isobaric(time1, isobaric1, y, x)
    groups: 
```

```python
#print (data.variables['u-component_of_wind_isobaric'][0])
print (data.variables['lat'][:])
```

```text
[[17.87115668 17.9637901  18.05570545 ... 11.17123674 11.04457979
  10.91753102]
 [18.10795748 18.20108494 18.29349116 ... 11.37401449 11.2467457
  11.11908431]
 [18.34510656 18.4387305  18.53163001 ... 11.57699036 11.44910733
  11.32083096]
 ...
 [46.32311069 46.49107039 46.65795369 ... 34.6918627  34.48046283
  34.26868522]
 [46.56076006 46.72953969 46.89724095 ... 34.87939382 34.6671838
  34.45459761]
 [46.79790957 46.9675121  47.13603437 ... 35.06633751 34.85331722
  34.63992237]]
```


