from datetime import datetime
import numpy as np
from netCDF4 import Dataset, num2date
from scipy.ndimage import gaussian_filter
from siphon.catalog import TDSCatalog
import numpy as np, numpy.linalg as lin, math

def to_bearing(lat,lon,brng,d):
    R = 6378.1 #Radius of the Earth
    lat1 = math.radians(lat)
    lon1 = math.radians(lon)
    lat2 = math.asin( math.sin(lat1)*math.cos(d/R) +
         math.cos(lat1)*math.sin(d/R)*math.cos(brng))
    lon2 = lon1 + math.atan2(math.sin(brng)*math.sin(d/R)*math.cos(lat1),
                 math.cos(d/R)-math.sin(lat1)*math.sin(lat2))
    lat2 = math.degrees(lat2)
    lon2 = math.degrees(lon2)
    return lat2,lon2

def get_data(lat,lon,day,month,year,hour):

    # form grid which has NE, and SW cornes brg kilometers away
    # from center lat,lon
    brg = 1000
    upper_right = to_bearing(lat,lon,np.deg2rad(45),brg)
    lower_left = to_bearing(lat,lon,np.deg2rad(225),brg)

    north = int(upper_right[0])
    south = int(lower_left[0])
    east = int(upper_right[1])
    west = int(lower_left[1])

    side = np.cos(np.deg2rad(45))*brg*2
    print (side, 'km')
    area = side*side*1e6

    dt = datetime(year, month, day, hour)

    base_url = 'https://www.ncei.noaa.gov/thredds/catalog/model-narr-a-files/'
    cat = TDSCatalog(f'{base_url}{dt:%Y%m}/{dt:%Y%m%d}/catalog.xml')
    ds = cat.datasets.filter_time_nearest(dt)
    ncss = ds.subset()

    query = ncss.query()
    query.lonlat_box(north=north, south=south, east=east, west=west)
    query.all_times()
    query.add_lonlat()
    query.accept('netcdf')
    query.variables('u-component_of_wind_isobaric',
                    'v-component_of_wind_isobaric')

    data = ncss.get_data(query)
    u_wind_var = data.variables['u-component_of_wind_isobaric']
    v_wind_var = data.variables['v-component_of_wind_isobaric']
    u_wind = u_wind_var[0, 0, :, :].squeeze() 
    v_wind = v_wind_var[0, 0, :, :].squeeze() 
    np.savez('uwind',u_wind)
    np.savez('vwind',v_wind)
    print ('alan', area)

if __name__ == "__main__": 
    
    lat=25;lon=-90; year = 2005;month = 8;day = 30; hour = 10
    get_data(lat,lon,day,month,year,hour)
