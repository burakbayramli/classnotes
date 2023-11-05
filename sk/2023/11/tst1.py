import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import netCDF4, simplegeomap as sm

#f = netCDF4. Dataset('SILAM-AQstat-glob06_v5_8_2023102700.nc4')
f = netCDF4. Dataset('SILAM-AQstat-glob06_v5_8_2023102300.nc4')
print ('0--------------------')
print(f. variables) # get all variable names.
print ('1--------------------')
print(f. variables.keys()) # get all variable names.
print ('2--------------------')
pm25 = f.variables['daymean_cnc_PM2_5'] 
pm25 = f.variables['daymean_cnc_PM2_5'].shape
print ('3--------------------')
print (pm25)
print ('4--------------------')

x,y = np.meshgrid(np.linspace(-180,180,600),np.linspace(-90,90,297))
z = f.variables['daymax_cnc_PM2_5'][4,0,:,:] 

z = z * 1e8
z = np.log(z)

sm.plot_continents(0, 20, zoom=18, incolor='black', outcolor='white', fill=False)
#sm.plot_continents(37,27, zoom=2, incolor='black', outcolor='white', fill=False)

plt.pcolormesh(x,y,z,cmap='OrRd')

plt.savefig('/tmp/out.jpg',pil_kwargs={'quality':40})

df = pd.DataFrame(z)
df.to_csv('/tmp/out.csv')
