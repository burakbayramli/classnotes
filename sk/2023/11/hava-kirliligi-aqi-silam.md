# Hava Kirliligi, SILAM, AQI

```python
import requests, os

def download_silam(f, outdir):
   base_url = "https://silam.fmi.fi/thredds/fileServer/dailysilam_glob06_v5_8/files/"
   target_file = outdir + "/" + f
   url = base_url + "/" + f
   if not os.path.isfile(target_file):
       r = requests.get(url, allow_redirects=True)
       open(target_file, 'wb').write(r.content)

f = 'SILAM-AQstat-glob06_v5_8_2023110400.nc4'
outdir = "/tmp"
download_file(f, base_url, outdir)
```










[1] https://silam.fmi.fi/thredds/catalog/dailysilam_glob06_v5_8/files/catalog.html

[2] https://silam.fmi.fi/aqforecast.html

[3] https://github.com/fmidev/opendata-resources/blob/master/examples/python/timeseries-airquality.ipynb

