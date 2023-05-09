# basemap

Kurmak icin bazi paketlerin birbiriyle muhakkak uymasi gerekiyor. Bu
uyumluluk altta,

```
pip install GEOS
pip install matplotlib==1.5.3
pip install https://github.com/matplotlib/basemap/archive/v1.0.7rel.tar.gz
pip install pillow
```

Termux icin usttekilerden once bir de

```
pkg install libxml2-dev

pkg install libxslt-dev
```

gerekli.

Ornek kod

```
import pandas as pd

import numpy as np

import matplotlib.pyplot as plt

from mpl_toolkits.basemap import Basemap



fig = plt.figure(figsize=(8, 8))

m = Basemap(projection='lcc', resolution='h',

            width=1E6, height=1.2E6, 

            lat_0=45, lon_0=-100,)

m.shadedrelief()

m.drawcoastlines(color='gray')

m.drawcountries(color='gray')

m.drawstates(color='gray')



x, y = m(-122.3, 47.6)

plt.plot(x, y, 'ok', markersize=5)

plt.text(x, y, ' Seattle', fontsize=12);

plt.show()
```

Daha onceki bir yazida basemap kurulumu icin sunu soylemistik:
once basemap paketini de indirin. Basemap acildiginda geos-... alt
dizinine girin, ve ./configure, make, sudo make install ile bu paketi
kurun, sonra bir uste cikip basemap python setup.py install yapin.












