# Oracle, Pandas, HDF5

Daha onceki yazilarda SQL*Plus uzerinden CSV dosyalarina veri
aktarimini gorduk. SQLplus'in bazi eksikleri var ne yazik ki, bu
program metin bazli terminaller caginda ekranda birseyler gostermek
icin yazilmis, hizli, verimli veri aktarimini pek
beceremiyor. Disaridan parametre gecmek bile buyuk problem. Bunlarin
yerine Oracle Python baglanti kutuphanesi kurulup direk Pandas icinden
baglanti yapilabilir.

Diger yandan CSV ciktisi almak yerine daha az yer tutan HDF formati secilebilir.

Simdi bunlarin hepsini yanyana koyalim:

```
import pandas as pd
import pandas.io.sql as sql
import cx_Oracle as odb
conn = odb.connect("kullanici/sifre@DB")query = """select ...
from ...
"""

df = sql.read_frame(query, conn)
df.to_hdf("dosya.h5" % i,"table")
```

Direk Python icinden baglandik, sorguyu islettik, Pandas DataFrame'i
direk sorgu verisinden yarattik, ve ciktiyi HDF formatinda diske
yazdik. Bu formati kullanmak icin PyTables adli bir paket lazim,
kurmak icin

```
sudo apt-get install python-tables
```






