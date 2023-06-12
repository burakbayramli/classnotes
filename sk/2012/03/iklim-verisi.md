# İklim Verisi

Dünyadaki belli bölgeler için kaydedilmiş eski iklim kayıtları için
[şu](ftp://ftp.ncdc.noaa.gov/pub/data/gsod/) FTP sitesine
bakılabilir. Her sene bir dizin, görüldüğü gibi bazı bölgeler için
1928'e kadar giden kayıtlar var, ve bu sene dizinleri altında her
kayıt ayrı bir bölgenin yıllık iklim kayıtlarını içeriyor.

Bölgelerin kodları [şurada](ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt)
listeli, mesela İstanbul'daki bölgelerden bir tanesi 170600 koduna
sahip, bu kodu alıp sene dizinine gidiyoruz, ve
170600-99999-2011.op.gz dosyasını indiriyoruz. Dosya içinde her satır
bir gün, ve her kolon değişik bir iklim ölçüsü, günün ortalama
sıcaklığı, nem gibi.


```python
fin = open("170600-99999-2023")
for line in fin.readlines():
  line = line.strip()
  break
```

```python
print (line)
```

```text
0158170600999992023010100004+40967+028817FM-12+003399999V0200101N001019999999N003000199+00701+00601103411ADDAA106000091AY101021AY201021MA1999999103001MD1990001+9999MW1101OD139900211999REMSYN07017060 11930 00102 10070 20060 30300 40341 50000 60001 71000 333 91104=
```

```python
import pandas as pd

def substr(s,pos,len):
   return s[pos-1:pos+len-1]
   
usaf = substr(line,5,6)
wban = substr(line,11,5)
ts = pd.to_datetime(substr(line,16,12))
report_type = substr(line,42,5)
wind_direction = substr(line,61,3)
wind_direction_qual = substr(line,64,1)
wind_observation = substr(line,65,1)
wind_speed = substr(line,66,4)
wind_speed_qual = substr(line,70,1)
air_temperature = substr(line,88,5)
air_temperature_qual = substr(line,93,1)
res = [usaf, wban,ts,report_type,wind_direction,wind_direction_qual,wind_observation,wind_speed,wind_speed_qual,air_temperature,air_temperature_qual]
print (res)
```

```text
['170600', '99999', Timestamp('2023-01-01 00:00:00'), 'FM-12', '010', '1', 'N', '0010', '1', '+0070', '1']
```

Kaynaklar

[1] https://github.com/dimajix/weather-analysis/blob/master/Weather%20Analysis.ipynb

[2] https://www.ncei.noaa.gov/pub/data/noaa/isd-format-document.pdf


