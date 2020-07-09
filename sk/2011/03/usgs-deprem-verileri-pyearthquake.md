# USGS Deprem Verileri - pyearthquake


USGS Deprem Verileri - pyearthquake




Bu Python paketi ile USGS sitesine baglanarak istenen zaman araligindaki deprem verilerini almak, onlari bir harita uzerinde basmak mumkun oluyor. Daha once blog'da paylastigimiz deprem Python kodu statik, tek bir veri dosyasi icinde, pyearthquake ile en son verileri, istenen detayda almak mumkun.

Suradaki yazida guzel bilgiler var. Kurmak icin PyPi paketini indirin. Basemap icin surasi. Sonra ayni komutu pyearthquake icin yapabilirsiniz.

Ornek kod:
from pyearthquake import *
catalog = usgs.retrieve_catalog("M1+PAST_7DAY")
print len(catalog)
mag6_list = [event for event in catalog if float(event["Magnitude"]) >= 6.0]
print len(mag6_list)
for row in mag6_list:
   print row["Eqid"], row["Magnitude"], row["Depth"],
   row["Datetime"], row["Depth"], row["Region"]  
usgs.plot_events(catalog)
Bu kod en son 7 gunluk, sonra Richter olceginde 6.0'dan buyuk deprem verileri alacaktir, ve sonuncu verileri bir haritada basacaktir. Istediginiz noktalara zoom yapmak icin zoom ikonuna tiklayip istenen bolgeyi haritada bir dikdortgen icine aldiginiz zaman o bolgenin detaylari gorulecektir. Ustte paylastigimiz yazida bunun Japonya icin yapildigini goruyoruz.




