# Tarih Boyutu

Veri ambari icin Tarih boyutundan bahsettik. Bu boyut gereken tum
tarihleri gun, ay, yil, vs icin ayri kolonlarda olacak sekilde iceren
bir tablodur. Bu tarih verisi tabii ki son derece statik / degismeyen
bir veri olacaktir, mesela 1990'dan itibaren tum tarihleri her gunu
icerecek sekilde kapsayabilir. Bizim ihtiyaclarimiz icin dakika da
gerekiyordu, bu verileri uretmek icin bir Python script yazdik. Tarihi
boyut haline getirmenin avantajlari o tarih aniyla alakali her veriyi
ayri kolon olarak o satira yazabilmek. Mesela o tarih is gunu mu.,
hangi ceyrege (quarter) ait, vs gibi bir suru ozetleyici bilgi bu
boyuta eklenebilir. Raporlama araclari bu kolonlari aninda kullanmaya
baslayabilirler.from datetime import datetimefrom datetime import
datefrom datetime import timedeltad = timedelta(minutes=1)curr =
datetime(1990, 1, 1, 0, 0)for i in range(30 * 365 * 24 * 60 ): quarter
= int((curr.month-1)/3)+1 if curr.weekday(): weekday = "True" else:
weekday = "False" print "%d\t%d\t%d\t%d\t%d\t%d\t%d\t%s" % (i+1,
curr.year, curr.month, curr.day, curr.hour, curr.minute, quarter,
weekday) curr += dScript'i isletip sonuclari > ile istediginiz cikti
dosyasina yonlendirebilirsiniz.




