# Yourkit Java Profiler

Kurumsal Java kitabi icin 2006 senesinde profiler araclarini
incelemistik. O zamandan beri piyasa pek degismemis; halen Profiler
piyasasi open source baglaminda genis secenekler sunmuyor. Eger para
verilecek bir alan varsa, Profiler bunu verecek yegane yerlerden biri
herhalde. Bizim sahsi kullanimimiz birkac gunun otesine gecmiyor, o
sebeple ticari bir urunu deneme lisansi altinda alarak para vermeden
kullaniyoruz. Daha uzun sureli ve sirket temelli kullanimlar icin
durum daha degisik olabilir.Halen Profiler'lar arasinda en iyi urun
Yourkit Java Profiler. Kurulumu en son versiyonunda daha da
basitlesmis. Indirip unzip ettikten sonra dizine girip lib altina
bakin, ve

java -jar yjp.jar -integrate

komutunu isletin. Bu komut bir menu getiriyor, YJP'yi entegre edecek
app server listesi secenek olarak sunuluyor. Biz 2) sectik, yani JBoss
2.x/3.x/4.x. Daha sonra JBoss'un kullandigi run.sh dosyasinin yeri
size soruluyor, bu dosyanin tam dizinli yerini veriyoruz, ve diger
sorulara olagan (default) degerleri giriyoruz. Sonuc olarak bu
entegrasyon programi JBoss bin dizini altinda bir run_with_yjp.sh
dosyasi yaratiyor.

Bu script'in ne yaptigi bariz; program JBoss'u YJP kontrolu altinda
baslatiyor ve boylece Profiler GUI ile bu JVM'e baglanip bilgi
toplayabilmeye basliyoruz. Her turlu cetrefil PATH ayari vs otomatik
olarak bu script icinde yapilmis. Bizim ek secenekler sokusturmamiza
gerek yok. Ayrica normal run.sh script'i temiz birakilmis, o hala
normal ihtiyaclar icin kullanilabilir.GUI malum, YJP kontrolundeki
herhangi bir JVM belli bir port uzerinden bilgi paylasir (olagan deger
10000); Eger JBoss'u sonuc ortamindaki bir makinada ise, bu makinaya
her port acik olmayabilir, o zaman SSH tunelleme gerekir. Makinayassh
-L10000:localhost:10000 kullanici@makinaile baglanirsak, artik "bizim
makinamizadaki" 10000 port degeri uzaktaki makinaya tunellenir, ve
sonuc ortamini bu sekilde profillememiz mumkun olur. JBoss YJP ile
baslatilinca, YJP/bin altindaki yjp.sh ile GUI baslatilir ve gerekli
servise baglanip bilgi toplanmaya baslanabilir.





