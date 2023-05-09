# HProf

"Fakir adamin Profiler araci" Java kutuphanesinin icinden cikan HProf
araci. Bu arac cok sukseli bir cikti vermiyor fakat ciddi problemleri
ilk bastan gormek, saptamak icin kullanilabilir. Kurumsal Java
kitabinda bu aractan bahsetmistik. Profiler'i kullanmak icin bir
uygulamayi o aracin kontrolunde baslatmak lazim; sonra uygulama
bitince (CTRL-C, kill -9 ile durdurulunca) size java.hprof.txt adli
bir dosya uretiliyor. Bu dosya icinde uygulamamizin kullanimi ile
alakali bir suru istatistik mevcut.Kullanmak icin JBoss bin/run.sh
dosyasini bin/run_with_hprof.sh diye kopyalayalim. Sonra satir #60'a
gidip JAVA_OPTS diyen satiri

JAVA_OPTS="-agentlib:hprof=cpu=samples,depth=7 ... "

olarak degistirelim. Bu sekilde JBoss'u run_with_hprof.sh script'i ile
baslatinca, kullanildigi anda profile edilen bir uygulamamiz
olacak. Secenekler ne diyor? CPU kullanimina bak, sadece orneklem
yaparak olc (her seyi olcme) ve metot cagrilari zincirini 7 seviye
derine gidecek sekilde kayitla. Rapor dosyasi uretilince en altta
uygulamamizin en coktan aza dogru sayida, yuzdede cagrilan
metotlarinin listesini goreceksiniz. En cok cagrilanlar performans
iyilestirmesi icin bir aday olabilir elbette.Yuk yaratmak icin ise,
daha once Selenium ve Python temelli bir aractan bahsettik. Cok basit
yuk ihtiyaclari icin Apache ab adli arac kullanilabilir. Diyelim ki
uygulamamiz URL uzerinden parametre kabul edebiliyor, bu durumda bu
parametreleri /tmp/params adli bir dosyaya yazariz,
"anahtar1=deger1&anahtar2=deger2" gibi..

Bu parametreleri ab ile uygulamaya gondermek icinab -kc [KAC PARALEL]
-n [KAC TANE] -p /tmp/params -T application/x-www-form-urlencoded
http://www.bizimsite.com/sayfa.seamUstteki komut [KAC PARALEL] tane
sanal kullanici (thread) yaratacak, ve bu kullanicilari [KAC TANE]
kadar en sonda verilen URL adresine saldirtacak. Bu sekilde
uygulamamiz uzerinde iyi bir yuk yaratabiliriz. java.hprof.txt
dosyasinda ise text editorumuz ile kendi uygulamamizin Java paket
ismini arariz, en alttaki metot listesine bakariz, problem noktalarini
saptamaya calisiriz.





