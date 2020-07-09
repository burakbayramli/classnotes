# gksudo


gksudo



Eger app server programini sudo olarak isletiyorsak, o zaman server'in sayfalarimiza tekabul eden "gecici dosyalari" (server/default/tmp altinda) root kullaniya ait olarak uretilecektir. Biz hizli gelistirme amacli olarak pagecopy.py adli bir Python script'i ile bu gecici dizinlere parasutle xhtml dosyalari atiyorduk, boylece server baslatmaya gerek kalmiyordu.Ama IDE'niz normal bir kullanici altinda basladigi icin, o kullanici tarafindan root'a ait dosyalarin ustune yazarken hata mesaji gelir. Cozum: Python script'i root olarak isletmek. Fakat puf nokta: Script'i sudo ile isletirsek sifre sorma IDE icinde gerceklesecek, buna text girisi yapmak mumkun olmayabilir. O zaman sudo yerine gksudo kullanilacak ki IDE disinda bir GUI penceresi acilsin, sifre oradan alinsin.Bu sifre bir kere alininca IDE acik oldugu surece bir daha alinmasina gerek yoktur. Build.xml altta:<target name="send"><exec executable="gksudo">  <arg line="python"/>  <arg line="src/script/pagecopy.py"/></exec></target>




