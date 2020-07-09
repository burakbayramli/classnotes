# Milyonluk Satirlari Birlestirmek


Milyonluk Satirlari Birlestirmek



Veri ambari projelerinde bazen duyulur "bu birlesimin iki tarafinda milyon satirdan fazlasi var, bu birlesim (join) komutu geri gelmeyebilir". Hakikaten de iki tarafin da cok sayida veri icerdigi durumda birlesimin cok zaman alacagi durumlar vardir. Fakat sunu sormak lazim, birlesim sonrasi ne yapiyoruz?Diyelim ki A,B uzerinde milyon satirlik veri var, A'da bir kolonu guncelleyecegiz, onun icin B'yi  birlestiriyoruz, ve ek bilgiyi oradan aliyoruz. Eger A uzerinde indeks var ise, guncelleme tabii ki yavas olacaktir, bunun birlesim ile alakasi yok, cunku indeksler genel olarak erisimi hizlandirir, veri eklemeyi, guncellemeyi yavaslatir (yine bir muhendislik al/ver -trade off- durumu). Eger A ve B arasinda birebir iliski var ise, mesela bir id kolonu iki tarafta da tekil, o zaman her kullanim icin birlesim yavas olmayabilir.Evet iliskisel teoriye (relational theory) gore A,B birlesimi once bir kartezyen kumesi yaratir, yani teorik olarak A ve B'deki tum satirlarin kombinasyonu once hazirlanir, bu ornegimizde 1 milyon x 1 milyon satir eder, sonra bu koca kume icinde birbirine uyanlar (ortak id uzerinden) elenir.Fakat veri tabanlari, pratikte, bu tur islemleri hissederek hizlandirici onlemleri alirlar. Iki tarafta tekil olan id ile birlesim var ise, tek satirdan tek satira ziplayarak gereken veri alinabilir.Yani sartlara iyi bakmak lazim. Veri ambar projelerinde gecici tablolar yaratmak her zaman iyi tekniktir, gereken veriler birbirine uyan id tasiyacak sekilde burada parca parca hazirlanabilir, sonra tum parcalar birlestirilerek baska bir bos, ya da indeksleri kapatilmis bir (nihai)  tabloya toptan olarak eklenebilir. Bu durumda birlesimlerin hizli isledigi gorulecektir.




