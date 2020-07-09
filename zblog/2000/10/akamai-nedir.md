# Akamai Nedir


Akamai Nedir



 Internet programlari ile ugrasanlar, herhalde Akamai isimli bir servis duymuslardir. Projelerinizde bazen soyle kelimeler duyabilirsiniz "Web sayfalarimizi Akamai-ze edelim", "Akamai baglantisi yapildi mi?", vs.              Nedir Akamai? Bu cok unlu olan servis, aslinda bir hizli erisim deposundan baska bir sey degildir. Mesela sayfalarinizda grafik sekiller varsa, ve o sakillere su sekilde ulasiyorsaniz  img kaynak="images/mavi_resim.jpg"   Sayfalariniz akamaize edildikten sonra,  img kaynak="http://net.akamai.net/200101102233/www.siteminismi.com/images/mavi_resim.jpg"  seklinde bir kod kullanmaniz gerekir.               Boylelikle bu resim sizin sitenizden degil, Akamai sirketinin sitesinden kullaniciya gonderilir. Akamai buyuk masraflar yaparak, Internet'e cok guclu servis makinalari kurmustur, ve ayrica gunun her saatinde Internet'in hangi bolumunun ne kadar mesgul oldugunu bilirler. Sizin sayfaniz Akamai servisinden grafik istedigi zaman, kullanicinin nerede olduguna bagli olarak, ona 'en yakin' yerden bu grafik ona verilir.               Her isteyen tabii ki bir Akamai linki ekleyip, sayfalarini hizlandirmayi beklemesin. Oncelikle Akamai ile bir anlasma kurulmasi, ve bu servis icin para odenmesi gerekir. Para odendikten sonra Akamai sirketi, yukaridaki link icin 'izin' verir, yani bilgisayarlarin da sizin icin yer acar. Grafikler ilk kez istenmeye baslayinca, Akamai grafik sizin sitenizden alip, kendi bilgisayarlarina koyacaktir.               Programcilar icin bir uyari: Sayfa kodlarken sakin akamaize edilmis kodlar ile beraber kodlamayin. Yoksa, Internet baglantisi olmadan sayfa gelistirmesi yapamazsiniz, hem de sayfalariniz cok karisir. En rahat olani bizce, normal linkleri, aninda akamai-ze etmek. Bunu Sayfa Servis Programiniz (Apache gibi), otomatik olarak yapabilir. Bu tur isleme  URL-rewriting denir, yani "aninda link degistirme".




