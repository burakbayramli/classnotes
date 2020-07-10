# Internet Kisayollari

LINK veya shortcut dedigimiz Internet kisayollari, Internet'te belki
de en çok kullanilan özelliklerden biridir. Asli amaci Internet
üzerinde herhangi bir sunucuya bagli bir dosyayla baglanti kurmak olan
kisayollar ayrica elektronik posta yollamak, Telnet baglantisi kurmak
gibi degisik amaçlar için de kullanilabilir.  <a suraya=" [Adres]
[:port]" >[GÃ¶rÃ¼ntÃ¼]</a> Internet kisayollarinin kullanimi
yukaridaki gibidir. Adres kismi baglanilacak HTML sayfasini, görüntü
kismi ise kisayolun, sayfadaki görüntüsünü belirler. Örnegin
yazacaginiz DrA Online gibi bir satir ekranda DrA Online olarak
görülecek ve sizi dogruca DrA Türkiye'nin Web sitesine baglayacaktir.
Adres kismi oldukça basit olmasina ragmen, bazi küçük püf noktalari da
yok degil. Örnegin, adres kismini kullanicinin size mail atmasi için
kullanabilirsiniz. Bunu gerçeklestirmek için kisisel e-posta
adresinizin önüne mailto: takisini getirmeniz yeterlidir. Örnegin
hazirladiginiz sayfada kullanicilarin size mail atabilmeleri için Bana
<a suraya=" mailto:energydata@yahoo.com" >mail</a> atabilirsiniz.
gibi bir satir kullanabilirsiniz.  Kisayollar ayrica Telnet, FTP,
Gopher servislerini de çagirabilirler. Bunlari çagirmak için, sizin de
tahmin edebileceginiz gibi ftp://, gopher:// takilarini site isminin
önüne getirmek yeterlidir. Genel kullanimda verdigimiz :port takisinin
ne ise yaradigini merak edenler için hemen bir açiklama getirelim: Bu
taki tüm web servisleri için 80 olup genelde kullanilmalarina gerek
yoktur. Hizla gelisen Web teknolojisi çoook eskiden kullanilan :port
takisinin kullanimini gereksiz bir hale getirmistir.  Internet
kisayollari, Web tarayicinizin ayariyla oynamadikça ve HTML sayfasinda
aksini belirtmedikçe ekranda mavi alti çizili yazilar olarak
görülürler. Siz ilgili linke tikladiginizda ve isaret ettigi sayfayi
izlediginizde o sayfadaki link maviden mora dönüsür.  Daha önce de
belirttigimiz gibi kisayol renkleri HTML sayfasindaki kodlarla
degistirilebilirler, üstelik siz bunu yaptiginizda kullanici web
tarayicisinin ayariyla oynayarak kisayolun rengini kendisine göre
tayin edemez. Iste bunun nasil yapilacagi: Sayfanizin basinda '
tag'iyla birlikte link=deger vlink=deger takilarini kullanarak ziyaret
edilmis ve edilmemis kisayol renklerini belirleyebilirsiniz. Her
zamanki gibi bunu yapmak için Hex renk kodlarini ve hazir renk
adlarini kullanabilirsiniz. Örnegin sayfadaki kisayollarin alti sari
çizgili görülmesini, ayni kisayollarin ziyaret edilince kirmiziya
dönüsmesini istiyorsunuz bunu yapmak için <BODY BGCOLOR=WHITE
LINK=YELLOW VLINK=RED> satirini kullanabilirsiniz. Mutlaka siz de
rastlamissinizdir: Bazen resimler de bir Internet kisayolu olarak
kullanilabiliyor. ' Bunu nasil yapabilirim?' diyorsaniz, hemen çok
basit oldugunu belirtelim. Genel kullanim bölümünde verdigimiz
[görüntü] bölümüne önceki örneklerimizde hep yazi yazdik, bir de resim
koymayi deneyelim: <a suraya=" http://draskin.150m.com" ><img kaynak="
dra.GIF?50m=image" alt=" DrA Online" ></a> satirini yazdiginizda
dra.GIF isimli resim DrA Online'a baglanmak için bir kisayol olarak
kullanilacaktir. Burada dikkat etmeniz gereken bir husus var, o da
resimlerin link olarak kullanildiginda mavi (ya da link için
atadiginiz renk neyse) bir çerçeve içine alindigidir. Bunu önlemek
için resmi yükleyen ' tag'in sonuna bir border=0 ekini
girebilirsiniz. Söyle: <a suraya=" http://draskin.150m.com" ><img
kaynak=" dra.GIF?50m=image" border=0></a> Dikkat Etmeniz Gerekenler *
Bir kisayol olusturduktan sonra mutlaka ' Acaba bir hata yapmis
miyim?' diyerek kisayolu kontrol edin. Bozuk kisayollarin sitenizin
ziyaret sayisini ve itibarini zedeleyecegini unutmayin.  * Sonradan
doldururum diye kisayolu önceden yerlestirmeyin. Sitenin basina ' insa
asamasinda' yazsaniz bile yapmadiginiz bölümlerin linklerini
koymayin. Bunun yerine hazirladiginiz bölümleri kisayol
olusturmaksiniz Web sayfasina yerlestirin.  * Sitenize bulundugunuz
makineden FTP programiyla uzunca bir shareware yolluyorsunuz
diyelim. Shareware'inizin kisayolunu önceden yollamayin, geçen zaman
süresince sitenize baglanip o program ile ilgilenenler ' 404 Not
Found' hatasi ile karsilasirlar.  * Internet adresi verirken basina
http://, mail adresi verirken basina mailto: koymayi unutmayin. Bunu
yapmazsaniz verdiginiz draskin.150m.com veya energydata@yahoo.com gibi
bir adres lokal bir dosya olarak görülür ve linkine tiklayinca dogal
olarak hata verir.  Internet kisayollarini ve resimlerin kisayollara
atanmasi için nasil kullanildigini ögrendik...  Iste bize en çok
sorulan sorulardan biri: ' Web projem bir klasörde duruyor, lokal
olarak çalisiyorum ve bir alt veya üst klasördeki dosyayi bulundugum
sayfadan çagirmak istiyorum. Nasil yapacagim?'  Üzerinde çalistiginiz
HTML sayfasi C:\PROJE\1 klasöründe olsun, sizin de C:\PROJE
klasöründeki DENEME.HTM sayfasini çagirmak istediginizi varsayalim. Bu
dosyayi çagirmak için söyle bir satir kullanmalisiniz <A HREF="
../DENEME.HTM" >KISAYOL DENEME.HTM</A> ../ ifadesini istediginiz kadar
kullanabilirsiniz. Iki alt klasördeki dosyayi çagirmak için
../../DENEME.HTM gibi bir ifade kullanabilirsiniz. Bir üst klasördeki
(C:\PROJE\1\BASKA\SAYFA.HTM gibi) dosyayi çagirmak için ise söyle bir
yol izlemelisiniz: <A HREF=" BASKA/SAYFA.HTM" >KISAYOL</A> Eger bir
Web sunucusu üzerinde çalisiyorsaniz ve sabit diskinizin tümü veya bir
alt dizinini Web sayfalariniz için tanimlanmissa kök dizine inmek ve
buradan baska bir klasöre geçmek için / karakterini
kullanabilirsiniz. Örnegin <a suraya=" /GIRIS.HTM" >GIRIS</a> satiri
sizi web için ayrilan bölümün en dibine kadar indirecek ve GIRIS.HTM
dosyasina kisayol olusturacaktir. Bunun gibi resimler dizinindeki bir
resmi çagirmak için <img kaynak=" /resimler/dra.gif" > gibi bir ifade
kullanmalisiniz. Kisayollar için söyleyecegim simdilik bu kadar. Son
olarak sayfalarinizin profesyonelligini ve sayfaniza baglananlarin ruh
sagliginin korunmasi için "Dikkat Etmeniz Gerekenler" kutumuzu
okumanizi ve sayfalarinizda uygulamanizi siddetle tavsiye ederim
Yazarin izni ile http://draskin.150m.com sitesinden alinmistir




