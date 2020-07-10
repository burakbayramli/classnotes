# Program Nasıl Test Edilir

Uzun bir program yazdınız ve dogru calısıyormu diye merak
ediyorsunuz. Eger gorsel program yazdiysaniz hemen calistirip birkac
yerine klik edebilirsiniz. Eger gunluk program yazsiysaniz, calistirip
acaba verileri isliyormu diye kontrol edersiniz. Acele yapilabilecek
testlerden biri budur.  Daha iyi uygulanan metod 'otomotik
testlerdir'. Bu testler bir grup kayit edilmis test
komutlaridir. Programiniza disardan baglanip (yada icinde yeralip)
olasi butun ilginc senaryolari yaratirlar, ve sonuclari kontrol
ederler.  Genelde bu testler, program derleme sonunda otomatik olarak
isletilirler. Derleme kelimesinin anlami icin diger yazimiza
bakabilirsiniz.  Tabii ki bu tip testleri hazirlama daha zordur, ve
zaman alacaktir. O yuzden eger cabuk bir proje icindeyseniz, belki bu
metodu uygulamayabilirsiniz. Fakat uygulamaniz sizin yarariniza
olacaktir.  Otomatik testlerin yararlarina deginelim: * Otomatik
olduklari icin, bir kullanicinin görsel olarak, klikler ile test
etmesine gerek kalmaz.  * Butun programınızı baştan asağı test
edebildigi icin, ufak degisiklikleri fark edecektir ve bu hataları
size haber verecektir. Hepimizin bildigi gibi, bazen hata duzeltelim
derken, programin baska bir yerine yeni hata ekledigimiz cok
olmuştur. Otomotik testleriniz bu tip yeni hatalari yakalayacaktır Şu
anda piyasada otomotik testler icin JUnit, HttpUnit ve Cactus gibi
programlar mevcut. Bu programlar baslangıç noktasi ve temel
saglar. Özel testlerinizi siz kendiniz hazirlayacaksınız.




