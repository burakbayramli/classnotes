# Java Enum Tipleri

Enum tipleri program baslamadan belli sayida olan ve bir sabit "kod"
ile eslenmesi programlama acisindan rahat olacak degerleri listesini
temsil etmek icin kullanilir. Atanan kodlarin "tip guvenlikli" olmasi
guclu tipleme iceren dillerin ikili muhasebesini / kontrolunu devreye
sokabilmis olur. Yanlislikla bir String tipini sabitiniz ile
esleyemezsiniz, tipler uymaz, derleyici hata verir. Bu kontrol
iyidir.Bir Web ortaminda bu yapilardan ek olarak beklediklerimiz
programciya bir sekilde "hepsini gezme" ozelligi saglamasi, bir sayi
degeri olan "sirasini" verebilmesidir. Birinci ozellik bize
kullaniciya bir listeden onceden tanimli bazi secenekleri sectirme
baglaminda lazim oldu, ikinci ise, rasgele olarak bir listeden belli
bir "sey" secmek gerektiginde gundeme geldi. Rasgele sayi dogal olarak
bir numaradir ve bunu onceden tanimli listemiz ile bir sekilde
ilintilendirmemiz gerekiyordu.Java Enum tipleri burada yardimci
olabilir. Bizim kullandigimiz kalip (pattern) suna benziyor.public
static enum EnumType { A("Bu bir A"), B("Bu bir B"); private String
name; EnumType(String name) { this.name = name; } public String
getName() { return name; } public int getOrdinal() { return ordinal();
}}Artik A, B birer "tip" olarak tanimlanmistir. Soyle bir kullanim
mumkundur.EnumType a = EnumType.A;log.debug("a.getName()=" +
a.getName());log.debug("a.getOrdinal()=" + a.getOrdinal());Tum listeyi
gezmek icinfor (EnumType t : EnumType.values()) {
log.debug("t.getName()=" + t.getName());}Ustteki liste rahatlikla Seam
uzerinden @Out ile de-enjekte edilebilir ve kullaniciya icinden
sebebilecegi bir secenek sunulabilir. Son bir nokta; Enum tiplerini
Seam tarafinda == isareti ile esitlik testing tutmak gerekirsa, o
zaman tiplerin String karsiligi kullanilmali, mesela yukaridaki
ornekte kullanim .. == 'A' olacaktir.





