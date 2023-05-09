# Otomatik Test ve JUnit

Programlarınızı test etmenin yararına baska bir yazıda değindik. Şimdi
test etme yöntemlerine gelelim.  Test programlari yazmak zor degil;
Sıkıcı ve zaman alıcı olabilirler. Test programlarinin yapmasi gereken
"ana" seyler sunlardir.  * Baştan sona bütün testleri işletebilme *
Bir test hata bulursa, ötekilere devam edebilme özelliği * Sonunda
rapor verme özelliği, ve bir test hata bulmuşsa, bütün testin 'hatalı'
sayılması Eskiden bu özellikleri taşıyan küçük test "programcıklarını"
kendimiz yazardık.

```
public static void main(String[] kelimeler){
  TestEdilenParca parca = new TestEdilenParca();
  parca.setDeger("yazi!");
  String s = parca.getDeger();
  if ("yazi!".equals(s)) {
    System.out.println("Test basarili");
  }
}
```

Yukarıdaki test parçası tek bir test için geçerli. Bütün testleri
işletmek için, 'test üzeri' olan başka bir nesnenin, her testi ana
bölümden teker teker çağırmalıdır.  JUnit ile işimiz
rahatladı. Java'nın bir özelliği 'Reflection' denen, metod ismini
bilmeden metod çağırma yeteneğidir. Reflection kullanarak, test metod
isimlerini bile ayrı ayrı çağırmamıza gerek kalmaz. testXXX gibi olan
yani bütün 'test' ile başlayan metod isimleri JUnit tarafindan
otomatik olarak çağırılır.  Yukarıdaki kritere tekrar bakalım: JUnit
butun testleri işlettikten sonra, onlarin sonuçlarını toplayabiliyor,
ve 'geçti' 'kaldı' gibi onları işaretleyebiliyor. Tek bir test kalmış
ise hepsi kalmış sayılır, vs..  Bir tavsiye daha: Otomatik
testlerinizi 'kod derleme' icine katın. Yani bütün proje kodunuz
derlendikten sonra, en son testleriniz otomatik olarak, yeni kod
üzerinde işlenmelidir. Bu çok önemli! Derlemede hiç hatası olmayan kod
bile "doğru kod" sayılmaz. Derlenebilen VE testlerden geçebilen kod
doğru koddur.


