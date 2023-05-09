# Döngüler ve Ziyaretler (Iterators)

Java, C++ ve C gibi dillerde bir dizin (array) elemanlarını teker
teker ziyaret etmek için, döngü kavramı kullanılır. Mesela 'liste'
değişkeninde saklanan bir dizin elemanlarını şöyle ekrana basabilriz.

```
int i; for (i=0; i<liste.size(); i++) {
  System.out.println(i.get(i));
}
```

Harika. Fakat bu kod daha ufak olamazmıydı? Diyelim ki, aynı listeyi
tekrar ziyaret etmemiz gerekse idi, tekrar baştan başlayacaktık, ve
aynı kodu yazacaktık.  Fooooor i eşiiiiittir 0, vs.  Amaç Soyut olarak
düşünürsek, amacımız basitçe şudur: Bir listenin her elemanı üzerinde
aynı işlemi uygulatmak.  Bu tanıma dayanarak, yukarıdaki koddaki
gereksiz ifadeleri bulalım.

* for (i=0: Niye sürekli sıfırdan başlayacağımızı belirtiyoruz? Bütün
elemanlar üzerinde işlem yapacağımız belli. Dil sözdizimi burada bizi
uğraştırıyor.

* i++: Ayrı bir gereksizlik. Özet olarak indis tanımlamak hem külfet,
hem de bu ek hareket yanlış yapmamızı rahatlaştırabilir. Eğer aynı
değişkeni tekrar tekrar kullanmamız gerekse, ve sıfıra eşitlemeyi
unutsak, programımıza bir görünmez hata sokmuş olacağız.  Alternatif
olarak, ziyaretçi (iterator) kavramını kullanabiliriz.  Iterators
"Java'da iterators nesnesi var" denildiğini duyar gibi oluyorum.

Evet, bu doğru bir gözlem. For yerine Iterator kullanan örnek kodun
son halini gösterelim.

```
Iterator i = liste.iterator();while ( i.hasNext() ) {
  System.out.println(i.next());
}
```

Bu kodda ilerleme olarak indis takip etmekten kurtulduk. Ama döngü
yaratmaktan kurtulamadık.  "Döngüyü bile yazmamıza gerek bırakmayan,
ufak bir kod ile bu işi yapmamızı sağlayacak bir sözdizim yapısı
olamaz mı?" Peki döngüyü kim gerçekleştirecek?  Dizin nesnesinin bu
işi yapması daha iyi olabilir?  Sonuçta listenin gerçekleştirimine
sahip olan nesne (Vector) nesnesidir, ve baştan sona gitme hareketini
en iyi yapabilecek, ve tekrara yer bırakmayacak şekilde içinde tutacak
olan en iyi nesne odur.  Rubi ve Ziyaretçiler Ruby dilinde ziyaretçi
kavrama oldukça ileridir.

Ziyaret için şöyle bir ibare yeterli.

```
liste.each do |eleman|print eleman
end
```

İnanılmaz! Her elemanı ziyaret etmek için tek söylememiz gereken 'each
(her)' kelimesi oldu, gerisi, bu her eleman üzerinde işletilecek kod
parçasını begin ve end arasına kodlamaktır.  Bu tür bir kullanım,
while {..} arasında girilen koddan biraz farklıdır. Rubi dilinde begin
ve end arasındaki kod, liste nesnesine 'gönderilmektedir'. Bu kodu
alan liste nesnesi, bu kod parçasını her eleman üzerinde teker teker
işletir.  Arka planda bu işlemi gerçekleştirebilmek için Rubi
yorumlayıcısı hareket eden dinamik kod kavramını kullanmaktadır. Bu
yüzden esnek bir ziyareçi kalıbı mümkün olabilmektedir.

Java dilindeki while .. {..} arasındaki kod parçası, her seferinde
üzerinde olduğu elemanı 'almak' ve üzerinde işlem 'yapmak'
zorundadır. Rubi seçeneğinde kod parçası listeye verilmekte, ve bu
kodun, nesne içinde uygulanması listenin kendisine bırakılmaktadır. Bu
şekilde bir kullanım ile aşağıdaki diğer ibareler mümkün olmaktadır.
f = File.open("testdosyası")f.each do |satir|print satirendf.close
Rubi dilindeki gelişmiş ziyaretçi mimarisi sayesinde, Java'da olduğu
gibi yardımcı Iterator nesnelerine, ya da daha ilkel olan for döngüsü
ve indis kavramlarına gerek kalmıyor.





