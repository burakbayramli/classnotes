# Dosya İşlemleri ve Unix

Dosya İşlemleri ve Unix

Programlama sırasında bir dosyayı aramamız ya da değiştirmemiz
 gerekebiliyor. Eğer aradığımız tek bir dosya ise, ve yapacağımız
 değişiklik tek bir tane ise sorun yok.  Fakat arama ve değişiklik
 toptan, yani birçok dosya üzerinde aynı anda yapılacak bir
 arama/değişiklik ise, Unix komut satırının yardımına ihtiyacımız var.
 Unix Find Komutu Find (bul) komutu, birçok dosyaya bakabilen, bir üst
 dizinden başlayıp aşağı doğru inerek bu dosyalar üzerinde işlem
 yapabilen bir komuttur.  Eğer xml soneki (postfix) taşıyan bütün
 dosyaları bulmak istiyorsak, find /baslangic/dizini -name '*.xml'
 .. komutunu kullanabiliriz.  Fakat find'ın yapabildikleri bundan
 ibaret değildir. Mesela, her dosya 'içine' bakıp, içinde metin123
 gibi bir kelime taşıyan dosyaları göstermek istersek: find
 /baslangic/dizini -name '*.xml' -exec grep -l 'metin123' {} \; Bu
 komutu dikkat ile inceleyelim: Find kısmını biliyoruz. -exec'ten
 sonra gelen kısım, "find'ın baktığı her dosya için, -exec'ten sonra
 gelen komutu bu dosya üzerinde işlet" demektir. Nitekim, grep komutu
 her dosya üzerinde işletilip metin123 kelimesi bu dosya içinde
 aranacaktır. -l seçeneği, "eğer kelime dosyada bulundu ise, dosyanın
 ismini göster" demektir.  {} karakterleri, find'ın oluşturduğu döngü
 içerisinde, her dosya isminin yerine geçen bir tanımlamadır. Diğer
 bir örnek göstermemiz gerekir ise, eğer bütün xml ile biten
 dosyaları, üst bir dizinden başlayarak özyineli bir şekilde
 alt-dizinlerde "silmek" istesek, şöyle derdik.  find
 /baslangic/dizini -name '*.xml' -exec rm -rf {} \; rm komut
 bildiğimiz gibi silmek (remove) için kullanılır.  Dosya İçinde Kelime
 Değiştirmek Karşımıza şöyle bir problem çıkabilir: Kod içinde sürekli
 tekrarlanan bir hatayı, komut satırından tek bir komut ile düzelmek
 istiyoruz. Mesela hata 'while(1)' kelimesi olsun, ve bu kelimeyi
 'while(true)' ile değiştirmemiz gereksin. Tek bir dizin içinde, bu
 düzeltmeyi şöyle yapabiliriz.  perl -pi -e 's/while(1)/while(true)/g'
 *.java Bu komut için Perl gerekiyor. Ruby ile de aynı komutu
 işletebilirsiniz.  s ve g harfleri arasında, metin bulmak için
 istediğiniz düzenli ifadeyi kullanabilirsiniz. Yani, arama şartları
 yukaridakindan çok daha çetrefilli olabilir. Basit bir örnek tam uyum
 arayan yukarıdaki örnektir.  (Düzenli ifadeler çok güçlü bir
 kavramdır, öğrenmenizi tavsiye ederim).  Şimdi, yukarıdaki perl
 betiği eşliğinde yapılabilen arama/değiştirme işlemini, üst bir
 dizinden başlayarak her dosya üzerinde özyineli olarak yapalım.  find
 /baslangic/dizini -name '*.java' -exec perl -pi -e
 's/while(1)/while(true)/g' {} \; Umarım çok karışık gelmemiştir. Tek
 tek parçalara bölerek bakıldığında komut daha da rahat
 anlaşılacaktır.  Tarih Find ve grep gibi komutlar, Unix felsefesini
 çok güzel yansıtır. Bu felsefeye göre işletim sisteminin sağladığı
 araçlar, tek ve sadece tek bir görevi çok iyi bir şekilde yapacak
 şekilde yazılırlar. Betik ile bir iş görmesi gereken idareciler (ya
 da programcılar) ihtiyaçları olan konuya göre, Unix komutlarını
 birbirine ekleyerek, birinin çıktısını ötekinin girdisi yaparak,
 istedikleri sonucu alırlar.  Mesela bütün dizinler altındaki her
 dosyanın içindeki harf sayısını sayan bir Unix aracı yoktur. Ama tek
 bir dosyanın içindeki harf sayısını veren bir araç vardır
 (wc). Ayrıca bütün dosyalara "bakabilen" bir araç vardır
 (find). Birinci araç ile ikinci birleştirilerek, istenilen sonuç elde
 edilir.




