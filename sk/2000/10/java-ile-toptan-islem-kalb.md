# Java İle Toptan İşlem Kalıbı

Kaynak kod

Toptan (batch) işlem yapmak, bilgi işlem sistemlerinde çoğunlukla
karşımıza çıkan bir problemdir. Mesela, iki ayrı sistem arasında bilgi
transferi yapmak gerekiyor. Kaynak tablo var, sonuç tablosu JDBC
Connection nesnesi ile erişilebiliyor. Fakat ayrı sistemler oldukları
için, bir "toptan" çalışacak bir Unix sürecinin, sürekli ayakta
bekleyerek yeni gelen kayıtları görmesini bekliyoruz; Süreç yeni kayıt
gördüğü an transferi yapması gerekiyor, işi bittiği zaman belli bir
süre için uykuya yatması lazım.  Bu kodu basit bir şekilde while(true)
.. döngüsü içinde ve tek bir işlem içinde yazabiliriz. Fakat, birim
testten geçirmemiz zamanı gelince, testleri nasıl yazacağız? İşlemci,
sonsuz döngüye girdi ise, birim testlerin de sonsuza kadar beklemesi
mi gerekecek?  Bu tabii ki imkansız. Demek ki yapmamız gereken,
"dışarıdan" (test ortamı içinden) işlemcinin döngüsüne
karışabilmek.

Yani sadece birkaç tur atmaya zorlamak. Böylece, birkaç turden sonra
beklediğimiz değerleri jUnit assertEquals işlevi ile test edebiliriz.
Öteki bir kavram, uykuya yatmak: Toptan işlemler bazen 5-10 dakika
kadar işleme ara vermeyi seçebilirler. Mimarinize göre bunun kararını
programcılar verecektir. Fakat, birim testler için bu tekrar bir
problem yaratacaktır. Testlerin geçti/geçmedi cevabını verebilmesi
için (her derlemeden sonra), 5-10 dakika beklemesi mi lazım?  Gene
hayır. Bu probleme çözüm, "taklit nesne" kalıbından geliyor. Eğer
işlemci, uyumayı kendi içinde tuttuğu bir nesneye devrecek şekilde
yazılırsa, birim test ortamı bu nesne yerine kendi taklit nesnesini
koyabilir.

Bu taklit nesne, gerçek nesneyi kalıtım (extends) yolu ile
uzatacak, ve uyku fonksiyonunu "tekrar" tanımlayacaktır. Bu yeni
tekrar tanım, derhal geri dönecek şekilde yazılacaktır. Ek olarak
taklit nesnenin içinde 'sözde uyku' boolean değerini true (doğru)
değerine bile değiştibilir. Böylece birim testleri, uyumuş olma/olmama
gibi koşulları bile testlerine dahil edebilirler.  Taklit nesneler ile
gerçek nesneyi aldatmak, birim testler için son derece önemli bir
kavramdır. Bir bakıma test edilen nesnenin etrafını çepeçevre
sarıyoruz, sarılan nesnenin bundan hiç haberi olmuyor. O nesneye
kalırsa, o, hala Uykucu nesnesini çağırıyor, dış bağlantılarını
yapıyor, iletiler yolluyor. Fakat bu işi bizim 'uzatılmış' nesneler
üzerinden yaptığı için, sonuçlar "kısıtlı" oluyor. Testlerimize de
aynen böylesi lazım...

Kaynaklar

Örnek Java kodlarını ekte bulabilirsiniz. Bu örnek programda, hayali
bir muhasebe sistemine veri aktarıyoruz. Özellikle IslemciTest'e
dikkat etmenizi tavsiye ediyorum. Ve Islemci nesnesi üzerindeki main()
tanımını da inceleyin. Main, bir Java nesnesinin komut satırından
başlangıç noktasıdır, bu noktadan giriş yapılınca İşlemci'nin taklit
olmayan nesneler ile çalışacağına dikkat edin. Yani, bu şekildeki
çalışım, gerçek ortam hâlidir, yani Uykucu gibi nesnelerin gerçek
hâlleri kullanılacaktır. Fakat Test'ten giriş yapılınca, setUykucu ile
bu nesnelerin yerine taklit olanların konulacağını göreceksiniz.





