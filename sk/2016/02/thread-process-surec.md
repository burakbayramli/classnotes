# Ne Zaman Thread Ne Zaman Süreç?

Thread aynı süreç içinde paralel işlem yapmak için kullanılabilecek
bir teknolojidir; bir kod parçasını, iş ünitesini bir Thread'e vererek
işletebiliriz. Hava durumu örneğinde her şehir için ayrı bir Thread
başlatıyor olabilirdik.

Bir Thread başlatmak çok basittir, kod içinden tek bir çağrı ile
hallolur. Belli sayıda Thread yaratıp işlemleri bu "havuz" üzerinden
de yaptırabilirdik, yani sayı kısıtlaması da mümkündür.

Bu teknolojinin çok kullanışlı olduğu yerler var, mesela Web
sunucuları içinde servise gelen her istek için bir Thread
yaratılabilir, ya da Telekom şirketlerinde gelen bağlantılar ayrı
Thread'ler üzerinden halledilebilir, vs.  Diğer yandan, Thread'lerin
idaresi, takibi süreçlere göre daha zor; başlatılan bir Thread
dışarıdan durdurulamaz, onları işletim sistemi bazında "dışarıdan"
izleyecek araçlar standart değildir. Kıyasla süreçler Unix'in
belkemiğidir, onları takip etme, durdurma, başlatma bağlamında bir
sürü araç vardır. Thread'ler biraz programlama dilinin kendi
dünyasındadır denebilir, dışarıya açık değillerdir.

Süreçler idare, bakım açısından daha iyi bilinirler, mesela "bir
sürecin çökmesi" durumunda yapılabilecekler bilinir, sürecin geri
döndürdüğü kodlar vardır, "tekrar başlatmak", çıktısını bir dosyaya
yönlendirmek, gibi seçenekler hakkında bir sürü bilgi, tecrübe
mevcuttur. Ve süreçler dilden bağımsızdır. Python, C++, C süreçlerini
istediğimiz Unix aracı ile takip, kontrol edebiliriz.

Python üzerinde Thread'lerin bir negatif durumu şu; Python
yorumlayıcısı süreci tek mikroişlemciye / çekirdeğe bağlıdır (ünlü GIL
kilidi bu); bu demektir ki süreç içinde yaratılan tüm Thread'ler aynı
çekirdekte işleyecektir. O zaman eğer başka şeyi bekleyen, ona takılıp
kalmış kod yok ise, daha fazla Thread yaratmak bir performans
ilerlemesi sağlamaz. Süreçler ile her sürecin farklı bir çekirdeğe
gitme olasılığı oldukça yüksektir, Unix bu kaynak bölüştürmesini çok
doğal bir şekilde yapar.

Hem süreçler, hem de Thread'ler için ek kodlama gerekir, fakat beni
Thread'ler durumunda hep rahatsız eden şey onların potansiyel olarak
işlemekte olan programın tüm diğer bölümlerine bakabilecek olmaları -
ki bu sebepten dolayıdır ki Thread bazlı hataları ayıklamanın çok saç
baş yoldurucu olduğu hep söylenir; hatayı düzeltmek için onu
tekrarlamak (duplicate) istiyorsunuz mesela ama Thread'lerin kafasına
estiği zaman hata çıkıyor, esmediği zaman çıkmıyor!  Bu mümkün çünkü
Thread'lerin işleme sırası için bir kural yok, eğer bir şekilde, hangi
sırada belli olmadığı şekilde Thread'ler bir noktayı bozmuş ise, bu
durumu tekrar ortaya çıkartmak çok zor olacaktır. Süreçlerin
yaratabileceği hatalar, varsa, gerçekten paylaşılan kaynaklar ile
alakalı olabilir, veri tabanları, mesaj kuyrukları, dosya sistemi,
vs. Fakat bunlar zaten paylaşılan doğal kaynaklardır.

Süreçleri başlatmak daha pahalıdır, Thread'ler çok hızlı bir şekilde
başlatılabilirler. Süreç seçeneğinin negatif tarafını dengeleyen şu
var; toptan işlemler için paralelizasyon yapıyorsak, işin en başında
belli sayıda süreç başlatırız; bu bedeli başta öderiz, ardından iş
bitene kadar o belli sayıda süreci ayakta tutmakla, takip etmekle
haşır neşir oluruz.
