# Milyonluk Satirlari Birlestirmek

Veri ambarı projelerinde bazen duyulur "bu birleşimin iki tarafında
milyon satırdan fazlası var, bu birleşim (join) komutu geri
gelmeyebilir". Hakikaten de iki tarafın da çok sayıda veri içerdiği
durumda birleşimin çok zaman alacağı durumlar vardır. Fakat şunu
sormak lazım, birleşim sonrası ne yapıyoruz?Diyelim ki A,B üzerinde
milyon satırlık veri var, A'da bir kolonu güncelleyeceğiz, onun için
B'yi birleştiriyoruz, ve ek bilgiyi oradan alıyoruz.

Eğer A üzerinde indeks var ise, güncelleme tabii ki yavaş olacaktır,
bunun birleşim ile alakası yok, çünkü indeksler genel olarak erişimi
hızlandırır, veri eklemeyi, güncellemeyi yavaşlatır (yine bir
mühendislik al/ver -trade off- durumu). Eğer A ve B arasında birebir
ilişki var ise, mesela bir id kolonu iki tarafta da tekil, o zaman her
kullanım için birleşim yavaş olmayabilir.Evet ilişkisel teoriye
(relational theory) göre A,B birleşimi önce bir kartezyen kümesi
yaratır, yani teorik olarak A ve B'deki tüm satırların kombinasyonu
önce hazırlanır, bu örneğimizde 1 milyon x 1 milyon satır eder, sonra
bu koca küme içinde birbirine uyanlar (ortak id üzerinden)
elenir.Fakat veri tabanları, pratikte, bu tür işlemleri hissederek
hızlandırıcı önlemleri alırlar.

İki tarafta tekil olan id ile birleşim var ise, tek satırdan tek
satıra zıplayarak gereken veri alınabilir.Yani şartlara iyi bakmak
lazım. Veri ambar projelerinde geçici tablolar yaratmak her zaman iyi
tekniktir, gereken veriler birbirine uyan id taşıyacak şekilde burada
parça parça hazırlanabilir, sonra tüm parçalar birleştirilerek başka
bir boş, ya da indeksleri kapatılmış bir (nihai) tabloya toptan olarak
eklenebilir. Bu durumda birleşimlerin hızlı işlediği görülecektir.





