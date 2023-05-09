# Oracle'ı Hızlandırmak ve İndisler

Coğunlukla tipik bilgi işlem (OLTP) performans sorunlarının altında,
veri tabanı indislerinin yanlış kurulmuş olması yatar. Bu yazıda
düzgün indis kullanımı için bazı kurallar vereceğiz.


Düzgün İndis Kullanımı

Performans eniyileştirme kararlarının çoğu, hangi indisi nerede kullanalım sorusu etrafında geçer. Bilgi işlem tarihinde, indisler bazen gereğinden az, bazen de gereğinden fazla kullanılmıştır. Benim tecrübeme göre, iki şekilde VTİ (veri tabanı idarecisi) çok gördüm. Ekteki liste, indisleri düzgün kullanmak için bir tavsiye listesidir.

* WHERE ifadelerinde sık kullanılan kolonları indisleyin.

* İki tabloyu birbirine zincirlemek (JOIN) için kullanılan kolonları indisleyin.

* Seçiciliği yüksek olan kolonları indisleyin.

* Seçiciliği en az olan kolonları BİTMAP indisi ile indisleyin.

Not: Seçicilik (selectivity), herhangi bir kolon X için, her satırın
ne kadar değişik değer taşıdığına verilen isimdir. Eğer seçicilik bir
kolon için yüksek ise, o kolondaki değerler her satırda daha sık
değişir.

Üstteki tavsiyelere ek olarak, birkaç ek indis kullanımı daha tavsiye
edebiliriz.

1'e yakın seçiciliği (yüksek) olan, ve nokta sorgu (point query)
bağlamında kullanılan kolonlar için, Oracle hash indisi
kullanın. Nokta sorgu, kesin uyuşan, tek bir satıra hedefli bir sogru
demektir. Mesela bir kimlik kütüğünde bir şahsı getirmek için sosyal
sigorta numarası üzerinden yapılan bir sorgu, nokta sorgusuna bir
örnektir.

Öteki tür kolonlar için normal (B*tree) indisleri kullanabilirsiniz;
bu tür kolonlar genelde bir yelpaze türünden bir pencere içine düşen
birden fazla satırı geri getirmek için kullanılacaklardır. Bu sorgular
WHERE ibaresinden sonra, ya büyüktür işareti ya küçüktür işaretini
kullanan, ya da LIKE % gibi kolon içine bakarak satır tarayan
sorgulardır.

Sık olarak güncelleştirilen, yani 'üzerine yazılan' kolonlar için
indis kullanmakta dikkatli olun. Ayrıca, üzerinde çok silme ve ekleme
işlemi yapılan 'tabloların' da kolonlarını genelde indislemeMEk için
özen gösterin. Bu bahsedilen iki işlem de Oracle'ın indisleri izlemek
için içinde tuttuğu B*tree veri yapısında çok sık değişimlere yol
açacağı için, performans acısından pahalı seçenekler olacaktır, ve bu
sürekli değişim indis iç yapısının bozulmasına, istikrarının
dağılmasına (fragmentation) yol açacaktır. Asal anahtarlar ve
göstergeç anahtarları bu kural için istisnadır.

Asal ve göstergeç anahtarlarını kesinlikle indislemeniz
gerekir. Oracle, asal anahtar kolonları üzerinde kendiliğinden zaten
bir tekil indis (unique index) yaratacaktır, ama göstergeç kolonlar
üzerinde hiçbir indis yaratılmaz. Bu indisi CREATE ve ya ALTER ile
sizin yapmanız gerekecektir.

İndislerinizi, gerektiğinde birden fazla kolonun oluşturduğu birleşik
indis haline getirebilirsiniz. Böyle yapmak ile Oracle'ın
eniyileştirici (optimizer) alt-programına yardımcı olur. Fakat dikkat:
Oracle, Sybase'den ayrılan bir şekilde daha sıkı bir indis
kapsayıcılığı kuralını takip eder. Bu kurala göre, bir sorgu sırasında
indisin devreye sokulması sadece ve sadece eğer o kolon birleşik
indisin başından başlayarak aynı sırada bir şekilde uyum bulunmuş ise
devreye sokulacaktır. Mesela, bir tablonun a, b ve c kolonları
üzerinde bir a+b+c birleşik indisi var ise, eniyileştirici bu indisi
sadece WHERE ibaresinde a, a+b, ya da a+b+c kolonlarını aynı sırada
bulduğu durumda kullanacaktır. B, c, b+c, ya da a+c kolonlarının
WHERE'de bulunması, Oracle için indisi devreye sokmaz.

Üzerinde sorgu içinde fonksiyon işletilen kolonları (MIN ve MAX
haricinde) hiç indislemeyin. Oracle eniyileştirici böyle kolonlar için
indis kullanmaz.

Alternatif olarak, bir türetilmiş kolon yaratabilirsiniz, ya da tiyo
ya da numaralar kullanarak sorgunuzu silbaştan
tasarlayabilirsiniz. EXPLAIN PLAN komutunu kullanarak sonuçlarınızı
kontrol edin.

Genelde üzerinde NULL ya da eşitsizlik karşılaştırılması yapılan
kolonları indisleyeMEyin. Aşağıdaki operatörler eğer bir kolon
üzerinde kullanılıyorsa Oracle eniyileştirici indis kullanmaz.


* IS NULL

* IS NOT NULL

* !=

Eşitsizlikten kurtulmak için, eğer mümkünse sorguyu = a da IN
kullanacak şekilde tekrar yazabilirsiniz.

İçinde görüntüye (view), ya da alt-sorguya (subquery) referans yapan
sorgularda indis kullanımı için dikkatli olun. Bu tür sorgular
üzerinde EXPLAIN PLAN'i tekrar tekrar işleterek indislerinizin
hakikaten kullanılıp kullanılmadığını ortaya çıkarın. Eğer
kullanılmıyorlar ise, alt-sorguyu onu kullanan üst sorguya daha sıkı
katarak indis kullanımını tetiklemeye uğraşın, ya da alt-sorguyu, ufak
birbirinden ayrı sorgular halinde parçalayın. Aynı şekilde,
görüntülerden tamamıyle vazgeçmeye hazır olun, ya da bu görüntüleri
somutlaşmış görüntü (materialized views) haline getirmeyi tasarlayın.

Sonuç olarak, indis kullanımını uygun olduğu zaman yapın. İndislemiş
olmak için indislemeyin. İş hayatımızda üzerinde 'hiç' indis olmayan
veri tabanı da gördük, 'her kolon' üzeinde indis koyulmuş veri tabanı
da...Üzerinde hiç indis olmayan veri tabanının kötü olduğu kesindir,
cünkü en azından, asal anahtarlar üzerinde indis olması en tecrübesiz
VTİ'ın bile bildiği bir şeydir. Her kolon üzerinde indis olması ise ya
gereksinim belgesinde, ya veri tabanı tasarımında ya da uygulama
tasarlanmasında olan eksikliklerin bir işaretidir.






