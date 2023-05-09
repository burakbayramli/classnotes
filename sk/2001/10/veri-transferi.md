# Veri transferi

(Bu yazı boyunca CRM sistemi, CRM için kullanılan bir veri ambarını
temsil edecektir) CRM sistemleri boşluk içinde yaşamazlar. Aslında
bütün CRM sistemleri, bilgisi için başka sistemlere muhtaçtır. CRM
sistemlerine veri alışverişi genelde akşam vakti, eğer çok veri varsa
bütün gün boyunca sürer. Bu veri transferi, hem direk transfer olarak
yapılır, bazen de veri yarıyolda biraz masajlanır. Bunun çok önemli
sebepleri vardır. Düşünün, Migros icin CRM veri transferi
yazıyorsunuz, ve veriler 4 değişik yerden geliyor. Bu değişik veri
çeşitleri birbirine uymayabilir, o yüzden transfer sıresında "ortak"
bir kayıt şekline çevirilirler. CRM veri tabanı planlaması bu ortak
kayıt şeklini bulmak için yapılır.  Biraz önce bahsettiğimiz veri
anlaşmazlığı, bir çok seviyede başınıza gelebilir. Mesela, ilk başta
müşteri kayıtları uymaz. Müşteri 234 ile musteri 444 değişik veri
tabanlarında ayrı olmalarına rağmen aslında ayni insanlar. Veri
transfer programı bunu nasıl anlayacak? Bu sorunun yanıtı veri
transferi masajlama sırasında verilir.  Bizim genelde kullandığımız
yöntem, müşterinin kişisel bilgileri içinden bir 'anahtar'
yaratmaktır. Müşteri ilk isimden 3 harf, son isimden 2 harf, posta
kodunda 5 harf toplayıp birbirine eklerseniz, elinize bir müşteri
anahtarı çıkacaktır. Bu anahtari her müşteri için, ve her dış veri
tabanı için yaratın. O zaman birbirine uyan müşteriler aynı müşteri
demektir, ve CRM tabanında birleştirilebilirler. Bu konular diğer
yazılarımızda detayla işlenecek.  Bir önemli konu daha: Web sitesi
veri tabanı ile rapor veri tabanları her zaman ayrı olmalı. Mesela
"web tabanı" içinde abone kayıtları, mal listesi var. Eğer siteyi
analiz etmek istiyorsanız, akşamları veriyi ikinci CRM tabanına
transfer etmeniz gerekir. Eğer analizi direk web veri tabanı üzerinde
yapmaya kalkarsanız, web sayfalarını yavaşlatırsınız. Çünkü bazı web
sayfaları, web veri tabanına cok bağlıdır ve taban hızlı cevap
vermezsa (CRM analizi yüzünden), sayfalar çabuk yüklenemez. Bunu
kesinlikle yapmayın. Zaten CRM veri tabanı çok büyük olacaktır, web
veri tabanı o kadar veriye yer bulamaz.  Transfer kodlaması iyi
planlanması gereken, geliştirme için zaman gerektiren bir iştir. Eğer
bir CRM projeniz varsa, bu iş bölümünü katiyen hafife almayın. CRM
projelerinin %80'i transfer kodlamakla geçer. Çok önemli!  Transfer
genelde SQL dili ile yapılır. Güzel ikonlar ile transfer yapmanıza
yardımcı olan programlar var: Fakat SQL kullanmak programcıya daha çok
kontrol verecektir. Kod seviyesinde bu kontrol ileride işinize yarar,
transfer programı yavaş işliyor ise, hızlandırmak için değişik
eklemeler yapmak gerekebilir. Her veri tabanı paketi değişik SQL
hızlandırma metodları içerir. Bunları kullanmanın en iyi yolu genelde
kodu veya veri tabanını düzeltmekten geçer. Veri tabanınızın bütün
hızlandırma özelliklerini kullanın. Doğru yazılmış SQL kodu ile
bilmeden yazılmış SQL arasındaki fark 10 kat bile olabilir!





