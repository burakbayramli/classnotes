# XP ve Özellikler

XP yönteminin tamamen özellik kontrollü (function oriented) bir sistem
olduğu herhalde açık. Bu açıdan, proje başında 3-4 aylık tasarım yapan
ve bir torba belge üreten yöntemlerden XP ayrılıyor. XP projelerinde,
'mimari bitti mi?', 'tasarım belgesi hazır mı?' soruları yerine,
'özellik bitti mi?' sorusunu duyarsınız. Zaten bir projede kullanıcı,
o proje için mevkisini riske eden üst düzey yönetici için önemli olan
da budur.

Özellik.  Bu yüzden, özellik olarak kartların üzerine
yazılan şeylerin, hakikaten özellik olması çok
önemlidir. Bitme/bitmeme mefru 'özellik' olduğu için, eğer özellik
olmayan bir şey kart üzerine yazılmış ise, kargaşa
yaratacaktır. Mesela:

Özellik:E-posta gönderecek bir Java yardımcı nesnesi yaz.

Programcılar:Ahmet, Veli

Geliştirme kalemleri:- Nesneyi yaz (Tahmin: 1 gün)

Bu yanlış bir özelliktir. Her şeyden önce, bu özelliğin müşteri
tarafından konulduğu şüphelidir, zaten müşteri nesne filan
bilmez. Büyük bir ihtimalle programcının 'lazım olur, koyalım' diyerek
koyduğu bir 'özelliktir' bu.  Bu sözde özellik tipik bir mimari
kodlamanın, özellik kartı olarak araya kaynaması durumudur. Özellikle
vurgulamak gerekir ki, XP mimariyi sadece bir özelliğe gerek olduğu
kadar yazar. Öyle her özelliğe lazım olacağını tahmin ettiğimiz bir
'mimari nesneyi' baştan, ve o mimariyi kullanacak özellik daha ortada
bile olmadan kodlayamayız.

Yukarıdaki örnekte, ortak bir e-posta gönderen yardımcı 'kütüphane'
kodu vardır, fakat bu kodu hangi özellik kullanacaktır?  Nasıl
kullacaktır? Ne kadar iyi tahmin yeteneğiniz olursa olsun, bir
özelliğin bu yardımcı nesneyi gerçekten kullaması gerektiğinde, ilk
arayüz tasarımınızda boşluklar bulacaksınız. Tabii ki siz çok
tecrübeli programcısınız, tabii ki 8 programlama dili biliyorsunuz,
fakat konumuz bu değildir.

Konumuz, niye gereksiz bir şey hakkında, efor sarfedeceğimizdir.
Ayrıca, üstteki gibi bir sözde özelliğin bitip bitmeme kriteri
tanımsızdır. Zaten en iyi turnusol testi de budur. Müşteriniz bu
şekilde bir özelliği test edemiyor ise, biliniz ki mimari bir işlem
aradan kaynadı.  Daha iyi bir özellik şöyle olabilirdi.

Özellik:Kullanıcı, banka transferi yaptıktan sonra ona e-posta
gönderilir.

Programcılar:Ahmet, Veli

Geliştirme kalemleri:

  * JSP (görsel) kod içine e-posta tetikleyici kod yerleştir. (Tahmin: 0.5 gün)-

  * E-posta gönderecek nesneyi yaz (Tahmin: 1 gün)





