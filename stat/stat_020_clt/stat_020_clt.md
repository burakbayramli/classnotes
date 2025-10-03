# Büyük Sayılar

Büyük Sayılar Kanunu (Law of Large Numbers)

Bu kanun, örneklem (sample) ile rasgele değişkenler, yani matematiksel
olasılık dağılımları arasında bir bağlantı görevi görür. Kanun kabaca
bildiğimiz günlük bir gerçeğin matematiksel ispatıdır. Yazı-tura atarken
yazı çıkma ihtimalinin 1/2 olduğunu biliyoruz; herhalde çoğumuz bu
yazı-tura işlemin "bir çok kere" tekrarlandığı durumda, toplam sonucun
aşağı yukarı yarısının yazı olacağını bilir.

Matematiksel olarak, farzedelim ki her yazı-tura atışı bir deney
olsun. Deneylerin sonucu $X_1, X_2...X_n$ olarak rasgelen değişkenlerle
olsun, bu değişkenlerin dağılımı aynı (çünkü aynı zar), ve birbirlerinden
bağımsızlar (çünkü her deney diğerinden alakasız). Değişkenlerin sonucu 1
ya da 0 değeri taşıyacak, Yazı=1, Tura=0.

Büyük Sayılar Kanunu tüm bu deney sonuçlarının, yani rasgele değişkenlerin
averajı alınırsa, yani $\bar{X} = X_1 + .. + X_n$ ile, elde edilen sonucun
$X_i$'lerin (aynı olan) beklentisine yaklaşacağının söyler, yani $n$ büyüdükçe
$\bar{X}_n$'in 1/2'ye yaklaştığını ispatlar, yani $E[X_i] = 1/2$
değerine. Notasyonel olarak $E(X_i) = \mu$ olarak da gösterilebilir.

Özetlemek gerekirse, bir olasılık dağılımına sahip olan, görmediğimiz bir
"yerlerde'' olan bir dağılımdan bir örneklem alıyoruz, örneklem bir zar
atma işlemi gibi (simülasyon ile bu değişkenleri de doldurabilirdik), sonra
bu değişkenlerin averajını alıyoruz, ve bu averajın o görmediğimiz
bilmediğimiz "gerçek'' dağılımın $\mu$ değerine yaklaştığını görüyoruz. 

Formülsel olarak, herhangi bir $\epsilon > 0$ için,

$$ \lim_{n \to \infty} P(|\bar{X} - \mu| \le \epsilon) = 1$$

ya da

$$ \lim_{n \to \infty} P(|\bar{X}_n-\mu| > \epsilon) = 0 $$

ya da 

$$ P(|\bar{X}_n-\mu| > \epsilon) \rightarrow 0 $$

Burada ne söylendiğine dikkat edelim, $X_i$ dağılımı *ne olursa olsun*, yanı
ister Binom, ister Gaussian olsun, *örneklem* üzerinden hesaplanan sayısal
ortalamanın (empirical mean) formülsel olasılık beklentisine yaklaştığını
söylüyoruz! $X_i$'ler en absürt dağılımlar olabilirler, bu dağılımların
fonksiyonu son derece çetrefil, tek tepeli (unimodal) bile olmayabilir, o
formüller üzerinden beklenti için gereken entegralin belki analitik çözümü bile
mevcut olmayabilir! Ama yine de ortalama, o dağılımların beklentisine
yaklaşacaktır.  İstatistik ile olasılık teorisi arasındaki çok önemli bir
bağlantı bu.

Sonuç şaşırtıcı, fakat bir ek daha yapalım, sezgisel (intuitive) olarak
bakarsak aslında sonuç çok şaşırtıcı olmayabilir. Niye? Diyelim ki genel
veri $N(\mu,\sigma^2)$ şeklinde bir Normal dağılımdan geliyor ve örneklem
de bu sebeple aynı dağılıma sahip. Bu durumda örneklemdeki veri
noktalarının $\mu$'ya yakın değerler olmasını beklemek mantıklı olmaz mı?
Çünkü bu dağılım "zar atınca'' ya da bir genel nüfustan bir "örnek
toplayınca'' (ki bunu bir anlamda istatistiksel bir zar atışı olarak
görebiliriz) onu $\mu,\sigma^2$'e göre atacak. Örneklemi zar atışı
sonuçları olarak gördüğümüze göre elde edilen verilerin bu şekilde olacağı
şaşırtıcı olmamalı. Ve bu zar atışlarının ortalamasının, son derece basit
bir aritmetik bir işlemle hesaplanıyor olsa bile, $\mu$'ye yaklaşması
normal olmalı.

Bu arada, bu argümana tersten bakarsak Monte Carlo entegralinin niye
işlediğini görebiliriz, bkz [3].

Özellikle örneklem ile genel nüfus (population) arasında kurulan bağlantıya
dikkat edelim. İstatiğin önemli bir bölümünün bu bağlantı olduğu
söylenebilir. Her örneklem, bilmediğimiz ama genel nüfusu temsil eden bir
dağılımla aynı dağılıma sahip olan $X_i$'dir dedik, ve bu aynılıktan ve
bağımsızlıktan yola çıkarak bize genel nüfus hakkında bir ipucu sağlayan
bir kanun geliştirdik (ve birazdan ispatlayacağız).

İspata başlayalım.

$X_1,X_2,..,X_n$ bağımsız değişkenler olsun. 

$$ E(X_i) = \mu $$

$$ Var(X_i) = \sigma $$

$$ \bar{X}_n = \frac{1}{n} \sum_{i=1}^n X_i  $$

$\bar{X}_n$ de bir rasgele değişkendir, çünku $\bar{X}_n$ değişkeni her
$X_i$ dağılımıyla alakalı.

İspata devam etmek için $\bar{X}_n$ dağılımının beklentisini bulmamız gerekiyor.

$$ E(\bar{X}_n) = E(\frac{1}{n} \sum_{i=1}^n X_i)  $$

E doğrusal bir işleç (linear operatör) olduğu için dışarıdan içeri doğru
nüfuz eder. 

$$ = \frac{1}{n} \sum_{i=1}^n E(X_i) = \frac{1}{n}n\mu = \mu $$

Dikkat edelim, bu *ortalamanın* beklentisi, ortalamanın kendisinin
hangi değere yaklaşacağını hala göstermiyor. Eğer öyle olsaydı işimiz
bitmiş olurdu :) Daha yapacak çok iş var.

Şimdi $\bar{X}_n$ dağılımının standart sapmasını da bulalım. Diğer bir
olasılık kuramına göre

$$ Y = a + bX $$

$$ Var(Y) = b^2Var(X) $$

oldugunu biliyoruz. O zaman,

$$ \bar{X}_n = \frac{1}{n} \sum_{i=1}^n X_i  $$

$$ Var(\bar{X}_n) = Var(\frac{1}{n}\sum_{i=1}^nX_i) = 
\frac{1}{n^2}\sum_{i=1}^n Var(X_i)
$$

$$ 
Var(\bar{X}_n) = \frac{1}{n^2}\sum_{i=1}^n \sigma^2 = 
\frac{1}{n^2}n\sigma^2 = \frac{\sigma^2}{n} 
\qquad (3)
$$

Artık Çebişev kuramını kullanmaya hazırız. Ispatlamaya calistigimiz neydi?
$n \rightarrow \infty$ iken,

$$ P(|\bar{X}_n-\mu| > \epsilon) \rightarrow 0 $$

Çebişev'den

$$ P(|\bar{X}_n-\mu| > \epsilon) \le \frac{Var(\bar{X}_n)}{\epsilon^2} $$

$$ P(|\bar{X}_n-\mu| > \epsilon) \le \frac{\sigma^2}{n\epsilon^2}
\rightarrow 0 $$

$\sigma^2 / n\epsilon^2$'in sıfıra gitmesi normal çünkü $n$ sonsuza gidiyor.

Peki $P(|\bar{X}_n-\mu| > \epsilon)$'nin sıfıra gittiğini gösterdik mi? 

$\sigma^2 / n\epsilon^2$'nin sıfıra gittiğini gösterdik. $\sigma^2 /
n\epsilon^2$ de $P(|\bar{X}_n-\mu| > \epsilon)$'den büyük olduğuna göre, demek
ki o da sıfıra iner.

Çebişev Eşitsizliğinin ispatı ek bölümde bulunabilir.

$\square$

Büyük Sayılar Kanunu örneklem ortalamasının ve varyansının $X_i$'in
beklentisi ve varyansı ile bağlantı kurar. Merkezi Limit Teorisi bir adım
daha atar, ve der ki "$\bar{X}$'in dağılımı Gaussian dağılım olmalıdır
yani normal eğrisi şeklinde çıkmalıdır!''. Teorinin detayları bu bölümde
bulunabilir. 

Merkezi Limit Teorisi (Central Limit Theorem -CLT-)

Büyük Sayılar Kanunu örneklem ortalamasının gerçek nüfus beklentisine
yaklaşacağını ispatladı. Örneklem herhangi bir dağılımdan
gelebiliyordu. CLT bu teoriyi bir adım ilerletiyor ve diyor ki kendisi de
bir rasgele değişken olan örneklem ortalaması $\bar{X}$ Normal dağılıma
sahiptir! Daha detaylandırmal gerekirse, 

Diyelim ki $X_1,..,X_i$ örneklemi birbirinden bağımsız, aynı dağılımlı ve
ortalaması $\mu$, standart sapması $\sigma$ olan (ki o da aynı dağılıma
sahip) bir nüfustan geliyorlar. Örneklem ortalaması $\bar{X}$, ki bu
rasgele değişkenin beklentisinin $\mu$, ve (3)'e göre standart sapmasının
$\sigma / \sqrt{n}$ olduğunu biliyoruz. Dikkat: $\bar{X}$'in kendisinden
değil, *beklentisinden* bahsediyoruz, BSK'deki aynı durum, yani
ortalama dağılımının ortalaması. Teori der ki $n$ büyüdükçe $\bar{X}$
dağılımı (bu sefer kendisi) bir $N(\mu, \sigma/\sqrt{n})$ dağılımına
yaklaşır.

Bu ifade genelde standart normal olarak gösterilir, herhangi bir normal
dağılımı standart normal'e dönüştürmeyi daha önce görmüştük zaten,
beklentiyi çıkartıp standart sapmaya bölüyoruz, o zaman örneklem dağılımı
$\bar{X}$,

$$ Z = \frac{\bar{X} - \mu}{\sigma / \sqrt{n}} $$

dağılımına yaklaşır diyoruz, ki $Z = N(0,1)$ dağılımıdır, beklentisi sıfır,
standart sapması 1 değerindedir. 

Bu teorinin ispatını şimdilik vermeyeceğiz. 

Kaynaklar

[1] Wolfram Mathworld, *Maximum Likelihood*, [http://mathworld.wolfram.com/MaximumLikelihood.html](http://mathworld.wolfram.com/MaximumLikelihood.html)

[2] *Introduction to Probability and Statistics Using R*

[3] Bayramlı, Istatistik, *Monte Carlo, Entegraller, MCMC*
