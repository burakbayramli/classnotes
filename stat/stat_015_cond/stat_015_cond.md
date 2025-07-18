# Koşulsal Olasılık ve Koşulsal Beklenti  (Conditional Probability, Conditional Expectation)

Olasılık teorisinin en faydalı tekniklerinden biri koşulsal olasılık ve koşulsal
beklentidir. Bunun iki sebebi var. Birincisi pratikte çoğunlukla elimizde bir
bilgi geçtiği durumda olasılık ve beklenti hesabı yaptığımız, yani istediğimiz
hesapların "koşullu'' olması. İkincisi olasılık ya da beklenti hesabında bu
hesabı ilk önce bir başka rasgele değişkene koşullamanın çok faydalı olması.

Diyelim ki tavla oynarken iki zar atıyoruz, temel olasılıktan biliyoruz ki her
seferinde 36 mümkün sonuçlardan biri ortaya çıkacak, mesela (1,2) ya da (5,5),
vs, o zaman, eğer zar hileli değilse her sonucun olasılığı 1/36. Şimdi diyelim
ki ilk zarın 4 geldiğini gördük, ve birisi diğer zarın üstünü kapattı, ve bu
bilgi ışığında bize iki zarın toplamının 6 olma olasılığının ne olduğunu
sordu. İlk zarın 4 olduğu bilgisi verildiğine göre toplamı gözönüne almadan önce
altı tane mümkün sonucu düşüürüz,, (4,1), (4,2), (4,3), (4,4), (4,5), (4,6). Bu
seçeneklerin herbirinin ortaya çıkma ihtimali birbirine eşit. Biraz önceki
sonuçları olaylar olarak düşünürsek, $E$'yi iki zarın toplamının 6 olması olayı,
$F$'yi ilk zarın 4 olma olayı olarak tanımlayabiliriz, bu durumda aradığımız
sonuç,

$$ P(E|F) $$

olarak gösterilir. Bu formülün açılımı

$$ P(E|F) = \frac{P(EF)}{P(F)}
\qquad (1)$$

$P(EF)$ hem $E$ hem $F$ olaylarının aynı anda olma olasılığı, yani $E$ kümesi ve
$F$ kümesinin kesişimi. Bölendeki $P(F)$ bir anlamda $P(E|F)$ hesabını $F$
bazında yapma amaçlı; çünkü $F$ olduğunu "biliyoruz'' ve artık örneklem
uzayımız $F$ haline geliyor, bu uzay içinde $E$'nin olma olasılığına
bakıyoruz. Not: Hesabın geçerli olması için $P(F)>0$ olmalı tabii ki. 

Biraz önceki örnek için aradığımız cevap 1/6 çünkü altı mümkün sonuç içinde
sadece (4,2) olayı bizi ilgilendiriyor. 

Bağımsız Olaylar

İki olay $E,F$'nin birbirinden bağımsız olduğu söylenir, eğer

$$ P(EF) = P(E)P(F) $$

ise. (1) denklemi üzerinden bu

$$ P(E|F) = P(E) $$

sonucunu verir, bu sonuç akla yatkın olmalı, eğer $E,F$ bağımsız ise, $F$'in
verilmiş olması bize $E$ hakkında hiçbir şey söylemez. 

Bayes Formülü

Yine $E$ ve $F$ olayları var, ki $EF$ hem $E$, hem de $F$'nin olma durumu, o
zaman $E$

$$ E = EF \cup EF^c $$

olarak gösterilebilir çünkü bir öğenin $R$ içinde olması için ya $E$ ve $F$
içinde olması, ya da $E$ içinde olması ama $F$ içinde olmaması lazımdır. $EF$ ve
$EF^c$ birbirlerinin tam tersi, karşılıklı dışarlayan (mütually exclusive)
olaylar oldukları için alttaki doğru olacaktır,

$$ P(E) = P(EF) + P(EF^c) $$

$$ = P(E|F)P(F) + P(E|F^c)P(F^c) $$

$$ = P(E|F)P(F) + P(E|F^c)(1-P(F)) $$

Üstteki son formül $P(E)$'nin bir ağırlıklı ortalama olduğunu söylüyor;
ağırlıklar $F$'in olma ve olmama olasılığı, ve bu ağırlıklar $F$'nin olduğu ve
olmadığının verildiği durumdaki $E$ olasılıklarının ağırlıklı ortalamasını
alıyorlar.

Örnek

Bir hastalık için bir labaratuarun test tekniğinin yüzde 95 başarısı var. Fakat
bu test bazen "yanlış pozitif'' cevabı da verebiliyor; hasta olmayan kişilerin
yüzde 1'i için, yani 0.01 olasılıkla test hasta diyebiliyor. Eğer toplumun yüzde
0.5'inde bu hastalığın olduğu biliniyorsa, herhangi bir kişinin testi pozitif
geldiğinde o kişinin gerçekten hasta olma olasılığı nedir?

Cevap

$D$ test edilen kişinin hasta olma olayı diyelim, $E$ testin pozitif
olması. Aradığımız $P(D|E)$ olasılığı.

$$ P(D|E) = \frac{P(DE)}{P(E)} $$

$$
= \frac{P(E|D)P(D)}
{P(E|D)P(D) + P(E|D^c)P(D^c)}
\qquad (2)
$$

$$
\frac{(0.95)(0.005)}
{(0.95)(0.005) + (0.01)(0.995)} $$

$$ = \frac{95}{294} \approx 0.323 $$

Yapılan bazı hareketlere dikkat: 4 üstteki denklemde $P(DE)$'yi onun bir altında
$P(E|D)P(D)$, yani $P(ED)$'ye çevirdik, çünkü $P(DE)$ ile $P(ED)$ aynı şey.

Ayrıca çözüme yaklaşma şeklimiz istenen $P(D|E)$ için $P(E|D)$ ve onunla alakalı
olan rakamları kullanmak; problemde bildiğimiz $E$'nin verildiği durum değil,
onun tersi, $D$'nin verildiği durum. Yani test tekniği hakkında elimizde bazı
bilgiler var, *bu bilgiler ışığında* test pozitif verirse bu sonuca ne kadar
inanalım diyoruz bir anlamda.

(2) formülü, onun ikiden fazla seçenek için ayrıksal, ya da sürekli olarak
genelleştirilmiş hali Bayes Formülü olarak biliniyor. 

Rasgele Degiskenler

Hatırlayalım, iki olay $E,F$ için $P(F)>0$ ise, $F$'in verildiği durumda (koşul)
$E$'nin olasılığı

$$ P(E|F) = \frac{P(EF)}{P(F)} $$

O zaman $X,Y$ ayrıksal rasgele değişkenler ise $Y=y$ verildiği durumda $X$'in
koşulsal olasılık kütle fonksiyonunu (conditional probability mass function)
şöyle tanımlayabiliriz,

$$ p_X(x|y) = P(X=x|Y=y) $$

$$ = \frac{P(X=x,Y=y)}{P(Y=y)} $$

$$ = \frac{p(x,y)}{p_Y(y)} $$

ki tüm $y$ değerleri için $P(Y = y) > 0$ olmalı. Benzer şekilde $Y=y$
verildiği durumda $X$'in koşulsal olasılık dağılım fonksiyonu, ki yine tüm $y$
değerleri için $P(Y = y) > 0$ olacak şekilde,

$$ F_{X|Y}(x|y) = P(X \le x | Y=y) $$

$$ \sum_{a \le x} p_X(a|y)$$

Son olarak $Y=y$ verildigi durumda $X$'in kosulsal beklentisi,

$$ E(X|Y=y) = \sum_{x}x P(X=x | Y=y ) $$

$$ = \sum_x x p_X(x|y) $$

Yani herşey daha önce normal olasılık tanımlarında olduğu gibi, sadece şimdi
tüm ifadeler $Y=y$ olayına koşullu. Bunun söyleyebiliyoruz çünkü eğer $X$
$Y$'den bağımsız olsaydı

$$ p_X(x|y) = P(X=x|Y=y)  $$

$$ = P(X=x) $$

olurdu, ve bu formülü üstteki formüllerde koşullu olanın yerine koyduğumuzda
normal olasılık denklemlerini elde ederdik.

Örnek

$p(x,y)$'in $X,Y$ rasgele değişkenlerinin ortak olasılık kütle fonksiyonu
olduğunu farz edelim, ve

$$ p(1,1) = 0.5, \quad
p(1,2) = 0.1, \quad
p(2,1) = 0.1, \quad
p(2,2) = 0.3
$$

olsun. $Y=1$'in verili olduğu durumda $X$'in olasılık kütle fonksiyonunu
hesaplayın.

Çözüm

$Y$ sadece 1 olabileceğine göre, $p_{X|Y}(1|1)$ ve $p_{X|Y}(2|1)$'i hesaplanırsa
iş biter. Bu değerleri ayrı ayrı bulacağız çünkü dağılım bir formül değil,
üstteki gibi sunulan ayrıksal olasılıklarla her ihtimal ayrı çözülmeli.

$$ p_{X|Y}(1|1) = P(X=1|Y=1) = \frac{P(X=1,Y=1)}{P(Y=1)}
$$

$$ = \frac{p(1,1)}{p_Y(1)} = \frac{5}{6} $$

$p_Y(1)$'i nasil bulduk? Soyle,

$$ p_Y(1) = \sum_x p(x,1) = p(1,1) + p(2,1) = 0.6 $$

Ayrıca iki üstte $5/6$ oldu çünkü $0.5/0.6=5/6$. Devam edelim,

$$ p_{X|Y}(2|1) =
\frac{p(2,1)}{p_Y(1)} = \frac{1}{6}
$$

Sürekli Durum

Sürekli rasgele değişkenler için koşullu olasılık yoğunluk fonksiyonları 

$$ f_{X|Y}(x|y) = \frac{ f_{X,Y}(x,y)}{f_Y(y)} $$

Eğer koşullu yoğunluk üzerinden olay hesabı yapmak istersek, ve $f_Y(y) > 0$
olduğunu farzederek,

$$ P(X \in A | Y = y) = \int_A f_{X|Y}(x|y) \mathrm{d} x $$

Örnek 

$$ f(x,y) = 
\left\{ \begin{array}{ll}
x+y & \textrm{eğer } 0 \le x \le 1, 0 \le y \le 1 \\
0 & \textrm{diğer}
\end{array} \right.
$$

$P(X < 1/4 | Y = 1/3)$ nedir? 

Cevap 

Üstteki olasılık hesabı için $f_{X|Y}$ fonksiyonuna ihtiyacımız var,

$$ f_Y(y) = \frac{ 1}{2} + y  $$

olsun. Ana formülümüz neydi? 

$$ f_{X|Y}(x|y) = \frac{f_{X,Y}(x,y)}{f_Y(y)} $$

$$ = \frac{ x+y }{\frac{1}{2} + y} $$

$$
P(X < 1/4 | Y = 1/3) = 
\int_{ 0}^{1/4} \frac{ x+ \frac{1}{3} }{\frac{1}{2} + \frac{1 }{3}} \mathrm{d} x = 
\frac{\frac{1}{32}+ \frac{1}{3} }{\frac{1}{2} + \frac{1}{3}} = 
\frac{14}{32}
$$

Beklentileri Koşullayarak Hesaplamak

$E(X|Y)$ beklenti hesabını düşünelim, bu hesap $Y$'nin bir fonksiyonu olacaktır
bir bakıma, yani her $y$ için $E(X|Y=y)$ farklı bir sonuç verecektir. Önemli
nokta, $E(X|Y)$'nin kendisi de bir rasgele değişkendir. Koşulsal beklentinin çok
önemli özelliklerinden biri her $X,Y$ rasgele değişkeni için

$$ E(X) = E(E(X|Y)) $$

eşitliğinin doğru olmasıdır. Biraz değişik bir notasyona göre,

$$ E_Y (E_{X|Y} (X|Y)) = E(X) $$

İspat

Ayrıksal durum için,

$$ E_Y (E_{X|Y} (X|Y)) = E_Y \bigg( \sum_{x} x \cdot P(X=x|Y) \bigg) $$

$$  = \sum_y \bigg( \sum _x x \cdot P(X=x|Y=y) \bigg) P(Y=y) $$

$$  = \sum_y  \sum _x x \cdot P(X=x|Y=y) P(Y=y) $$

$$  = \sum_x x \sum _y P(X=x|Y=y) P(Y=y) $$

$$  = \sum_x x \sum _y P(X=x,Y=y) $$

$$  = \sum_y x  P(X=x) $$

$$ = E(X) $$

Soru

Bir maden işçisi kaza sonrası bir madende tıkalı kalıyor. Dışarı çıkabilmesi
için önünde üç değişik kapı var; birinciyi seçerse 2 saat yürüdükten sonra
dışarı çıkacak. Eğer ikinciyi seçerse 3 saat yürüdükten sonra tekrar madene
dönecek. Üçüncüyü seçerse bu sefer 5 saat yürüdükten sonra madene dönecek. İşçi
hangi kapının onu dışarı çıkartacağını bilmiyor, herhangi bir kapıyı eşit
olasılıkla seçecek. İşçinin dışarı çıkma süresinin beklentisi nedir?

Cevap

$X$ değişkeni işçinin dışarı çıkma süresini belirten rasgele değişken olsun,
$Y$ ise işçinin ilk seçtiği kapı.

$$
E(X) = E(X | Y=1) P(Y=1) + E(X | Y=2) P(Y=2) +E(X | Y=3) P(Y=3) 
$$

$$
E(X) = \frac{1}{3} \big( E(X | Y=1)  + E(X | Y=2) +E(X | Y=3) \big)
\qquad (3)
$$

Buraya kadar basit: Fakat sorudaki puf noktaya dikkat, eger kapi 2 ay da 3
secilirse isci tekrar madene donuyor, yani bastaki haline geri gelmis oluyor. O
zaman

$$ E(X|Y=1) = 2$$

$$ E(X|Y=2) = 3 + E(X) $$

$$ E(X|Y=3) = 5 + E(X) $$

Üstteki iki denklemi (3) içine koyarsak,

$$
E(X) = \frac{1}{3} \big( 2  + 3 + E(X) + 5 + E(X) \big)
$$

$$ E(X) = 10 $$

İşçinin dışarı çıkma beklentisi 10 saattir.

Kaynaklar

[1] Ross, *Introduction to Probability Models, 10th Edition*


