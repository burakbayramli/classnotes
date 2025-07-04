# Kafadan Hesap

Kafadan zor hesapları yapabilmek aslında süper zeka gerektirmiyor. Matematiksel
bazı numaralar ile çok zor bazı hesapları ya kesin, ya da yaklaşıksal olarak
yapmak mümkün. Dizilerde ve filmlerde bazen yanlış bir şekilde bilimci kişiler
mesela $\pi$ sayısını "noktadan sonra 50. basamağa kadar'' bilmekle övünüyor,
ya da çok ağır hesapları sanki kafasında bilgisayar varmış gibi
yapıyorlar. Hesap için kafada bilgisayara gerek yok, matematiksel geçişler, çok
iyi bilinen formüller yeterli. Mesela [1, sf. 186]'dan bir örnek,

"[Ünlü bilim adamları Bethe ve Feynman] birbirleriyle hesap yarışı yapmaktan
zevk alırlardı. Bir defasında bir formül üzerinde çalışırlarken 48'in karesini
hesaplamaları gerekti. Feynman masada duran hesap makinasına uzandı, tam o
sırada Bethe 'çözüm 2300' dedi. Ama Feynman tuşlara basmaya devam etti, bir
yanda 'tam sonucu bilmek istemiyor musun?' dedi. Bethe tam o sırada 'tam sonuç
2304' der. '50'ye yakın sayıların karesini almayı bilmiyor musun?' diye sorar,
ve numarayı açıklar. 50'nin karesi 2500, bu basit. 50'den biraz büyük ya da
küçük sayılar için sonuç 2500'den birkaç 100 daha küçük ya da büyüktür, 48
sayısı 50'den 2 küçük olduğu için $48^2$ 2500'den 200 küçüktür, ve 2300'e
geliriz. Nihai doğru sonuç için bir düzeltme terimi 4 ekleriz ve 2304'e
erişiriz"

Gerçi kitabın kendisi de tam numarayı göstermemiş. Numara karesel formül ile
alakalı. Karesel formülü lise matematiğinden biliyoruz,

$$ (x-y)^2 = x^2 - 2xy + y^2 $$

Şimdi, eğer $48$ karesine erişmek istersek, onu yukarıdaki formülde $x=50,y=2$
olarak belirtiriz, böylece eşitliğin sol tarafı $x-y = 48$ olur. Bunun güzel
tarafı şimdi eşitliğin sağındaki açılım su hale gelir,

$$ = 50^2 - 2(50)(2) + 2^2 $$

Yani basit toplamlar ve çarpımlar bunlar. Bu hesapları artık kafadan yapmak çok
kolay, $2500 - 200 + 4$. Yani 2304.

Not: Benzer numarayı $(x+y)^2$ kullanarak ta yapabiliriz, mesela 43'ün karesini
almak için, ve $(40+3)^2$ açılımıni kullanırız, $x^2+2xy+y^2$ içinde sayıları
yerine koyunca yine basit çarpımlar, toplamlar elde ederiz. 

Karekök Hesabı

66'nin karekökunu nasıl hızlı şekilde hesaplarız? Yine üstteki durumda olduğu
gibi bir matematiksel geçiş ile basit işlemleri elde etmeye uğraşırız. Tek
terimli Taylor Açılım 

$$ f(x + \Delta x) \approx f(x) + f'(x)\Delta x $$

Eğer karekök işlemi üzerinde tek terimli Taylor Açılımı yaparsak,

$$ \sqrt{x + \Delta x} \approx \sqrt{x} + \frac{\Delta x}{2\sqrt{x}} $$

elde ederiz. Şimdi 66'yi karekökünü bildiğimiz bir sayı artı başka bir şey
olarak temsil etmeye uğraşalım, mesela 64+2 olabilir bu, çünkü 64'un karekökünü
biliyoruz,

$$
\sqrt{66} = \sqrt{64 + 2} \approx \sqrt{64} + \frac{2}{2\sqrt{64}} =
8 + \frac{1}{8} = 8.125 $$

Kaynaklar

[1] Gleick, *Genius*

[2] Mahajan, *Street Fighting Mathematics*

[3] Stack Exchange, {\em Mental Math: Finding Square Roots to 1 Decimal
  Point}, [http://math.stackexchange.com/a/469440/6786](http://math.stackexchange.com/a/469440/6786)





