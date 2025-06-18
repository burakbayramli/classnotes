# Ders 13

Bugünkü dersimizin hedefi özel çözümler bulmak. Formu yazalım

$$ y'' + Ay' + By = f(x) $$

Ve genel çözüm $y = y_p + c_1y_1 + c_2y_2$ formunda olacak. 

Gerçek şu ki eşitliğin sağ tarafına yazılabilecek her fonksiyon o kadar
ilginç değil. İlginç olanlardan bir tanesi üstel (exponential)
fonksiyonlar, yani $e^{ax}$ formundaki fonksiyonlar, ki çoğunlukla $a<0$
kullanılır. Diğer bazı ilginç olanlar 

$$ \sin \omega x $$

$$ \cos \omega x $$

gibi salınım örnekleri, ki bunlar da elektriksel devrem bağlamında
alternatif AC/DC akımı temsil ediyorlar.

Ya da "gittikçe yokolan salınım'' ilginç. Burada,

$$ e^{ax}\sin \omega x $$

$$ e^{ax}\cos \omega x $$

gibi örnekler var. Ama aslında üstteki tüm ilginç fonksiyonlar genel tek
bir forma bağlanabilir, bu form için üstel sayının kompleks olmasına izin
vermek gerekiyor. Form şöyle

$$ e^{(a+i\omega)x} $$

$\omega = 0$ ise o zaman $e^{ax}$ elde ederim. $a=0$ ise $\sin\omega x$,
$\cos\omega x$ elde ederim. İkisi de sıfır değilse o zaman gittikçe yokolan
salınımı elde ederim. 

Bundan sonra habire $a+i\omega$ yazmamak için onun yerine $\alpha$
kullanacağız, $\alpha$'yi görünce onun bir kompleks sayı olduğunu
anlayın. Yanı eşitliğin sağ tarafı $e^{\alpha x}$ olacak. 

Birazdan göreceğimiz üzere, bu tür bir girdi kullanmak aslında kolaylıkla
çözüm sağlıyor. Yerine geçirme (substitution) kuralını kullanarak çözüme
erişmek çok kolay. 

$$ y'' + Ay' + By = f(x) $$

Polinom operatör kullanırsak

$$ (D^2 + AD + B )y = f(x) $$

Parantez içindekine $p(D)$ diyelim. Ve şu formülü ortaya atalım. 

$$ p(D)e^{\alpha x} = p(\alpha)e^{\alpha x}  $$

Bu yerine geçirme kuralı (substitution rule). 

İspat

$$ (D^2 + AD + B )e^{\alpha x} $$

$$ = D^2e^{\alpha x} + ADe^{\alpha x} + Be^{\alpha x} $$

$$ = \alpha^2 e^{\alpha x} + A \alpha e^{\alpha x} + Be^{\alpha x} $$

$$ = e^{\alpha x}(\alpha^2  + A \alpha + B)$$

Parentez içinin $p(\alpha)$ olduğunu görüyoruz. 

$$ = e^{\alpha x}p(\alpha)$$

Şimdi bunları yeni bir teori için kullanalım

Üstel Girdi Teorisi 

Bu teori Üstel Cevap Formülü (Exponential Response Formülü -ERF- olarak ta
biliniyor).

ODE

$$ y'' + Ay' + By = e^{\alpha x} $$

$e^{\alpha x}$, problemde $e^{(3+i)x}$ olabilir mesela, o zaman $\alpha
= (3+i)$ olacaktır. 

Operatör formunda,

$$ p(D)y = e^{\alpha x} $$

için özel çözüm şudur

$$ y_p = \frac{e^{\alpha x}}{p(\alpha)} $$

Bu teori bu dersin en önemli teorilerinden biri. Bu dersteki pek çok
kavramı yanyana getiriyor. 

İspat

İspatlamak için çözüm $y_p$'yi ana denkleme koyalım ve alttaki ifadenin doğru
olup olmayacağına bakalım. ODE'yi tekrar ifade edelim, 

$$ p(D)y =  e^{\alpha x} $$

ama $y$ yerine $y_p$ koyalım, ve bakalım ifadenin sol tarafını
dönüştürünce, sağ taraftaki aynı sonuç çıkacak mı?

$$ = p(D)y_p$$

$$ = p(D)\frac{e^{\alpha x}}{p(\alpha)}  $$

Üst taraf için yerine geçirme kuralını kullanalım

$$ = \frac{p(\alpha)e^{\alpha x}}{p(\alpha)} $$

$$ = e^{\alpha x} $$

Sağ tarafta aynı ifadeye eriştik, demek ki teori doğru. Peki ya $p(\alpha)
= 0$ olsaydı? Bu önemli bir istisnai durum, ama problemde böyle olmadığını
 farzediyoruz.

Örnek 

$$ y'' - y' + 2y = 10e^{-x}\sin(x) $$

Sağ taraf gittikçe çürüyen (decaying) bir salınım. Genel çözümü bul.

Özel çözümü bulalım ve sağ tarafı kompleksleştirelim. 

$$ (D^2 - D + 2)y = 10 e ^{(-1 + i)x} $$

Kompleksleştirmeyi nasıl yaptım? Dikkat edersek, $10e^{-x}\sin(x)$ ifadesi
$10 e^{(-1 + i)x}$ ifadesinin kompleks kısmını temsil ediyor. 

$$ e ^{(-1 + i)x} = e^{-x} e^{ix} $$

Euler açılımına göre $e^{ix} = \cos(x) + i\sin(x)$, sadece hayali kısmı alırsak

$$ e^{-x} \sin(x)  $$

Başındaki 10'u da eklemeyi unutmuyoruz tabii. Bu tekniği daha önce de
kullanmıştık, ve kullandığımız zaman orijinal ODE'deki değişkenin üzerine
bir dalga işareti koymuştuk, çünkü elimizde farklı bir ODE var, farklı
ODE'den aldığımız çözümün kompleks kısmını almak gerekecek. Yeni formül

$$ (D^2 - D + 2)\tilde{y} = 10 e ^{(-1 + i)x} $$

Özel çözüm ne? ERF'ten hareketle

$$ \tilde{y}_p = \frac{10 e^{(-1+i)x}}{(-1+i)^2 - (-1+i) + 2} $$

$$  = \frac{10 e^{(-1+i)x}}{3 - 3i} $$

$$ = \frac{10}{3}\frac{(1+i)}{2} e^{-x} \bigg( \cos(x) + i\sin(x) \bigg)$$

$y_p$, $\tilde{y}_p$'nin hayali kısmı olacak

$$ \frac{5}{3}e^{-x}( \cos(x) + \sin(x)) $$

Bu son formdan hoşlanmıyorsak, onu hemen çevirebiliriz, dik üçgeni
hatırlayalım, kenarlar 1 ve 1 ise hipotenüsü $\sqrt{2}$

$$ = \frac{5}{3}e^{-x} \sqrt{2}\cos(x - \frac{\pi}{4})$$

Ya $p(\alpha) = 0$ Olursa?

Bu durumda ERF yerine yeni bir formül gerekecek. Bu yeni formül için de
yeni bir Yerine Geçirme Kanunu gerekecek. 

Bu noktada $\alpha$ sembolü yerine $a$ sembolünü kullanacağız, ama temsil
edilen şey hala kompleks bir sayı (not: kompleks olması garanti değil bu
arada, en azından kompleks olmasına izin veriliyor). 

Üstel Kaydırma Kanunu (Exponential Shift Law)

Bu kanun alttaki ifadeyi basitleştirmekte kullanılıyor. Önce bu ifadenin
içerdiği cebirsel zorluğu görelim.

$$ p(D)e^{ax}u(x) $$

Eğer bu formülün türevini alırsak, $u(x)$'in 2. hatta daha fazla
derecelerdeki türevini almamız gerecek. Bundan kurtulmanın bir yolu yok mu?
Var. Üstel Kaydırma Kanununa göre eğer $e^{ax}$ $p(D)$ üzerinden sol tarafa
geçerse, $p(D)$ değişerek $p(D+a)$ haline gelir. 

$$ p(D)e^{ax}u(x) = e^{ax}p(D+a)u(x)$$

İspat

Özel bir şarta bakalım, $p(D) = D$ olsun. O zaman 

$$ p(D)e^{ax}u(x) $$

şu hale gelir

$$ = De^{ax}u$$

$$ = De^{ax}Du + ae^{ax}u $$

$$ = De^{ax}Du + ae^{ax}u $$

$$ = e^{ax}(D + a)u $$

ki son ifade kaydırma kanununu ile uyumlu. 

Peki $P(D) = D^2$ olsaydı? Bunun ispatı için üstteki işlemlerin hepsini
tekrarlamaya gerek yok, mesela $D^2e^{ax}$ hesabı yapmaya gerek yok,
önceki hesabı kullanalım, $De^{ax}u$'u zaten biliyoruz

$$  D^2e^{ax}u = D(De^{ax}u) = D(e^{ax}(D+a)u)  $$

$$ = e^{ax}(D+a)[(D+a)u] = e^{ax}(D+a)^2u $$

Bu sonucun $D^3$, $D^4$, vs. için genelleştirilebileceğini görebiliyoruz
herhalde. Matematiksel tümevarım (induction) ile $D^NN$ için bu formül
ispatlanabilir. 

Devam edelim

$$ (D^2+AD+B)y = e^{ax} $$

$a$'nin kompleks olabileceğini unutmayalım, ama problemimiz şu: $p(a) =
0$. Şimdi özel çözümü nasıl elde edeceğim? 

$$ y_p = \frac{x e^{ax}}{p'(a)} $$

Peki ya $p'(a) = 0$  olursa? Bunun için $a$'nın $p(D)$'nin basit bir kökü
olduğunu farzedeceğiz, çünkü bu faraziye sayesinde $p'(a)$ hiçbir zaman
sıfır olamaz. 

Ya iki kökün ikisi de $a$ olsaydı? Elde ikiden fazla kök olamaz tabii,
çünkü bir karesel denklemin ancak o kadar kökü olabilir. O zaman özel çözüm
şöyle olacaktı

$$ y_p = \frac{x^2e^{ax}}{p''(a)} $$

Daha üst seviye polinomlar için bu işin nereye gittiği belli oluyor
herhalde. 

Bunlardan birini ispatlayalım isterseniz. Şunu mesela

$$ y_p = \frac{x e^{ax}}{p'(a)} $$

Bu ispatı üstel kaydırma kanunu kullanarak yapacağız. 

İspat

Basit kök durumu

$$ p(D) = (D-b)(D-a) $$

ve $b \ne a$ çünkü iki tane farklı kök var. O zaman türevi alırsak

$$ p'(D) = (D-a) + (D-b) $$

$$ p'(a) = a-b $$

Önerdiğimiz özel çözüm şuydu

$$ \frac{P(D) e^{ax} x}{p'(a)} $$

Amacımız bu çözüm içine daha önce elde ettiğimiz sonuçları koyarsak,
ODE'nin sağ tarafını, girdiyi elde etmek, böylece elimizde bir çözüm
olduğunu anlayabilmiş olacağız. 

$$ = e^{ax}(D+a-b)\frac{Dx}{p'(a)} = e^{ax} \frac{(a-b) \cdot 1}{a-b} $$

$$ = e^{ax} $$

Örnek

$$ y'' - 3y + 2y = e^x $$

$e^x$ ile $e^{ax}$ formunu karşılaştırırsak, $a$'nin 1 olduğunu görürüz,
yani $a$ reel bir sayı. Ayrıca $a$, $D^2-3D+2$'nin basit bir kökü, yerine
koyarsak $D^2-3D+2$'nin sıfır verdiğini görürdük. 

$$ y_p = \frac{xe^x}{-1} $$

alttaki -1 nereden geldi? 

$$ p'(D) = 2D - 3 $$

$$ p'(1) = -1 $$

Yani nihai sonuç

$$ y_p = -xe^x $$





