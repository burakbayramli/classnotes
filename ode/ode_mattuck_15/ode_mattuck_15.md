# Ders 15

Fourier Serileri

Ünlü matematikçi Fourier şunu keşfetmişti; periyodik olan bir fonksiyon F(x)
sinüs ve cosinüs terimlerinin toplamı olarak temsil edilebilir.

$$ F(x) = a_o + \sum_{n=1}^{\infty}a_n \cos nx + \sum_{n=1}^{\infty}b_n \sin nx  $$

Bu fonksiyonda a ve b sayı değerlerinin bilinmesi gerekmektedir. Onları nasıl
buluruz? 

$a_k$ değerlerini bulmak için iki tarafı $\cos kx$ ile çarpıp
$\int_{-\pi}^{\pi}$ ile entegralini alırsak,

$$
\int_{-\pi}^{\pi} F(x)\cos kx \mathrm{d} x =
\int_{-\pi}^{\pi} a_0 \cos
kx \mathrm{d} x +  \sum_{n=1}^{\infty}\int_{-\pi}^{\pi} a_n \cos nx \cos kx \mathrm{d} x +
$$

$$ \sum_{n=1}^{\infty}\int_{-\pi}^{\pi} a_n \sin nx \cos kx \mathrm{d} x   $$

Eşitliğin sağ tarafında birinci terim $\cos(kx)$, $\sin(kx)$'e dönüşür. Fakat sinüs
fonksiyonu $\pi$ ve $-\pi$ noktalarında (ya da onların k ile çarpılmış $2\pi$,
$-2\pi$, vs. gibi katlarında) sıfır değerine sahip olduğu için, bu terim tamamen
sıfır olacaktır, formülden atılabilir.

İkinci terimde $\cos(nx)\cos(kx)$'in üstteki gibi entegrali eğer k ve n eşit
değilse, sıfırdır. Sadece n ve k eşit ise $a_k(\cos kx)^2$ değeri elde edilir.
$(\cos kx)^2$'in ise üstteki şekilde entegrali $\pi$ sonucunu verir. Yani ikinci
terimde olan, o sonsuza kadar giden koca toplam içinde sadece tek bir terim sağ
kalabilir.

Üçüncü terimde $\sin nx \cos kx$ çarpımının entegrali her zaman
sıfır değerini döndürür. Bu terim de formülden atılır. Geri kalanları tekrar
düzenlersek, 

$$ a_k = \int_{-\pi}^{\pi} F(x)\cos kx \mathrm{d} x $$

sonucunu elde ederiz. $b_k$ için benzer işlemler, ama bu sefer $\sin kx$ ile çarpılarak yapılırsa ve
sonuç aşağı yukarı aynı.

$$ b_k = \int_{-\pi}^{\pi} F(x)\sin kx \mathrm{d} x $$

$a_0$ için ise, $\cos kx$ ya da $\sin kx$ ile çarpmaya gerek yok. Sadece iki
tarafın entegralini almak yeterli, $a_o$'i istediğimiz için $n=0$ demektir, o
zaman $\sin, \cos$ içeren hiçbir terime ihtiyaç yoktur.

$$  \int_{-\pi}^{\pi} F(x) \mathrm{d} x =  \int_{-\pi}^{\pi} a_o \mathrm{d} x $$
$$  =  a_o x \ \bigg|_{-\pi}^{\pi}  $$
$$  = a_0 (\pi -(-\pi))  $$
$$  = 2\pi a_0  $$

Yani

$$ a_0 = \frac{1}{2\pi}\int_{-\pi}^{\pi}F(x) \mathrm{d} x $$

Kompleks Sayıları Kullanmak

$a_o$, $a_n$ ve $b_n$ yerine tek bir $c_n$ turu sayı kullanmak istersek,
kompleks sayı sistemine geçmek lazım. O zaman ilk $F(x)$ formülünü de
dönüştürmemiz lazım.

Trigonometrik fonksiyonlarda bilinen iki eşitlik şöyledir:

$$ \cos(\theta) = \frac{e^{i\theta}+e^{-i\theta}}{2} $$

$$ \sin(\theta) = \frac{e^{i\theta}-e^{-i\theta}}{2i}  $$

Bu formülde $i$ değeri hayali sayı olarak bilinen $\sqrt{-1}$ değeridir. 

$F(x)$ formülünü üstteki trigonometrik eşitliklere göre dönüştürelim. 

$$ F(x) = .. +  \sum_{n=1}^{\infty}a_n \cos nx + \sum_{n=1}^{\infty}b_n \sin nx $$

$$ = .. + \sum_{n=1}^{\infty} a_n \frac{e^{inx} + e^{-inx}}{2} +  \sum_{n=1}^{\infty} b_n \frac{e^{inx} - e^{-inx}}{2i}\\ $$

$$ = .. + \sum_{n=1}^{\infty} \frac{a_ne^{inx}}{2} + \frac{a_ne^{-inx}}{2} +
\frac{b_ne^{inx}}{2i} - \frac{b_ne^{-inx}}{2i} $$

$$ = .. + \sum_{n=1}^{\infty} \frac{a_ne^{inx}}{2} + \frac{a_ne^{-inx}}{2} -
\frac{i \ b_ne^{inx}}{2} + \frac{ \ b_ne^{-inx}}{2} $$

Benzer terimleri, yani $e^{inx}$ ve $e^{-inx}$ terimlerini beraber yazalım:

$$ = .. + \sum_{n=1}^{\infty} \frac{a_n-b_ni}{2}e^{inx} + \frac{a_n+b_ni}{2}e^{-inx} $$

Bölümde olan $2i$ içindeki $i$ nasıl yukarı çıkabildi? Bu durum, hayali
sayıların bir özelliğiyle alakalı: $1/i = -i$. Böylece ikinci ve dördüncü
terimdeki artı ve eksi işaretleri değişmiş oldu. Daha kısa yazmak için

$$ c_{-n} = \frac{a_n + b_n}{2} $$

$$ c_{n} = \frac{a_n - b_n}{2} $$

olarak temsil edersek

$$F(x) = \sum_{n=-\infty}^{\infty} c_ne^{inx} $$

Üstteki $c_{-n}$ ve $c_n$ kullanımı bize ek bir avantaj sağlıyor:
$-inx$ ibaresindeki eksi değeri de çekip çıkartabiliyoruz, eksi değer
$i$'den alınıp $n$'ye veriliyor yani, ve eksilik toplamdaki alt sınır
olarak tanımlanıyor. Nasıl olsa final formülde $i$ ve $n$ çarpıldığı
için sonuç değişmiyecek, ve tek bir terim kullanabileceğiz. Ek olarak
$a_0$ ise $c_o$ haline geldi.

Üstteki entegralli tekniğin benzerini $c_n$ için de
kullanabiliriz. Eşitliğin sağ tarafındaki kısım üstteki formülde
$\Sigma$ toplamının açılmış halini kullanalım, ve iki tarafı da
$e^{-ikx}$ ile çarpalım, sonra $-\pi$ ve $\pi$ arasında entegralini
alalım:

$$ \int_{-\pi}^{\pi}F(x)e^{-ikx}\mathrm{d} x = \int_{-\pi}^{\pi}c_0e^{-ikx}\mathrm{d} x + $$

$$ \int_{-\pi}^{\pi}c_1e^{ix}e^{-ikx}\mathrm{d} x + ... +  $$

$$ \int_{-\pi}^{\pi}c_ke^{ikx}e^{-ikx}\mathrm{d} x + ... $$

Toplamdaki tüm terimleri göstermedik, önemli olan kısım zaten k'inci terim, yani
$e^{-ikx}$ ile çarpılan $e^{ikx}$ ifadesi. Bu çarpım basit bir cebirsel işlemle
$e^{-ikx}e^{ikx} = e^{-ikx + ikx} = e^{0} = 1$, yani bir değerine eşit. Diğer
tüm terimler eğer entegrali hesaplarsak görebileceğimiz gibi sıfıra eşit. Bir
değerinin $-\pi$ ve $\pi$ arasında entegrali $2\pi$. 

O zaman

$$ 2\pi c_k = \int_{-\pi}^{\pi}F(x)e^{-ikx}\mathrm{d} x $$

$$ c_k = \frac{1}{2\pi}\int_{-\pi}^{\pi}F(x)e^{-ikx}\mathrm{d} x $$

Sıfıra eşitliğin nasıl olduğunu cebirsel olarak gösterelim. Entegrali alalım,

$$ \int_{-\pi}^{\pi}e^{inx}e^{-ikx}\mathrm{d} x $$

$$ = \int_{-\pi}^{\pi}e^{i(n-k)x}\mathrm{d} x $$

$$ = \frac{e^{i(n-k)x}}{i(n-k)} \bigg|_{-\pi}^{\pi}  $$

$$ = e^{in\pi}e^{-ik\pi} - e^{-in\pi}e^{ik\pi} = 0 $$

Fourier Transformu

Fourier transformu Fourier serilerinin özel bir şartı olarak türetilebilir
[1]. Daha önce gördük ki 

$$ 
x(t) = \sum_{-\infty}^{\infty} c_n e^{jnw_0t} 
\qquad (1) 
$$

üstte $i$ yerine $j$ kullandık, $\omega_0 = 2\pi / T$ ve $T = 2\pi$. $c_n$'lerin
(üstte $c_k$) nasıl hesaplandığını da görmüştük,

$$ 
c_n = \frac{1}{T} \int_{T} x(t) e^{-jn\omega_0t} \mathrm{d} t 
\qquad (2) 
$$

Şimdi, $T \to \infty$ olduğunu durumu düşünelim, bu durumda temel frekans
$\omega_0$ çok ufalacaktır, ve $n\omega_0$ herhangi bir değere sahip
olabilecek sürekli bir değişken haline gelir (çünkü $n$'nin menzili
$\pm \infty$), o zaman yeni bir değişken $w = n\omega_0$, ve $X(w) = Tc_n$
olarak tanımlayabiliriz. Bunları önceki denkleme sokarsak Fourier Transform
denklemini (ya da İleri Yönde Fourier Transformunu) elde ederiz,

$$ X(w) = \int_{-\infty}^{\infty} x(t) e^{-jwt} \mathrm{d} t$$

Ters Yönde Fourier Transform (Inverse Fourier Transform) için yine (1) ve
(2) ile başlıyoruz, ve yanyana iki harmoniğin / frekansın arasındaki
aralığı, mesafeyi düşünüyoruz [1], [2, sf. 223],

$$ \Delta \omega = (n+1) \omega_0 - n \omega_0 = \omega_0 = \frac{2\pi}{T} $$

Şimdi (2)'yi (1) içine koyalım, ve $1/T = \omega_0 / 2\pi$ üzerinden,

$$ 
x(t) = \sum_{n=-\infty}^{\infty} \bigg[ 
\frac{1}{T} \int_{-T/2}^{T/2} x(t) e ^{-jn\omega_0 t} \mathrm{d} t
\bigg]  e ^{jn\omega_0 t}
$$


$$ 
= \sum_{n=-\infty}^{\infty} \bigg[ 
\frac{\Delta \omega}{2\pi} \int_{-T/2}^{T/2} x(t) e ^{-jn\omega_0 t} \mathrm{d} t
\bigg]  e ^{jn\omega_0 t}
$$


$$ 
= \frac{1}{2\pi} \sum_{n=-\infty}^{\infty} \bigg[ 
\int_{-T/2}^{T/2} x(t) e ^{-jn\omega_0 t} \mathrm{d} t
\bigg] \Delta \omega e ^{jn\omega_0 t}
$$

$T \to \infty$ iken $\Delta \omega$ yerine $\omega_0$ kullanabiliriz, ve
tüm ayrıksal frekanslar $n\omega_0$ üzerinden alınan toplamı tüm frekanslar
üzerinden alınan entegral ile değiştirebiliriz, $n\omega_0$ yerine $w$
koyarız, böylece bir ikili entegral elde ediyoruz, 

$$ x(t) = \frac{1}{2\pi} \int_{-\infty}^{\infty} \bigg[
\int_{-\infty}^{\infty} x(t) e ^{-j\omega t} \mathrm{d} t
\bigg] e ^{j\omega t}  \mathrm{d} \omega
$$

İç entegrale bakarsak o kısmın Fourier transform olduğunu görebiliriz, dış
entegral de Ters Yönde Fourier Transformu olacaktır, $x(t)$'den ileri
gittik, sonra geri gittik, yine $x(t)$'nin kendisine geldik.


Kaynaklar

[1] *Introduction to the Fourier Transform*, [http://lpsa.swarthmore.edu/Fourier/Xforms/FXformIntro.html](http://lpsa.swarthmore.edu/Fourier/Xforms/FXformIntro.html)

[2] Sadiku, *Signals and Systems Primer with MATLAB*




