# Vektör Calculus, Kurallar, Matris Türevleri

Aksi belirtilmedikçe altta $a,x$ gibi vektörler kolon vektörleri olacaktır,
yani $m \times 1$, ya da $n \times 1$ gibi boyutlara sahip olacaklardır. 

$m$ boyutlu vektör $x$'i alan ve geriye tek sayı sonucu döndüren bir $f(x)$
fonksiyonunun $x$'e göre türevini nasıl alırız? Yani $x \in \mathbb{R}^m$
ve bir vektör,

$$ x = 
\left[\begin{array}{ccc}
x_1 \\ \vdots \\ x_m
\end{array}\right]
$$

Bu durumda $x$'in her hücresine / öğesine göre kısmi türevler (partial
derivatıves) alınır, sonuçta tek boyutlu / tek sayılı fonksiyon, türev 
sonrası $m$ boyutlu bir sonuç vektörünü yaratır, yani

$$
\frac{\partial f}{\partial x}  =
\left[\begin{array}{c}
\frac{\partial f}{\partial x_1} \\
\\
\frac{\partial f}{\partial x_2} \\
\vdots \\
\frac{\partial f}{\partial x_m} 
\end{array}\right]
$$

Bu sonuç tanıdık gelmiş olabilir, bu ifade gradyan olarak ta bilinir. 

$$ \frac{\partial f}{\partial x}  = \nabla f = grad \ f(x) $$

Türev bir kolon vektörü olarak çıktı çünkü $x$ de bir kolon
vektörüydü. Elde edilen vektör sürpriz değil çünkü tek, skalar bir değer
veren bir fonksiyonun $x$ içindeki *her öğesinin* nasıl değiştiğine
göre bunun fonksiyon üzerindeki etkilerini merak ediyorduk, üstteki vektör
öğe bazında bize aynen bunu gösteriyor. Yani tek skalar sonuç $m$ tane
türev sonucuna ayrılıyor, çünkü tek sonucun $m$ tane seçeneğe göre
değişimini görmek istedik. 

Not olarak belirtelim, gradyan vektörü matematiksel bir kısayoldur, yani
matematikte kuramsal olarak türetilerek ulaşılan ana kurallardan biri
denemez. Fakat çok işe yaradığına şüphe yok.

Eğer bir $A$ matrisinin tüm öğeleri tek sayı $\theta$ gibi bir değişkene
bağlı ise, o matrisin $\theta$'ya göre türevi, tüm elemanlarının teker
teker $\theta$'ya göre türevidir,

$$ 
\frac{\partial A}{\partial \theta} = 
\left[\begin{array}{cccc}
\frac{\partial a_{11}}{\partial \theta} & 
\frac{\partial a_{12}}{\partial \theta} & \dots & 
\frac{\partial a_{1n}}{\partial \theta} 
\\
\frac{\partial a_{21}}{\partial \theta} & 
\frac{\partial a_{22}}{\partial \theta} &  \dots & 
\frac{\partial a_{2n}}{\partial \theta}  
\\
\vdots & \vdots & \ddots & \vdots 
\\
\frac{\partial a_{m1}}{\partial \theta} & 
\frac{\partial a_{m2}}{\partial \theta} &  \dots & 
\frac{\partial a_{mn}}{\partial \theta}  
\end{array}\right]
$$

Şimdi ilginç bir durum; diyelim ki hem fonksiyon $f(x)$'e verilen $x$
çok boyutlu, hem de fonksiyonun sonucu çok boyutlu! Bu gayet mümkün bir
durum. Bu durumda ne olurdu? 

Eğer $f$'in türevinin her türlü değişimi temsil etmesini istiyorsak, o
zaman hem her girdi hücresi, hem de her çıktı hücresi kombinasyonu için bu
değişimi saptamalıyız. Jacobian matrisleri tam da bunu yapar. Eğer $m$
boyutlu girdi ve $n$ boyutlu çıktı tanımlayan $f$'in türevini almak
istersek, bu bize $m \times n$ boyutunda bir matris verecektir!
Hatırlarsak daha önce gradyan sadece $m$ boyutunda bir *vektör*
vermişti. Şimdi sonuç bir matris. 

$$ 
J(x) = \frac{\partial f(x)}{\partial x} =
\left[\begin{array}{ccc}
\frac{\partial f_{1}(x)}{\partial x_1} & \dots & 
\frac{\partial f_{1}(x)}{\partial x_1} 
\\
\vdots & \ddots & \vdots 
\\
\frac{\partial f_{n}(x)}{\partial x_n} & \dots & 
\frac{\partial f_{n}(x)}{\partial x_n}  
\end{array}\right]
$$

Hessian ise $f$'in ikinci türevlerini içerir [3]. Bu matrisin $i,j$'inci
öğesi fonksiyonun $\frac{\partial^2 f(x)}{\partial x_j \partial x_i}$'inci
türevidir, ki $x = [x_1,x_2,...,x_n]$ vektörü olmak üzere. 

$$
\nabla^2 (x) = 
\left[\begin{array}{rrr}
\frac{\partial^2 f}{\partial x_1^2} & \cdots & \frac{\partial^2 f}{\partial x_1x_n}\\
\vdots & \ddots & \vdots \\
\frac{\partial^2 f}{\partial x_1x_n} & \cdots & \frac{\partial^2 f}{\partial x_n^2}\\
\end{array}\right]
$$

Hessian matrisı simetrik ve reel bir matristir. 

Vektörlere gelelim. $a,x \in \mathbb{R}^n$ ise, $a^Tx$'in $x$'e göre türevi nedir? 

$a^Tx$ bir noktasal çarpım olduğuna göre sonucu bir tek sayıdır
(scalar). Bu noktasal çarpımı bir fonksiyon gibi düşünebiliriz, bu durumda
demektir ki tek sayılı bir fonksiyonun çok boyutlu $x$'e göre türevini
alıyoruz, yani gradyan durumu tekrar vuku buldu,

$$ 
\frac{\partial (a^Tx)}{\partial x} = 
\left[\begin{array}{c}
\frac{\partial (a^Tx)}{\partial x_1} \\ 
\vdots \\ 
\frac{\partial (a^Tx)}{\partial x_n} 
\end{array}\right] 
$$

$$  =
\left[\begin{array}{c}
\frac{\partial (a_1x_1 + ... + a_nx_n)}{\partial x_1} \\ 
\vdots \\ 
\frac{\partial (a_1x_1 + ... + a_nx_n)}{\partial x_n} 
\end{array}\right] 
= 
\left[\begin{array}{c}
a_1 \\
\vdots  \\
a_n
\end{array}\right] =
a
$$

Niye her satırda $a_1,a_2$  gibi değerler elde ettiğimizin sebebi bariz
herhalde, çünkü mesela ilk satırda $x_1$'e göre türev alındığı durumda
$a_1x_1$ haricindeki tüm terimler yokolacaktır, çünkü o terimler içinde
$x_1$ yoktur ve Calculus'a göre sabit sayı sayılırlar.

Peki $a^Tx$'in $x^T$'ye göre türevi nedir? 

$x^T$'nin yatay bir vektör olduğuna dikkat, yani satır vektörü, o zaman
sonuç yatay bir vektör olacaktır (kıyasla gradyan dikeydi).

$$ 
\frac{\partial (a^Tx)}{\partial x^T} = 
\left[\begin{array}{ccc}
\frac{\partial (a^Tx)}{\partial x_1} &
\dots 
&
\frac{\partial (a^Tx)}{\partial x_n} 
\end{array}\right] 
\qquad (1)
$$

$$ =
\left[\begin{array}{ccc}
a_1 & \dots & a_n
\end{array}\right] = 
a^T $$

Eğer bir $x \in \mathbb{R}^m$ vektöründen $A$ matrisi $x$ ile çarpılıyor
ise, bu çarpımın $x$'e göre türevi nedir? 

$$ \frac{\partial(Ax)}{\partial x^T} = A
$$

İspat: Eğer $a_i \in \mathbb{R}^n$ ise (ki devriği alınınca bu vektör yatay hale
gelir, yani altta bu yatay vektörleri üst üste istiflediğimizi düşünüyoruz),

$$ A = \left[\begin{array}{c}
a_1^T \\ \vdots \\ a_m^T
\end{array}\right] $$

O zaman $Ax$ ne olur? [4] yazısındaki "satır bakış açısı'' düşünülürse,
$A$'in her satırı, ayrı ayrı $x$'in tüm satırlarını kombine ederek tekabül
eden sonuç satırını oluşturur (tabii bu örnekte $x$'in kendisi bir vektör o
yüzden "satırları'' tek bir sayıdan ibaret),

$$ Ax = \left[\begin{array}{c}
a_1^Tx \\ \vdots \\ a_m^Tx
\end{array}\right] 
$$

Üstteki bir vektör, her öğesi tek sayı. Türevi alırsak (dikkat yatay
vektöre göre türev alıyoruz), ve (1)'i dikkate alırsak, 

$$ 
\frac{\partial Ax}{\partial x^T}  =
\left[\begin{array}{c}
\frac{\partial(a_1^Tx) }{\partial x^T} 
\\ 
\vdots \\ 
\frac{\partial(a_m^Tx) }{\partial }
\end{array}\right] = \left[\begin{array}{c}
a_1^T \\ \vdots \\ a_m^T
\end{array}\right]  = A
$$

Şu türev nasıl hesaplanır? 

$$ 
\frac{\partial (x^TA^T)}{\partial x}
$$

Çarpımı açalım, $x$ devriği yatay bir vektör, $A$'nin satırları $a_i$'ler
ise devrik sonrası kolonlar haline gelirler, yani

$$ 
\left[\begin{array}{ccc}
x_1 & \dots & x_n
\end{array}\right]
\left[\begin{array}{ccc}
\uparrow & & \uparrow \\
a_1 & \dots & a_m \\
\downarrow & & \downarrow 
\end{array}\right]
$$

Şimdi matris çarpımı satır bakışını kullanalım, çarpan $x$ satırı bir
tane, o zaman sonuç tek satır olacak. Bu tek $x$ satırının öğeleri, $A^T$
satırlarını teker teker çarpıp toplayacak ve o tek sonuç satırını meydana
getirecek. Yani $x_1,x_2,...$ sırayla $a_1$'in tüm öğelerini çarpıyor,
aynı şekilde $a_2$ için aynısı oluyor, vs. Bu durumu daha temiz bir
şekilde alttaki gibi belirtebiliriz,

$$ 
= \left(\begin{array}{ccc}
x^Ta_1 & \dots x^Ta_m 
\end{array}\right)
$$

Bu ifadenin türevini almak çok kolay, 

$$ 
= \left(\begin{array}{ccc}
\frac{\partial (x^Ta_1)}{\partial x} &
\dots
\frac{\partial (x^Ta_m)}{\partial x} 
\end{array}\right)
$$

$$ 
= \left(\begin{array}{ccc}
a_1 & \dots a_m 
\end{array}\right)
$$

$$ 
\frac{\partial (x^TA^T)}{\partial x} = A^T 
\qquad (2) 
$$

Başka bir türev: Diyelim ki $x \in \mathbb{R}^n, z \in \mathbb{R}^m$. 
Yeni bir vektör $c = A^Tz$ tanımlayalım ki vektör $n$ boyutunda. O zaman 

$$ 
\frac{\partial (z^TAx)}{\partial x} =  
\frac{\partial (c^Tx)}{\partial x} = c =  A^Tz
$$

Diğer bir türev

$$ 
\frac{\partial (z^TAx)}{\partial z} =  Ax
$$

Üstteki (2)'nin bir sonucu olarak görülebilir.

Eğer $x=x(\alpha),z=z(\alpha)$ olursa, türev almada Zincir Kuralını kullanalım,

$$ 
\frac{\partial (z^TAx)}{\partial \alpha} =  
\frac{\partial (z^TAx)}{\partial x} \frac{\partial x}{\partial \alpha}  + 
\frac{\partial (z^TAx)}{\partial z} \frac{\partial z}{\partial \alpha}  
$$

$$ 
\frac{\partial (z^TAx)}{\partial \alpha} =  
A^Tz \frac{\partial x}{\partial \alpha}  + 
Ax \frac{\partial z}{\partial \alpha}  
$$

Eğer $z = x = \alpha$ dersek,

$$ 
\frac{\partial (x^TAx)}{\partial \alpha} =  
\frac{\partial (x^TAx)}{\partial x} =  
A^Tx \frac{\partial x}{\partial \alpha}  + 
Ax \frac{\partial x}{\partial \alpha}  
$$

$$ 
= Ax + A^Tx  = (A^T+A) x
$$

Diyelim ki $A$ simetrik bir matris, o zaman $A^T=A$, ve

$$ 
= (A^T+A)x = 2Ax
$$

$x^Tx$ ifadesinin $x$ vektörüne göre türevi nedir? En basit yol, $x^TAx$
kalıbını kullanmak, ve $A = I$ yani birim matrisi koymak. Bu durumda 

$$ 
= \frac{\partial (x^TIx)}{\partial \alpha}  = 2Ix = 2x
$$

Daha zor yoldan, bu bir noktasal çarpım olacaktır, $x_1x_1 + x_2x_2 + .. +
 x_nx_n$  yani $x_1^2 + x_2^2 + .. + x_n^2$. Bu tek bir skalar sonuç, o sonucun her
$x$ öğesine göre türevinin alınması,

$$ 
\left[\begin{array}{c}
\frac{\partial (x_1^2 + x_2^2 + .. + x_n^2)}{\partial x_1} \\
\dots \\
\frac{\partial (x_1^2 + x_2^2 + .. + x_n^2)}{\partial x_n} 
\end{array}\right]
$$

$$ 
= \left[\begin{array}{c}
2x_1 \\
\dots \\
2x_n
\end{array}\right] = 
2x
$$

İzler (Trace)

$$ \frac{\partial }{\partial A} Tr(A) = I $$

$$ \frac{\partial }{\partial A} Tr(AB) = B^T $$

$$ \frac{\partial }{\partial A} Tr(A^TB) = B $$


$$ \frac{\partial }{\partial A} Tr(ABA^T) = A(B + B^T) $$

Eğer $B$ simetrik ise üstteki şu hale getirilebilir

$$  = 2AB $$

Yönsel Türev (Directional Derivative)

Bir vektör fonksiyonu $f = \vec{f}(x,y,z)$ düşünelim, 

$$
f = \left[\begin{array}{r} f_1(x,y,z) \\ f_2(x,y,z) \\ f_3(x,y,z) \end{array}\right]
$$

Formel olarak $f \cdot \nabla$ bir operatör tanımlar [2, sf. 74],

$$
f \cdot \nabla = 
\left[\begin{array}{ccc} f_1 & f_2 & f_3 \end{array}\right]
\cdot 
\left[\begin{array}{ccc} 
\frac{\partial }{\partial x} & \frac{\partial }{\partial y} & \frac{\partial }{\partial z} 
\end{array}\right]
$$

$$
= f_1 \frac{\partial }{\partial x} +
f_2 \frac{\partial }{\partial y} +
f_3 \frac{\partial }{\partial z} 
$$

Üstteki bir operatör. Onu mesela bir $g$ üzerinde uygulayabiliriz,

$$
(f_1 \frac{\partial }{\partial x} +
f_2 \frac{\partial }{\partial y} +
f_3 \frac{\partial }{\partial z}) 
g = 
f_1 \frac{\partial g}{\partial x} +
f_2 \frac{\partial g}{\partial y} +
f_3 \frac{\partial g}{\partial z} 
$$



Bazı Eşitlikler

$a,b$ vektör fonksiyonu olmak üzere, 

$$
\nabla (a \cdot b) = 
(a \cdot \nabla) b + (b \cdot \nabla) a + 
a \times (\nabla \times b) + 
b \times (\nabla \times a)
$$









Kaynaklar 

[2] Spiegel, *Vector Tensor Analysis*

[3] Kutter, *Multivariable Advanced Calculus*

[4] Bayramlı, Lineer Cebir, *Matris Çarpımı, Ders 1*




