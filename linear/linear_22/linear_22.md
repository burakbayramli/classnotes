# Ders 22

Bu ders özdeğerler hakkındaki 2. dersimiz. İlk derste y$Ax = \lambda x$
formülüne eriştik, $x$ özvektör ve $\lambda$ özdeğer, ve bu değerleri
hesaplayabilmeyi öğrendik. Şimdi onları kullanmayı göreceğiz. Bu kullanımı
görmenin iyi yollarından birisi, bir matrisi "köşegenleştirmek
(diagonalization)'', ki bu formül $S^{-1}AS =\Lambda$ olarak gösterilir,
çok temel bir formüldür bu ve bugünkü dersin merkez noktası.

$A$'nin özvektörleri $S$'in kolonlarını oluşturacak, ve bu $S$'i içeren
$S^{-1}AS$ sihirli formülüne bakacağız, burada neler oluyor? Ortada bir
tersi alma işlemi var, $S^{-1}$ ile, demek ki özvektör matrisi $S$'in tersi
alınabilir olması lazım, o zaman bize $n$ tane bağımsız özvektör lazım [ki
onları içeren matris tersi alınabilir olsun].

Şu matris çarpımını düşünelim şimdi, 

$$ 
AS =  
A \left[\begin{array}{rrrr}
\uparrow & \uparrow & & \uparrow \\
x_1 & x_2 & \cdots & x_n \\
\downarrow & \downarrow & & \downarrow
\end{array}\right]
$$

Bu çarpımı her $S$ kolonu için ayrı ayrı yaptığımı düşünebilirim, her
kolonun $A$'yi çarpması ayrı bir kolon sonucunu verirdi. Bu sonuçlar ne
olurdu? Mesela $Ax_1$'i düşünelim, $x_1$ bir özvektör olduğuna göre (çünkü
$S$'i özvektörler ile oluşturmuştuk) o zaman özdeğer/vektör $Ax = \lambda x$ 
formülüne göre $Ax_1 = \lambda_1 x_1$ elde ederdik, ki $\lambda_1$
1. özdeğer olurdu, ardından, 2., 3., vs. özvektörler için bu aynı 
şekilde devam ederdi, ve buna göre,

$$ 
= \left[\begin{array}{rrr}
& &  \\
\lambda_1x_1 & \cdots & \lambda_nx_n \\ 
& & 
\end{array}\right]
$$

elde ederdik. Güzel duruyor. Daha da güzel olabilir! Bir sonraki adımda üstteki
kolonlardaki özdeğerleri dışarı çıkartmak istiyorum, 

$$ 
= \left[\begin{array}{rrr}
& &  \\
x_1 & \cdots & x_n \\ 
& & 
\end{array}\right]
\left[\begin{array}{rrr}
\lambda_1 & &  \\
& \ddots & \\
& & \lambda_n
\end{array}\right]
$$

İçinde $\lambda$'lar olan matriste köşegen haricindeki tüm diğer hücreler
sıfır değerini taşıyor. Bu çarpım bize istediğimiz sonucu verecek, hem de
gayet temiz matris formları kullanarak. Lineer Cebir ne güzel işliyor! 

Ve nihayetinde üstteki çarpımı $S\Lambda$ olarak gösterebilirim; Büyük
$\lambda$, yani $\Lambda$ işaretini "içinde özdeğerler taşıyan matris''
olarak belirttim böylece. Nihai formül $AS = S\Lambda$. Ardından bu
eşitliğin iki tarafını soldan $S^{-1}$ ile çarparak

$$
S^{-1}AS = \Lambda
$$

diyebilirim. Tabii tekrar edelim, üstteki form sadece ve sadece $S$ içinde
birbirinden bağımsız özvektörler var ise mümkündür. Bu olmayabilirdi, az
miktarda matrisler için bu durum geçerlidir, belki bir özvektör kendini
tekrar edebilirdi, bu durumda üstteki formu kullanamazdım. Fakat
göreceğimiz çoğu matrisin $n$ tane bağımsız özvektörü vardır, ve onları
köşegenleştirebiliriz (yani üstteki çarpım sonucu sağında $\Lambda$ olan
forma erişebiliriz).

Bu arada $AS=S\Lambda$'yi sağdan da $S^{-1}$ ile çarpabilirdim, o zaman 

$$
AS=S\Lambda
$$

ifadesi

$$
A = S \Lambda S^{-1}
$$

haline gelir. Hatırlamak bağlamında, mesela iki üstteki formül için,
$A$'nin özvektörlerini çarptığını düşünebilirim, yani $Ax$ formu, ama bu
sefer tüm özvektörler $x$ yerine $S$ var, ve ardından, üç üstteki formülde,
sonucu $S^{-1}$ ile çarpıyorum ve $AS$'i köşegen haline getirmiş
oluyorum. Bir üstteki formül ise bu nihai sonucu ifade etmenin değişik bir
yolundan ibaret, $S$'leri karşı tarafa geçiriyoruz ve üstteki ifadeyi elde
ediyoruz.

Üstte son ifade yeni bir ayrıştırma (factorization) olarak ta
görülebilir.. Eliminasyondaki $LU$'nun, ya da Gram-Schmidt'ten $QR$'in
yerine geçebilecek bir tür ayrıştırma yani. Form nedir? $S$ çarpı köşegen
matris çarpı $S$'in tersi. Bu kombinasyonu bu bölümde bol bol göreceğiz, ki
bu kombinasyonda $S$ ve onun tersi $S^{-1}$'in rol oynayacak.

Şimdi öğrendiklerimizi kullanmak istiyorum. Mesela $A^2$'i hesaplamak
istiyorum. $A^2$'in özdeğer/vektörlerine ne olur? Bu son derece temel bir
soru. Alttaki formül ile başlangıç yapacağım, eğer böyle bir formül ortaya
konabiliyorsa tabii, 

$$ Ax = \lambda x $$

Buradan $A^2$'e ulaşmaya çalışacağım. Üstteki formülün iki tarafını $A$ ile
çarpayım şimdi, böylece sol tarafta $A^2$ elde ederim,

$$ AAx = A\lambda x $$

$$ A^2x = \lambda A x $$

Eğer $Ax$ yerine $\lambda x$ kullanırsam, çünkü bu özvektör denkleminden geliyor,

$$ A^2x = \lambda^2 x $$

Bu basit hesap arkasından sonuca bakınca şu yorumu yapabiliyorum; $A^2$'nin
özdeğerleri $\lambda^2$'dir. Peki özvektörler? Özvektörler $A$'nin
özvektörleri ile aynıdır, çünkü $x$ değeri değişmeden kaldı. Bunu
$A=SAS^{-1}$'i kullanarak görebilir miyiz?

$$
A^2=S\Lambda S^{-1}S\Lambda S^{-1}
$$

$$
=S \Lambda \cancel{S^{-1}S} \Lambda S^{-1}
$$

$$
=S \Lambda \Lambda S^{-1}
$$

$$
=S \Lambda^2S^{-1}
$$

Bu bana ne söylüyor? Bir önceki metot ile aynı şeyi söylüyor, özdeğerlerin
karesi elde edilir ama özvektörler değişmez, bunu biliyoruz çünkü yine aynı $S$
matrisini bildiğimiz bir formda elde ettik.

Peki $k$ defa üst alma operasyonu yapsaydım, yani $A^k$ hesaplasaydım ne
olurdu? Bu durumda en son metotta $S^{-1}S$'ların $k$ çarpım içinde sürekli
yanyana geleceğini görebiliriz herhalde, ve tabii ki tüm bu yanyana gelen
$S^{-1}S$'ler birbirini iptal edecektir. Diğer yandan $\Lambda$ matrisi
$k$ defa kendisi ile çarpılacaktır, yani

$$
A^k = S\Lambda^k S^{-1}
$$

Bu gayet temiz bir sonuç; demek ki özdeğerler/vektörler matrislerin kendisi
ile çarpılmış halini anlamak için biçilmiş kaftan. Kıyasla matris katları
alınırken, mesela pivotlar oradan oraya savrulurlar, doğru dürüst analiz
edilemezler. Eğer $A=LU$ kullansaydım mesela $LU$ çarpı $LU$ çarpı $LU$
böyle gidecekti, 100 defa ardından $(LU)^{100}$ elde edecektim, ve bununla
hiçbir analiz yapılamazdı. Fakat özdeğer/vektör durumunda $S\Lambda S^{-1}$
99 kere kendi kendisini iptal edecek ve geri kalan semboller üzerinden bir
analiz yapabileceğim.

Özvektör/değer ile analiz edilebilecek ilginç bir diğer soru: bir matrisin
kendisiyle çarpımı ne zaman sıfıra gider? Yani stabilite sorusu ile
ilgileniyorum, 

Teori

$$
A^k \to 0 \textrm{ , ki } k \to \infty  
\qquad (1)
$$ 

Ne zaman bu doğru olur? $A$ matrisinin içinde bir yerlerde bu bilgi var. Bu
bilgi matrisin pivotlarından değil, onun özdeğerlerinde. $A$'nin ardı
ardında çarpımını alırken bu çarpımın küçülmesi ne demektir o zaman bunu
cevaplayalım, özdeğer/vektör formülüne bakarsak, $S,S^{-1}$ oldukları gibi
kaldıklarına göre tek değişecek olan $\Lambda^k$ ve bu değer sürekli
küçülmeli. Bu ne demektir? Tüm özdeğerlerin 1'den küçük olması
demektir. Tabii özdeğerler eksi değerli olabilir, ya da karmaşık (complex)
sayı olabilirler, o yüzden özdeğerin salt değerini (absolute value)
kullanacağım, yani $|\lambda_i| < 1$ ise diyeceğim. 

Tabii bunları söylerken hep aklımın bir tarafında başta yaptığım varsayım
var, ki bu varsayım elimde $n$ tane bağımsız özvektör olduğu. Eğer bu yok
ise üstteki yaklaşım kullanılamaz. Yani bu bağımsız özvektörler yok ise
$A$'yi köşegenleştiremeyiz, çünkü $A^{-1}$ hesaplanamaz.

Peki hangi matrisler kesinlikle köşegenleştirilebilir [hoca bu uzun kelime
için özür diliyorum dedi, İngilizcesi de uzun, diagonalizable]. Güzel durum
tüm $\lambda$'ların farklı olduğu durum, ki bu durumda tekrar eden özvektör
yoktur. Mesela Python ile bir rasgele matris yaratsam, ve onların
özdeğerlerine baksam, bu değerlerin ayrı / farklı (distinct) olduklarını
görürdüm. Bu teorinin ispatı için [1, sf 300]. 

```python
import numpy.linalg as lin
A = np.random.randn(10,10)
evals,evecs = lin.eig(A)
print (evals)
```

```
[ 4.67859706+0.j         -2.11268898+3.12677439j -2.11268898-3.12677439j
 -2.25765724+0.j         -0.75108024+1.40493786j -0.75108024-1.40493786j
  0.26116597+1.43164683j  0.26116597-1.43164683j  0.28743712+0.j
  0.53952583+0.j        ]
```

Üstteki durum için yani 10 tane ayrı özvektör olacaktır. Eğer tekrar eden
$\lambda$'lar görseydim o zaman matrise daha yakından bakardım, bu durumda
da bağımsız özvektörler olabilir, ama bu garanti değildir. Mesela (10,10)
birim matris üzerinde özdeğer/vektör hesabı yapsam tüm özdeğerler 1 olurdu,
ama elimde bol bol bağımsız özvektörler de olurdu (değişik hücresinde 1
taşıyan tüm vektörler).

```python
import numpy.linalg as lin
A = np.eye(10,10)
evals,evecs = lin.eig(A)
print ('ozdegerler', evals)
print ('ozvektor 1', evecs[0])
print ('ozvektor 2', evecs[1])
```

```
ozdegerler [ 1.  1.  1.  1.  1.  1.  1.  1.  1.  1.]
ozvektor 1 [ 1.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
ozvektor 2 [ 0.  1.  0.  0.  0.  0.  0.  0.  0.  0.]
```

Sadece iki özvektör gösterdik, ama 10'u da bağımsız. Cebirsel olarak ta bu
sonuca erişebiliriz, eğer $A$ birim matris ise, $S^{-1}AS = I = \Lambda$
olur değil mi? $\Lambda$'nin köşegeninde tamamen bir değerleri var, ki
bunlar özdeğerler. 

Peki üçgensel durumda? 

$$ A = \left[\begin{array}{rr}
2 &  1 \\ 0 & 2
\end{array}\right] $$

Bu durum problemli. Özdeğerler nedir? Determinant ile

$$ \det (A-\lambda I) = 
 \left[\begin{array}{rr}
2-\lambda &  1 \\ 0 & 2-\lambda
\end{array}\right]
$$

Determinantı hesaplarız, buradan bir formül çıkar, 

$$ (2-\lambda)^2 = 0 $$

O zaman $\lambda=2,2$. Sonraki adım özvektörler.

$$ A-2I = 
\left[\begin{array}{rr}
0 &  1 \\ 0 & 0
\end{array}\right]
$$

ve burada sıfır uzayına bakıyorum, çünkü özvektörler $A-\lambda I$'in sıfır
uzayında. Fakat üstteki matrisin sıfır uzayı tek boyutlu. İşte bu durum
elde yeterli özdeğer olmadığı durum. Sebep polinomdaki çarpımın
tekrarlanmış olması, $2-\lambda$ kendisi ile çarpıldı bu sebeple aynı
$\lambda$'yi iki kere elde ettik. Sıfır uzayında sadece$\left[\begin{array}{cc} 1 & 0 \end{array}\right]^T$ var. Elimizdeki 
tek özvektör bu. 

Şimdi (1)'i tekrar düşünelim, üstteki durum (1) teorisinin kapsamına
girmiyor çünkü bu durum tekrar eden özdeğer durumunu hesaba katmıyor, (1)
için birbirinden bağımsız $n$ özvektör lazım, ki $S^{-1}$
hesaplanabilsin. Yani bazı matrisler için köşegenleştirme mümkün olmuyor,
ama çoğunluğu için bu mümkün tabii.


Kaynaklar 

[1] Strang, *Introduction to Linear Algebra*, 4th Edition





