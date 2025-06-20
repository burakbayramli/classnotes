# Ders 33

Bir matrisin ne zaman mükemmel bir tersi (inverse) bulunur? $2 \times 2$
boyutlarında bir matris mesela, ve bizim matris tersi dediğimiz şey aslında
"iki taraflı ters (two sided inverse)''tır. Mükemmel ters matrisi sağdan
da soldan da çarpsak birim matrisi elde ederiz,

$$ A A^{-1} = I = A^{-1}A $$

Mükemmel durumda $m = n = r$, yani kare matris durumudur, yani satır, kolon
sayısı ve kerte (rank) birbirine eşittir. Bu duruma tam kerte (full rank)
ismi de verilir. Kitabın 2. bölümünde işledik.

3'üncü bölümde ise tam kerte olmayan durumlara baktık, ve kertenin ne
olduğunu bulmayı öğrendik. Bu durumların bazılarında matris tam kolon
kertesine (full column rank) sahip olabiliyordu, yani $n = r$. O zaman
kolonlar birbirinden bağımsız oluyordu, ama belki satırlar olmuyordu. Bu
durumda sıfır uzayında (nullspace) sadece 0 vektörü vardır, çünkü kolonlar
bağımsız, bu durumda kolonların hiçbir kombinasyonu bize sıfır sonucunu
vermez, sadece 0 vektörü ile çarpmak bize sıfır sonucunu verebilir.

Bu durumun en az kareler (least squares) bağlamında önemli olduğunu
hatırlayalım, eğer $Ax = b$'nin bir çözümü var ise bu çözüm tek çözümdür,
yani "0 ya da 1 çözüm vardır''.

$r=n$ durumunda $A^TA$ tam kerte olur, çünkü $n \times m \cdot m \times
n = n \times n$  boyutlarındadır, ve bu sonuç matrisi tersine çevirilebilir
(ınvertible) bir matristir.

Şimdi $A$'nin tek taraflı tersi (one-sided inverse) olduğunu göstermek
istiyorum.

$$ (A^TA)^{-1} A^T = A_{sol}^{-1} $$

Yani üstteki ifadenin $A$'nin soldan tersi olduğunu söyledim. Peki $I$'ya
olan eşitliği nereden biliyorum? Çünkü üstteki ifadeyi alıp $A$'yi soldan
çarparsam, ve parantezleri değişik yerlere koyarsam,

$$  A_{sol}^{-1} A  = (A^TA)^{-1} (A^TA) = I$$

$A^TA$ tanıdık gelebilir, bu ifade en az kareler ortamında katsayı matrisi
olarak görülür. Tam kolon kerte durumunda $A^TA$'sinin tersi alınabilir, ve
en az kareler bu şekilde işini görebilir.  $A_{sol}^{-1}$'nin boyutları $n
\times m$.

Sağdan Ters (Right Inverse)

Tam satır kertesi (full row rank) durumu kolon durumunun devriği alınmış
hali, bu durumda $A^T$'nin sıfır uzayı sıfır vektörünü içeriyor, çünkü
satırlar bağımsız, kolonlar değil, ve $r < m = n$. Bu durumda $Ax = b$'nin
çözümü kesinlikle vardır, fakat birden fazla çözüm vardır. Sağdan ters suna
benzer, 

$$ A_{sag}^{-1} = A^T(AA^T)^{-1} $$

yani 

$$ A A^T(AA^T)^{-1} = I$$

Sağdan ters soldan tersin ayna yansıması gibi. 

Şimdi soldan terse dönelim, eğer bu tersi soldan değil, sağdan
uygulasaydık, bu durumda birim matris elde edemezdik, peki ne elde ederdik?


$$ A (A^TA)^{-1} A^T = ?$$

Üstteki ifade tanıdık geliyor mu? Bu ifade yansıtma (projection) matrisi
$P$ değil mi? Hatırlarsak $P$ matrisi $A$'nin kolon uzayına yansıtma
yapılmasını sağlıyordu. Bir bağlamda aslında $P$'ye imkansız bir görev
vermiş olduk, o birim matrisine erişmeye uğraşıyor fakat bunu her yerde
yapamıyor, yapabildiği yerde 0 diğer yerlerde başka değerlere sahip
oluyor. 

Aynı şekilde sağ tersi soldan çarparsak, o zaman 

$$ A^T (AA^T)^{-1}A = ?$$

Bu matris nedir? Yine yansıtma matrisi, bu sefer satır uzayına olan bir
yansıtmadır. 

Özel hali gördük (sağdan, soldan). 

Artık genel hali görebiliriz ki buna sözde hal (pseudo case) ismi
veriliyor. Diyelim ki bir $A$ var, bu $A$'nin kertesi $r < n$, yani satır
uzayının biraz sıfır uzayı var, $r < m$, yani kolon uzayının biraz sıfır
uzayında birşeyler var. Matris terisini "bozan'' şey nihayetinde bu sıfır
uzayları değil midir? O zaman bu koşulda olabilecek en iyi matris tersi
nedir?

Bir vektör $x$ var ise, $Ax$ bize $A$'nin kolon uzayında bir sonuç verir,
çünkü $Ax$, $A$'nin kolonlarını bir şekilde kombine eder. Şimdi diyelim ki
$x$ vektörü $A$'nin kendi satır uzayından geliyor olsun. Hatta hem $x$ ve
$y$, $A$'nin satır uzayından geliyor olsunlar, o zaman hem $Ax$ hem de $Ay$
$A$'nin kolon uzayında olacaktır ve $Ax \ne Ay$ eğer $x \ne y$ işe. 

Peki eğer $Ax = Ay$ işe? O zaman 

$$ A(x-y) = 0 $$

Bu durumda $x-y$ hangi uzaydadır? Tabii ki satır uzayındadır, çünkü $x$
satır uzayında, $y$ satır uzayında ise bu iki vektörün farkı da satır
uzayında olmalı. Devam edelim, fakat bu durumda $x-y$ hem satır uzayında
hem de sıfır uzayında. Bu vektör hangi vektördür? Sıfır vektörüdür. Yani
$Ax = Ay$ ise $x = y$ olmaya mecbur.

Yani $A$'nin herhangi bir satırıyla $A$'nin kolon uzayına geçiş
yapabiliyoruz. $Ax$, ya da $Ay$, vs. 

O zaman, ve eğer kolon uzayından satır uzayına geri gelebilirsek, 
yani $y = A^{+}(Ay)$ ile yani, bunu yapmamızı sağlayan $A^{+}$ ile
gösterilen  şey bir sözde terstir. Bu ifadenin iyi tarafı sıfır 
uzaylarından bizi tamamen uzak tutuyor olması.

Sözde ters $A^{+}$'i nasıl buluruz? Bir yöntem SVD kullanmak.

SVD ile $A = U \Sigma V^T$. 

Kertesi $r$ olan bir matrisin $\Sigma$'sinin sadece $r$ köşegen öğesi sıfır
olmayan değer içerir, diğerleri sıfırdır.

$$ 
\Sigma = 
\left[\begin{array}{cccc}
\sigma_1 & & & \\
 & \ddots & & \\
 &  & \sigma_r & \\
 &  &  & 0 \\
\end{array}\right]
$$

Sözde tersi durumunda köşegenler neye benzer? Tam tersi düşünelim, bu
kolay, bir bolu hesabı yapıyoruz, bu hesabı sadece sıfır olan köşegen
değerlere uygulayamazdık, o zaman

$$ 
\Sigma^{+} = 
\left[\begin{array}{cccc}
1/\sigma_1 & & & \\
 & \ddots & & \\
 &  & 1/\sigma_r & \\
 &  &  & 0 \\
\end{array}\right]
$$

Dikkat, $\Sigma$ $n \times m$ boyutlarında, $\Sigma^{+}$ ise $m \times n$ boyutlarında. 

$\Sigma \Sigma^{+}$ nedir? 


$$ \Sigma \Sigma^{+} =  
\left[\begin{array}{cccc}
1 & & & \\
 & \ddots & & \\
 &  & 1 & \\
 &  &  & 0 \\
\end{array}\right]
$$

Boyut $m \times m$. Çarpımı değişik yönden yaparsam? 

$$ \Sigma^{+}\Sigma  =  
\left[\begin{array}{cccc}
1 & & & \\
 & \ddots & & \\
 &  & 1 & \\
 &  &  & 0 \\
\end{array}\right]
$$

Benzer durum, fakat boyut $n \times n$. 

$A = U \Sigma V^T$ formülüne dönersek, $A^{+}$ nedir? 

$V^{T}$ birim dikgen (orthogonal) matristir, ve sözde tersi devriğine
eşittir. $\Sigma$'yi gördük, $U$ için benzer durum $U^T$. 

$$ A^{+} =  V \Sigma^{+} U^T$$



