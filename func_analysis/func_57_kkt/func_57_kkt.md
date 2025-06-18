# Karush-Kuhn-Tucker (KKT) Şartları

KKT şartları birkaç basit kavramın bir araya gelmesiyle oluşan çok kuvvetli
bir kavram. Bu şartlar 4 tane. Alttaki gibi genel bir problemle bağlantılılar,

$$
\min_x f(x) \quad \textrm{öyle ki}
$$
$$
h_i(x) \le 0, \quad i=1,..,m
$$
$$
l_j(x) = 0, \quad j=1,..,r
$$

Lagrangian'ı hatırlarsak, 

$$
L(x,u,v) =  f(x) + 
\sum_{i=1}^{m} u_i \underbrace{h_i(x)}_{\le 0} + 
\sum_{i=1}^{r} v_i \underbrace{l_i(x)}_{=0}
$$

Problemin dışbükey olması mecbur değil. 

Bu şartlar,

1) Durağanlık şartı

Lagrangian'da $u,v$ sabitledim, ve $x$'e göre Lagrangian'ın altgradyanını
alırsam, 0 değeri bu altgradyan içinde olmalı. 

$$
0 \in \partial_x \left(
f(x) + \sum_{i=1}^{m} u_i h_i(x) + \sum_{j=1}^{r} v_j l_j(x) 
\right)
$$

2) Tamamlayıcı Gevşeklik (Complementary Slackness)

$$
u_i \cdot h_i(x) = 0 \quad \forall i
$$

Üstteki şart diyor ki ya $h_i(x)$ sıfır olmalı, yani $i$'inci eşitsizlik
sıkı olmalı, ya da ona tekabül eden ikiz değişken $u_i$ sıfır olmalı, yani
eşitsizlik ya sıkı -eşitlik- ya da dikkate alınmamalı.

3) Ana Olurluk

$$
h_i(x) \le 0, \quad l_j(x) = 0 \quad \forall i,j
$$

4) İkiz Olurluğu

$$
u_i \ge 0, \quad \forall i
$$

Optimallik için $x,u,v$ üstteki 4 şartı yerine getirmeli, genel bağlamda bu
o $x$'in ana problemi, $u,v$'nin ikiz problemi çözmesi anlamına geliyor. 

İspatlayalım. Önce ters yönden gelerek bir ispat, diyelim ki elimde ana ve
ikiz çözümler var. Eğer o "çözüm varsa, oradan KKT şartları meydana
çıkmalı'' ispatını görelim. Bu yöne "gereklilik'' (necessary)
denebilir. Diyelim ki $x^\ast$, ve $u^\ast,v^\ast$ ana ve ikiz çözümleri var, ve
ikizlik boşluğu sıfır (yani güçlü ikizlik aktif, Slater şartını
hatırlarsak). O zaman

$$
f(x^\ast) = g(u^\ast,v^\ast)
$$

Tanım itibariyle, her $u,v$ için $g(u,v)$ fonksiyou Lagrangian'ın $x$
üzerinden minimumu. Üsttekinden hareketle ve $u,v$ için $u^\ast,v^\ast$
kullanırsak,

$$
= \min_x f(x) + \sum_{i=1}^{m} u_i^\ast h_i(x) + \sum_{j=1}^{r} v_j^\ast l_j(x) 
$$
 
Üstte minimum buluyorum yani $x$ üzerinden üstteki Lagrangian'ı $x^\ast$'deki
halinden daha fazla ufaltmam mümkün değil,

$$
\le f(x^\ast) + \sum_{i=1}^{m} u_i{^\ast} h_i(x^\ast) + \sum_{j=1}^{r} v_j{^\ast} l_j(x^\ast) 
\qquad (1)
$$

Devam edersek, $u_i^\ast$ olurlu, o zaman üstteki ortadaki terim $\ge 0$,
$v_i^\ast$ olurlu o zaman üçüncü terim sıfır. Bu demektir ki üstteki ifade
$f(x^\ast)$'den daha az olmalı.

$$
\le f(x^\ast) 
\qquad (2)
$$

Sonuç olarak 

$$
f(x^\ast) \le f(x^\ast*)
$$

elde ettik, o zaman üstteki ifade aslında eşitsizlik değil bir eşitlik
çünkü eşitsizliğin iki tarafında aynı şey var. 

Eşitsizliklerin eşitlik olması ne anlama gelir? Demek ki $x^\ast$ Lagrangian'ı
$u^\ast,v^\ast$ noktasında minimize edecektir. Durağanlık bu demek zaten değil
mi? $x$'e göre Lagrangian'ın altgradyanını aldım ve bu sıfıra eşit.  Ayrıca
tamamlayan gevşeklik şartı da ispatlanmış oluyor, (1) ile (2) eşit
olmalıysa, ve (1)'deki 3. terimin sıfır olduğunu biliyoruz, 2. terimdeki
tamamı pozitif olan toplam öğeleride sıfır olmalı çünkü başka türlü (2)'ye
eşitliği elde edemeyiz. Eh bu durum da tamamlayan gevşeklik şartı değil
midir?

Ana-ikiz olurluğunu zaten bedava elde ettik. Elimizde bir çözüm varsa onlar
ana-ikiz olurlu olmalı. 

Yani ispatlamış olduk ki eğer elimizde ana-ikiz boşluğu olmayan bir çözüm
varsa, $x^\ast,u^\ast,v^\ast$ çözüm üçlüsü KKT koşullarına uymaktadır. KKT
koşularının güçlü ikizlik durumunda şart olduğunu göstermiş olduk.

Diğer yönden ispat etmeye uğraşalım. 

Elimizde KKT şartlarını tatmin eden bir $x^\ast,u^\ast,v^\ast$ üçlüsü olduğunu
düşünelim, ve bu üçlünün optimal olması gerektiğini ispat edelim, yani
$x^\ast$'in ana $u^\ast,v^\ast$'nin ikiz problem için optimal olması
gerektiğini.. Eğer $x^\ast,u^\ast,v^\ast$ KKT şartlarına uygunsa, durağanlığa göre

$$
g(u^\ast,v^\ast) = f(x^\ast) + \sum_{i=1}^{m} u_i{^\ast} h_i(x^\ast) + \sum_{j=1}^{r} v_j{^\ast} l_j(x^\ast) 
\qquad (3)
$$

Bu tabii $g$ tanımı ile bağlı,

$$
g(u^\ast,v^\ast) = \min_x L(x,u^\ast,v^\ast)
$$

ve durağanlıkla 

$$
= \min_x L(x^\ast,u^\ast,v^\ast)
$$

böylece üç üstteki tanım ortaya çıkıyor. 

(3) içindeki ikinci terimde tamamlayıcı gevşekliği uygularsak toplamdaki
her öge $x^\ast$ için sıfır, toplam sıfır, ikinci terim tamamen sıfır.

KKT koşullarından bir diğeri olurluk, o zaman (3)'teki üçüncü terimde her
toplam öğesi sıfır, toplam sıfır. Geriye tek kalan

$$
g(u^\ast,v^\ast) = f(x^\ast) + 
\cancel{\sum_{i=1}^{m} u_i{^\ast} h_i(x^\ast)} + 
\cancel{\sum_{j=1}^{r} v_j{^\ast} l_j(x^\ast)}
$$

$$
= f(x^\ast)
$$

Eğer $g(u^\ast,v^\ast) = f(x^\ast)$ ise o zaman ana ve ikiz kriter arasında ikizlik
boşluğu yoktur demektir, ve daha önce gördüğümüz gibi bu elimizdekinin bir
çözüm olduğunun işaretidir. 

Yani birkaç farklı yönden gelerek ispatladık ki $x^\ast$ ve $u^\ast,v^\ast$
KKT koşullarına uyuyorsa, o zaman elimizdekiler optimal ana ve ikiz
çözümleridir. KKT koşulları her zaman yeterlidir. Dikkat edilirse
dışbükeylikten hiç bahsetmedik, KKT yeterli (sufficient), onlara uygunluk
varsa optimal ana ve ikiz çözüme erişmişiz demektir. Gereklilik (necessary)
durumu eğer güçlü dışbükeylik varsa, elimizdeki çözüm KKT koşullarına da
uymaya mecbur. Biraz söz cambazlığı gibi gelebilir ama tanımların net
olması, ve ilerideki teorilere faydası açısından bu iyi. 

Soru

Eğer KKT koşullarını tatmin etmek optimallik için yeterliyse dünyadaki her
problemi KKT üzerinde rahatça çözebilir miyim?

Hayır çünkü 1. KKT şartı, durağanlık şartıyla problem çıkabilir. Pürüzsüz
bir problem için durağanlık için $f$ ve $h_i,l_i$'in gradyanlarını almam
yeter, sıfıra eşitlerim vs. Bir de dışbükeylik varsa tipik olarak
altgradyanı yazmak basittir. Sonucu bulmak her zaman kolay olmayabilir ama
en azından durağanlığı formülize etmek kolaydır. Fakat bazı fonksiyonlarda
dışbükeylik yoksa altgradyanları hesaplamanın zorluğu ötesinde bazen
altgradyan olmayabilir bile. Genelde pürüzsüz durumlarda KKT koşullarını
kullanmanın zor tarafı budur.

Bu sebeple formel bir uyarıyı slaytlara koyduk, "eğer $f$ dışbükey değilse
türevi alınabilir bir $f$ için bile direk $\partial f = \{\nabla f(x)\}$
diyemeyiz''. 

Eğer ana problemde hiçbir kısıtlama olmasaydı o zaman 2. 3. ve 4. KKT
şartları yokoluyor bu normal çünkü ikiz değişken olmayacak, 1. şarttaki
2. ve 3. terim yokolur, sadece altgradyanın sıfırı içerme şartı geriye
kalıyor. Bazen bilimcilerin bu tür problemlerde salt altgradyan optimalliği
kullanıp "KKT koşulları kullandım'' dediğini duyabilirsiniz, ben bile
dalgınlıkla bunu söyleyebiliyorum bazen. Fonksiyon var, altgradyanını alıp
sıfıra eşitliyorum, bu "KKT koşulu'' diyorum. Teknik olarak doğru tabii,
sadece KKT'nin çok basit bir hali bu.

Örnek

Eşitlik şartları olan karesel bir fonksiyonun minimizasyonuna bakalım. 

$$
\min_x \frac{1}{2} x^T Q x + c^T x \quad \textrm{öyle ki}
$$
$$
Ax = 0
$$

$Q \succ 0$ olacak, yani $Q$ pozitif yarı-kesin, o zaman üstteki problem
dışbükey. Eşitsizlik şartı yok.

KKT şartlarını yazalım. Lagrangian nedir? 

$$
L(x,u) = \frac{1}{2} x^T Q x + c^T x + u^T A x
$$

Durağanlık için üsttekinin gradyanını alıp sıfıra eşitlerim, 

$$
Qx + c + A^T u = 0
$$

Tamamlayıcı gevşeklik

(Boş, çünkü eşitsizlik şartları yok)

Ana-ikiz olurluk

İkiz olurluk boş, çünkü eşitsizlik yok, yani $u$'nin pozitif olma şartı
yok. Ana olurluk ise 

$$
Ax = 0
$$

Biliyoruz ki üstteki iki şartı tatmin edersem $x$ ana, $u$ ikiz optimal
çözüm olacak. 

Güzel değil mi? Hatta bu lineer bir sistem olarak yazılabilir,

$$
\left[\begin{array}{ccc}
Q & A^T \\ A & 0
\end{array}\right]
\left[\begin{array}{c}
x \\ u
\end{array}\right] =
\left[\begin{array}{r}
-c \\ 0
\end{array}\right] 
$$

Eğer üstteki sistemi $x,u$ bulmak için çözersem, optimal noktaları
bulabilirim. Eğer matris tersi alınabilir ise tersini alırım, eşitliğin
sağındaki ifade ile çarparım, bu bana ana-ikiz sonucu verir. 

Üstteki matrise bazen KKT matrisi deniyor, KKT koşullarını kullanarak
oluşturulduğu için. 

[atlandı, su doldurma problemi, SVM]

Ekler

Örnek

Şimdi [2]'den alınan bir örneği görelim. 

$$
\min_{x_1,x_2} x_1^2 + x_2^2 \quad \textrm{öyle ki}
$$
$$
x_1 + x_2 = 5
$$

Burada bir eşitlikle kısıtlanmış QP (equality constrained QP) var ve biraz
önce gördüğümüz KKT matrisini oluşturarak onu tek adımda çözebiliriz. 
Kriter ve kısıtlama matris formunda şöyle, $x = (x_1,x_2)$ alırsak,

$$
f(x) = \frac{1}{2} x^T 
\underbrace{
\left[\begin{array}{cc}
2 & 0 \\ 0 & 2
\end{array}\right]}_{Q} x + 
\left[\begin{array}{cc}
0 & 0
\end{array}\right]^T x
$$
$$
\underbrace{
\left[\begin{array}{cc}
1 & 1
\end{array}\right]}_{A}
\left[\begin{array}{c}
x_2 \\ x_2
\end{array}\right] = 5
$$

(Sıfır vektörü var çünkü $x_1^2,x_2^2$ içeren terimler var ama $x_1,x_2$
içeren terimler yok)

KKT matrisini şöyle oluştururuz, 

$$
\left[\begin{array}{ccc}
Q & A^T \\ A & 0
\end{array}\right]
\left[\begin{array}{c}
x \\ u
\end{array}\right] =
\left[\begin{array}{r}
-c \\ b
\end{array}\right] 
$$

$$
\left[\begin{array}{ccc}
2 & 0 & 1 \\
0 & 2 & 1 \\
1 & 1 & 0
\end{array}\right]
\cdot
\left[\begin{array}{c}
x_1 \\ x_2 \\ u
\end{array}\right] = 
\left[\begin{array}{c}
0 \\ 0 \\ 5
\end{array}\right] 
$$

```python
import numpy.linalg as lin
X = np.array([[2,0,1],[0,2,1],[1,1,0]]) 
a = np.array([[0],[0],[5]])
print (lin.solve(X,a))
```

```
[[ 2.5]
 [ 2.5]
 [-5. ]]
```

Doğrulamak için paket ile çözelim,

```python
from cvxopt import matrix
from cvxopt import solvers
Q = matrix([ [2.0, 0], [0, 2.0] ])
p = matrix([0.0, 0.0])
G = matrix([[0.0,0.0],[0.0,0.0]])
h = matrix([0.0,0.0])
A = matrix([1.0, 1.0], (1,2))
b = matrix(5.0)
sol=solvers.qp(Q, p, G, h, A, b)
print (sol['x'])
```

```
     pcost       dcost       gap    pres   dres
 0:  1.2500e+01  1.2500e+01  2e+00  1e+00  1e-15
 1:  1.2500e+01  1.2500e+01  2e-02  1e-02  0e+00
 2:  1.2500e+01  1.2500e+01  2e-04  1e-04  0e+00
 3:  1.2500e+01  1.2500e+01  2e-06  1e-06  0e+00
 4:  1.2500e+01  1.2500e+01  2e-08  1e-08  0e+00
Optimal solution found.
[ 2.50e+00]
[ 2.50e+00]
```

Ekler

Soru

Eğer KKT şartını eşitlik sınırlanmış QP (bilahere LP) çözmek için
kullanabiliyorsam, ve mesela LP diyelim, her eşitsizlik içeren problemi
standardizasyon ile eşitlikle sınırlı probleme çevirebiliyorsam, KKT
matrisi ile her türlü LP'yi çözemez miyim? Niye farklı metotlara giriş
yapmak gerekiyor? 

Cevap

Standardizasyon sonrası hala elimizde pozitiflik sınırlamaları var, ki
bunlar da birer eşitsizlik sınırı aslında. KKT matrisi çözmek bu
eşitsizlikleri tatmin etmez, basit bir matris tersi alıyoruz, elimize geçen
değişkenlerin pozitif kalma zorunluluğunu bu metot ile zorlayamayız.

Kaynaklar

[1] Tibshirani, *Convex Optimization, Lecture Video 12*, 
[https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg](https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg)   

[2] Lendek, *Optimization Lecture, Quadratic Programming*
[http://www.lendek.net/teaching/opt_en/](http://www.lendek.net/teaching/opt_en/)


