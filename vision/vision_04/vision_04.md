# Ders 4

Önceki derste Lie grup ve cebiri gördük. Lie cebirine geçiş sebebi lineer bir
uzayda optimizasyon yapmak istememiz, alternatif gayrı-lineer uzayda, zor
kısıtlamalar üzerinden optimizasyon. Lie cebiri ile işleri
kolaylaştırdık. Ayrıca cebirden gruba giden bir üstel harita oluşturduk, ve onun
tersi bir logaritma idi.

Sonra fırıldak cebirini gördük, ki bir $4 \times 4$ matrisi olan
$\hat{\xi}$, eğri $g(t)$'ye üzerinde bir teğet vektör olarak
görülebilirdi, ve $\hat{\xi}$'ye bir fırıldak ismini verdik. $g(t)$'yi
sağdan çarpınca alttakini elde etmiştik.

$$ \dot{g} = \dot{g}g^{-1}g = \hat{\xi} g $$

so(3) durumunda olduğu gibi tüm mümkün fırıldaklar bir teğet uzayı
oluşturuyorlardı, ki bu uzay bir Lie cebiridir.

$$ 
se(3) \equiv \bigg\{
\hat{\xi} =
\left[\begin{array}{rrr}
\hat{w} & v \\ 0 & 0
\end{array}\right] 
\biggm\vert
\hat{w} \in so(3), v \in \mathbb{R}^3 
\bigg\}
\subset \mathbb{R}^{4 \times 4}
 $$

Üstteki uzay Lie grubu SE(3)'e teğet idi.

Daha önce olduğu gibi $\land$ ve $\lor$ operatörlerini tanımlıyoruz, bu
sayede fırıldak $\hat{\xi} \in se(3)$ ile fırıldak kordinatları $\xi \in \mathbb{R}^6$ 
arasında gidip gelebilecektik.

$$ \hat{\xi} 
\equiv
\left[\begin{array}{c}
v \\ w
\end{array}\right]^{\land} 
=
\left[\begin{array}{cc}
\hat{w} & v \\ 0 & 0
\end{array}\right]  \in \mathbb{R}^{4 \times 4}
 $$

Hesaplanacak 6 derece serbestlik üstteki $v,w$ vektörlerinin
içeriklerinden oluşuyor. $\land$ uygulanan fırıldak vektörü, onun sonucu
fırıldak matrisi. 

$$ 
\left[\begin{array}{cc}
\hat{w} & v \\ 0 & 0
\end{array}\right]^{\lor} = 
\left[\begin{array}{c}
v \\ w
\end{array}\right] \in \mathbb{R}^6
 $$

Diferansiyel denklem sistemi

$$ \left\{ \begin{array}{ll}
\dot{g}(t) = \hat{\xi}g(t), \quad \hat{\xi} = \textrm{ bir sabit } \\
g(0) = I
\end{array} \right. $$

Bu sistemin çözümü

$$ 
g(t) = e^{\hat{\xi} t} = \sum_{n=0}^{\infty} \frac{(\hat{\xi} t)^n}{n!}
 $$

Ve daha önce kendimize sorduğumuz aynı soruya tekrar geldik, acaba
$\hat{\xi}$'nin katlarını kodlama sırasında hesaplamamız gerekecek mi?
Cevap yine hayır; aynen Rodriguez formülüyle daha temiz bir şekilde hesap
yapabildiğimiz gibi bu fırıldak durumunda da benzer bir temiz hesap var,

$$ e^{\hat{\xi}} = 
\left[\begin{array}{rrr}
e^{\hat{w}} & \frac{(I-e^{\hat{w}}) \hat{w}v + ww^Tv}{|w|} \\
0 & 1
\end{array}\right]
\qquad (1)
$$

Eğer $w = 0$ olsaydı $e^{\hat{\xi}}=\left[\begin{array}{rrr} I & v\\0 & 1\end{array}\right]$ 
elde ederdik, üstteki formülün $w \ne 0$ için tamamen hesaplanması
gerekirdi.  Formül içindeki $e^{\hat{w}}$ Rodriguez formülüyle hesaplanabilir.

Yani tekrar Lie grubundan Lie cebirine geçişi yapmış olduk. 

$$ \exp: se(3) \to SE(3); \qquad \hat{\xi} \to e^{\hat{\xi}} $$

Peki geriye gidiş mümkün mü? Bu noktada artık sürpriz olmayacak herhalde,
geriye gidiş yine logaritma ile mümkün. Matematiksel olarak ifade edelim, 

Teori 

Her $g \in SE(3)$ için bir fırıldak kordinatı $\hat{\xi}=(v,w) \in
\mathbb{R}^6$ vardır, ki $g = \exp(\hat{\xi})$. 

İspat

Verilen herhangi bir $R$ için $e^{\hat{w}} = R$ olduğunu biliyoruz. Bu bize
(1)'deki matrisin sol üst köşesini sağlıyor. Geriye tek kalan matrisin sağ
üst köşesindeki ifadeyi ispatlamak. Bu ifadeyi, 

$$ \frac{(I-e^{\hat{w}}) \hat{w}v + ww^Tv}{|w|} $$

$v$ için çözersek, daha doğrusu çözülebileceğini gösterirsek, ispat
tamamlanmış olur. 

Üzerinden tekrar geçmek gerekirse, hareket eden bir manzarayı (daha doğrusu
bizim / kameramızın hareketi yüzünden değişmekte olan arka planı)
modellemek için katı gövde transformasyonu kullanıyoruz, ki böylece dünya
kordinatlarındaki objeyi kamera kordinatlarına çeviriyoruz. Bu çevirme
işlemi rotasyon ve yer değiştirmeyi içeriyor, ki bu hesapları, her $t$ anı
için bir $4 \times 4$ matrisi

$$ g(t) = 
\left[\begin{array}{rrr}
R(t) & T(t) \\ 0 & 1
\end{array}\right] \in SE(3)
 $$

ile yapabiliyoruz. $t=0$ anında kamera çerçevesi / ekseni / görüntüsü
(frame) ile dünya görüntü birbirine eşit, yani $g(0) = I$. Dünyadaki
herhangi bir $X_0$ noktası için $t$ anında kamera görüntü hali ,

$$ X(t) = R(t)X_0 + T(t) $$

3D tekrar oluşturma işlemlerinde üç boyutlu dünyayı bir görüntüye göre
yaratmak gerekiyor; bu görüntünün seçiminde serbestsiz, istediğimiz
görüntüyü seçebiliriz, fakat gerçek dünya uygulamalarında bu görüntü
genellikle ilk kamera pozisyonu olarak seçilir. Kamera hareket eder, o
hareket olurken ve biz hesaplarımızı yaparken dünyayı hep sanki ilk
kameradan bakıyormuş gibi oluşturmaya çalışırız, kordinatlar sürekli bu ilk
kordinat sistemine tercüme ederiz. 

Üstteki formül homojen kordinatlarda

$$ X(t) = g(t) X_0 $$

Not: Hem normal hem homojen kordinatlarda aynı $X_0$'in kullanılmış olması
kafa karıştırabilir, bunu notasyonu temiz tutmak için bilerek yaptım, eğer
homojen durum için $X_0$ vektörüne '1' sayısını yeni bir öğe olarak eklemek
gerekir, fakat bu "yeni'' vektör için yeni bir notasyon ekleseydim
notasyon enflasyonu olacaktı. En temizi aynı $X_0$'i kullanmak, ama
aklınızda olsun, eğer homojenlikten bahsediyorsak, $X_0$'in dört ögesi var,
ve sonuncu öğe 1. 

Hareketleri Bitiştirmek 

Diyelim ki kameranın iki ayrı hareketi sonrası $t_1,t_2$ anlarında ardı
ardına iki görüntüye sırasıyla eriştik. $t_1$ anındaki baktığımız
noktaların $t_2$ anına transforme edilmesi için $g(t_2,t_1)$
kullanılacaktır, ve 

$$ X(t_2) = g(t_2,t_1)X(t_1) $$

Tabii $g$'nin yapısı öyle olmalıdır ki $t_0$'dan direk $t_2$'deki hale
gitmek ile önce $t_1$ sonra $t_2$ durumuna gitmek arasında fark olmamalı,
yani

$$ X(t_2) = g(t_3,t_2)X_2 = g(t_3,t_2)g(t_2,t_1)X(t_1) = g(t_3,t_1)X(t_1) $$

ki üstteki ifade tüm noktalar için doğru olmalı, yani,

$$ g(t_3,t_1) = g(t_3,t_2)g(t_2,t_1) $$

Bu geçiş mantığını takip edersek, şunu da söyleyebiliriz; $t_1$
görüntüsünün kordinatlarını $t_2$'ye, oradan tekrar geriye gidersek, şu
eşitliğin ortaya çıktığını görürüz,

$$ X(t_1) g(t_1,t_2)X(t_2) = g(t_1,t_2) g(t_2,t_1) X(t_1)$$

Bu eşitliğin tüm $X(t_1)$ noktaları için doğru olması gerektiği için, 

$$ g(t_1,t_2)g(t_2,t_1) = I 
\quad \Leftrightarrow \quad g^{-1}(t_2,t_1) = g(t_1,t_2)
$$

Hız

Eğer $t$ anındaki transformasyonun değişimini, bir hızı, yani
$\dot{X}(t)$'i tanımlamak isteseydim, $X(t) = g(t)X_0$ olduğunu
hatırlayarak ve bu ifadenin türevini alarak,

$$ \dot{X}(t) = \dot{g}(t)X_0 $$

Ayrıca

$$ X(t) = g(t)X_0 \iff X_o = g^{-1}(t)X(t) $$

olduğuna göre, üstteki $X_o$'i iki üstteki formüle koyabiliriz,

$$ \dot{X}(t) = \dot{g}(t)g^{-1}(t)X(t) $$

Üstteki form tanıdık gelebilir, fırıldak tanımına yaklaştık. Fırıldak
kordinatları üzerinden,

$$ \hat{V}(t) \equiv \dot{g}g^{-1}(t) = 
\left[\begin{array}{rrr}
\hat{w}(t) & v(t) \\ 0 & 0
\end{array}\right] \in se(3)
$$

alttaki ifadeyi elde ederiz, 

$$ \dot{X}(t) = \hat{V}(t)X(t) $$

Basit 3D kordinat sisteminde

$$ \dot{X}(t) = \hat{w}(t)X(t) + v(t) $$

olurdu. Neyse, vardığımız sonuç şudur; $\hat{V}(t)$ bize dünya görüntüsünün
kamera çerçevesine göre izafi hızını vermiş oluyor.

Farklı Çerçevelerdeki Hız

Aynen noktaları transform edebildiğimiz gibi hızı da bir çerçeveden
diğerine taşıyabilmeliyiz. Diyelim ki iki kişi, A ve B değişik açılardan
bir görüntüye bakıyorlar, bu kişiler değişimi nasıl görürlerdi? 
Diyelim ki $g_{xy}: Y = g_{xy}X(t)$. Yeni görüntüdeki hız, 

$$ 
\dot{Y}(t) = g_{xy}\dot{X}(t) = 
g_{xy}\hat{V}(t)X(t) = g_{xy}\hat{V}g_{xy}^{-1}Y(t)
$$

Yani birinci kameradaki olanın ikinci kamerada görülen izafi hızı alttaki
fırıldak ile hesaplanır, 

$$ \hat{V_y} = g_{xy} \hat{V} g_{xy}^{-1} Y(t) = ad_{g_{xy}}(\hat{V})$$

$ad$ ifadesi katımlı (adjoint) kelimesinden geliyor, 

$$ ad_g: se(3) \to se(3); \qquad \hat{\xi} \to g \hat{\xi}g^{-1} $$

se(3) üzerinde bir katımlı eşlemedir. Bu eşleme bir Lie cebir öğesini alır,
ona soldan ve sağdan bir katı gövde transformasyonu ve onun tersini
uygular, ve böylece yeni bir fırıldak elde etmiş oluruz. 

Bu noktaya kadar pek çok farklı formülden bahsettik, özetlemek açısından
alttaki tablo faydalı olabilir, 

$$ 
\begin{array}{|p{4.0cm}|p{4.0cm}|p{4.0cm}|}
\hline
   & \textrm{Rotasyon SO(3)} & \textrm{Katı gövde SE(3)} \\
\hline
  \textrm{Matris temsili} &  R $\in$ GL(3): $R^TR=I$, $\det(R)=1$ & 
  $g = \left[\begin{array}{rr} R & T \\ 0 & 1 \end{array}\right]$ \\
\hline
 \textrm{3-D kordinatlar} & $X=RX_0$ & $X = RX_0 + T$ \\
\hline
 Tersi & $R^{-1}=R^T$ & 
 $g^{-1} = \left[\begin{array}{rr} R^T & -R^T \\ 0 & 1 \end{array}\right]$\\
\hline
 \textrm{Üstel temsil} & $R = \exp(\hat{w})$ & $g=\exp(\hat{\xi})$ \\
\hline
 Hız & $\dot{X}=\hat{w}X$ & $\dot{X} = \hat{w}X + v$ \\
\hline
 \textrm{Katımlı eşleme} & 
 $\hat{w} \to R \hat{w} R^T$ & 
 $\hat{\xi} \to g \hat{\xi}g^{-1}$\\
\hline
\end{array}
$$

[Euler açıları atlandı]





