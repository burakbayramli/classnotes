# Ders 2.18

[bazı multigrid yorumları atlandı]

Krylov Matrisleri 

Bu matrislerden $K$ olarak bahsedeceğiz ve bu yöntem bağlamında 

$$ Ax = b $$

sistemini çözüyor olacağız. Krylov matrisleri şöyle yaratılır

$$ K_j = \left[\begin{array}{rrrrr}
b & Ab & A^2b & .. & A^{j-1}b
\end{array}\right] $$

Krylov altuzayı $\mathscr{K}$ ise üstteki kolonların lineer kombinasyonudur
(span), ya da üstteki matrisin kolon uzayıdır da denebilir. Bu tür bir
matrisle niye ilgilenirim? Jacobi işlemi aslında bu kolonların
kombinasyonlarından birini her adımda yavaş yavaş seçer, yani aslında
Krylov altuzayının bir parçasında çalışır. Daha doğrusu ufak ufak başlar, o
altuzayda yavaş yavaş büyür.

Jacobi sürekli bir kombinasyon seçimi yapar, tabii bu seçimin en iyi seçim
olduğu söylenemez. Seçimin en iyisini yapsak daha iyi olmaz mı? 

En iyiyi seçmek için kullanılacak metot eşlenik gradyan (conjugate
gradient) olacak. Bu metot $K$ içinde $x_j$'yi seçer. 

$\mathscr{K}$ uzayı yaklaşıksal çözümümüzü aradığımız yer tabii ki. Bu arada üstteki
$K$ matrisinin elemanlarını yaratmak çok kolay, matris çarpımı yapıyoruz, ve
bir sonraki eleman bir öncekinin $A$ katıdır, ve $A$ çoğunlukla seyrektir
(sparse), bazen de simetriktir (eşlenik gradyan metotu için $A$ simetrik,
pozitif kesin olmalı).

Ama EG metotundan önce Arnoldi kavramını görmemiz lazım. 

Uygulamalı Matematikte sürekli bir şeyler "seçeriz'', ve çoğunlukla baz
vektörleri seçeriz ve birkaç özellik ararız. Aradığımız özellikler
öncelikle hızdır, yukarıda gördüğümüz gibi, matris çarpımı var, bu çok
hızlı. Bir diğer özellik bağımsızlık. Bir diğeri baz vektörlerinin birimdik
(orthonormal) olması. Bu son özellik elde edilebilirse en iyisidir. Üstteki
$K$ pek iyi bir baz değildir. Arnoldi'nin amacı Krylov bazını
dikgenleştirmektir. $b,Ab,..$'yi alıp $q_1,q_2,..,q_j$ oluşturmaktır. Koda
bakalım,

Algoritma `arnoldi`

  * $q_1 = b / ||b||$, normalize et
  *  Her $j = 1,..,n-1$ için $q_{j+1}$'i hesaplamaya başla
  
     * $t = A * q_j$
     * Her $i = 1,..,j $ için $t$, $\mathscr{K}_{j+1}$ uzayında
     
       * $h_{ij} = q_i^T t$,  $h_{ij}q_i$, $t$'nin $q_i$'ye yansıması
       * $t = t - h_{ij}q_i$, yansımayı çıkart
     
  
  * // $t$, $q_1,..,q_j$'ye dikgen oldu
  * $h_{j+1,j} = ||t||$, $t$'nin büyüklüğünü hesapla
  * // $q_{j+1} = t / h_{j+1,j}$
  $q_1,..,q_j$ birimdik


Fikir Gram-Schmidt fikriyle çok benzer. 1. satırda ilk vektörü olduğu gibi
alıyoruz, sadece normalize ediyoruz. Sonra 3. satırda bir deneme
amaçlı bir vektör $t$'ye  bakıyoruz. Bu vektör ilk baştaki $b$'ye dikgen
olmayacak muhakkak. O zaman 5. satırda bir iç çarpım (inner product)
sonrası, 6. satırda $t$'den çıkartıyoruz. 8 ve 9. satırlarda bu
vektörü normalize ediyoruz. 

Eğer $A$ simetrik ise, $h_{ij}h_{ij-1}$ çarpımını birkaç kere çıkartmam
yeterlidir. 

Örnek 

$$  
A = 
\left[\begin{array}{rrrr}
1 &&& \\
 & 2 && \\
 && 3 & \\
 &&& 4 
\end{array}\right],
b = 
\left[\begin{array}{r}
1 \\ 1\\ 1 \\ 1
\end{array}\right]
,
K_4 = 
\left[\begin{array}{rrrr}
1 & 1 & 1 & 1\\
1 & 2 & 4 & 8\\
1 & 3 & 9 & 27\\
1 & 4 & 16 & 64 
\end{array}\right]
$$

$A$ hem simetrik, onun ötesinde köşegen, ayrıca oldukça seyrek. Krylov
matrisi de üstte. İlk kolonu $b$ ile aynı. 2. kolon için $A$ ile
çarpmışız. 3. için bir daha $A$ ile çarpmışsız, 4. için bir daha.

$K$ eğer bir baz ise, temsil ettiği uzay tüm $\mathbb{R}^4$'tür. Üstteki
örnekte $j = n = 4$, tüm değerleri işledik. Eğer $n$ çok büyük bir sayi ise
mesela $10^5$ gibi, $j << n$ yani sona gelmeden çok önce durmak
isteriz. Eşlenik gradyan bunu başarıyor. 

$K$ formatındaki bir matrise Vondermonde matrisi de denir, bu tür
matrislerde ilk kolon sabit, 3., 4., .. kolonlar ikincinin üstel 
halidir. 

Vondermond matrisleri pek iyi koşullandırılmış (conditioned) matrisler
değildir. Alakalı bir soru: iyi, kötü koşullandırılmış matrisi nasıl
anlarız? Matris eşsiz (singular) değil. Determinanti hesaplasak sıfır
çıkmaz. Ama neredeyse "eşsiz olmaya yakın''. Bunun testini nasıl yaparız? 

Matris eşsiz değil, o zaman özdeğerleri hesaplamak akla gelebilir,
oradan $\lambda_{min}$, $\lambda_{maks}$'i kontrol etmek.. Fakat simetrik
olmayan matrislerin özdeğerlerini hesaplamak hoş değildir, "güvenilir''
hesaplar değildirler. Çok kötü koşullandırılmış ama tüm özdeğerleri 1 olan
matrisler olabilir mesela, çaprazında 1'ler olur, çaprazın üstünden
katrilyonlar olabilir.. 

Bu işi doğru yapmanın yolu $V^TV$'ye bakmak. Yani genel kural, matris
simetrik değilse, devriği ile kendisini çarp, sonucun özdeğerleri hep
pozitif olur. $V^TV$'nin $i$'inci özdeğeri, $V$'nin $i$'inci özdeğerinin
karesi olacaktır. 

Bu arada $V^TV$ matrisine Gram matrisi denir. 

Eğer $Q^TQ$ olsaydı koşullandırma sayısı (condition number), yani en büyük /
en küçük özdeğer ne olurdu? $Q^TQ = I$  o zaman çaprazda hep 1 var, $1/1 =
1$. Bu en iyi koşullandırma sayısıdır. 

Şimdi şu çok önemli formül için gerekli her bileşene sahibiz. 

$$ AQ = QH $$

$A$ bize verilen -diyelim ki- simetrik matris. $Q$ Arnoldi'den gelen
baz. $H$ ise kodda görülen çarpan değerleri. Yani $QH$ bir nevi
Gram-Schmidt'teki gibi, hatırlarsak Gram-Schmidt $QR$ ile temsil
ediliyordu. $Q$ yine birimdik, Gram-Schmidt'te $R$ üst köşegen. 

$H$ hesaplanırsa

$$  H = 
\left[\begin{array}{rrrr}
5/2 & \sqrt{ 5/2} && \\
\sqrt{ 5/2} & 5/2 & \sqrt{ 4/5}& \\
 & \sqrt{ 4/5} & 5/2 & \sqrt{ 9/20}\\
 &&  \sqrt{ 9/20} & 5/2 
\end{array}\right]
$$

$H$ simetrik ve üçlü köşegen (tridiagonal). Üçlü köşegenlik bize ne
söyler?  Tekrarın (recurrence) kısa olduğunu.

$$ AQ = QH $$

formülüne dönelim, kolonsal olarak üstteki çarpımı nasıl gösteririz? 

$$ Aq_1  = \frac{ 5}{2}q_1  + \frac{ \sqrt{ 5}}{2} q_2 $$

Tek bir kalemde eğer $A$ simetrik ise $H$'in de simetrik olduğunu nasıl
gösteririm? $H$'nin formülü lazım, 

$$ H = Q^{-1}AQ $$

$Q^{-1}$ nedir? $Q$'nun dikgen olduğunu hatırlayalım, o zaman 
$Q^{-1} = Q^T$. Üstte yerine koyalım,

$$ H = Q^{T}AQ $$ 

Buna bakarak $H$ kesin simetriktir diyebiliriz, simetrik matrisler aynen
üstteki gibi yaratılır zaten, ortaya bir simetrik matris koyarsın, sağdan
herhangi bir matris, soldan onun devriği ile çarparsın, ve yeni bir
simetrik matris ortaya çıkar. 

Yani vardığımız sonuç Krylov bazının hızlı, basit şekilde dikgen hale
getirilebileceğidir.




