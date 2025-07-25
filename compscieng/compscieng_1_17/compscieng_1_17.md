# Ders 1.17

Sonlu Öğeler Metodu (Finite Elements Method)

FEM yöntemiyle diferansiyel denklem çözerken önce denklemin zayıf formu elde
edilir. Dikkat, bu formu daha sonlu öğeleri ortaya atmadan, matrislerden vs
bahsetmeden yazmak gerekir. Başlangıç diferansiyel denklem ve onun belli
şartlarda eşit olduğu (ama çözüm için faydalı olabilecek) başka bir formu
ortaya çıkartmaktan bahsediyorum. 

Zayıf formu "kuvvetli formdan'' çıkartıyoruz, kuvvetli form diferansiyel
denkleminin ilk hali,

$$
- \frac{\mathrm{d}}{\mathrm{d} x} \left( c(x) \frac{\mathrm{d} u}{\mathrm{d} x} \right) = f(x)
$$

Zayıf forma geçmek için eşitliğin iki tarafını ona $v$ sembolü vereceğim bir
"test fonksiyonu'' ile çarpıyorum. Dikkat, $u$ çözüm, $v$ üstteki formülü
"test'' etmek için kullandığım herhangi bir fonksiyon. 

$$
- \frac{\mathrm{d}}{\mathrm{d} x} \left( c(x) \frac{\mathrm{d} u}{\mathrm{d} x} \right) v(x) =
f(x) v(x)
$$

Sonra üsttekini entegre ediyorum,

$$
\int_{0}^{1} - \frac{\mathrm{d}}{\mathrm{d} x} \left( c(x) \frac{\mathrm{d} u}{\mathrm{d} x} \right) v(x) \mathrm{d} x=
\int_{0}^{1} f(x) v(x) \mathrm{d} x
$$

Böylece zayıf forma neredeyse erişmiş olduk. Not: herhangi bir $v(x)$ dedik ama
orada bazı şartlar olabilir, ileride göreceğiz. Üstteki formül her $v(x)$ için
doğru olmalıysa belki $v(x)$'in bir alanda konsantre olmasını zorlayabilirdim,
sonra başka bir $v(x)$ denerdim belki onun başka noktalara konsante olmasını
sağlayabilirdim... Üsttekinden neler çıkartabilirim buradan diye düşünüyorum şu
anda.. Fakat sundan eminim ki sol taraf sağ tarafa eşit olmalı. Bunu kullanarak
bir sürü takla attıktan sonra bile güçlü forma dönebileceğimi biliyorum.

Ama biz şu anda zayıf formu seviyoruz.. onu sevmeyi öğrenmemiz lazım, bize pek
çok bazı ek yetenekler sağlayacak çünkü. Eşitliğin sol tarafında ek neler
yapabilirim mesela? Sağ taraf tamam, orası kafama uygun.

Sol tarafı daha iyi hale getirebilirim, parçalı entegral (integrate by parts)
alarak.. Formülün sol tarafına sevmediğim bazı şeyler var, mesela $u$ üzerinde
iki tane türev var, ama $v$ üzerinde hiç türev yok, tek başına orada duruyor.
Ben simetri görmek istiyorum. Parçalı entegral nasıl yapılıyordu,

$$ \int y \mathrm{d} z = y  z - \int z \mathrm{d} y $$

değil mi? Bir şeyin türevi çarpı $v$ var, türevi $v$'ye geçiriyoruz, eşitliğin
sağ tarafı hala aynı,

$$
\int_{0}^{1} c(x) \frac{\mathrm{d} u}{\mathrm{d} x} \frac{\mathrm{d} v}{\mathrm{d} x} \mathrm{d} x =
\int_{0}^{1} f(x) v(x) \mathrm{d} x
$$

Nihai zayıf form bu. Parçalı entegral açılırken

$$
- \left[ c(x) \frac{\mathrm{d} u}{\mathrm{d} x}  \right]_{0}^{1}
$$

ortaya çıkıyor, bu sıfır olup yokoluyor, çünkü serbest uçta $c(x) \frac{\mathrm{d} u}{\mathrm{d} x}$
sıfıra eşit. Sabit uçta ise $u$ sıfıra eşit, tabii o zaman $v$ de orada sıfıra
eşit olmalı işte "herhangi bir $v$'' ibaresine bir sınırlama burada geliyor,
sabit uçta $v=0$ olacak şekilde bir $v$ seçilmeli.

Bu arada $v$'yi $u$'dan ufak sapmalar olarak görmek te faydalı.. Bu durumda
sabit uçta sapma sıfır demiş oluyoruz.

Dersin sonunda zayıf form nasıl kullanılır onu görelim. Galerkin'in fikri buydu,
Galerkin sürekli bir problemi ayrıksal bir probleme çevirmenin yolunu bulmuştu.
Bilinmeyen bir fonksiyon yerine bilinmeyen $N$ tane yeni fonksiyon ortaya
çıkart, ki bu yeni fonksiyonlar $KU = F$ içinde kullanılabilsin ve çözülebilsin.
$KU=F$'e FEM ile erişmeye uğraşıyorum dikkat, FD ile değil.

Galerkin dedi ki $N$ tane deneme fonksiyonları $\phi_1,...,\phi_N$ olsun. Bu
fonksiyonların ne olduğunu biz seçeceğiz, uygulamalı matematikte bu çok olur,
bir fonksiyon seçersiniz, iyi seçerseniz her şey iyi işler, kötüyse işlemez.
Neyse iyi, temel olanlardan seçtik diyelim, onların bir kombinasyonu üzerinden
$u$'yu yaklaşık şekilde temsil edebiliyoruz,

$$
U \approx U_1 \phi_1(x) + ... + U_N \phi_N(x)
$$

$\phi_1,...,\phi_N$ fonksiyon, $U_1,..,U_N$ tek sayı, onları ağırlık değerleri.
Şu anda bilinmeyen $U_i$ katsayıları üzerinden optimal bir $\phi_i$ kombinasyonu
bulacağım ki bu olabileceği kadar $u$'ya yakın olacak. Fakat $N$ tane bilinmeyen
katsayı var burada, o zaman bana $N$ tane denklem lazım. İşte $N$ tane test
fonksiyonu $V_1,...,V_N$ üzerinden zayıf formu $N$ kere kullanarak bu
denklemleri üretebilirim. Her $V_i$ bana yeni bir denklem verir, $N$ tane
bilinmeyenim var, bu bana bir kare matris verir, bir lineer sistem olur, 
$KU = F$'e böylece erişirim.

Fakat aslında bu fikir FEM'den 100 sene daha yaşlı. FEM'in katkısı deneme ve
test fonksiyonlarını belli bir şekilde seçmektir, kısaca basit polinom
olarak. Aklımıza gelebilir Galerkin niye bunu yapmadı? Belki yapmıştır, fakat
bugünün bilgisayar yazılımları bu yolun seçilmesini daha rahatlaştırdı muhakkak.
Bugün biz FEM çözerken üç, beş değil binlerce baz fonksiyon kullanabiliyoruz.







