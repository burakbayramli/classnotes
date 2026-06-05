# Örgü (Lattice) Boltzmann Metodu

Bu yöntemde bir sıvı veya gazın sanal parçacıklardan oluştuğu
varsayılır ve bu sıvı parçacıkları bir simülasyon bölgesinde hareket
edebilir, diğer sıvı parçacıklarıyla çarpışabilir. Simülasyon alanı
bir örgü / ızgara sistemi olarak ele alınır ve sıvı parçacıkları
düğümden düğüme hareket eder; yani bir bölge içinde serbestçe hareket
edemezler. Bu yöntemin MD yöntemine kıyasla en önemli farkı, ızgara
Boltzmann yönteminin sıvı parçacıklarının konumlarını ve hızlarını
değil, hızların parçacık dağılım fonksiyonunu kullanarak hesap
yapmasıdır [3, sf. 24].

Aşağıdaki şekil iki boyutlu bir sistem için ızgara Boltzmann yöntemini
göstermektedir. (A), bir simülasyon bölgesinin ızgara sistemine
bölündüğünü göstermektedir. (B) ise birim kare ızgara hücresinin
büyütülmüş halidir. Çözücü moleküllerinin grupları veya kümeleri
olarak ele alınan sanal sıvı parçacıklarının yalnızca komşu düğümlere
hareket etmesine izin verilir, daha uzak düğümlere değil. Yani 0
düğümündeki sıvı parçacıklarının bir sonraki zaman adımında orada
kalmasına ya da 1, 2, .., 8 numaralı düğümlere hareket etmesine izin
verilir. Bu durum, 1, 2, 3 ve 4 numaralı düğümlere hareket eden sıvı
parçacıklarının $c = \Delta x / \Delta t$ hızına, 5, 6, 7 ve 8
numaralı düğümlere hareket edenlerin ise $\sqrt{2}c$ hızına sahip
olduğunu ima eder; burada $\Delta x$ en yakın iki düğüm arasındaki
ızgara aralığı ve $\Delta t$ simülasyonlar için zaman
aralığıdır. $\sqrt{2}c$ değeri, (B)'deki köşegen mesafenin
$\sqrt{\Delta x^2 + \Delta x^2} = \sqrt{2}\Delta x$ olduğu ve buradaki
hızın $\sqrt{2}\Delta x / \Delta t$, yani önceki $c$'nin $\sqrt{2}$
katı olacağı gerçeğinden hesaplanabilir.

![](compscieng_bpp43lbm_01.jpg)

Bu yöntemin gerekli denklemlerini türetmeye başlayalım. Daha önce
[1]'de Boltzmann taşınım denkleminin şu şekilde olduğunu gördük:

$$\frac{\partial f}{\partial t} + c \cdot \nabla f = \Omega$$

$\Omega$, moleküller hareket ettikten sonra gerçekleşebilecek çarpışmaları temsil eder; $dr\, dc$ aralığındaki molekül sayıları arasındaki net farkı yakalar.

### Çarpışmalar

Hiç çarpışma olmasaydı, önceki sayım sonraki sayımla tamamen aynı
olurdu ve aralarındaki fark sıfır olurdu. Bunun olmadığı durumlarda
sıfırdan farklı bir $\Omega$'ya ihtiyaç duyarız. Bu, $\Omega$'nın
hesaplanmasının kolay olduğu anlamına gelmez. Gerçek bir çarpışma
sayımı sistemdeki tüm olası çarpışmaları hesaba katmak zorunda
olduğundan hesaplamak güçtür. Ancak çarpışma operatörünü, çözümün
sonucunda önemli bir hata ortaya çıkarmadan basit bir hesaplamayla
yaklaşık olarak ifade etmek mümkündür [2, sf. 28].

$\Omega$'yı yaklaşık olarak ifade edebilmek için BGK yaklaşımı adı verilen bir yöntem geliştirilmiştir. BGK kaba ama zekice bir basitleştirmedir: tüm çarpışmaları hesaplamak yerine şu soruyu sorar: "Mevcut dağılım denge durumundan ne kadar uzakta ve ne kadar hızlı gevşiyor?" Bu şu sonucu verir:

$$\Omega = \frac{1}{\tau}(f^{eq} - f)$$

burada $f^{eq}$, Maxwell-Boltzmann dağılımıdır — [1]'de türettiğimiz
denge dağılımı. Fiziksel fikir şudur: bir gazı yeterince uzun süre
kendi haline bırakırsanız, çarpışmalar onu bu denge durumuna
yönlendirur. Dolayısıyla $f^{eq}$, $f$'nin ulaşmaya çalıştığı yeri
temsil eder.

$$f^{eq} = \left(\frac{m}{2\pi k_B T}\right)^{3/2} \exp\left(-\frac{m|c-u|^2}{2k_BT}\right)$$

Birçok açıdan BGK modeli, sistemin durumunu fark terimi aracılığıyla
"algılar": $(f^{eq} - f)$. $f^{eq}$ doğrudan yerel, gerçek zamanlı
makroskopik özelliklerden ($\rho$ ve $u$) hesaplandığından, sıvının
tam anlamıyla gevşemiş olsaydı bulunması gereken matematiksel durumu
temsil eder.

- Sıvı zaten denge durumundaysa $f = f^{eq}$, yani $\Delta f =
  0$. Model, hiçbir düzeltmeye gerek olmadığını algılar.

- $f$ ile $f^{eq}$ birbirinden uzaklaşır ve $\Delta f$ büyürse, bu
  çarpışmaların olduğu anlamına gelir; bu fark, bir kontrol
  sistemindeki hata sinyali veya sensör okuması gibi davranarak
  moleküler popülasyonun sınır dışına ne kadar çıktığını tam olarak
  ölçer.

Sapma bu çıkarma işlemiyle algılandıktan sonra, BGK operatörü bu değeri sistemi değiştirmek için hemen kullanır:

$$\Omega = \frac{1}{\tau}(f^{eq} - f)$$

Bu hata sinyalini $\frac{1}{\tau}$ gevşeme faktörüyle çarpar ve pasif
bir ölçümü aktif bir geri yükleyici kuvvete dönüştürür.

- Belirli bir hızda hareket eden çok fazla molekül varsa ($f > f^{eq}$), terim negatif olur ve çarpışma adımı o yöndeki popülasyonları azaltır.
- Çok az molekül varsa ($f < f^{eq}$), terim pozitif olur ve çarpışma adımı o yöndeki popülasyonları artırır.

Değişiklik her zaman sapmayı doğrudan orantılıdır. Bir düğüm doğal denge durumundan ne kadar uzaksa, onu yeniden dengeye sokmak için değiştirme adımı o kadar agresif hale gelir.

BGK çarpışma operatörünü aynı anda iki görev yerine getiren akıllı bir geri besleme döngüsü olarak düşünebiliriz:

1. Algılar: $(f^{eq} - f)$'yi hesaplayarak, o belirli düğümdeki sıvının denge durumundan ne kadar uzakta olduğunu ölçen bir sensör görevi görür.
2. Değiştirir: Bu ölçümü, orantılı bir düzeltme hesaplamak için kullanır. Bir hız durumu kalabalıksa BGK onu boşaltır; azalmışsa doldurur.

Bu, kendi kendini düzenleyen matematiksel bir motordur: yüksek bozulma, büyük bir düzeltici çarpışma tepkisini tetiklerken, sakin ve dengelenmiş bir akış tamamen değişmeden geçer.

Bir not olarak belirtmek gerekir ki yukarıda açıklanan şema özünde doğrusal bir gevşeme modelidir; Newton'un soğuma yasası $dT/dt = -(T - T_\infty)/\tau$ ile aynı matematiksel yapıya sahiptir.

### Denge Dağılımı

Denge dağılımını daha kolay hesaplanabilir hale getirmek için üzerinde bazı işlemler yapmamız gerekiyor. Gaussian $\exp(-|c|^2/2RT)$, ayrık bir ızgara için çeşitli nedenlerle sorunludur:

- Sürekli $c$ için tanımlanmıştır
- Hiçbir zaman tam olarak sıfıra eşit olmadığından sonlu sayıda yöne kesilemez
- Simülasyonda tekrar tekrar değerlendirilmesi pahalıdır

Bunu aşağıdaki adımlarla çözebiliriz.

$$f^{eq} = \frac{\rho}{(2\pi RT)^{D/2}} \exp\left(-\frac{(c-u)^2}{2RT}\right)$$

Üsteldeki kareyi açarak:

$$= \frac{\rho}{(2\pi RT)^{D/2}} \exp\left(-\frac{c \cdot c - 2c \cdot u + u \cdot u}{2RT}\right)$$

Üsteli bölerek:

$$= \frac{\rho}{(2\pi RT)^{D/2}} \exp\left(-\frac{c \cdot c}{2RT}\right) \exp\left(\frac{2c \cdot u - u \cdot u}{2RT}\right) \tag{1}$$

Üstel fonksiyon bir Taylor serisi kullanılarak açılabilir:

$$\exp(x) = 1 + x + \frac{x^2}{2!} + \frac{x^3}{3!} + \cdots \tag{2}$$

Taylor açılımını herhangi bir basitleştirme yapmadan doğrudan (1)'e koyarak:

$$f^{eq} = \frac{\rho}{(2\pi RT)^{D/2}} \exp\left(-\frac{c \cdot c}{2RT}\right) \left(1 - \frac{-2c \cdot u + u \cdot u}{2RT} + \frac{(-2c \cdot u + u \cdot u)^2}{4R^2T^2} + \cdots\right) \tag{3}$$

burada parantez içindeki ikinci terim (2)'deki $x$ terimidir, üçüncü terim $x^2/2!$ terimidir; $O(u^3)$ terimleri atılır ve $(-2c \cdot u + u \cdot u)^2 \approx 4(c \cdot u)^2$ sadeleştirmesi yapılır (çünkü kare içindeki $u \cdot u$ terimi zaten $O(u^2)$ mertebesinde olduğundan bütün ifadeyi $O(u^4)$ yapar), bu da (3)'ten (4)'e geçişi sağlar.

İkinci üstelin Taylor açılımı yapılarak $O(u^3)$ terimleri atıldığında:

$$f^{eq} = \frac{\rho}{(2\pi RT)^{D/2}} \exp\left(-\frac{c \cdot c}{2RT}\right) \left(1 + \frac{2c \cdot u - u \cdot u}{2RT} + \frac{(c \cdot u)^2}{2R^2T^2}\right) \tag{4}$$

$W(c) = \frac{1}{(2\pi RT)^{D/2}} \exp\!\left(-\frac{c \cdot c}{2RT}\right)$ ve $RT = c_s^2$ yerine koyarak:

$$f^{eq} = \rho W(c) \left(1 + \frac{2c \cdot u - u \cdot u}{2c_s^2} + \frac{(c \cdot u)^2}{2c_s^4}\right)$$

$W(c) \to w_i$ ızgara yönü $i$ boyunca ayrıklaştırılarak:

$$f^{eq}_i = \rho w_i \left(1 + \frac{2c_i \cdot u - u \cdot u}{2c_s^2} + \frac{(c_i \cdot u)^2}{2c_s^4}\right) + O(u^2) \tag{5}$$

$f^{eq}_i$, Taylor açılımlı MB (3.4) ile $\rho$ ve $u$ kullanılarak hesaplanır:

$$f^{eq}_i = w_i \rho \left(1 + \frac{c_i \cdot u}{c_s^2} + \frac{(c_i \cdot u)^2}{2c_s^4} - \frac{u \cdot u}{2c_s^2}\right)$$

Taylor açılımı, üsteli ayrık bir ızgara üzerinde ele alınabilir olan $c \cdot u$ cinsinden bir polinomla değiştirir. Ardından yapılan iki yerine koyma işlemi zariftir:

- $W(c) = \exp(-c \cdot c / 2RT)(2\pi RT)^{-D/2}$, Gaussian'ı bir ağırlığa absorbe eder — ve bu ağırlık yalnızca $c$'nin büyüklüğüne bağlı olduğundan, her ayrık ızgara yönü $i$ için bir kez hesaplanan sabit bir $w_i$ sabitine dönüşür
- $RT = c_s^2$, termodinamiği ızgara ses hızına bağlar

Dolayısıyla (5)'te ayrıklaştırmanın ardından $f^{eq}_i$, sabit katsayıları $w_i$, $c_i$, $c_s$ olan — hepsi önceden hesaplanabilir — $u$ cinsinden yalnızca bir polinomdur. LBM'yi hesaplamalı açıdan bu kadar çekici yapan da budur.

$O(u^2)$ kesmesi aynı zamanda sınırlamayı da bize söyler: bu, düşük Mach sayısı yaklaşımıdır. $|u|/c_s$'nin küçük olmadığı yüksek hızlı akışlar için daha yüksek mertebeden terimler gerekecektir.

### Ana Denklemin Ayrıklaştırılması

Şimdi bir Taylor açılımına daha ihtiyaç duyacağız. Bu tekniği $f(..)$ için kullanmak istiyoruz. Çok boyutlu durumda $f(x_1, x_2, \ldots)$'yi $a_1, a_2, \ldots$ etrafında açmak şu şekilde ifade edilebilir:

$$f(x_1, x_2, \ldots) \approx f(a_1, a_2, \ldots) + \frac{\partial f}{\partial x_1}(x_1 - a_1) + \frac{\partial f}{\partial x_2}(x_2 - a_2) + \cdots$$

Buradaki küçük fark, $x$, $t$ etrafında açılım yapmak istememizdir; bir sonraki durum $x + c_i \Delta t$ ve $t + \Delta t$'dir. Bu nedenle yukarıda görülen $x_1 - a_1$ ve $x_2 - a_2$ türü ifadeleri şu şekilde yeniden belirtmemiz gerekir:

- $x + c_i \Delta t - x \to c_i \Delta t$
- $t + \Delta t - t \to \Delta t$

Artık Taylor açılımı şu hale gelir:

$$f(x + c_i \Delta t, t + \Delta t) \approx f_i(x, t) + \frac{\partial f_i}{\partial x} c_i \Delta t + \frac{\partial f_i}{\partial t} \Delta t \tag{6}$$

Ya da $\nabla$ gösterimini kullanarak şunu da söyleyebiliriz:

$$f_i(x + c_i \Delta t, t + \Delta t) = f_i(x, t) + \frac{\partial f_i}{\partial t} \Delta t + \nabla f_i \cdot c_i \Delta t + O(\Delta t^2)$$

Her iki taraftan $f_i(x, t)$'yi çıkarır ve tüm denklemi $\Delta t$'ye böleriz:

$$\frac{f_i(x + c_i \Delta t, t + \Delta t) - f_i(x, t)}{\Delta t} = \frac{\partial f_i}{\partial t} + c_i \cdot \nabla f_i + O(\Delta t)$$

$\Delta t \to 0$ limitini alırsak, yüksek mertebeden $O(\Delta t)$ terimleri yok olur. Geriye şu kalır:

$$= \frac{\partial f_i}{\partial t} + c_i \cdot \nabla f_i$$

Bu form tanıdık geliyor mu? Elbette! Bu, daha önce türettiğimiz Boltzmann taşınım denkleminin kendisidir ve bunun neye eşit olduğunu biliyoruz:

$$\frac{\partial f_i}{\partial t} + c_i \cdot \nabla f_i = \frac{1}{\tau}(f^{eq}_i - f_i) \tag{7}$$

Dolayısıyla (6)'daki sol tarafın ayrıklaştırılması bize iki şey sağladı. Birincisi algoritmanın nasıl ilerlediğini gösterdi, şöyle:

$$f_i(x + c_i \Delta t, t + \Delta t) = f^*_i(x, t)$$

Algoritmik açıdan bu saf bir bellek kopyalama işlemidir. Ardından $i$ yönü için $c_i$ hız vektörüne bakılır, az önce $x$ düğümünde hesaplanan çarpışma sonrası $f^*_i$ değeri alınır ve bir sonraki zaman adımı için tam olarak $x + c_i \Delta t$ konumundaki komşu düğümde $i$ yönüne karşılık gelen bellek yuvasına yazılır.

(6) sol tarafından elde ettiğimiz ikinci kazanım, bunun Boltzmann taşınım denklemine eşit olduğunu görmekti; dolayısıyla (7)'nin sağ tarafı da doğru olacaktır. Çarpışma matematiğini hesaplamak için bu gerçeği kullanabiliriz: her $i$ yönü için düğümün mevcut $\rho$ ve $u$ değerlerini polinom formülüne koyarak $f^{eq}_i$ denge değerini hesaplarız, $f_i$'den $f^{eq}_i$'yi çıkarırız, bunu $\frac{\Delta t}{\tau}$ (gevşeme faktörü) ile çarparız ve elde edilen sonucu orijinal $f_i$'den çıkarırız.























[devam edecek]

Kaynaklar

[1] Bayramli, *Fizik - Sıvı ve Gaz Mekaniği - 2*

[2] Mohamad, *Lattice Boltzmann Method Fundamentals and Engineering Applications with Computer Codes*

[3] Satoh, *Introduction to Practice of Molecular Simulation*

