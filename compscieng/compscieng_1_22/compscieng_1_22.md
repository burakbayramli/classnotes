# Ders 1.22

Basit bir $u$ ile başlayalım, $u = x^2 + y^2 = c$. Bu bize iki boyutta bir
çember veriyor. Bu formülün gradyanını alarak ne öğreniyorum? Bu çembere her
noktadan dışarı dik giden vektörü.

$$
\mathrm{grad} u = v = \left[\begin{array}{ccc}
2x \\ 2y
\end{array}\right]
$$

Şu soruyu soralım, bize iki $v_1$ ve $v_2$ fonksiyonları verilmiş (biraz önceki
örnekte $v_1 = 2x$, $v_2 = 2y$ olurdu) öyle ki

$$
v_1 = \frac{\partial u}{\partial x}
$$

$$
v_2 = \frac{\partial u}{\partial y}
$$

Buradan türevin tersine giderek bir $u$ nasıl elde ederim? Yani $x$ türevi $v_1$
olarak verilmiş, $y$ türevi $v_2$ olarak verilmiş bir $u$'yu bulabilir miyim?
Şansımız pek fazla değil, çünkü iki tane bilinen denklemim, ama bir tane
bilinmeyen değişkenim var. Çoğunlukla bu tür sistemlerde çözüm olmaz, ama bazen
olur. O zaman sonucu bulduğumuza dair tutarlılık testimiz ne olabilir?
Bu bizi kısmı türev bazlı bir eşitliğe götürüyor.

Düşünelim. Üstteki iki denklemi birbiriyle nasıl ilintilendirebilirim? Ki,
matris dilini kullanırsak, "$v_1,v_2$'nin gradyanin kolon uzayında olduğunu
anlayabileyim''. 

Ana fikir şu, $v_1$'in $y$ bazlı $v_2$'nin $x$ bazlı türevini al.

$$
\frac{\partial v_1}{\partial y} = \frac{\partial^2 u}{\partial y \partial x}
$$

$$
\frac{\partial v_2}{\partial x} = \frac{\partial^2 u}{\partial x \partial y}
$$

Fakat türevlerde sırabağımsızlık sebebiyle üstteki iki denklemin sağ tarafları
aynı şeyi söylemiyor mu? Evet. O zaman formüllerin sol tarafları da birbirine
eşit demektir,

$$
\frac{\partial v_1}{\partial y} = \frac{\partial v_2}{\partial x} 
$$

Ya da

$$
\frac{\partial v_1}{\partial y} - \frac{\partial v_2}{\partial x} = 0
\qquad (1)
$$

Bu formül önemli çünkü Vektör Calculus'un temel eşitliği.

Aslında bir anlamda bulduğum elimdeki bir vektörün gradyan olup olmadığının
testi. Eğer $v$ vektörü aynı $u$ bazlı değil ise ikinci türevler birbirine
eşit olmazdı. Mesela elimde bir $[\begin{array}{cc} 3x & 2x \end{array}]^T$
vektörü olsaydı, bunun bir gradyan olmadını bilirdim çünkü ikinci türevler
birbirine eşit değil.

[atlandı]

Testi bulduk. Şimdi "dolam (curl)'' kelimesini kullanmak istiyorum, üsttekine,
$\mathrm{curl} v = 0$ şartı diyebilirim. İtiraz edenleriniz olabilir, "ama dolam üç
boyuttadır'' vs. Daha önce hakikaten üç boyutta gördük bu kavramı, ve sonucun da
üç bileşeni oluyordu. Fakat üsttekine "düzlemde dolam'' diyebiliriz.  Ya da
sanki üç boyuttayız ama $v = [v_1(x,y), v_2(x,y), 0]$ durumu var, $z$ hep sıfır,
ve dolam hesabı bu şartta yapılınca dolamın tüm bileşenlerinden geri kalan
sadece üstte görülen formül olacaktır. Mesela $v_3$'un türevini kullanan kısım
yokolur, çünkü $v_3$ yok.

[atlandı]

(1) formülü bir nokta için geçerli. Bu formülü bir çember, döngü etrafında
nasıl uygularım? Kapalı bir devre etrafında uygulamak istiyorum,

$$
\oint v_1 \mathrm{d} x + v_2 \mathrm{d} y = 0
\qquad (2)
$$
 
Mesela bir hız alanında bir kapalı devre içinde gidiyorum, böyle bir alanda tüm,
toplam sirkülasyon, dönüş sıfır olmalı. (2) de aslında Vektör Calculus'ta bir
eşitlik, (1) ile (2) bağlantılı aslında, biri diğeri sıfır olunca sıfır oluyor..
Stokes Teorisi mesela (2) entegraline (1)'in çift entegral alınmış hali der, vs.

Şimdi uzaklaşım (divergence) konusuna gelelim. Eğer $\nabla \cdot w = 0$
görürsem bu ne demektir? Bir sıvı akışını düşünürsem, bu ifade kaynak yok
demektir. İçeriye ne giriyorsa o dışarı çıkıyor [bir şey eklenmiyor yani, kaynak
yok]. Bu ne kanunu? Bu da bir Kirchoff kanunu aslında, Akım Kanunu [tabii
fizikte pek çok diğer alanda benzer kanunlar var]. "Girenler eşittir çıkanlar''
matematiksel olarak nasıl derim? İşte Uzaklaşım Teorisi'ne şimdi ihtiyacım var.

Her nokta için giren eksi çıkanlar hesabını yapıp tüm bir bölge için topluyorum,
ve geriye kalan tek "çıkış'', o bölgenin sınırından dışarı çıkanlar olur tabii.
Bu doğal tabii, eğer bir net çıkış var ise, ya da giriş, o giriş ya da çıkış o
bölgenin sınırından giriyor ya da çıkıyor olacaktır. O zaman bir bölge üzerinden
entegral, $\iint_{\textrm{bölge}}$, sınırdan olan akış entegrali $\oint$
hesabına eşit olmalı. Eğer $w$ akışı gösteren vektör alanı ise, $s$ sınırından
akana, daha doğrusu akışın sınıra dik olan bileşenini sınır üzerinden toplarım,

$$
\iint_{\textrm{bölge}} \left( 
\frac{\partial w_1}{\partial x} +
\frac{\partial w_1}{\partial y} \right)
\mathrm{d} x \mathrm{d} y =
\oint w \cdot n \mathrm{d} s
$$

İşte iki boyutta Vektör Calculus'un önemli bir eşitliği bu. Bir anlamda
Calculus'un Temel Teorisi gibi bu ama şimdi iki boyuttayız.

Dikkat alan içindeki her nokta bazında giren, çıkan değeri $\frac{\partial w_1}{\partial x} + \frac{\partial w_1}{\partial y}$
ile hesaplanıyor. Bu değerleri tüm alan için entegre ediyoruz, tüm değişimi
buluyoruz. Ama bu hesabı tüm alan yerine sadece sınırlara bakarak ta
yapabiliriz. Eğer bir kaynak yok ise o zaman sınırlar bazında yapılan hesap
alan üzerinden yapılan hesap ile aynı çıkmalı.

Ekler

Uzaklaşım Teorisi tam formu

$$
\iint_R (\mathrm{div} w) \mathrm{d} x \mathrm{d} y = \int_B  ( w \cdot n ) \mathrm{d} s
$$

Uzaklaşım Teorisi hakikaten Calculus'un Temel Teorisinin çok boyutlu karşılığı,
tek boyutta Uzaklaşım Teorisi aynen şöyle olurdu [1, sf. 262],

$$
\int_{a}^{b} \frac{\mathrm{d} w}{\mathrm{d} x} \mathrm{d} x = w(b) - w(a)
$$

Tek boyutta normal vektörü görmüyoruz ama aslında orada. Bitiş noktası
$x=b$'de çıkış yönü $n$ sağa doğru, değil mi? Bu bize $+w(b)$ veriyor.
Başlangıç noktası $x=a$'da $n$ sola doğru işaret ediyor, yani dışarı doğru,
bu bize $-w(a)$ veriyor. İkisini toplayınca $w(b) - w(a)$ elde ediyoruz.

Kaynaklar

[1] Strang, *Computational Science and Engineering*



