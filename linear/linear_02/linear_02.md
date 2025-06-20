# Ders 2

$$ x + 2y + z = 2 $$

$$ 3x + 8y + z = 12 $$

$$ 4y + z = 2 $$

Lineer sistem çözme zamanı geldi. Çözüm için mesela determinantları
kullanmayacağız, bu konu ileride gelecek, ama eliminasyon metotu
kullanacağız. Eliminasyon tüm yazılım paketlerinin kullandığı çözüm
metotudur. Eğer başarıya ulaşırsa (ulaşamayabilir de) o zaman sonucu bulmuş
olur, ki çoğunlukla da başarılı olur. $A$ matrisi iyi huylu ise, başarılı
olur, ki üstteki örneğimiz öyle. Tabii biz eliminasyon uygularken kendimize
bir yandan şunu soracağız - hangi şartlarda bu işlem başarısız olurdu? Bu
soru öğretici olacak. 

Eliminasyon aslında çok basit bir işlemdir, sınıfta olan sizler, bizler
bile bu metotu bulabilirdik. Gauss buldu bu yöntemi tabii, çünkü bizden
daha önce doğmuştu. 

Eliminasyonu tarif ederken bu işlemleri matris operasyonları olarak
göstereceğim [yani tüm matrise etki eden türden cebirsel işlemlerden
bahsediyor hoca], çünkü dersimizdeki tüm temel fikirler, sözler, laflar
olarak değil matris operasyonları olarak gösterilecek. Bir operasyon mesela
bir matrisi çarpmak. 

Üstteki örneğe gelelim: bu sisteme tekabül eden $A$ matrisi, 

$$ 
\left[\begin{array}{rrr}
1 & 2 & 1 \\
3 & 8 & 1 \\
0 & 4 & 1
\end{array}\right]
$$

Artık tüm işlemleri bu matris üzerinde yapabiliriz. İlk baştaki eşitlikler,
artı, vs gibi semboller içeren sembollere bakmıyoruz bile. Eliminasyonun
ihtiyacı olan tek şey artık üstteki $A$ matrisi. 

Eliminasyon ne yapar? Mesela örnek bir işlem, 1. denklemi uygun bir sayıyla
çarpmak, 2. satırdan çıkarmak. Amaç $x$'i dışarı atmak. Böylece denklemi ve
bilinmeyenleri azaltmış olacağız. Çarpan ne olmalı? 

$$ 
\left[\begin{array}{rrr}
(1) & 2 & 1 \\
3 & 8 & 1 \\
0 & 4 & 1
\end{array}\right]
$$

1'inci satırda $x$ için verilen katsayı '1' sayısını kullanacağım. Matrisin
o hücresini işaretledim, onu kullanacağımı belirtmek için. Lineer cebirde
denir ki bu hücreyi 'pivot' olarak seçmiş oldum. Şimdi eliminasyon işlemini
yapalım; pivot satırını 3 ile çarpıp ikinciden çıkartalım.

$$ 
\left[\begin{array}{rrr}
    1 & 2 & 1 \\
    0 & 2 & -2 \\
    0 & 4 & 1
  \end{array}\right]
$$

Peki bu arada eşitliğin sağ tarafına, yani $b$'ye ne oldu? Değişimlerin ona
da yapılması lazım, bazı yazılım paketleri mesela Matlab sağ tarafı
sonradan değiştiriyor, ben de Matlab gibi olayım bari, değişimi sonradan
yapayım. 

Şimdiye kadar ne yaptık? 2,1 kordinatındaki sayıyı "temizlemiş'' olduk,
sıfır haline getirdik. Bundan sonra 3,1 kordinatına gidilecek, ama orası
zaten sıfır halinde; yani işleme gerek yok. 

Kalanlar nedir? Alttaki kısımdır,

$$ 
\left[\begin{array}{rrr}
    \dots & \dots & \dots \\
    \dots &  2 & -2 \\
    \dots & 4 &  1
  \end{array}\right]
$$

İki üstteki matriste tek bir $x$ haricinde tüm $x$'ler yokoldu, tek kalan
1. satırdaki $x$ işe zaten sonuç demektir. Üstteki durumda elimizde sadece
iki tane denklem kalmış gibidir. Bundan sonrası özyineli (recursive) bir
işlem, matris içinde daha ufak bir matris bulduk, oradan daha ufak başka
bir matrise gidebiliriz, bu sırada çözümleri teker teker buluruz.

İkinci pivota ve onun satırına gidelim,

$$ 
\left[\begin{array}{rrr}
    (1) & 2 & 1 \\
    0 & (2) & -2 \\
    0 & 4 &  1
  \end{array}\right]
$$


Bunu yapmamızın amacı 4,2 kordinatındaki '4' sayısını temizlemek. 2. satırı
2 ile çarpalım, 3. satırdan çıkartalım,

$$ 
\left[\begin{array}{rrr}
1 & 2 & 1 \\
0 & 2 & -2 \\
0 & 0 & 5
\end{array}\right]
$$

Bu matrise $U$ adını vereceğim, bu harfi kullandım çünkü üstteki matris bir
"üst-üçgensel (uppertriangular)'' matris. Eliminasyonun tüm amacı bu
matrise ulaşmaktır. Daha genel bağlamda söylemek gerekirse bilimsel
hesaplama dünyasının en temel, en yaygın işlemi üstte gördüğümüz,
yaptığımız bu işlemdir. Pek çok kişinin "bu hesabı nasıl daha hızlı
yaparım'' diye düşünce sarfettiğini, uğraştığını görürsünüz, çünkü çok, çok
gerekli bir iştir. 

İşleme dönelim: pivot seçerken şunu eklemek lazım, sıfır pivot olsaydı o
pivot olarak seçilmezdi. Üstteki durumda çıkmadı ama ortaya çıkabilirdi. 

Bu arada, ileriki derslere biraz atlama yapmak gerekirse, eğer üstteki
matrisin eğer determinantini hesaplamam gerekseydi pivotları alıp
birbiriyle çarpmam yeterli olurdu. 

Şimdi başarısızlık durumuna bakalım; üstteki işlem hangi koşullarda
başarısız olurdu? Eğer 1,1 kordinatını pivot seçtiğim zaman orada bir sıfır
olsaydı mesela.. Ne demiştik? Sıfır pivot olmaz. Peki bu durumda ne
yaparım? Basit bir numara, satır değiş-tokuşu yaparım, mesela en üstteki
satırı hemen altındaki ile değiş-tokuş yaparım, böylece sıfır artık
çaprazda yer almaz, ve baktığım satırdaki pivot artık sıfır olmaz. Not: Bu
arada bir matriste satır değişimi yapmanın denklem sistemi için hiçbir
zararı yoktur, çünkü ne de olsa her satır ayrıdır, her satır ayrı bir
denkleme tekabül eder. Denklemlerin hangi sırada yazıldığı önemli değil.

Fakat bazen kendimi kurtarmam mümkün olmayabilir, mesela eğer matris şu
halde olsaydı, 

$$ 
\left[\begin{array}{rrr}
1 & 2 & 1 \\
3 & 8 & 1 \\
0 & 4 & -4
\end{array}\right]
$$

eliminasyon yapa yapa sona gelirdik, ve çıkarma işleminden sonra $-4$
sıfır olurdu. 

$$ 
\left[\begin{array}{rrr}
    \dots & \dots & \dots \\
    \dots &  2 & -2 \\
    \dots & 4 &  0
  \end{array}\right]
$$

Bu durumda kendimizi kurtaramazdık, çünkü altta değiş-tokuş yapacak satır
bulamazdık. Böylece sıfır pivot durumu ortaya çıkardı, ve bu tersi
alınamayan matris işareti olurdu.

Neyse; biz başarı durumundan devam edelim, artık tüm bilinmeyenleri bulmak
için geride yerine koyma (backsubstitution) yapabiliriz. Matlab ne yapardı?
Biz ne yaparız? Bir teknik, geride yerine koyma işlemine başlamadan önce
$b$'yi alıp $A$'ya eklemlemek,

$$ 
\left[\begin{array}{rrrr}
1 & 2 & 1 & 2\\
3 & 8 & 1 & 12 \\
0 & 4 & 1 & 2
\end{array}\right]
$$

Bu bize eklemlenmiş (augmented) yeni bir matris verir. Böylece $A$
üzerinde yaptığım her işlemi otomatik olarak $b$ üzerinde yapmam
kolaylaşır. Tüm çarpma-çıkarma işlemlerini eklemlenmiş matris üzerinde de
uygularsam,

$$ 
\underbrace{
\left[\begin{array}{rrr}
1 & 2 & 1 \\
0 & 2 & -2 \\
0 & 0 & 5  
\end{array}\right.}_{U}
\underbrace{
\left. \begin{array}{r}
 2\\
 6\\
 -10 
\end{array}\right]}_{c}
$$

Burada $b$'nin dönüştüğü şeyi $c$ olarak belirttim. Yani $A$, $U$ oldu,
$b$ $c$ oldu. Geriye koyma işlemini yapalım şimdi, ondan önce üstteki
matrisin hangi denklem sistemine tekabül ettiğini yazalım,

$$ x + 2y + z = 2 $$

$$ 2y -2z = 6 $$

$$ 5z = -10 $$

Bu denklem sistemini çözmek artık çok kolay değil mi? İlk çözülecek denklem
en sondaki, $5z = -10 \rightarrow z = -2$. Artık $z$'yi biliyorum, onu
ikinci denklemde geride yerine koyarım (ki isim buradan geliyor, çünkü
sondan geriye doğru gidiyoruz), ve $y = 1$ elde ederim, sonra oradan
birinci denkleme atlarım, $x = 2$. 

Bu gördüklerimiz bu dersin ilk kısmıydı. İkinci kısımda  matris bakış
açısını işleyeceğiz. Şimdiye kadar yaptığımız işlemleri matris cebiri ile
nasıl gösterirdik? "Büyük resimden'' bahsediyoruz burada, yani teker teker
satır, hücre ile oynamaktan değil, tüm $A$'yi kullanan cebirsel
operasyonlardan bahsediyoruz. 

Çarpma işlemini hatırlarsak, ne demiştik, matrisi bir vektör ile çarpmak,

$$ 
\left[\begin{array}{rrr}
    \dots & \dots & \dots \\
    \dots &  \dots & \dots \\
    \dots & \dots &  \dots
  \end{array}\right]
\left[\begin{array}{r}
3 \\
4 \\
5
\end{array}\right] = 
3 \times \textrm{ 1. kolon } + 4  \times \textrm{ 2. kolon } + 
5  \times \textrm{ 3. kolon } 
$$

Şimdi benzer bir düşünce şeklini satırlar üzerinde gerçekleştirmek
istiyorum. Niye? Çünkü şimdiye kadar gördüğümüz eliminasyon için gereken
işlemler hep satır işlemleri. Acaba aynı matrisi soldan bir vektör ile
çarpsak ne olur? 

$$ 
\left[\begin{array}{rrr}
1 & 2 & 7
\end{array}\right]
\left[\begin{array}{rrr}
    \dots & \dots & \dots \\
    \dots &  \dots & \dots \\
    \dots & \dots &  \dots
  \end{array}\right]
 = 
1 \times \textrm{ 1. satır } + 2  \times \textrm{ 2. satır } + 
7  \times \textrm{ 3. satır } 
$$

Boyut olarak $1 \times 3 \cdot 3 \times 3 = 1 \times 3$, yani vektör ile
matrisi çarpınca bir vektör elde etmiş olduk, diğer bir deyişle
"satırların lineer kombinasyonunu hesaplamış olduk''.

Eliminasyon için gereken işlemleri, mesela 1. satırı 2. satırdan çıkarmayı
nasıl üstteki gibi bir çarpma operasyonu ile temsil ederim? Mesela ilk
yaptığımız işlem 1. satırı üç ile çarpıp 2. satırdan çıkartmak. Yani

$$ 
\left[\begin{array}{rrr}
 &  &  \\
 &  &  \\
 &  & 
\end{array}\right]
\left[\begin{array}{rrr}
1 & 2 & 1 \\
3 & 8 & 1 \\
0 & 4 & 1
\end{array}\right] =
\left[\begin{array}{rrr}
    1 & 2 & 1 \\
    0 & 2 & -2 \\
    0 & 4 & 1
  \end{array}\right]
$$

soldaki boş matris ne olmalı ki eşitliğin sağ tarafı doğru olsun ?

Boş matrisin ilk satırını düşünelim.. oraya ne gelsin? Eşitliğin sağına
bakalım, 1. satır aslında değişmeden kalıyor. Bu ne demektir? Soldaki
(dolu) matrisin 1. satırını olduğu gibi al demektir, ya da "satır
kombinasyonu'' dilinde belirtmek gerekirse, 1. satırdan bir tane 2. ve
3. satırlardan sıfır tane al demektir. 

$$ 
\left[\begin{array}{rrr}
1 & 0 & 0 \\
 &  &  \\
 &  & 
\end{array}\right]
\left[\begin{array}{rrr}
1 & 2 & 1 \\
3 & 8 & 1 \\
0 & 4 & 1
\end{array}\right] =
\left[\begin{array}{rrr}
    1 & 2 & 1 \\
    0 & 2 & -2 \\
    0 & 4 & 1
  \end{array}\right]
$$

Boş matrisin son satır nedir? Benzer mantık, bu sefer 1. ve 2. satırdan
sıfır tane, 3. satırdan bir tane


$$ 
\left[\begin{array}{rrr}
1 & 0 & 0 \\
 &  &  \\
0 & 0 & 1
\end{array}\right]
\left[\begin{array}{rrr}
1 & 2 & 1 \\
3 & 8 & 1 \\
0 & 4 & 1
\end{array}\right] =
\left[\begin{array}{rrr}
    1 & 2 & 1 \\
    0 & 2 & -2 \\
    0 & 4 & 1
  \end{array}\right]
$$

İlginç bir durum oldu, üstteki boş matris birim matrise (identity matrix)
benzemeye başladı, aslında hiçbir değişiklik yapmak istemeseydim, orta
satırı $\left[\begin{array}{rrr}0 & 1 & 0 \end{array}\right]$ yapardım 
o zaman soldan çarpan matris birim matris olurdu. Birim matrisler basit 
aritmetikteki "1'' sayısının eşdeğeridir, onunla yapılan çarpımın 
hiçbir etkisi yoktur.

Ama biz tabii ki değişiklik yapmak istiyoruz, eşitliğin sağındaki (ve
değişik olan) 2. satıra ulaşmak istiyoruz. 

Peki o zaman ortadaki satır ne olmalı? 1. satırı üçle çarpıp 2. satırdan
çıkartmak ne demektir? 1. satırdan "-3 tane'' alsam ve 2. satırdan "bir
tane'' alsam (3. satırdan hiç), ve birbirine eklesem istediğimi elde
edebilir miyim? Evet. Yani 

$$ 
\left[\begin{array}{rrr}
1 & 0 & 0 \\
-3 & 1 & 0 \\
0 & 0 & 1
\end{array}\right]
\left[\begin{array}{rrr}
1 & 2 & 1 \\
3 & 8 & 1 \\
0 & 4 & 1
\end{array}\right] =
\left[\begin{array}{rrr}
    1 & 2 & 1 \\
    0 & 2 & -2 \\
    0 & 4 & 1
  \end{array}\right]
$$

Kontrol edelim; mesela eşitliğin sağındaki -2, eşitliğin solundaki
1. matrisin 2. satırı ile 2. matrisin 3. kolonunun noktasal çarpımıdır, ve
hakiken bu çarpımı yapınca -2 elde ederiz. Bu arada gördüğünüz gibi
çarpmaya değişik bir bakış daha getirmiş olduk, satırların kombinasyonunu
gördük, kolonların kombinasyonunu gördük, şimdi de tek hücre değerini 
satır ve kolonun noktasal çarpımı olarak görmeyi öğrendik. Bu bakış
açılarının hepsini bilmek ilerisi için faydalı.

Artık elimizde tüm eliminasyon işlemlerini pür matris operayonları olarak
belirtmenin bir yolu var. Üstte eşitliğin solundaki 1. matrise
"eliminasyon matrisi'' ismi de verilir, diyelim ki ona $E$ sembolünü
verdik, ve bu örnekte onu $E_{21}$ olarak belirtmek uygun olabilir, ki bu
sembol eşitliğin sağında 2,1 kordinatında sıfır elde etmek için gereken
matris. 

2'inci adım için gereken $E$ nedir? 

$$ 
\left[\begin{array}{rrr}
 &  &  \\
 &  &  \\
 &  & 
\end{array}\right]
\left[\begin{array}{rrr}
  1 & 2 & 1 \\
  0 & 2 & -2 \\
  0 & 4 & 1
\end{array}\right] 
=
\left[\begin{array}{rrr}
1 & 2 & 1 \\
0 & 2 & -2 \\
0 & 0 & 5
\end{array}\right]
$$

Bu adım için 2. satırı iki ile çarpıp 3. satırdan çıkartmıştık. Değişen
sadece 3. satır olduğuna göre 1. ve 2. satır birim matrisin öğeleri olacak,
sadece 3. satır dolu,

$$ 
\underbrace{
\left[\begin{array}{rrr}
1 & 0 & 0 \\
0 & 1 & 0 \\
0 & -2 & 1
\end{array}\right]
}_{E_{32} }
\left[\begin{array}{rrr}
  1 & 2 & 1 \\
  0 & 2 & -2 \\
  0 & 4 & 1
\end{array}\right] 
=
\left[\begin{array}{rrr}
1 & 2 & 1 \\
0 & 2 & -2 \\
0 & 0 & 5
\end{array}\right]
$$

Eliminasyonunu tamamı için üstteki parçaları biraraya koymak
lazım. $A$'yi soldan önce $E_{21}$ ile, o sonucu da $E_{32}$ ile
çarpıyoruz. Yani

$$ E_{32} (E_{21}A) = U $$

Matris notasyonunu niye çok sevdiğimi anlıyorsunuz herhalde, üstteki ifade
gayet temiz. 

Şimdi lineer cebirde çarpımı operasyonu hakkında ilginç bir şey öğreneceğiz
- üstteki caprim nihai sırası değişemez, fakat "parantezlerin yeri
değişebilir''. Yani $E_{32}$ ile $E_{21}$ matrislerini önce çarpıp, o
sonucu *sonra* $A$ ile çarpabilirim. 

$$ (E_{32} E_{21}) A = U $$

Bunun eliminasyon için faydası şurada, eliminasyon matrislerini birbiriyle
çarpıp tek bir eliminasyon matrisi elde edebilirim. 

Parantezlerin yerinin değişebilmesi durumuna daha matematiksel bir isim
vermek gerekirse buna çağrışımsal kural (associative law) denebilir. 

Bu kural lineer cebirde pek çok kez karşımıza çıkacak. Denebilir ki
alanımızdaki pek çok adım, hatta pek çok gerçek (truth) parantezlerin
yerini değiştirme tekniğiyle ilintilidir. Kuralın doğru olduğunu ispatlamak
ta kolay değildir, matris çarpımının en ince detaylarına inip her iki turlu
çarpımı yapmak ve aynı olduklarını göstermek gerekir. 

Bir eliminasyon matris türü var, onu üstteki örnekte görmedik çünkü
gerekmedi. Ama bazı durumlarda gerekebileceğinden bahsetmiştik, ki bu
matris iki satırın yerini değiştiren bir eliminasyon matrisidir, ki bu
matrislere "permutasyon matrisleri'' ismi verilir. Mesela alttaki işlemi
gerçekleştirecek matris,

$$ 
\left[\begin{array}{rr}
 &  \\
 & 
\end{array}\right]
\left[\begin{array}{rr}
a & b \\
c & d
\end{array}\right]
=
\left[\begin{array}{rr}
c & d \\
a & b 
\end{array}\right]
$$

Düşünelim, 1. satır için mesela, eğer 1. satırdan 0 tane, 2. satırdan
sadece bir tane alırsam ve sonucu birbiri ile "toplarsam'', 

$$ 
\left[\begin{array}{rr}
0 & 1 \\
 & 
\end{array}\right]
\left[\begin{array}{rr}
a & b \\
c & d
\end{array}\right]
=
\left[\begin{array}{rr}
c & d \\
 & 
\end{array}\right]
$$

sonucunu elde etmez miyim? Evet. Geri kalanı da şöyle,

$$ 
\left[\begin{array}{rr}
0 & 1 \\
1 & 0
\end{array}\right]
\left[\begin{array}{rr}
a & b \\
c & d
\end{array}\right]
=
\left[\begin{array}{rr}
c & d \\
a & b
\end{array}\right]
$$

Burada 1. satırdan bir tane ikincidan sıfır tane almış oldum.

İşte soldan çarpan şey bir permutasyon matrisidir. Faydalı olabilecek güzel
bir numara bu arada; herhangi bir permutasyon matrisini elde etmenin en
kolay yolu bir birim matrisini almak ve olmasını istediğimiz değişimi onun
üzerinde yapmaktır, ve elde edilen yeni matris istediğimiz permutasyon
matrisi olacaktır.

Peki ya bir matrisin kolonlarının değiştirmek isteseydim ne yapardım?
Eliminasyon için bu gerekli değil ama zihin egzersizi olarak
soruyorum. Ondan önce bir diğer soru, bu matris ana matrisi soldan mı,
sağdan mı çarpmalı?

Cevap, sağdan. Çünkü şimdi bize "kolonların kombinasyonu'' lazım, ki
kolonlar üzerinde "bir tane ondan sıfır tane bundan'' diyebilelim, ve bunu
çarpımda yapmanın tek yolu bir matrisi sağdan çarpmaktır, çünkü kolonların
kombinasyonunu sağdan çarpım verir. 

$$ 
\left[\begin{array}{rr}
a & b \\
c & d
\end{array}\right]
\left[\begin{array}{rr}
 &  \\
 & 
\end{array}\right]
=
\left[\begin{array}{rr}
b & a \\
d & c
\end{array}\right]
$$

Yine aynı şekilde, 1. kolon için soldaki 1. kolondan sıfır tane,
2. kolondan bir tane lazımdır, 

$$ 
\left[\begin{array}{rr}
a & b \\
c & d
\end{array}\right]
\left[\begin{array}{rr}
0 &  \\
1 & 
\end{array}\right]
=
\left[\begin{array}{rr}
b &  \\
d & 
\end{array}\right]
$$

Değil mi? Tamamlarsak,

$$ 
\left[\begin{array}{rr}
a & b \\
c & d
\end{array}\right]
\left[\begin{array}{rr}
0 & 1 \\
1 & 0
\end{array}\right]
=
\left[\begin{array}{rr}
b & a \\
d & c
\end{array}\right]
$$

Bu da birim matrisin *kolonlarının* yer değiştirmiş halidir. 

Neyse, satır işlemlerine dönersek, eliminasyon matrislerini elde ediyoruz,
ve biraraya koyuyoruz. Fakat matris çarpımı hakkında önemli bir not
düşelim, parantezin yeri değişebilir ama çarpım sırası değişemez,

$$ A \cdot B \ne B \cdot A $$

Terimsel olarak sırabağımlılık (commutative) kanun geçerli değil
(çağrışımsal kanun geçerli).

Pekala, eliminasyon matrislerini elde ettim, ve onları çarparak tek bir $E$
matrisi elde edebilirim, vs. Fakat bunu yapmak istemiyorum. Aslında ters
yönde gitmek istiyorum, yani $A$'dan $U$ ya gitmek yerine, $U$'dan $A$'ya
gitmek istiyorum. Bunun için bana $E$'nin tersi lazım. Şimdiye kadar
tahtada gördüğünüz tüm matrislerin tersi var bu arada, yani iyi huylu
matris hepsi.

Ters Alma Operasyonu

Üstteki eliminasyon matrislerinden birini hatırlayalım, 

$$ E_{21} = \left[\begin{array}{rrr}
1 & 0 & 0 \\
-3 & 1 & 0 \\
0 & 0 & 1
\end{array}\right] $$

Öyle bir matris istiyorum ki bu matrisi nötralize etsin. Yani onun etkisini
yoketsin. Hangi matris bunu başarır? Hangi matris, eliminasyon matrisini
soldan çarpınca bana birim matrisini verir (öyle ya nötralize etmek bu
demektir, birim matrisi haline getirmek). 

$$ 
 \left[\begin{array}{rrr}
 &  &  \\
 &  &  \\
 &  & 
\end{array}\right] 
 \left[\begin{array}{rrr}
1 & 0 & 0 \\
-3 & 1 & 0 \\
0 & 0 & 1
\end{array}\right] 
=
 \left[\begin{array}{rrr}
1 & 0 & 0 \\
0 & 1 & 0 \\
0 & 0 & 1
\end{array}\right] 
$$

Bunun için eliminasyon matrisin ne yaptığını hatırlayalım; 1. satırı 3 ile
çarpıp 2. satırdan çıkartıyordu. Bu işlemin tersi nedir? 1. satırı 3 ile
çarpıp toplamak! Bunu yapınca bir önceki işlemi nötralize etmiş oluruz
değil mi? Yani,

$$ 
 \left[\begin{array}{rrr}
1 & 0 & 0 \\
3 & 1 & 0 \\
0 & 0 & 1
\end{array}\right] 
 \left[\begin{array}{rrr}
1 & 0 & 0 \\
-3 & 1 & 0 \\
0 & 0 & 1
\end{array}\right] 
=
 \left[\begin{array}{rrr}
1 & 0 & 0 \\
0 & 1 & 0 \\
0 & 0 & 1
\end{array}\right] 
$$

Notasyonel olarak $E^{-1}$ kullanılır, yani üstteki ifade $E^{-1}E = I$ olur.







