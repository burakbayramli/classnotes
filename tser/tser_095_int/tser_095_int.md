# Faiz

Faiz, borç alınan para miktarı için ödenen "borç alma bedelidir''. En basit
senaryoda eğer yıllık yüzde 10 üzerinden 1000 lira borç almışsam, 1 yıl sonra
hem 1000 lirayı, hem de 1000 x 0.10 = 100 lira faiz ödemem gerekir. Eğer bu
ödemeyi aylara bölüştürmem gerekirse her ay 100 / 12 = 8.33 ödemem gerekir,
yüzdeler üzerinden düşünürsem yüzde 10 / 12 = 0.83, yani aylık faiz ödemesi
yüzde 0.833. Bu basit durum, bu sebeple faiz direk ay sayısına bölünebilir.

Daha matematiksel olarak ana para $P$ borç aldım, $T$ süre sonrası basit faiz
$r$ ile ödeme yapılacak, $T$ anında geri ödenen para,

$$ P + rP = P(1+r) $$

Biraz daha farklı bir senaryo: diyelim ki $P$ borç aldım, bir sene sonra
ödenecek, faiz yıllık $r$ - fakat faiz yılda iki kere (semi-annually) biriken
faiz (compound interest) olarak hesaplanacak.

Biriken faiz durumunda yılın ortasına kadar basit faiz $r/2$ vardır, fakat bu
faiz ana paraya eklenir, ve bu eklenmiş para üzerinde yılın diğer yarısı için
tekrar $r/2$ faiz uygulanır. Yani ilk altı ay sonrası

$$ P(1+r/2) $$

borçlanmışızdır, yıl sonunda ise tüm borç

$$ P(1+r/2)(1+r/2) = P(1+r/2)^2$$

Soru

Yıllık \%8'den birikmesi çeyreksel (dört ayda bir) hesaplanan faiz ile 1000 lira
borç aldım. Yıl sonunda ne kadar geri öderim?

Cevap

Yıllık \%8'den birikmesi çeyreksel hesaplanan faiz demek her çeyrekte yüzde 2
basit faizin ana para üzerine her çeyrekte eklenmesi demektir, bir çeyrek
sonrası borç

$$ 1000(1+0.02) $$

iki çeyrek sonrası

$$ 1000(1+0.02)(1+0.02) = 1000(1+0.02)^2 $$

üç çeyrek sonrası

$$ 1000(1+0.02)(1+0.02)(1+0.02) = 1000(1+0.02)^2 $$

Dört çeyrek sonrası, yani yıl sonunda geri ödenecek borç

$$ 1000(1+0.02)(1+0.02)(1+0.02)(1+0.02) = 1000(1+0.02)^4 = 1082.40$$

Soru

Pek çok kredi kart şirketi birikmesi aylık yapılan yıllık yüzde 18 faiz oranı
üzerinden kart sahiplerine borç verir (daha doğrusu kart üzerindeki ödenmemiş
miktara uygulanan faiz budur). Eğer yıl başında borç verilen miktar $P$ ise ve
arada hiç ödeme yapılmamışsa, borçlanılmış mıktar nedir?

Cevap

Bu tür bir birikim hesabı aylık basit faiz, aylık basit faiz 18/12=\%1.5'in her
ay ana para üzerine eklenmesi demektir, bu durumda bir sene sonra

$$ P(1+0.015)^{12} = 1.1956 P $$

olacaktır. Bu örneklere bakınca görüyoruz ki aktif olarak üzerinden geri ödeme
yaptığımız faiz, yıllık basit faiz $r$'den daha yüksek. Tabii ki bunun sebebi
birikme hesabı sırasında her periyotta daha önceki periyotlarda üzerine faiz
eklenme yapılmış ana para üzerinde bir daha faiz hesabı yapıyor olmamız. Bu tür
durumlarda $r$'ye nominal faiz adı verilir, ve efektif faiz $r_{eff}$ ise

$$ r_{eff} = \frac{\textrm{yıl sonunda geri ödenen miktar}-P}{P}$$

Yıl sonu ödemenin $P(1+r)(1+r)(...)$ olduğunu hatırlarsak,

$$
r_{eff}
= \frac{P(1+r)(1+r)... - P}{P}
= (1+r)^p-1
$$

ki $p$ periyot sayısıdır (4 tane çeyrek, 12 tane ay, vs). Örnek, eğer nominal
faiz $r$ ise birikme çeyreksel yapılıyor ise, yıl sonu efektif faizi,

$$ r_{eff} = (1+r/4)^4 -1  $$

İlk örnekteki durumda efektif faiz yüzde

```python
eff = (1+0.08/4)**4 - 1 
print (eff * 100.)
```

```
8.243216
```

İkinci örnekte ise yüzde

```python
eff = (1+0.18/12)**12 - 1 
print (eff * 100.)
```

```
19.5618171462
```

Yani alttaki doğru olduğuna göre,

$$ P(1+r_{eff}) = \textrm{yıl sonunda geri ödenen miktar} $$

$r_{eff}$'in hesabında kullanılan birikim faizi yerine tek dönemlik $r_{eff}$
basit faizi kullanmak eşdeğerdir.

Sürekli (Continuous) Biriken Faiz

Nominal faiz $r$ ile borç aldık diyelim ama faiz *sürekli* şekilde birikerek
hesaplanıyor. Yıl sonu ne kadar borcumuz var? Bu soruya cevap verebilmek için
"süreklilik'' için uygun bir tanım bulmamız lazım. Hatırlayalım, eğer borç
ödeme süresi içinde $n$ eşit periyot üzerinden biriktiriliyorsa, nihai ödenecek
borç $P(1+r/n)^n$ idi. Bu durumda süreklilik, $n$'nin gittikçe büyümesi
olabilir, yani aynı süre içine sığdırılan periyotlar sonsuz çoğalır, tabii bu
periyotların süresi sonsuz küçülür, yani bu bir limit hesabıdır,

$$ P \lim_{n \to \infty} (1+r/n)^n = Pe^r $$

$e^r$'nin nasıl limit ile ifade edildiğini görmek için bkz [3]. 

Soru

Eğer bir banka nominal oran yüzde 5 sürekli birikimli faiz oranı ile borç
veriyorsa, yıl sonu için efektif faiz oranı nedir?

Cevap

$$ r_{eff} = \frac{P\cdot e^{0.05} - P}{P} = e^{0.05} - 1 \approx 0.05127  $$

Yani efektif faiz yılda \%5.127. 

Eğer $t$ seneliğine borç aldıysak ve yıllık nominal faiz $r$ ise $t$ süresi
sonunda $Pe^{rt}$ borçlanmışız demektir, çünkü süre sonunda etkili olan nominal
faiz $rt$'dir, yani

$$ P \lim_{n \to \infty} (1 + rt / n)^n = Pe^{rt}
\qquad (1)
$$

İkiye Katlama Kuralı (The Doubling Rule)

Soru

Eğer paramızı birikimi yıllık hesaplanan ve $r$ faizi ile kazandıran bir banka
hesabına yatırdıysak, paramızı ikiye katlamak ne kadar zaman alır?

Cevap

İlk başta verdiğimiz $D$ parası $n$ sene sonrası $D(1+r)^n$ olacağına göre,
aradığımız öyle bir $n$ senesi ki alttaki denklem doğru olsun,

$$ (1+r)^n = 2 $$

Yani $D=1$ kullanmış olduk, 1'in 2 olduğu duruma bakıyoruz. O zaman, üstteki
denklemin sol tarafında, $r$'yi hem $n$ ile çarpıp bölersek (yani hiçbir etki
olmayan bir operasyon, ama bir forma ulaşmaya uğraşıyoruz),

$$ (1+r)^n = \bigg( 1 + \frac{nr}{n} \bigg)^n  $$

$$ \approx e^{nr} $$

ki geçişi (1)'deki denklem ile yaptık. Bu yaklaşıksal hesap eğer $n$ çok ufak
değilse yapılabilir. Aradığımız ikiye katlanma durumu idi,

$$ e^{nr} = 2 $$

İki tarafın log'unu alalım,

$$ nr = \log 2 $$

$$ n \approx \frac{\log 2}{r} = \frac{0.693}{r} $$

Eğer $r = 0.01$ ise, yani yüzde 1 ise, paramızın ikiye katlanması için $\approx
0.7 / 0.01 = 70$ sene geçmesi gerekir. 

Bugünkü Değer Analizi (Present Value Analysis)

Bir para miktarının bugünkü değerini $PV$ ve gelecekteki değerini $C$ olarak
gösterirsek, birikimli faiz hesabı üzerinden

$$ C = PV (1+r)^i $$

olduğunu görmüştük. Eğer gelecekteki bir miktarı zamanı geriye sararak bugünkü
değerini görmek istesek, 

$$ PV = \frac{C}{ (1+r)^i}  $$

Ya da

$$ PV = C (1+r)^{-i}  $$

Bugünkü Değer analizi bize farklı nakit / gelir akışlarına (income stream)
bakarak hangisinin daha tercih edilir olduğunu bulmamızda yardımcı olabilir.

Soru

Diyelim ki 5 sene boyunca size sene sonu belli miktarda ödemeler yapılacak, ve
ödemeler şöyle (birim 1000 lira)

A: 12, 14, 16, 18, 20

B: 16, 16, 15, 15, 15

C: 20, 16, 14, 12, 10

Eğer faiz oranı yıllık biriken nominal yüzde 2 ise, herhangi bir nakit akışı
için değer

$$ \sum_{i=1}^{5} (1+r)^{-i} x_i$$

ile hesaplanır. Ödeme zincirindeki her birimi bugüne getiriyoruz, ve bugüne
uyarlanmış bu ödemeleri topluyoruz. En yüksek olan miktar en tercih edilir
olur. 

```python
a = np.array([12, 14, 16, 18, 20])
b = np.array([16, 16, 15, 15, 15])
c = np.array([20, 16, 14, 12, 10])
```


```python
def pv(x,r):
    res = 0
    for i in range(len(x)):
        res += x[i]*(1+r)**(-(i+1))
    return res

print ('      A                 B           C')
r = 0.1; print (r, pv(a,r), pv(b,r), pv(c,r))
r = 0.2; print (r, pv(a,r), pv(b,r), pv(c,r))
r = 0.3; print (r, pv(a,r), pv(b,r), pv(c,r))
```

```
      A                 B           C
0.1 59.2130443152 58.5973387312 56.3287405853
0.2 45.6995884774 46.3869598765 45.6854423868
0.3 36.4863328961 37.8944930284 38.119221208
```

Sonuçlara bakınca ufak $r$'ler için A'nın daha tercih edilir olduğu
görülüyor. Ama biraz daha büyük $r$'ler için, mesela üstte $r=0.2$ için, B daha
iyi. Daha da büyük $r$'ler için C tercih edilir.

Soru

Bir şirketin 5 sene boyunca bir tür iş makinasına ihtiyacı var. Ellerinde böyle
(eski) bir makina var, makinanın şu anki değeri 6000 lira ki önümüzdeki 3 sene
içinde bu makina her sene 2000 lira değer kaybedecek, ve bu süre sonunda hiçbir
değeri kalmayacak, kullanılamayacak. Bu makinanın sene başı itibariyle yıllık
işletme bedeli ise 9000 lira, ve bu bedel her sene 2000 lira artıyor; makina
eskidikçe onu işletmek daha pahalıya mal oluyor.

22000 liraya yeni bir makina, herhangi bir sene başında, alınabilir. Yeni
makinanın ömrü 6 senedir ve değeri, alındığı ilk iki sene içinde her sene 3000
lira, ardından her takip eden sene içinde yılda 4000 lira azalacaktır. Yeni
makinanın işletme bedeli alındığı ilk senede 6000 lira, takip eden her sene 1000
lira artacak şekildedir. Eğer faiz oranı yüzde 10 ise, şirket yeni makinayı ne
zaman almalıdır?

Cevap

Bu sorunun cevabı için farklı nakit akışı senaryolarına bakmak lazım. Senaryolar
şirketin yeni bir makinayı hangi sene başında alacağına göre değişir. Nakit
akışından artı olan akış yapılan bir ödemeyi, eksi miktarlar ele geçen miktarı
temsil edebilir; daha sonra her nakit akışı üzerinde Bugünkü Değer hesabı (BDH)
yapılarak tüm senaryolar tek bir sayıya indirgenir, ve bu sayılar arasında en az
olan seçilir (en az ödeme). Ödemeler yeni makina almak, ya da makina işletimi
için ödenen para, kazançlar ise eldeki makinanın satımından elde eden gelir
olacaktır. Tabii yeni makinanın alım senesi eskisinin ne zaman satıldığını
belirler, ayrıca yeni makina da 5 sene sonra satılacaktır.  Senaryolar şöyle
(sene başı olacak şekilde, ve her rakam 1000 lira biriminden),

1. Sene: 22, 7, 8, 9, 10, -4

2. Sene: 9, 24, 7, 8, 9, -8

3. Sene: 9, 11, 26, 7, 8, -12

4. Sene: 9, 11, 13, 28, 7, -16

Eğer yeni makina alımı 1. sene başında olsaydı 22K (K=bin lira) makina için
ödenirdi, bu dışarı doğru artı akis, 6K işletme masrafı var, ama eski makina
satılıyor, bu -6K, sonuç 22K. 2. sene 7K, bu işletme masrafı. Ardından gide gide
6. sene başına geliyoruz, burada 5 sene geçmiş, yeni makinanın değeri azalmış,
onu satınca ele geçen miktar $22 - 3K \cdot 2 + 4K \cdot 3 = 4$, yani nakit
akışı dilinde -4.

3. sene başı yeni makina alım senaryosu için, 1. sene 9K eski makina işletme
masrafı, 2. sene 11K işletim masrafı, 3. sene yeni makina alınıyor ve 22K
ödeniyor, 6K işletim masrafı var, eski makina satılıyor -2K, sonuç 26K. 4. sene
için 7K işletim, 5. sene 8K işletim, 6. sene 3 yaşındaki yeni makina satılıyor,
-12K. Diğer nakit akışları benzer şekilde gösterilebilir.

Eğer faiz oranı 0.10 ise, mesela 1. sene nakit akışının BDH'i 

$$
22 + \frac{7}{1.1} + \frac{8}{(1.1)^2} + \frac{9}{(1.1)^3} +
\frac{10}{(1.1)^4} - \frac{4}{(1.1)^5}  = 46.083
$$

Diğer akışlar benzer şekilde hesaplanır, ve tüm BDH'ler,

46.083, 43.794, 43.760, 45.627

Bu akışların içinde en düşük olanı 43.760, yani 3. sene başı yeni makina
alımıdır. O zaman şirket 2 sene sonra yeni makinayı almalıdır. 

Soru

20 sene sonra emekli olmayı düşünen bir kişi önündeki her 240 ayın (20*12=240)
başında $A$ miktarını bankaya yatıracak. Emekli olduktan sonra her 360 ayın
başında para çekecek. Yıllık nominal faiz oranı yüzde 6 üzerinden, ve aylık
biriken faiz hesabı kullanılarak, bu kişinin 360 ay boyunca para çekebilmesi
için $A$'nin ne kadar büyük olması gerekir? 

Cevap

Aylık faiz $r = 0.06 / 12 = 0.005$ olur. Bir $\beta = \frac{1}{1+r}$
tanımlayalım, o zaman kişinin bankaya yatırıdığı tüm paranın BDH'si

$$
A + A\beta + A\beta^2 + ... + A\beta^{239} =
A \frac{1-\beta^{240}}{1-\beta}
$$

Üstte kullanılan eşitlik [2] bölümünde gösterildi. Şu eşitlik,

$$ 1 + b + b^2 + ... + b^n = \frac{1-b^{n+1}}{1-b} $$

Soruya dönelim, 360 boyunca her ay çekilen paranın $W$ olduğunu farz edelim, tüm
bu para alımlarının BDH'si,

$$
W\beta^{240} + W\beta^{241} + ... +  W\beta^{599} =
W\beta^{240} \frac{1-\beta^{360}}{1-\beta}
$$

Üstelin 240'tan başladığına dikkat, çünkü para çekimi  240 ay sonra başlıyor,
yani o zaman yapılan çekimleri yatırmaların yapıldığı zamanlar üzerinen getirip
bugüne uyarlamak gerekiyor. $A$'yi bulmak için bir üstteki ile iki üstteki
denklemin birbirine eşit olması gerekir, çünkü yatırılan paralar ile emeklilik
finanse edilecek. 

$$
A \frac{1-\beta^{240}}{1-\beta} =
W\beta^{240} \frac{1-\beta^{360}}{1-\beta}$$

Bilinenler $W=1000$, ve $\beta = 1/1.005$, yerine koyarsak $A=360.99$
buluruz. Yani 24 sene boyunca her ay hesabına 361 lira koymak bu kişinin ileride
30 sene boyunca 1000 lira çekebilmesini sağlar. 

Soru

Ev almak istiyoruz ve 100,000 liraya ihtiyaç var. Ödemeyi emlak kredisi
(mortgate) ile yapacağız, bir bankaya gittik, banka 100K borcu verecek, şartları
şöyle: borcun geri ödemesi 15 sene süresinde aylık, yüzde 0.6 faiz oranı
üzerinden yapılacak. Banka kırtasiye işlemler için 600 lira, evin kontrolü için
400 lira, ve 1 "nokta (point)'' istiyor, ki burada 1 nokta tüm borcun yüzde
1'ine tekabül ediyor, bu miktar da bankaya işin başında ödenmeli.

Cevap

Öncelikle her ay ne kadar ödeyeceğimizi hesaplayalım. Bu miktara $A$ dersek,

$$ A [\alpha + \alpha^2 + ... + \alpha^{180}] = 100,000 $$

ki $\alpha = 1/1.006$. Burada yüzde 0.6 aylık biriken faiz demek ki. O zaman

$$ A = \frac{100,000(1-\alpha)}{\alpha (1-\alpha^{180})} = 910.05 $$

```python
n = 15 * 12
alpha = 1/1.006
P1 = 100000
A1 =  P1*(1-alpha) / (alpha*(1-alpha**n))
print ('A=',A1)
```

```
A= 910.046739324
```

Efektif yıllık faiz yüzde

```python
print (100 * ((1+0.006)**12 - 1))
```

```
7.44241677219
```

Fakat bir pürüz var; Eğer hakikaten elimize 100,000 geçseydi, hesap bu
olurdu. Fakat ilk başta 600 lira, kontrol için 400 lira, yüzde 1 ödeme derken,
yani 1000 ile 600+400+1000 = 2000 lira ödemeyi başta yaptığımız için aslında
elimize geçen 100,000-2,000 = 98,000 liradır. Aynı hesabı

```python
P2 = P1 - 2000
A2 =  P2*(1-alpha) / (alpha*(1-alpha**n))
print ('Yeni A=', A2)
```

```
Yeni A= 891.845804538
```

Fakat [1, sf. 58]'de ilginç bir başka ek soru soruluyor. Diyelim ki banka
bize $r=0.006$ verdi, fakat aylık ödeme olarak daha önce bulunan $A=910$ liralık
ödemeyi istedi. O zaman bize verilen $r$ gerçek $r$ değil. Gerçek $r$ nedir?
Bunun için $\alpha$ yerine $\beta=1/1+r$ kullanalım, ve

$$ A [\beta + \beta^2 + ... + \beta^{180}] = 98,000 $$

$$ \frac{\beta (1-\beta^{180})}{(1-\beta)} = 98,000 / 910 = 107.69$$

Bölümde 1. $A$'nin kullanıldığına dikkat. Şimdi, amaç $r$'yi bulmak olduğuna
göre, biraz yeniden düzenleme ile $r = \frac{1-\beta}{\beta}$ diyerek 

$$ \frac{1 - (\frac{1}{1+r})^{180}}{r} = 107.69 $$

Amaç $r$'yi bulmak, daha çetrefil çözüm rutinleri kullanabilirdik burada, ama
nasıl olsa $r > 0.006$ olduğunu biliyoruz, ve biraz deneme yanılma ile hedef
sayıya erişince eldeki $r$'ye bakarız, ve sonucu bulmuş oluruz. 

```python
print ('Hedef', P2 / A1, '\n')
func = lambda r : (1 - (1/(1+r))**180 ) / r
r = 0.006; print (func(r))
r = 0.0062; print (func(r))
r = 0.00625; print (func(r))
r = 0.00628; print (func(r))
```

```
Hedef 107.686776695 

109.884466016
108.271268844
107.873426844
107.63575696
```

Bu $r$ için efektik yıllık faiz

```python
print (100 * ((1+r)**12 - 1))
```

```
7.80182002033
```

Enflasyon Etkisini Çıkartmak

Yıla göre fiyatları içeren bir zaman serisinden enflasyon etkisi nasıl
çıkartılır? Diyelim ki enflasyon verisi de aynı zaman serisine endeksli
olarak bir yüzde olarak verilmiş. İnternet'te arama yapınca bu işi yapacak
o tek satırlık (öne-liner) kodu bir türlü bulamadık; CPI gibi bambaşka bir
sayıdan bahsediliyor, saçma sapan konulara dalınmış. Bu işlem basit bir şey
olmalıydı, çünkü enflasyon bir yüzde olarak normal fiyata bir şey ekler,
çıkartmak bu kadar zor olmamalı.

Bu işlemde dikkat edilmesi gereken bir pürüz şudur: enflasyon etkisi sadece
baz fiyata değil, "bir önceki senedeki enflasyonun büyüttüğü eki de"
büyütür. Önceki sene 100 olan yüzde 10 büyüme ile 110 olur, yani sonraki
sene yüzde 10 ile 100'ün değil 110'un yüzde 10'u eklenir, ve 121 elde
edilir. Bu sebeple enflasyon etkisini çıkartacak kod bunu hesaba katmalı.

İş başa düştü, bu çıkartma işini kodlamak için basit bir örnekle
başladık. Diyelim ki baz fiyat, ve enflasyon hiç değişmiyor,

$$
\begin{array}{cccc}
Sene & Fiyat & Enflasyon & Fiyat Formülü \\
\hline
1 & 100 & 10 & ... \\
2 &   110  &   10   & 100 \cdot (1+0.1) \\
3 &   121   &  10   & 100 \cdot (1+0.1) \cdot (1+0.1) \\
4 &   133.1 &  10   & 100 \cdot (1+0.1) \cdot (1+0.1) \cdot (1+0.1) 
\end{array}
$$

3. kolon fiyatın formülsel olarak belirtilmiş halidir. Bu örneği kullanarak
fiyatların ve enflasyonun değişken olduğu durumu halletmek te mümkün olur;
tek yapılması gereken 100 ile çarpılan fiyatı fiyat kolonundan almaya
başlamak.

Ama ondan önce çıkartma formülü.

Mesela 3. sene $100 \cdot (1+0.1) \cdot (1+0.1)$, bu formül bir önceki
senenin fiyatını alıyor, ki bu fiyatın formülü $100 \cdot (1+0.1)$, böylece
artışın artışını da hesaba almış oluyor. Şimdi formülleri içeren tüm
kolonlara bakınca kalıp ortaya çıkıyor (matematik kalıpların dilidir
-language of patterns-); bu kalıp nedir? $(1+01)$'in her sene artan bir
şekilde başlangıç fiyatı 100 ile çarpılıyor olmasıdır. Eğer her sene
enflasyon değişik ise, o zaman her satırda $(1+enflasyon/100)$ ile çarpım
yapıyoruz, ve bu çarpımlar bir önceki çarpıma eklenerek büyüyorlar. O zaman
her sene enflasyonun etkisini çıkartmak için bu çarpım zincirinin ne
olduğunu bulmak ve her sene fiyatını bu çarpıma bölmek yeterli!

Eğer her sene enflasyon aynı olsaydı, mesela yüzde 10, o zaman
$(1+10/100)$'un gerekli katlarını alarak bu bölümü yapabilirdik. Değişik
olunca Pandas ile bir takla atarak istediğimizi elde ederiz; her satırda
bir önceki tüm kolonların 1 ile toplanmış değerlerini çarpmak lazım, bunun
için `cumprod()` işlevi kullanılır.

```python
import pandas as pd
s="""sene;fiyat;enflasyon
1;100;
2;110;10   
3;121;10   
4;133.1;10"""
from io import StringIO
df = pd.read_csv(StringIO(s),sep=';').fillna(0)
print (df)
```

```
   sene  fiyat  enflasyon
0     1  100.0          0
1     2  110.0         10
2     3  121.0         10
3     4  133.1         10
```

1 ile toplanmış yüzdelerin çarpım kümülatifi

```python
df['enfkumulatif']  = (1+df['enflasyon']/100.).cumprod()
```

Etkiyi çıkartmak için

```python
df['reel']  = df['fiyat'] / df['enfkumulatif']
print (df)
```

```
   sene  fiyat  enflasyon  enfkumulatif  reel
0     1  100.0          0         1.000   100
1     2  110.0         10         1.100   100
2     3  121.0         10         1.210   100
3     4  133.1         10         1.331   100
```

Görüldüğü gibi her satırda reel fiyat 100 liraya dönüş yaptı. Reel fiyat
hep aynı çünkü başka hiçbir artışı gözönüne almadık (verimiz o şekilde
çünkü). Eğer örneğimizde fiyat kolonu mesela petrol fiyatları olsaydı, bu
fiyatlar bir de piyasa sebebiyle ortaya çıkan artışları da içerecekti, ve
enflasyon etkisini çıkartınca elimize gerçek fiyat artışları (ya da
düşüşleri) geçecekti.

Yıllık Faizi Günlük Faize Çevirmek

Belli bir periyotta olan faizin etkisini bulmak için, $p$ periyot ve $\beta$
faiz diyelim, anaparayı $(1+\beta)^p$ ile çarparız. Acaba geriye nasıl gideriz?
Ya da büyük bir periyot faizinden daha ufak bir periyota geçiş yapabilir miyiz?
Mesela yıllık enflasyonu günlük enflasyona (sabit olduğunu farz edelim) çevirmek
için ne yapılır? Yıllık enflasyon $y$ günlük $r$ olsun, $p$ tane periyot olacak,
ki bu örnekte 365,

$$
(1+r)^p = (1+y) 
$$

$$
 1+r = (1+y)^{\frac{1}{p}} 
$$

$$
 1+r = (1+y)^{\frac{1}{p}} 
$$ 

$$
r =  (1+y)^{\frac{1}{p}} -1
$$

Örnek olarak yıllık faiz 3.2 olsun, günlük artış yüzde kaçtır?

```python
d = 3.2 / 100.
r = (((1+d)**(1/365.))-1.0)
print (("%0.6f" % (r * 100)))
```

```
0.008630
```

Kaynaklar

[1] Ross, *An Elementary Introduction to Mathematical Finance*

[2] Bayramlı, Diferansiyel Denklemler, *Seriler*

[3] Bayramlı, Diferansiyel Denklemler, *Bir Limit Olarak $e$*

[4] Question, Stackoverflow, *If the annual inflation rate is 3.2 perc, how can I calculate the daily inflation rate??*,
    [https://math.stackexchange.com/questions/2029960/if-the-annual-inflation-rate-is-3-2-how-can-i-calculate-the-daily-inflation-ra](https://math.stackexchange.com/questions/2029960/if-the-annual-inflation-rate-is-3-2-how-can-i-calculate-the-daily-inflation-ra)

  

