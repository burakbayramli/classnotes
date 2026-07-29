# Para, Enflasyon, Kurlar

Çok ticari açık veren, dışa bağımlı, sürekli kriz yaşayan ve
hiperenflasyon problemi yaşayan ülkelerdeki makroekonomik değişkenler
arasındaki ilişki nedir? Daha önemlisi değişkenler arasında ne tür bir
sebep - sonuç ilişkisi vardır, yani hangi parametredeki değişim bir
diğerinde değişime *sebep* olur (tam tersi doğru olmayabilir).

Klasik istatistik yöntemlerden Granger istatiği nedensellik (causal)
sorularına cevap verebilir. Çok boyutlu zaman serileri için kendisiyle
regresyon olan bir sistemi VAR (vector autoregression) ile analiz
edebiliriz. Fakat her iki yaklaşımda da problemi bir şablona sokmak
gerekecek.

Bayes, ve Olasılıksal Programlama teknikleri ile istediğimiz her türlü
ilişkiyi ileri yönde kodlayıp, bilinmeyen değişkenleri bulmak için
veri üzerinden simülasyon yapabiliriz. Bayes yaklaşımı bize bilinmeyen
değişkenlere, onlara özel, bir dağılım tipi atamamızı sağlar, ve önsel
dağılımlar ile parametrelerin arandığı uzayı daraltmak mümkündür.

Önsel Dağılımlar

$$
\alpha_i \sim \mathcal{N}(0, 1^2) \quad \text{ki } i \in \{\text{xch}, \text{ir}, \text{inf}\}
$$

$$
\gamma_{\text{path}} \sim \mathcal{N}(0, 1^2)
$$

$$
\beta_i \sim \mathcal{N}(0.5, 0.5^2) \quad \text{1-gecikmeli kendisiyle regresyon terimleri}
$$

$$
\sigma_i \sim \text{Half-Normal}(\sigma = 1) \quad \text{yapısal hata ölçeği}
$$

Olurluk Fonksiyonları (Gaussian Olurlukları)

$$
\text{Exch}_t \sim \mathcal{N}\left(\alpha_1 +
\gamma_{\text{bop}\rightarrow\text{xch}} \cdot \text{BOP}_{t-1} +
\beta_1 \text{Exch}_{t-1}, \, \sigma_{\text{xch}}^2\right)
$$

$$
\text{IR}_t \sim \mathcal{N}\left(\alpha_2 +
\gamma_{\text{bop}\rightarrow\text{ir}} \cdot \text{BOP}_t +
\gamma_{\text{xch}\rightarrow\text{ir}} \cdot \text{Exch}_t +
\beta_2 \text{IR}_{t-1}, \, \sigma_{\text{ir}}^2\right)
$$

$$
\text{Inf}_t \sim \mathcal{N}\left(\alpha_3 +
\gamma_{\text{xch}\rightarrow\text{inf}} \cdot \text{Exch}_t +
\gamma_{\text{ir}\rightarrow\text{inf}} \cdot \text{IR}_t + \beta_3
\text{Inf}_{t-1}, \, \sigma_{\text{inf}}^2\right)
$$

Birleşik Sonsal

Bayes Kuralını kullanarak örneklem alınan birleşik sonsal,

$$p(\boldsymbol{\alpha}, \boldsymbol{\gamma}, \boldsymbol{\beta},
\boldsymbol{\sigma} \mid \mathbf{Y}) \propto \left[ \prod_{t=1}^T
p(\text{Exch}_t \mid \cdot) \cdot p(\text{IR}_t \mid \cdot) \cdot
p(\text{Inf}_t \mid \cdot) \right] \cdot p(\boldsymbol{\alpha})
p(\boldsymbol{\gamma}) p(\boldsymbol{\beta}) p(\boldsymbol{\sigma})$$

Because each structural equation conditioned on its DAG parents is
independent in its error term $\epsilon_{i,t}$, the joint likelihood
factors neatly into the product of these Gaussian univariate
likelihoods.

```python
import pymc as pm
import numpy as np
import pandas as pd
import arviz as az
import matplotlib.pyplot as plt

df_clean = pd.read_csv('arg_final.csv', index_col=0, parse_dates=True)

def standardize(series):
    return (series - series.mean()) / series.std()

df_clean['bop_z']        = standardize(df_clean['bop'])
df_clean['xch_deprec_z'] = standardize(df_clean['xch_deprec'])
df_clean['ir_z']         = standardize(df_clean['real_ir'])
df_clean['inf_z']        = standardize(df_clean['inflation'])

# -----------------------------------------------------------------------------
# Gecikmeli t-1 turu dizinleri hazirla
# -----------------------------------------------------------------------------
bop_t  = df_clean['bop_z'].values[1:]
bop_l1 = df_clean['bop_z'].values[:-1]

xch_t  = df_clean['xch_deprec_z'].values[1:]
xch_l1 = df_clean['xch_deprec_z'].values[:-1]

ir_t   = df_clean['ir_z'].values[1:]
ir_l1  = df_clean['ir_z'].values[:-1]

inf_t  = df_clean['inf_z'].values[1:]
inf_l1 = df_clean['inf_z'].values[:-1]

with pm.Model() as macro_feedback_dag:
    
    # Kesiler
    a_xch = pm.Normal("a_xch", mu=0, sigma=1)
    a_ir  = pm.Normal("a_ir",  mu=0, sigma=1)
    a_inf = pm.Normal("a_inf", mu=0, sigma=1)
    
    # Standard Sapmalar
    sigma_xch = pm.HalfNormal("sigma_xch", sigma=1)
    sigma_ir  = pm.HalfNormal("sigma_ir",  sigma=1)
    sigma_inf = pm.HalfNormal("sigma_inf", sigma=1)
    
    # Path 1: BOP Deficit -> Exchange Rate Depreciation
    gamma_bop_to_xch = pm.Normal("gamma_bop_to_xch", mu=0, sigma=1)
    
    # Path 2: BOP Deficit & Depreciation -> Central Bank Interest Rate Hike
    gamma_bop_to_ir  = pm.Normal("gamma_bop_to_ir", mu=0, sigma=1)
    gamma_xch_to_ir  = pm.Normal("gamma_xch_to_ir", mu=0, sigma=1)
    
    # Path 3: Exchange Rate Depreciation & Interest Rates -> Inflation
    gamma_xch_to_inf = pm.Normal("gamma_xch_to_inf", mu=0, sigma=1)
    gamma_ir_to_inf  = pm.Normal("gamma_ir_to_inf", mu=0, sigma=1)
    
    # Autoregressive Lag Coefficients
    beta_xch = pm.Normal("beta_xch", mu=0.5, sigma=0.5)
    beta_ir  = pm.Normal("beta_ir",  mu=0.5, sigma=0.5)
    beta_inf = pm.Normal("beta_inf", mu=0.5, sigma=0.5)
    
    # --- Equation 1: Exchange Rate Depreciation ---
    # Tests if a BOP deficit (negative bop_z) drives FX depreciation
    mu_xch = a_xch + gamma_bop_to_xch * bop_l1 + beta_xch * xch_l1
    obs_xch = pm.Normal("obs_xch", mu=mu_xch, sigma=sigma_xch, observed=xch_t)
    
    # --- Equation 2: Policy Interest Rate Reaction Function ---
    # Tests if Central Bank hikes interest rates in response to BOP pressure & FX drops
    mu_ir = a_ir + gamma_bop_to_ir * bop_t + gamma_xch_to_ir * xch_t + beta_ir * ir_l1
    obs_ir = pm.Normal("obs_ir", mu=mu_ir, sigma=sigma_ir, observed=ir_t)
    
    # --- Equation 3: Exchange Rate Pass-Through to Inflation ---
    # Tests if currency drops directly trigger inflation bursts
    mu_inf = a_inf + gamma_xch_to_inf * xch_t + gamma_ir_to_inf * ir_t + beta_inf * inf_l1
    obs_inf = pm.Normal("obs_inf", mu=mu_inf, sigma=sigma_inf, observed=inf_t)
    
    # --- Indirect Mediated Chain: BOP -> Interest Rate Reaction ---
    bop_ir_reaction = pm.Deterministic("bop_to_ir_reaction", gamma_bop_to_ir)
    
    # -------------------------------------------------------------------------
    # 4. MCMC Sampling
    # -------------------------------------------------------------------------
    trace_feedback = pm.sample(draws=1000, tune=1000,
                               chains=2, return_inferencedata=True,
			       progressbar=False)

# -----------------------------------------------------------------------------
# 5. Visualizing Posteriors
# -----------------------------------------------------------------------------
az.plot_posterior(
    trace_feedback, 
    var_names=[
        "gamma_bop_to_xch", 
        "gamma_bop_to_ir", 
        "gamma_xch_to_ir", 
        "gamma_xch_to_inf",
        "gamma_ir_to_inf"
    ],
    figsize=(10, 6)
)
plt.tight_layout()
plt.savefig('dag_results.jpg')
```

![](dag_results.jpg)

Paranın Miktar Teorisi (Quantity Theory of Money) 

Bir liranın günlük hayat içinde dolaşımını düşünelim. Ben gidiyorum mesela
köşe başındaki satıcıdan bir poğaça alıyorum. Bu satıcı lirayı kızına
veriyor, kızı da onu lunaparkta ata binmek için kullanıyor. Atı kiralayan
Ali lirayı eve götürüyor, bir süre koltuk altında kaybediyor, tekrar
buluyor, sonra onu kahve almak için kullanıyor. Bütün bir sene içinde bu
lira üç kere harcanmış oluyor.

![](tser_macro_01.png)

PMT'yi anlamak için bu kavramlar yeterli aslında. 1 lira para, $M$ ile
gösteriyoruz, o paranın bir senede kaç kere kullanıldığı para hızı, onu $V$
ile gösteriyoruz ki üstteki örnekte $V=3$. Poğaça, ata binmek, kahve
ekonomideki gerçek ürünler ve servisler, onlara $Y$ diyelim, ve ürün /
servislerin fiyatı ise $P$ oluyor. PMT için gerekli değişkenler bunlar. 

![](tser_macro_02.png)

Tüm ekonomi bazında düşünürsek, $M$ para arzı (money supply) yani
ekonomideki tüm para miktarı. $V$ ürün ve servis almak için bir liranın kaç
kez kullanıldığı; bazıları parayı yastığın altına koyar onların parası
"yavaştır'', kimisi habire alışveriştedir, onların parası hızlıdır. Hız
tüm paraların hız averajı üzerinden hesaplanıyor tabii. $P$ tüm ürün ve
servislerin (yine ortalama) fiyat seviyesi. Son olarak $Y$, ekonomide
satılan tüm ürün ve servislerin miktarı, yani Gayrisafi Yurtiçi Hasıla,
GSYH (İngilizce GDP). Onun fiyat seviyesi ile çarpılmış hali Reel GSYH
(Nominal GDP), bu ekonominin ürettiği ürünlerin tüm lira bazında
toplamı. Formülün bir tarafı bu.

Formülün diğer tarafı $M$ çarpı $V$; ekonomideki tüm para miktarını o
paranın ekonomi içinde senede kaç kez döndüğü ile çarparsak bu bize, aynı
şekilde, reel GSYH'yı vermez mi? Para senede kaç kez bir ürün için birisine
verilmiş ise, bu ekonomide o kadar satış yapılmış demektir. Böylece
formülün iki tarafını elde etmiş olduk.

![](tser_macro_03.png)

Bu formüle bir teori bile denemez, gayet bariz bir eşitlik
(identity). Formüle bir diğer bakış açısı şöyle, eldeki tüm para çarpı o
paranın kaç kez harcandığı ekonomideki tüm alıcıların yaptıklarını kapsar,
satılan her şey çarpı o şeylerin fiyatı ekonomideki tüm satıcıların
yaptıklarını kapsar. Satılan her şey tanım itibariyle alınmış olduğuna göre

$$ 
M \cdot V = P \cdot Y 
\tag{1}
$$

doğru olmalıdır. Tabii üstteki formüldeki değişkenlerin nasıl ölçülmesi
gerektiğiyle ilgili tartışmalar var, burada farklı yaklaşımlar
olabilir. Mesela kredi kartları para olarak $M$ içinde sayılsın mı
sayılmasın mı?

Bu tür farklılıklar bir tarafa, üstteki formül bir eşitlik olarak, ekonomi
hakkındaki fikirlerimizi organize etmesi açısından çok faydalı. Mesela
enflasyon ($P$'nin yükselmesi) sebebi nedir sorusunu formül üzerinden
cevaplayabiliriz.

Enflasyon

PMT formülünde iki tarafı $M$'ye bölelim,

$$ \frac{MV}{Y} = P$$

Bu denklem bize diyor ki eğer fiyatlar değişiyorsa bunun üç farklı sebebi
olabilir. $M$ değişiyor, $V$ değişiyor, ya da $Y$ değişiyor. Unutmayalım
fiyatlar yani $P$ (efendim domates ne kadar, pantolon, manto, ekmek
fiyatları vs) kısa sürede oldukca hızlı değisebilir, mesela bir sene içinde
fiyatların ikiye, üçe katlandığı görülmüştür. Fakat genel ekonomik
açısından biz biliyoruz ki $V,Y$ oldukca sabittir. $Y$, yani GSYH, bir sene
içinde çok, çok fazla değişmez. Bir sene içinde yüzde 10'lük çıkış muazzam
bir büyüme kabul edilir, nadir meydana gelir, ama ikiye, üçe katlanma
neredeyse hiç olmaz. Fiyatlardaki aşırı değişimi ekonomideki aşırı büyüme
ile açıklayamayız [2].

Peki paranın dolanım hızı $V$? Bu değişken 1 liranın bir sene içinde
ekonomide ortalama kaç alım yaptığını gösterir, ABD ekonomisinde bu sayı
aşağı yukarı 7 seviyesindedir. Hızı belirleyen maaşların ne sıklıkla
ödendiği, haftalık mı, aylık mı mesela, ya da bir borç senetinin, çekin
tahsil edilmesinin ne kadar zaman aldığı (ülkenin banka yapısı, bürokrasisi
ile alakalı) gibi faktörlerdir. Bunlar da yavaş değisen faktörler, bu
sebeple hız $V$ artabilir, ya da azalabilir, ama bu ABD örneğinde 8
seviyesine çıkış ya da 6 seviyesine iniş ölçüsünde olur. O zaman $P$'deki
çok büyük değişimi $V$ ile de açıklayamayız. 

Geriye ne kaldı? $M$ kaldı. Yani fiyatlardaki çıkış para arzındaki çıkışla
alakalıdır. Eğer bir ekonomide aynı miktardaki ürün ve servisin peşinde
olan para seviyesi artarsa, fiyatların yukarı çıkması gerekir. Herkesin
cebinde 100 lira olabilir, ve kahve 1 lira, sinema fiyatı 3 lira olabilir,
ama herkesin cebindeki para ikiye katlansa, ama daha fazla kahve daha fazla
sinema salonu açılmasa, kahve 2 lira, sinema 6 lira olacaktır. 

Devletler Niye Enflasyon Yaratır

Enflasyon para arzının artmasıyla ortaya çıkar, ve aslında bir tür
vergidir, çünkü zenginliği bir anlamda halktan devlete aktarır çünkü devlet
para basıp bu parayı, yeni arzı piyasada mal almak için kullanıyorsa devlet
istediğini alır, ama uzun vadede herkesin cebindeki paranın değeri düşer,
yani para basamayan halk kaybetmiş olur. Bu tür "enflasyonist verginin''
aslında pek iyi işleyen bir yöntem olduğu söylenemez, o sebeple devletler
bu yöntemi ellerinde başka bir çare kalmadığında kullanırlar [4].

Ama yöntemin uzun vadedeki negatif tarafından önce kısa vadede olana
yakından bakalım. Aslında kısa vadede artan para arzı üretim $Y$'de artmaya
sebep olabilir. Ufak bir ekonomi düşünelim, fırıncı, terzi, ve marangoz
olsun. Diyelim devlet vergiden alacağı parayı kullanmak yerine, yeni para
basıyor ve bu parayı askerlere maaş olarak veriyor. Askerler alışverişe
çıktıklarında, giysi, ekmek ve aldıklarında ilk başta mesela fırıncı çok
mutlu olur, ekmeğini satacak yeni talep ortaya çıkmıştır. Fırıncı yeni
talebi karşılamak için daha uzun saatler çalışır, daha fazla eleman ise
alır, talep artınca fiyatları da arttırabilir. Fırıncı "oh ne güzel, artan
ekmek fiyatları ve satışı sayesinde kazandığım ek para ile daha fazla giysi
ve sandalye alabilirim''. Fakat askerler marangozdan, ve terziden de aynı
alışveriş yapmaktadırlar, onların da fiyatları artmaktadır. Fırıncı yeni
parasıyla pantolon almak için terziye gittiğinde neredeyse kazandığı ek
para kadar fiyatların artmış olduğunu görür, yani alım gücü aynı
kalmıştır. Parasının değeri düşmüştür.

Fakat devletin işi de bozulacaktır, çünkü onlar için de fiyatlar artmıştır,
para basıp aynı malları aldırmak istediğinde yeni fiyatlarla aynı alım için
daha fazla para basmak zorundadır. Basarsa bu hikaye aynı şekilde devam
eder, para değerini kaybetmeyi sürdürür, ayrıca bir süre sonra esnaf
uyanır, fiyat artışını öngörmeye başlarlar / olacağını bilirler, ve artık
"yeni talebi'' tatmin etmek için daha fazla üretmezler.

Kısa ve uzun vade ile kasettiğimiz budur. Para arzını arttırmak başta
üretimi arttırabilir, ama devletler bu numarayı istismar edebilirler,
mesela Zimbabve'de seçim kazanmak için Mugabe'nin yaptığı gibi, ve sonuçlar
her zaman kötü biter. Çünkü kısa vadede üretim artışı ardından esas artış
fiyatlara yansır ve üretim aynı seviyeye düşer, diğer yandan eğer devlet
enflasyonu çok yukarıdan azaltmak istiyorsa para arzında daralma yaratması
gerekince ilk başta üretimde düşüş olacaktır, ki bu ekonomik durulma, hatta
kriz demektir, ta ki arzdaki daralma fiyatlara yansıyıncaya kadar. Yani
enflasyonist numara başta işe biraz yarayabilir, ardından işe yaramamaya
başlar, ve kriz kaçınilmaz hale gelir. Bu noktada enflasyonun azalması
ekonomide yavaşlama, işsizlik artışına sebep olur. 

Bu sebeple bazıları enflasyonu bir uyuşturucuya benzetir. İlk başta
"uçurabilir'' ama bünye alıştıkça aynı uçuşu sağlamak için daha ve daha
fazla ilaç gerekir, öyle ki bir süre sonra kişi normal olmak için bile
ilaca ihtiyaç duyar, ve ardından ilaç kullanımı durdurulunca bir süre
"yokluk krizi (withdrawal)'' ile hasta şok yaşar, ardından acılı bir süreç
sonrası normale dönülür. ABD'de 70'li yıllarda olan buydu, 70'ler boyunca
enflasyon arttı, arttı, ama 70'ler sonunda artık işe yaramıyordu, işsizlik
düşmüyordu. Bu noktada stagflasyon denen şey ortaya çıktı yani hem
enflasyon hem işsizliğin aynı anda olduğu durum. 80'lı yıllar başında
Reagan başkan olunca enflasyon düştü, ama bunun bedeli 81/82 yıllarındaki
kriz oldu.

Para Nedir 

Biraz önce devletin "para bastığından'' bahsettik, para basılıp askerlere
veriliyordu. Fakat [3]'e göre para arzının esas odağı aslında bankaların
işyerlerine ve bazen özel kişilere verdiği *kredidir* çünkü banka borç
verirken para basar. 

Pek çok kişi bu bilgiden habersizdir. Bir şirket bankaya gidip borç
istediğinde, borç verilirse banka hiç yoktan para yaratmaktadır, ve bu
"yeni parayı'' borç isteyene vermektedir. Bu tabii ki para arzında
genişlemeye sebep olacaktır, ve (1) denklemi üzerinden ekonomi
etkilenir. Bu sebeple arz hakkında her türlü hesap ekonomideki borç
miktarını göz önüne almak zorundadır [3]. 2008 krizine kadar pek çok
kişinin bilmediği bu durum yakın zamanda İngiliz ve Alman Merkez Bankaları
tarafından kabul edildi. Evet bankalar hiç yoktan para yaratıyorlar. Yani
bankaların "özel kişilerin mevduatını borç olarak verdiği'' doğru
değildir. Bankaların borç vermek mevduata ihtiyacı yoktur, parayı basar ve
verir [5].

[devam edecek]

Kodlar

[data.py](data.py)

Kaynaklar

[1] Marginal Revolution University, *Quantity Theory of Money*, 
    [https://youtu.be/q59tZKP0HME](https://youtu.be/q59tZKP0HME)

[2] Marginal Revolution University, *Causes of Inflation*, 
    [https://youtu.be/gi7jx5IJtik](https://youtu.be/gi7jx5IJtik)

[4] Marginal Revolution University, *Why Governments Create Inflation*, 
    [https://youtu.be/E6A_WpUY2LI](https://youtu.be/E6A_WpUY2LI)

[5] *Money as Debt*, 
    [https://www.youtube.com/watch?v=2nBPN-MKefA](https://www.youtube.com/watch?v=2nBPN-MKefA)

