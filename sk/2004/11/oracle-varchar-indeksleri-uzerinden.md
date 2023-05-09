# Oracle VARCHAR İndeksleri Üzerinden Erişim Yavaş mıdır?

İnternet'te gezinen bir efsaneye göre, "Oracle VARCHAR, VARCHAR2
kolonları üzerinden yaratılan indeksler üzerinden erişim yavaş
olmaktadır", ve "bu tür kolonlar üzerinden indeks yaratılmaktan
kaçınılmalıdır".  Kulağa peri masalı gibi gelen bu genelleme hakkında,
biz de bir bilene danıştık. Beraber Martha Stewart Omnimedia
şirketinde veri ambarı (data warehouse) projesinden çalıştığım Oracle
uzmanı arkadaşıma bu soruyu yönelttim. Kendisinden şu cevabı aldım:

Cevap: Büyük bir ölçüde, bu tam bir peri masalı (myth). Durumun böyle
olmadığını değişik ölçülerde tablolar yaratıp üzerlerinde join'ler ve
nokta sorgular işleterek çok rahat ispatlayabilirsin. Benim bu peri
masalının niye ortada olduğuna dair birkaç teorim bile var.  1) 50,000
satırdan daha az veri içeren küçük tablolarda, direk tarama (direct
scan) [1], herhalde daha hızlı sonuç verir. Çünkü donanım seviyesinden
düşünürsek, eğer indeks kullanıyorsak, sabit disk kafası önce önce
indeksi okuyacak, ondan sonra veri içeren tabloyu okuyacak. Tablo
zaten çok küçük ise, erişimi ara "bir diğer işlem" olmadan yapmak,
daha hızlı olabilir.  2) Integer ve number üzerinden indekslerin daha
hızlı olduğu biliniyor. [2].  3) Bir VARCHAR indeksini yaratmak daha
oldukça fazla zaman ve yer tutuyor!!!  Bu durum, bahsettiğim peri
masalından sorumlu olabilir. Ama mevzubahis olan kolon birçok kere
erişiliyorsa, o zaman bu indeksi yaratmak, harcanan zamana ve disk
alanına kesinlikle değecektir [3].

Yâni, büyük bir tablo ve üzerinden çok arama yapılan bir VARCHAR
kolonu üzerinde indeks koymak, o sorguyu kesinlikle daha
hızlandıracaktır. Bunu çok kolay test edebiliriz. 10 milyon satırlık
ve üzerinde sadece isim olan bir tablo yarat. Sonra select * from
table where isim = 'xxxxxxx' de, ve sorgunun ne kadar hızlı geri
geldiğini bir yere not et. Meselâ bu örnekte 5 dakika olsun.  Sonra
isim kolonu üzerinde bir btree indeksi yarat (bu indeksi yaratmak 10
dakika sürebilir), ve aynı sorguyu tekrar işlet. Sonuç anında geri
dönecektir. Evet indeksi yaratmak için 10 dakikayı harcadık, ama 2. ve
3. seferde artık kâra geçiyoruz ve indeksi yaratmak için harcadığımız
vaktin bir önemi kalmıyor [3].  Hash Join'ler ve İndeksler Bir de eğer
elinde MEGA donanım/beygir gücü varsa, o zaman indeks kullanmamak daha
hızlı olabilir. Bahsettiğim donanım, mikroişlemciler üzerinde Massive
Parallel'i destekleyen, ve üzerinden JOIN yapılan tüm kolonların
hafızaya önceden yüklemeye izin verebilen türden bir donanımdır. Swap
diskler ve asal diskin de dehşet hızlı olması yardım eder. Bu tür bir
kuruluşta, parallellik ve hash join'leri kullanmak, varchar kolon
üzerinden indeks kullanmaktan daha hızlı olacaktır. Fakat daha küçük
donanımlarda eğer bütün hash join hafızada yapılamıyor ve kolonlar çok
hızlı okunamıyorsa, varchar olsun olmasın, indeks kullanmaktan başka
çaremiz olmayacaktır.  Notlar [1] İndeks üzerinden değil her satıra
teker teker bakılara arama yapılan senaryodur.  [2] Yâni, her nasılsa
"integer indekslerinden daha yavaş" betimlemesi, "çok yavaş"
betimlemesine dönüşüvermiştir.  [3] Arkadaşım veri ambarcısı olduğu
için, bir seferde milyon satıra varan günlük veri transferleri
yapmaktadır, ve bu transferler sırasında ara tablolar (staging tables)
üzerinde indeksler her seferinde silbaştan yaratılır. Bu indeksleri
sürekli yarattığı için, indeks yaratma zamanı da kendisi için önemli
bir faktördür. OLTP cenahı için indeks yaratma zamanı önemli
olmayabilir, ama arkadaşın VARCHAR indeksi üzerinden "erişim" hakkında
söyledikleri her Oracle kullanıcısı için geçerlidir.  Martha Stewart
bahsedilen veri ambarını şu anda Oracle 10g üzerinde tutmaktadır.





