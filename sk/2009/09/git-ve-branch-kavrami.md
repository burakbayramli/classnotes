# Git ve Branch Kavrami

Bazen koda deney amacli bazi uzatmalar eklemek, iyilestirmeler
gerekebiliyor, ve/fakat bu kodlama eklerinin uzun zaman almasi da
mumkun, ayrica bu ek kodlar ozelliklerin ana koluna hemen de lazim
olmayabiliyor. O zaman bu "paralel" gelisme kendine has bir ritmde,
"en gerekli" gelistirme safhasindan kismen ayri bir sekilde
ilerleyebilir.Bu en son Schemafree'ye zlib sıkıstırma (compression)
destegi eklerken basimiza geldi. Bir gunluk bir calisma sonrasi hala
yapilacak is vardi, bu calismayi kaybetmek istemedik, kodlari commit
etmek istiyorduk, ama bu ek kodlar uygulama ana kodlari icin kritik
degildi, SF ana kodunu simdilik degistirmemek istiyorduk. Iste branch
kavrami tam bu noktada ise yariyor.O ana kadar ekledigimiz tum
dosyalari (daha hicbir git komutu kullanmadan eklemek tabii, sadece
dosya sistemi bazli eklemekten bahsediyoruz) yeni bir branch'e yazmak
istiyorsak, hemen o anda birgit checkout -b [BRANCH ISMI]komutu
uygulariz. Bu noktada artik o belirttigimiz isimdeki branch
uzerindeyiz. Daha once "master" adli bir branch'te idik, bu Git'in siz
hicbir branch belirtmezseniz olagan olarak kullandigi branch ismidir
(evet, master'in kendisi de bir branch'tir). Yeni branch yarattiktan
sonra yapilan her turlu commit islemi bizi bu yeni branch ismi
uzerinde tutacaktir.

Eger master branch'e geri donmek istersekgit checkout masterkomutu
yeterli. Uzaktaki bir Git deposuna yeni branch'i gondermek icingit
push ssh://[isim]@[makina]/bir/dizin/proje [BRANCH ISMI]gibi bir komut
lazim. Eger push sirasinda branch ismi belirtmezsek olagan deger olan
master kullanilir.Simdi biraz daha derinlere inelim (gerci branch
kavramini kullanmak icin takip edecek bilgilere gerek yok); Su onemli
bir nokta, Git uzerinde yapilan her commit, nerede olursak olalalim, o
anda depodaki tum kodu temsil eden tekil (unique) bir SHA1 hash
kimligi uretilmesini saglar; git log komutunu kullaninca bu kimlikleri
goruyorsunuz. Yani herhangi bir branch'te commit noktalarinin
kimliklendirilme mekanizmasi aynidir.O zaman branch isimleri nedir? Bu
isimler aslinda surekli hareket eden "isaretlerden" ibaret. Mesela
"master" isminin isaret ettigi SHA1 kimligi aslinda surekli degisiyor,
her commit komutu uyguladiginizda "kayiyor". Her zaman en son noktayi
gostermek icin bu yapiliyor. Aslinda sadece SHA1 kimliklerini
kullanarak ta git ile calisabilirdiniz, ama bu rahat olmazdi. git
checkout -b, o zaman, aslinda yeni bir isaret yaratmaktan baska bir
sey yapmiyor.

Ilginctir: Eger istersek, git log'da gorulen listedeki herhangi bir
noktaya SHA1 kimligi kullanip "zamanda geriye giderek" donebilir, ve o
noktadan baslayarak yeni commit'ler yapmaya baslayabilirdik. Sadece bu
bile bizi degisik bir branch yonunde hareket ettirmeye baslardi.. Git,
hangi SHA1'den gelip hangisine gittigini surekli kontrol eder, ve bu
"oncesi / sonrasi" zinciri, bir branch olusturmak yeterlidir. VE
sonradan, istersek, bu branch'e git checkout -b ile bir isim
koyabiliriz, ve o andan itibaren, artik o ismi kullanarak o yeni
branch'in, SHA1 degisim zincirinin "sonuna" da otomatik olarak
gidebilmeye baslariz.




