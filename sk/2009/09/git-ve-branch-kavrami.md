# Git ve Branch Kavrami

Bazen koda deney amaçlı bazı uzatmalar eklemek, iyileştirmeler
gerekebiliyor, ve/fakat bu kodlama eklerinin uzun zaman alması da
mümkün, ayrıca bu ek kodlar özelliklerin ana koluna hemen de lazım
olmayabiliyor. O zaman bu "paralel" gelişme kendine has bir ritmde,
"en gerekli" geliştirme safhasından kısmen ayrı bir şekilde
ilerleyebilir.Bu en son bir projemize yeni bir özellik eklerken
başımıza geldi. Bir günlük bir çalışma sonrası hala yapılacak iş
vardı, bu çalışmayı kaybetmek istemedik, kodları commit etmek
istiyorduk, ama bu ek kodlar uygulama ana kodları için kritik değildi,
SF ana kodunu şimdilik değiştirmemek istiyorduk. İşte branch kavramı
tam bu noktada ise yarıyor.O ana kadar eklediğimiz tüm dosyaları (daha
hiçbir git komutu kullanmadan eklemek tabii, sadece dosya sistemi
bazlı eklemekten bahsediyoruz) yeni bir branch'e yazmak istiyorsak,
hemen o anda bir git checkout -b [BRANCH İSMİ] komutu uygularız. Bu
noktada artık o belirttiğimiz isimdeki branch üzerindeyiz. Daha önce
"master" adlı bir branch'te idik, bu Git'in siz hiçbir branch
belirtmezseniz olağan olarak kullandığı branch ismidir (evet,
master'in kendisi de bir branch'tır). Yeni branch yarattıktan sonra
yapılan her türlü commit işlemi bizi bu yeni branch ismi üzerinde
tutacaktır.

Eğer master branch'e geri dönmek istersek git checkout master komutu
yeterli. Uzaktaki bir Git deposuna yeni branch'i göndermek içıngıt
push ssh://[isim]@[makina]/bir/dizin/proje [BRANCH İSMİ]gibi bir komut
lazım. Eğer push sırasında branch ismi belirtmezsek olağan değer olan
master kullanılır.Şimdi biraz daha derinlere inelim (gerçi branch
kavramını kullanmak için takip edecek bilgilere gerek yok); Şu önemli
bir nokta, Git üzerinde yapılan her commit, nerede olursak olalalım, o
anda depodaki tüm kodu temsil eden tekil (ünique) bir SHA1 hash
kimliği üretilmesini sağlar; git log komutunu kullanınca bu kimlikleri
görüyorsunuz. Yani herhangi bir branch'te commit noktalarının
kimliklendirilme mekanizması aynıdır.O zaman branch isimleri nedir? Bu
isimler aslında sürekli hareket eden "işaretlerden" ibaret. Mesela
"master" isminin işaret ettiği SHA1 kimliği aslında sürekli değişiyor,
her commit komutu uyguladığınızda "kayıyor". Her zaman en son noktayı
göstermek için bu yapılıyor. Aslında sadece SHA1 kimliklerini
kullanarak ta git ile çalışabilirdiniz, ama bu rahat olmazdı. git
checkout -b, o zaman, aslında yeni bir işaret yaratmaktan başka bir
şey yapmıyor.

İlginçtir: Eğer istersek, git log'da görülen listedeki herhangi bir
noktaya SHA1 kimliği kullanıp "zamanda geriye giderek" dönebilir, ve o
noktadan başlayarak yeni commit'ler yapmaya başlayabilirdik. Sadece bu
bile bizi değişik bir branch yönünde hareket ettirmeye başlardı.. Git,
hangi SHA1'den gelip hangisine gittiğini sürekli kontrol eder, ve bu
"öncesi / sonrası" zinciri, bir branch oluşturmak yeterlidir. VE
sonradan, istersek, bu branch'e git checkout -b ile bir isim
koyabiliriz, ve o andan itibaren, artık o ismi kullanarak o yeni
branch'in, SHA1 değişim zincirinin "sonuna" da otomatik olarak
gidebilmeye başlarız.





