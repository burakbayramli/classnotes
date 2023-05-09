# Git ile Baslangic

Git ile kaynak kod idaresine baslamak cok kolay. Depoya cevirilecek
bir dizine girilir ve alttakiler komut satirinda isletilir.git initgit
add .git commit -m "ilk commit burada"Gorsel olarak Git kullanmak icin
Gitk var. Emacs uzerinden Git komutlari kullanabilmek icin git-emacs
paketi oldukca iyi; tanidik C-x v v komutlari bulunabilir, yorumlar
editor icinden girilebiliyor, vs.Git sisteminin neler yapabildigini
ogrenmek icin Git Magic e-kitabini da tavsiye ederim.Git her
kullanicinin kendi deposunu merkez alarak "yerel" calisabilmesini
saglar; mesela dallanma, birlestirme gibi hicbir islem icin
network'teki bir "merkezi" servise bagli olmaniz gerekli
degildir.

Git'teki ana kavram "klonlama", "cekmek" ve "itmek" kavramlaridir, ki
bu islemlerin hepsi de ayni depodan baslayan ama uzerlerinde ayri
calisilan bagimsiz depolari temel alirlar. Iki programci, sadece
birbirlerine gercekten kod vermek istedigi zaman, network uzerinden
birbirine kod itip, ceker.Bizim ilk Git kullanisimiz yedekleme
amacliydi, ayrica iki makinada ayni kod uzerinde calisiyor olmamiz bir
sekilde idare edilmeliydi. Isin ilginc tarafi, bahsedilen turden
kullanimlar icin Git'in mentalitesinden hic disari
cikmiyorsunuz.

Yedeklemede, yedek olan depo sanki üzerinde hiçbir programcının
çalışmadığı bir "klon" haline geliyor, ayrı makinada üzerinde
çalışılan aynı kod ise (aslında aynı olan) sanki iki programcının iki
ayrı klon üzerinde çalışıyor olması haline geliyor.Basit yedekleme ile
başlayalım: Bir dizini Gitleştirdikten sonra, yedek için onu başka bir
dizine klonluyoruz:git clone /dizin/ismi/Eğer ssh üzerinden başka bir
makinaya yedekliyorsak, önce uzaktaki makinada /dizin/ismi diye bir
dizin yaratıp o dizin altında "git init" işlettikten sonra lokal
makinada şunu işletebiliriz:git push ssh://kullanıcı@makina/dizin/ismi
masterBunu işlettikten sonra uzaktaki makinadaki dizinin boş olduğunu
görebilirsiniz; o dizin altında "git checkout master" işlettikten
sonra dosyalar görünür olacaktır.Kendi kopyamız üzerinde çalışıyoruz,
istediğimiz kadar commit ediyoruz, vs. Sonra klona (yani yedeğe)
farkları göndermek istiyorsak, o zaman yine aynı komutu benzer şekilde
işletiyoruz.git push ssh://kullanıcı@makina/dizin/ismiUzaktaki
makinada güncellenme gerçekleştirilmiştir; fakat o makinada da en son
fiziki dosyalara bakmak isterseniz, o zaman "git reset --hard"
komutunu vermeyi unutmayın. Bu komut Git'e "en son commit noktasına
gitmesini" söyleyen bir komuttur, en son commit noktası da push
ettiğiniz nokta olduğuna göre fiziki dosyalar o noktadaki hale
dönüşecektir.Diyelim ki kendi kod bazımıza bir şey oldu, silindi,
vs. Derde gerek yok; Yedeklenmiş klon, klonlandığı (ve güncellendiği)
andan itibaren tüm commit tarihini, her şeyi içeren asıl bir depodur,
o zaman bu depoyu kendimize geri klonlayarak kaldığımız yerden devam
edebilirdik.İki makinada farklı klonlarda çalışma durumunda ise,
klonları senkronize etmek isteyen kendine kodu "çeker" ve çakışmaları
çözerek kendi deposuna commit eder, sonra git push ile bu senkronize
edilmiş hali ikinci klona gönderir.Önemli bir faktör: Git çok hızlı
çalışmaktadır.

Linüs Torvalds Git'i kernel kodunu idare edebilmek için yazdı, ve bu
kodun ne boyutlarda olduğu bilinen bir şey. Bu gereklilikler ışığında
Git hızlı işleyecek şekilde tasarlanmış.GithubGithub sitesi "sosyal
kodlama" sloganıyla yola çıkmış bir sitedir. Bir Git kod deposu, hem
yerel hem Github'da tutulur, tabii Git'in "kişiye özel" ruhuna uygun
olarak, birisi Github'da gördüğü bir "programcının projesinde"
değişiklik yapmak isterse, bunu o depoyu o "programcının hesabından
kendi hesabına" klonlayarak yapar. Sonra, tabii ki, kendi hesabından
bir de kendi lokal makinasına klonlama yapar. Lokal kopyada
çalışmalarına başlar.

Mesela; biz Voldemort projesiyle ilgileniyoruz ve bu projeyi ijuma
adlı arkadaştan klonladık.Bizim kendi başlattığımız proje pyjde Github
üzerinde, oradan isteyen klonlabilir. Bunu yaparak aslında Github'i
hem bir yedekleme noktası, hem de başkaları ile kodu paylaşma ortamı
olarak kullanıyoruz.Yedeklemeden bahsedelim: Kendi yerel pyjde
klonumuzda (ya da aslımızda) çalışıp commit ediyoruz. Kodu herkese
göstermek ve/ya yedeklemek istediğimizde ise,git push
git@github.com:burakbayramlı/pyjde.gitkomutu yeterli oluyor. Üstteki
ürl'in ne olduğunu Github size açık bir şekilde gösteriyor,
karıştırmaya hacet yok. Yanlız dikkat: kullanılması gereken "Your"
Clone ÜRL, öteki ÜRL değil.

Ayrıca Github'a kod gönderebilmek için ssh public anahtarınızı
Github'a vermiş olmanız lazım. Ssh konusunu blog'da çok kez işledik
(Kurumsal Java kitabında da konuda bir bölüm var). ssh-keygen -t rsa
ile ürettiğiniz .ssh/id_rsa.pub dosyası içindekileri Github'a verince
iş bitiyor, bir güven hattı oluşuyor, ve şifresiz şekilde Github sanki
kendi network'unuze dahil bir makinaymış gibi oraya kod
gönderebiliyorsunuz. Ssh ayarları üstteki kullanıcı@makina örneği için
de geçerli tabii ki. Birden fazla makina üzerinde çalışıyorsanız o
makinada anahtar üretimi yapıp bu yeni anahtarın public olanını
Github'a verince (Add Key ile) o makinadan da şifresiz olarak
çalışabilmeye başlıyorsunuz.Bir not daha: Anahtarı proje bazınde
değil, hesabınız (account) bazında tanımlayın. Bütün projeleriniz aynı
anahtarı kullanabilsin yani.

Bunun için Github sayfanızda "Account Settings" menüsünü seçin, ve
oradan "SSH Public Keys"'e gidin. Burada id_rsa.pub dosyanizin
içindeki değerleri ekleyebileceğiniz seçenekler var. Birden fazla açık
anahtar eklenebilir, eklenenler silinebilir, vs.Bir süredir Git
kullanıyorum, ve kesinlikle CVS ve Sübversion'a dönmek gibi bir
isteğim kalmadı.

Gidişat dağıtık çalışabilen kod depolama sistemleri yönünde; bu sosyal
network kavramı ile de birebir uyum içinde; Github'da, aynen
Twitter'da olduğu gibi, birini "izlemeye" başlayabiliyorsunuz. Bir
"news feed" özelliği var ve Facebook'ta arkadaş takip eder gibi
projesini klonladığınız ya da izlediğiniz kişinin commit'leri news
feed'inizde gözüküyor!

Github sadece açık yazılım projeleri için değil, artık şirketler de
kodlarını Github üzerine tutuyorlar. Bir admin sizi eklemezse bu
projeleri göremiyörünüz yani ama bunun haricinde herşey normal bir Git
projesi idaresi ile aynı.





