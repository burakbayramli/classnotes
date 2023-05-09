# Sürekli Entegre Etmek

Yazılım projelerinde, bütün kaynak kodu baştan asağı derlemek ve
işleyen bir program çıkartmak çok önemlidir. Bu eylemin bu kadar
önemli olmasına rağmen, bazı projelerde bu tekniğin izlenmediğini
görüyoruz. Bu satırlarda Matt adlı arkadaşımızın bir projemiz icin
kurduğu 'derleme sistemini' gözden geçireceğiz. Matt benim
ThoughtWorks şirketinde çalışan arkadaşlarımdan biri, ve kurduğu
tümleştirme sistemi öteki projelerimizde de kullanılmaya
başlandı. Sistemin en göz alıcı noktası bütün kodu derlemek ve bütün
testleri bu çıkan program üzerinde otomatik olarak işletmek. Bunun
sayesinde programcıların ayrı kodları birbirine 'hergün' entegre
ediliyor, ve sonuçta program farklılıkları önceden tesbit edilmiş
oluyor.

Yazılım tarihi eski olmasa bile, birçok bilinen ve işleyen derslerle
doludur. Nedense bu dersler hakkında konuşulsa da, çok azı işleme
konar. En önemli derslerden biri kodu bastan aşağı derlemek ve
testlere tâbi tutmaktır, ve bunu gün içerisinde birkaç kere
yapmaktır. Günlük derleme fikri yeni değil, McConnell tavsiyesi de
böyledir, ve Microsoft'ta eskiden beri takip edilen iyi metodlardan
biridir. Fakat burada XP'ye daha çok hak vermek lazım, onlara göre
günde bir kere derleme 'en az' yapılması gereken bir şeydir. Daha çok
yapılırsa daha iyi.  Sürekli Entegrasyon terimini bu yazıda
kullanacağız, bu terimi XP grubundan aldık. Bu kavram çok eskiden beri
ortadaydı, XP takip etmeyen bircok kişi tarafindan takip
ediliyordu. Biz şirketimizde XP kullandığımız için terimleri ve
fikirleri aynen kullanıyoruz, fakat söylemek gerekir ki, bütün metodu
(XP) takip etmeseniz bile parça halinde bazı 'dersleri' size yardımcı
olacaktır.  Günlük, otomatik derleme sistemini kurmak için birkaç şey
gerekir:

* Bütün kodun bulunduğu, herkezin erişebileceği bir depo. Bu depo en
son sürüm ve önceki sürümlere rahat erişebilmenizi sağlamalı.

* Derleme sistemi öyle kurulmalı ki, her programcı, tek bir komut
satırı ile, kaynak kodun hepsini derleyebilmeli.

* Test sistemi öyle kurulmalı ki, bütün testler, yeni derlenen
programın üzerinde 'otomatik' olarak işletilmeli, gene tek bir komut
satırı ile.

* Bu işlemin sonunda, programın en son halini temsil eden calıştırılır
program elde edilmeli.  Bütün bunların yapılması biraz disiplin
gerektirecek. Öteki taraftan, bir kere işler hale gelince bu sistemi
sürekli işler halde tutmak cok kolay.  Sürekli Entegrasyonun Yararları
Sürekli Entegrasyonun en zor taraflarından biri, yazılım takımının
günlük hayatında getirdiği değişikliklerdir. Bu değişiklikler bariz
olmayabilir, hele hele bu stilde işleyen bir takımda hiç
calışmadıysanız...

Aslında birey bazında, her programcı tek tek bu sistemi takip
etmektedir, fakat tek kişi uzerinde odaklı olarak bunu
yapmaktadır. Denebilir ki, tek kişi, kendi kodunu, gene kendi kodu ile
sürekli entegre etmektedir. Fakat takım halinde calışmak, beraberinde
değişik problemler getirir, bu yüzden takım bazında yapılacal bir
sürekli entegre eylemi, potansiyel problemlerin sayısını aşağı
indirecektir. Bu sistemi tekip ederken disiplinli olup, sistemden
sapmamak da gerekir.  Sürekli Entegrasyonun en büyük yararı, "hata
bulmakta" olacak. Bazen yazılım takımı belli bir hatanın peşinde
saatler harcar, çünkü bir programcının kodu ötekinin kodunu bozmuştur
(birbirlerinin kodunu kullanıyorlar ise). Böyle hataları bulmak
zordur, çünkü hata iki tarafta ayrı ayrı değildir. Hata 'iki kod
parçasının birbiri ile konuştuğu' yerdedir.

Bu tip hatalar, göze çarpmadan günlerce ortaya çıkmayabilirler,
oldukları yerde büyürler. (Belki de İngilizce terimde yazılım
hatalarına 'bug', yani böcek denmesi bu yüzdendir).  Sürekli
entegrasyonun yardımı ile, bu tür yazılım hataları, daha yapıldığı
günde yakalanabilir. Hata yakalanınca, zaten iki kodun nerede
birleştiği bilindiği için, kodun her yerine bakmanız gerekmez. Bunun
sayesinde arama/tarama işlemini kaynak kodunun yarısına
indirebilirsiniz. Eğer hatayı bulamazsanız bile, calışmayan özelliği
sonraki sürüme dâhil etmezsiniz olur biter, hatayı erken yakaladığinız
için, hangi özelliğe bağlı olduğunu bilirsiniz. (Tabii ki yeni
özelliği istemeniz, hatanın kötülüğünden daha baskın çıkabilir. Bu
halde hataya geçiş belgesi verebilirsiniz, ama hiç değilse bilerek
yapılmış bir seçim olur bu seçim, bilginiz haricinde çıkacak bir
hatadan bin kat daha iyidir).

Ayrıca, bütün entegrasyon hatalarını yakalamayabilirsiniz. Bu
anlattığımız sistem, test etmeye dayanır, ve bildiğiniz gibi testler
bütün hataları ortaya çıkarmayabilirler. (Eğer yeteri kadar test
yazmamışsanız). Önemli olan nokta, sürekli entegre kurmak icin
harcadığınız eforun yararlı bir getiri sağlayacağıdır. Harcadığınız
zaman, size daha fazla zaman ve para olarak geri dönecektir. Bu
sistemin olması, hiç olmamasından çok daha iyidir.  Özetle, sürekli
entegrasyon ile zamandan istifade etmeniz mümkün olacaktır,
entegrasyon hatalarını sistematik bir şekilde yakaladığınız için, hata
aramakla daha az zaman harcar olacaksınız. Bunları deneysel sonuçlara
dayandıramıyoruz, fakat gözlemlerimize dayanarak sürekli entegrasyonun
güçlü bir şekilde arkasındayız.

Martin Fowler





