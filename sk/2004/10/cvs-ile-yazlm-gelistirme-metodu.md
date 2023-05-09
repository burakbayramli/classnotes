# CVS ile Yazılım Geliştirme Metodu

CVS en cok kullanılan Kaynak Kod İdare programlarından
birisidir. "Açık Yazılım" (Open Source) projeleri sürekli CVS
kullanırlar.  * CVS, kilitsiz sistem denen yöntem ile calışır. Kaynak
deposundan alınan bütün kayıtlar her programcı her an
değiştirebilir. Bazı Kaynak Kod İdare programları buna izin
vermez. Açık Kod projeleri dünyanın her tarafına yayılı olduğundan,
kilitsiz çalışma stili onlara daha uygun gelmektedir * CVS çok hafif
bir programdır, CVS'i barındıran servis makinasının çok güçlü olması
gerekmez.

Kitleme usulü çalışan KKI sistemlerinde, kayıtlar once kitlenir, başka
kimse erişemez, sadece tek bir kişi değişim yapıp kayıdı depoya geri
verir.  Dallı/Budaklı Kod Ağacı metodu kullanan KKİ sistemleri, ana
kod ağacından, bir dal yaratıp onun üzerinde çalışmaya imkan verir, ve
gerektiğinde bu daldan ana koda 'birleğtirme (merge)'
yapılabilir. Çakışmaların (aynı kodda ayrı değişiklikler yapmış iki
değişik kişi alâkalı) mutlaka çözülmesi gerekir. CVS dallanmaya izin
verir.

Fakat bizim tavsiyemiz, CVS ile dallanma kullanılmaması.  Proje
tecrübemizde karşımıza bir çok KKİ sistemi çıktı. PVCS (kitlemeli),
ClearCase (dallama/budaklama sistemi çok rahattır) adlı programları
kullandık. Bunların içinden ClearCase, bazı işleri rahat yapmanızı
sağlasa da, genelde bütün düzeninizi zorlastıracaktır, ve kurması,
idaresi müthiş zor bir programdır.

PVCS'in durumu rezalet ötesidir, o yüzden kategoriye alınmasına bile
gerek yok.  CVS kullanmaya karar verdiyseniz, isabet ettiniz. Aynı
şirkette ve projede 40 kadar programcının CVS'i başarı ile
kullandığını duyduk. CVS kullanırken dikkat etmeniz gereken husus
şudur: Bütün programcılar, ana depoya kendileri 'birleştirme'
yaptıkları için, bazen bütün kaynak kodunuzda hatalar artabilir. Yani,
birbirinden habersiz yapılan değişiklikler, ana kod ağacını hasta bir
hale getirebilir. Bazı gördüğümüz KKİ düzenlerinde, tek bir kişi bütün
programcı dallarından ana ağaca birleştirmeyi yapıyordu. (CVS'te
herkes kendi yapar). Bu kişi her değişime bakabildiği için (ve görevi
üzere), birbirine uymayan değişiklikleri iptal edebiliyor. Fakat bizce
bu tek kişilik görev çok zor, ve bir insana çok yükleniyor.  Onun
yerine, kontrolü herkese geri veriyoruz. Fakat, ana ağacın durumunu
surekli kontrol etmemiz gerekiyor. Bunun çok basit bir yolu
var. Otomatik bir şekilde, her saatte bir, bütün kaynak kodunu
derleyip, testlerden geçirirsiniz. Eger yanlışlar bulunursa, otomatik
olarak bu yanlışlar e-posta ile herkese gönderilir.  Önce yeni bir
Unix kullanıcısı yaratın. Adı 'canavar' olsun mesela. Canavar her
saatte bir, Unix cron kontrolünde uyanır. Uyanınca ilk yapacağı şey
cvs co komutunu işletmek olacak. Böylece depodan en yeni kaynak kodlar
alınır.

Bundan sonra canavar, ant, make ile bütün kaynak kodu
derleyecek. Sonra JUnit testlerini işletecek. Bütün bunları bir "takip
dosyasında" (log file) görünmesi lâzım. İş bittikten sonra bir grep
komutu ile 'Syntax error', TEST FAILED gibi cümleler
aranır. Bulunursa, e-posta, takip kayıdının tamamı eklenerek
postalanır. Suçlu programcılar bulunup falakaya yatırılır. (Şaka
Şaka..)  Aşağıdaki betik (script) bizim son projemizde kullandığımız
bir betiktire. Projenizin derleme komutu değişik olabilir, onu rahatça
değiştirebilirsiniz. Unutmayın, derlemeden sonra testleri mutlaka
işletin. Sonuçların aynı takip dosyasında olmasına özen gösterin.

#!/usr/local/bin/perlchdir ($ENV{'HOME'});$cvs_command = "cvs co
sizinprojeniz > /tmp/co_out.txt 2 $log_name 2 Ve aşağıdaki satırları
cron için girin.  0 10,11,12,13,14,15,16,17,18,19,20,21,22 * * 1-5
/falan/filan/derle.pl > /dev/null 2





