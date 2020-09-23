# Seam, Selenium ve Yuk Testleri

Web uygulamamiz uzerinde yuk testleri isletmek icin Kurumsal Java
kitabinda jMeter aracini tavsiye etmistik. Fakat Seam uzerinde
JMeter'in islemedigini farkedince alternatif bir arac bulmak
gerekti. Web egitim slaytlarimizda kabul testleri (acceptance test)
icin Selenium adli bir arac gosteriliyordu; Bu arac bir Firefox
eklentisidir, ve kaydetmeye baslayinca Firefox uzerinden yaptiginiz
tum aksiyonlari, girilen verileri alip tekrar geri calma (replay) gibi
servisler saglamaktadir. Tek kullanici, tek test icin guzel bir
ortamdir. Kurmak icin indirme sayfasindan Selenium IDE xpi eklentili
dosyaya tiklayin, Firefox otomatik olarak programi kuracaktir.Fakat
simdi bizim istedigimiz Selenium ile bir testi kaydedip, N tane sanal
kullanicinin ayni testi uygulama uzerinde isletebilmesi. Selenium,
herhangi bir testi Python script olarak kaydedebiliyor. Bu iyi,
scriptleme isleri bu sayede kolaylasabilir.Test kaydetmek icin Firefox
Tools | Selenium IDE menusunden uygulamayi aciyoruz, kayit isleminiz
bittiginde ise File | Save Test Case As .. | Python secimini
yapiyoruz, bir dosya ismi giriyoruz. Uretilen kod suna benziyor:from
selenium import seleniumimport unittest, time, reclass
exploretest(unittest.TestCase):def setUp(self):
self.verificationErrors = [] self.selenium = selenium("[makina]",
[PORT], "*chrome", "http://[site.ismi]") self.selenium.start()def
test_exploretest(self): sel = self.selenium
sel.open("/home.seam?conversationId=708") sel.click("link=Explore")
sel.wait_for_page_to_load("30000") sel.type("j_id4:day", "3") ...def
tearDown(self): self.selenium.stop() self.assertEqual([],
self.verificationErrors)if __name__ == "__main__":unittest.main()Bu
kodu komut satiri ortamindan geri caldirmak icin, cunku paralel
sekilde test isletmek icin once geri caldirma islemini
scriptleyebilmemiz lazim, Selenium indirme sayfasindan Selenium RC
adli paketi aliyoruz ve bir dizinde aciyoruz. Actiktan sonra dizinde
selenium-server-x ve selenium-python-client-driver-x gibi dizinler
goreceksiniz. Bunlardan birincisi Python scriptlerini geri isletirken
konusulan servis, ikincisi ise uretilen Python kodlarinin import
ettigi kodlarin oldugu yer. Bu kodlari uretilen Python scriptine
tanitmak icin "import sys" ve "sys.path.append('[SELENIUM RC
DIZINI]')" kodlarini en basa koymaniz gerekecek (ya da Python cevre
degiskenleriyle biraz takla lazim). Bu olduktan sonra Sel RC baslatmak
icin server dizinine gidipjava -jar selenium-server.jar -port
[PORT]girip servisi baslatabilirsiniz. [PORT] icin buyuk bir rakam iyi
olabilir, 12000 mesela. Bu arada, uzerinden kayit yaptiginiz Seam
programiniz isliyor olacak tabii. Artik uretilen test scriptine gidip
direk python komutu ile isletirseniz, arka planda bir Firefox
baslatildigini, kaydettiginiz hareketlerin aynen orada yapildigini, ve
sonra Firefox'un kapatilip testin bitirildigini goreceksiniz. Testin
basarili olup olmadigi python programi tarafindan text bazli olarak
ekrana basilacaktir.Buraya kadar basit. Fakat paralel testler soz
konusu olunca problem surada: Yuk testleri icin bu testlerden 100 tane
isletmemiz gerekebilir. O zaman 100 tane Firefox programi ekrana
cikacaktir! Bu hem performans acisindan kotu hem de gereksiz cunku biz
otomize bir sekilde bu testleri isletirken gorsel hicbir seyi
umursamiyoruz. Bizi tek ilgilendiren bildigimiz bir senaryo uzerinden
bir yuk yaratmak ve programin performansina bakmak. Programin
"dogrulugunu" zaten Selenium IDE ile tekil geri calma yaptigimizda
goruyorduk. Ayrica paralel testler genelde (her zaman) hicbir gorsel
birimi olmayacak bir Linux server makinasi uzerinde isletilecek, o
zaman isin GUI tarafini iptal etmek / kapatabilmek onemli bir
ihtiyac.Bunu nasil yapacagiz? Gorsel olmadan (headless) sekilde
Selenium isletmek icin guzel bir teknik bulduk. Bu arkadas xvfb diye
bir program kullanarak Selenium'dan gelen tum X komutlarini monitora
gondermeden yakalattiriyor, ve iptal ediyor. Selenium X var zannediyor
ama aslinda yok. Kurmak icin "apt-get install xvfb". Isletmek icin iki
tane xterm acin: birinde "Xvfb :99 -ac" komutunu isletin. Ikinci xterm
icinde "export DISPLAY=:99" deyip, arkasindan ayni xterm icinde hemen
ustteki gibi Selenium RC server baslatin. Artik python ile kaydedilen
testi geri caldigimizda hicbir Firefox acilmadigini gorecegiz.Simdi
paralelizasyona gelelim. Bundan sonraki basamak, bizim ekimiz. Birden
fazla, paralel sekilde ayni testi isletmenin bir teknigi. Bu amacla RC
server sureclerinden N tane baslatacagiz ve test isletici program, tek
surec ama N thread icinden bu ayri sureclere baglanacak, ve testleri
paralel sekilde isletecek. Isin temiz olmasi icin, tum bunlari tek
Python script'i icinden yaptirtacagiz. Ama ilk once Selenium IDE
tarafindan urettirdigimiz test programina gidelim; burada ufak bazi
degisiklikler lazim. Diyelim ki uretilen ustteki exploretest.py
script'i icindeki exploretest class'i olsun. Bu class'in ustteki hali
suna cevrilmeli:class exploretest(unittest.TestCase):def __init__
(self, port): self.port = portdef setUp(self): self.selenium =
selenium("[makina]", self.port, "*chrome", "http://[site]")
self.selenium.start() ...Degisen ne? setUp ile class tanimi arasina
bir kurucu method (constructor) eklemisiz. Bu lazim cunku pek cok
Thread pek cok exploretest objesi yaratacak ve bu objelerin her
birinin ayri bir RC server portuna isaret ediyor olmasi lazim. Ikinci
degisimle, selenium() cagrisi artik disaridan verilen self.port
parametresini kullaniyor.Bu script disinda runselserver.sh adli bir
script lazim. runselserver.sh suna benzer:#!/bin/bashexport
DISPLAY=:99cd [SELENIUM RC SERVER DIZINI]java -jar selenium-server.jar
-port $1Tamam. Bu programi chmod u+x ile isletilebilir hale getirmeyi
unutmayin. Simdi gelelim esas okkali programa. testrunner.py adli bu
programi server sureclerini, threadleri baslatma, isletme islerinin
hepsini gerceklestiren ana program.import osimport subprocessimport
timeimport threadingfrom exploretest import exploretestimport
unittestclass Caller(threading.Thread):def __init__(self, port):
threading.Thread.__init__(self) self.port = portdef run(self): print
"Calling" a = exploretest(self.port) a.setUp() a.test_exploretest()
a.tearDown()# number of parallel testtestnumber = 4# start portport =
12000for i in range(testnumber):print str(port+i)cmd =
['[DIZIN]/runselserver.sh %s' % str(port+i)]p = subprocess.Popen(cmd,
shell=True, stdin=subprocess.PIPE,
stdout=subprocess.PIPE)time.sleep(2)print "running tests"ts = []for i
in range(testnumber):a = Caller(port+i)a.start()ts.append(a)for t in
ts:t.join()Bu programi kullanarak yeni urettiginiz kendi testlerinizi
cagirttirmak icin exploretest yazan her yerde kendi test isminizi
koyabilirsiniz. testnumber parametresi ile kac tane paralel test
isletilecegini kontrol edebilirsiniz. Bu kadar!Notlar: o N surec N
thread icin sadece bir tane Xvfb isletmek yeterli. Bu komut ayri bir
xterm icinde bir kere isletilir.o Bir potansiyel problem: Xvfb
programi pek stabil degil, her isletim sonrasi Segmentation Fault ile
cokuyor, ama ondan once isi basariyla tamamliyor. O zaman her test
oncesi bu programi yeniden baslatmak lazim. Biraz hammaliye (ki zaten
otomize edilebilir), bu da aklimizda olsun.o Yuk testleri isleten
makinanin guclu bir makina olmasi iyi olur cunku bu makinanin N tane
firefox programini baslatmasi gerekecektir. Her ne kadar firefox'un
gorselligini Xvfb ile iptal etmis olsak bile, sonucta firefox isler
kodu yine de calisacak, ve bu pek ufak bir program degil.




