# EC2 Elastik Ip Adresi

Amazon EC2 "elastik IP adresi" diye bir servis sunuyor. Aslinda bu
servisle elde ettiginiz bir "statik IP" (yani servisin sundugu
kabiliyetin tam yaptigi sey elastik kelimesi ile bir tezat olusturuyor
olabilir) ama bu adres makinadan makinaya esnek bir sekilde
"baglanabildigi (associate)" icin elastik bir kullanim
sunmakta.. Sonuc olarak tek bir IP adresini istediginiz kadar elinizde
tutabiliyorsunuz, ve bu adresi istediginiz EC2 makinasina
baglayabiliyorsunuz. Baglantinin etki etmesi saniyeler aliyor; ve
arkasindan hic degismeyen IP adresiniz yeni makinaya baglanabilmeye
basliyor.

EC2 ortaminda makinalarin (instance) ne kadar rahat acilip,
kapatilabilecegini bildigimize gore, boyle bir ortamda elastik IP
adresinin cok faydali bir servis olacagi acik. Teorik olarak eski
siteniz islemeye devam ederken, yeni bir EC2 kumesnde yeni kodunuzu,
yapinizi test edebilirsiniz, ve tatmin olunca, ana adresi, elastik IP
adresini yeni makinaniza degistirdiginiz anda sitenizin trafigi hemen
yeni makinalara akmaya baslayacaktir.Elastik IP adresleri bir makinaya
bagli oldugu zaman ucret odenmiyor, fakat rezerv edilmis ama
"baglanmamis" halde ise Amazon sizden bir ucret kesiyor. Bunun sebebi
Internet'te IP adreslerinin kisitli bir kaynak (precious resource)
olmasidir; bu sebeple Amazon eger bir IP adresini rezerve ettiyseniz
onu kullanmanizi istiyor. Bu sayede bir adresi "kapip" onu hic
kullanmayan kisilerin tesvik edilmesi engelleniyor.Adres nasil alinir?
Cok basit:ec2-allocate-addressBu komut bir IP adresi geri
dondurecektir ve bu adres iptal etmediginiz surece artik sizin EC2
hesabiniza bagli olacaktir.

Adresi bir EC2 makinasina baglamak icinec2-associate-address -i
[MAKINA ISMI] [IP]komut yeterli. [MAKINA ISMI] ec2-describe-instances
komutundan gelen listede "i-" ile baslayan makina ismi olacak, [IP]
reserve edilmis olan IP adresiniz. Makina dogal olarak daha once
ec2-run-instances ile baslattiginiz bir EC2 makinasidir. Herhangi bir
anda reserve ettiginiz IP adreslerini gormek
icinec2-describe-addresseskomutu yeterlidir.WWW ile baslayan site
ismini IP adresi ile baglamak, site ismini aldiginiz sirketle
halledilecek bir sey; elastik IP adresinizi bu sirkete bildirdiginiz
anda site isminiz ile IP adresiniz arasindaki baglanti da kurulmus
olur, ve bu baglantinin bir daha degistirilmesi gerekmez. Makinalar
degistikce ec2-associate-address ile ayni adres yeni makinalara
baglanabilir, ve ayni isim ayni adres ile calismaya devam eder. Site
ismi -> IP adresi baglantisi genelde surekli degistirilen bir sey
degildir (etki etmesi uzun suruyor) o sebeple fazla olabilecek
degisimi EC2 tarafinda yapabilerek optimal bir cozumu bulmus oluyoruz.





