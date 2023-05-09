# Derin Öğrenme, YSA, Teori

Derin YSA (yapay sinir aglari) alaninda bazi gelismeler: Naftali
Tishby adli enformasyon teorisi arastirmacisi derin aglarin niye bu
kadar iyi isledigi hakkindaki bulgularini paylasiyor.


Tishby'ye gore DYSA'larin iyi islemesinin sebebi her katmanin bir
sonraki katmana bilgi aktarirken bir sıkıştırma (compression) yapmaya
mecbur kalmasi (cunku az sayida noron ile cok sayida veriyi temsil
etmek zorunda) ve bunun bir ogrenme sureci ortaya cikartmasi. Ayrica
ogrenmenin onemli bir parcasi unutmak :)

Tabii sıkıştırma = ogrenme gibi bir esitlik oldugu biliniyordu, 90'li
yillarda (galiba M. Kearns adli arastirmaci) bu alanda bir tez
yazmisti, fakat Tishby ogrenmeyi YSA baglaminda anlatiyor. Gerci cok
katmanli YSA'larda eger girdiyi ve ciktiyi ayni hale getirirsek ilk
katmanin kodlayici (encoder) son katmanin kodçözücü (decoder) haline
geldigi de biliniyordu, ama Tishby enformasyon teorisi matematigini
konuya uygulamis, ve unutma, katmanlarin genellestirmesi vs gibi yeni
gozlemler yapmis. DYSA babalarindan Hinton bu yeni arastirma hakkinda
bayagi mutlu olmus, diye duyduk.

Hinton derken, bu arastirmadan once Hinton sektorun "icinde oldugu
durumdan" sikayetciydi, yapay ogrenmede sil bastan yapip yeni bir
bakis acisi getirmeliyiz diyordu. Soyleme zamani ilginc, derin ogrenme
uygulamalari patlama yapmis halde, TensorFlow, Torch gibi altyapilari
kullanan onbinlerce gelistirici var artik, Google Cloud, Amazon AWS'de
ucuz, neredeyse sinirsiz donanim kullanima hazir (DYSA fazla donanim
ile daha iyi calisir). Zannediyorum ki Hinton'un bir sikayeti DYSA'lar
hakkindaki teorik eksiklerdi - ve hakliydi, mekanizma isliyor, ama
kimse neden isledigini bilmiyor. Gerci bu durum Newton'un Calculus'u
kesfettigi zamandan farkli degil, Newton da Calculus'un niye
isledigini tam olarak bilmiyordu, isin o kismi icin limitler
teorisinin gelistirilmesi gerekti, birkac asir sonra, her neyse,
Tishby'nin bulgulari iyi oldu.

Paralel baglamda, derin ogrenmenin diger babalarinan Yann LeCun ve YSA
uzmani / psikolog Gary Marcus "YSA teknolojisi daha ileri yapay zeka
icin yeterli mi?" hakkinda bir tartisma yaptilar. Bu tartisma aslinda
Hinton'un sikayeti uzerine basladi, Marcus "gordun mu benim dedigimi
soyluyor", LeCun "oyle demiyor" vs derken "acik munazara yapalim"a
dondu is. Marcus yillardir YSA'larin YZ icin iyi bir secim olmadigini
soyler, son zamanlardaki basarilar tabii onu defansif hale getirdi,
fakat hala baska bir yaklasim gerekecek dusuncesini devam
ettiriyor. Yeni yaklasimin ne oldugunu o da bilmiyor tabii ama su anda
arastiriyor (diye duyuyoruz). 
