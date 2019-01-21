# Faydali Unix Komutlari


Faydali Unix Komutlari




Hangi surec (process) port 8080'i dinliyor?

netstat -n --tcp -p -l |grep ":8080"

Sadece bastaki satiri gostermek

head -1 [DOSYA]

Bir dosyanin son N satirini atlamak / gostermemek (silme amacli olabillir), mesela son iki satir icin 

head -n-2 [DOSYA]

Bir dosyanin ilk satirini atlamak icin

tail -n+2 [DOSYA]

Bir komut ciktisini text olarak baska bir komut icine gommek

`` kullanilir. Mesela  

echo "b.txt" > a.txt
echo "filan" > b.txt
echo "falan" >> b.txt

Yani a.txt icinde b.txt yaziyor, simdi 

wc -l `cat a.txt`

dersek, a.txt icindeki b.txt arguman olarak wc'ye verilecek. Ve

2 b.txt

sonucu gelecek. Yani burada a.txt degil onun icindeki b.txt wc'ye gecildi.

Eger bir Unix script'inizin .bashrc icinde tanimladiginiz alias (kisayollari) kullanmasini istiyorsaniz, bu script'i soyle isletmeniz gerekir:

bash -i [script]

Emacs icin benzer durum, M-x compile mesela alias'lari gormuyor. Onun icin

(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

tanimlamak lazim.

Sonsuz kere bir komutu isletmek icin, mesela surec listesini surekli gostermek

while true; do ps -eaf; sleep 2; done

Belli satirlar arasindaki satirlari cekip cikartmak, mesela 10 ile 20

sed -n 10,20p [DOSYA]

Rasgele satirlar cekip cikarmak (.005 bir oran, daha fazla satir icin bu sayi arttirilabilir)

cat [DOSYA] | perl -n -e 'print if (rand() < .005)'

Dosya icinde metin aramak (/usr seviyesinden asagi dogru inerek, tum altdizinlerde, xml soneki tasiyan dosyalarda)

find /usr -name '*.xml' -exec grep -l 'filan' {} \;

Dosya icinde kelime degistirmek (xml sonekli dosyalarda, altdizinlere girmez)

perl -pi -e 's/filan/falan/sg' *.xml

Usttekinden daha iyisi replace.py diye ufak bir Python script yazmak,

import os, re, sys
filename = sys.argv[1]
content = open(filename).read()
fout = open(filename,"w")

content = content.replace("burak","bayramli")
fout.write(content)
fout.close()


Ustteki script'i python replace.py [dosya] seklinde cagirabiliriz. Bu tek dosya uzerinde degisim yapar. Eldeki content uzerinde her turlu regex, vs. bazli islemleri yapabiliriz. Hatta gerekirse basa, sona istedigimiz metni ekleyebiliriz.

Eger bircok dosya uzerinde ustteki degisimi yapmak istersek,

find .-type f -name "*" -exec python replace.py {} \;

Dosyayi kullanan program hangisi

lsof [DOSYA]

Bir dizinden başlayarak özyineli (recursive) olarak o dizin altındaki en büyük dosyaları listelemek için

du -h | sort -hr | head -20  

Bir makinadaki tüm donanımları listele (Ubuntu)


lspci
Harici sabit diskte bazı dizinlerimin yedeğini tutuyoruz, ve iki yerel dizini senkronize etmek için

rsync --delete -r /tmp/dira/ /tmp/dirb

Ustteki rsync komutu kaynak dizini "belirleyici (master)" olarak kabul edecektir, ve /tmp/dira icinde olan her sey, yeni dosya, mevcutun silinmesi, ekleme islemi aynen /tmp/dirb'ye yansitilacaktir. Not: Eger /tmp/dirb FAT32, NTFS gibi Windows temelli bir dosya sistemi ise azar azar guncelleme ise yaramaz, tum dosyalar her seferinde tekrar silbastan yollanir. Bunun sebebi Win temelli dosya sistemlerinin dosya sahibi, erisim haklari gibi bilgileri -Unix ile uyumlu- tutmuyor olmasidir. 

Bir dosya içinde mesela her "dropbox" kelimesinden önce gelen 700 karakteri göster

grep -E -o ".{700}dropbox" dosya.txt

Programın sonuçlarını hem ekrana basmak aynı anda bir log dosyasına yazmak

sh script.sh  2>&1 | tee out.log





