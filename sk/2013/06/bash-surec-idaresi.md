# Bash, Surec Idaresi

Komut satirindan surec kontrolu, idaresi gibi isler icin Unix bash'in
guzel ozellikleri var. Bash icin bir script'i sh script.sh ile degil
bash script.sh olarak isletmek gerekli. Mesela

```
set -m # Enable Job Controlfor (( i = 0; i < $2; i++ ))do
    cmd="baz komut "
    cmd+=" ";cmd+=$2;cmd+=" ";cmd+=$i
    if [ $1 == "secim1" ]; then
        xterm -hold -e $cmd &
    fi
    if [ $1 == "secim2" ]; then
        $cmd &
    fidonewaitecho "hepsi tamam"
...
```

Ustteki script icinde surec kontrolu (job control) icin -m en basta
tanimlanmali.

Diyelim ki bu script'e verilen birinci secenek (yani $1) ile bir
string veriyoruz, ve string uzerinde if..else irdelemesi yapilarak
script farkli seyler yapabiliyor. Ayrica baz bir komuta ek string'leri
dinamik olarak ekleyerek dinamik bir komut olusturabiliyoruz. Isletmek
icin mesela

bash test.sh secim1 2

Ikinci secenek ile (yani $2) ile bir sayi veriyoruz diyelim, bu sayi
kadar bir dongu baslatabiliriz, ve dongude her indis icin baska "alt
komutlari" arka planda isletebiliriz. Birinci secenek her komut icin
xterm baslatmak, ya da pur arka planda "sessiz olarak" isletmeyi
kontrol ediyor olabilir.

Bash surec kontrolunun guzel tarafi, mesela wait komutu ile bu arka
plana atilmis tum surecler bitene kadar ana script'in bekleyebilme
ozelligidir. Belki tum alt komutlar bitince bazi ek birlestirme,
vs. islemleri yapilacaktir. Onlari yapmanin en uygun yeri burasi, yani
hemen wait sonrasi. Bunu test etmek icin su script'e bakalim

```
set -m # Enable Job Controlecho "1 saniye bekle..."sleep 1 &echo "3 saniye bekle..."sleep 3 &waitecho "hepsi tamam"
```

Bu script isletilence 3 saniye bekleme olacaktir, 1 saniyelik "uyku
(sleep)" komutu bitmis olsa bile 3 saniyelik olanin bitmesi
beklenecektir. Ve (and), yani & isareti bilindigi gibi Unix'te bir
komutu arka plana atmaya / isletmeye yarar (background process).





