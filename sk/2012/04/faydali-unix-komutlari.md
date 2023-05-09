# Faydalı Unix Komutları

Süreçleri listelemek,

```
ps -eaf
```

Listedeki ikinci kolon süreç kimliği (PİD). Onu alıp

```
kill -9 [PID]
```

ile sureci yokedebiliriz.


Hangi süreç (process) port 8080'i dinliyor?

```
netstat -n --tcp -p -l |grep ":8080"
```

Sadece bastaki satiri gostermek

```
head -1 [DOSYA]
```

Bir dosyanin son N satirini atlamak / gostermemek (silme amacli
olabillir), mesela son iki satir icin

```
head -n-2 [DOSYA]
```

Bir dosyanin ilk satirini atlamak icin

```
tail -n+2 [DOSYA]
```

Bir komut çıktısını text olarak başka bir komut içine gömmek

`` kullanilir. Mesela  

```
echo "b.txt" > a.txt
echo "filan" > b.txt
echo "falan" >> b.txt
```

Yani `a.txt` içinde `b.txt` yazıyor, şimdi 

```
wc -l `cat a.txt`
```

dersek, `a.txt` icindeki `b.txt` arguman olarak `wc`'ye verilecek. Ve

```
2 b.txt
```

sonucu gelecek. Yani burada `a.txt` degil onun icindeki `b.txt` `wc`'ye gecildi.

Eğer bir Ünix script'inizin `.başhrç` içinde tanımladığınız alias
(kısayolları) kullanmasını istiyorsanız, bu script'i şöyle işletmeniz
gerekir:

```
bash -i [script]
```

Emacs icin benzer durum, `M-x compile` alias'lari gormuyor. Onun icin

```
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")
```

tanımlamak lazım.

Sonsuz kere bir komutu işletmek için, mesela süreç listesini sürekli
göstermek

```
while true; do ps -eaf; sleep 2; done
```

Belli satırlar arasındaki satırları çekip çıkartmak, mesela 10 ile 20

```
sed -n 10,20p [DOSYA]
```

Rasgele satırlar çekip çıkarmak (.005 bir oran, daha fazla satır için
bu sayı arttırılabilir)

```
cat [DOSYA] | perl -n -e 'print if (rand() < .005)'
```

Dosya içinde metin aramak (`/usr` seviyesinden aşağı doğru inerek, tüm
altdızinlerde, xml soneki taşıyan dosyalarda)

```
find /usr -name '*.xml' -exec grep -l 'filan' {} \;
```

Dosya içinde kelime değiştirmek (xml sonekli dosyalarda, altdizinlere girmez)

```
perl -pi -e 's/filan/falan/sg' *.xml
```

Üsttekinden daha iyisi `replace.py` diye ufak bir Python script yazmak,

```python
import os, re, sys
filename = sys.argv[1]
content = open(filename).read()
fout = open(filename,"w")

content = content.replace("burak","bayramli")
fout.write(content)
fout.close()
```

Üstteki script'i `python replace.py [dosya]` şeklinde çağırabiliriz. Bu
tek dosya üzerinde değişim yapar. Eldeki content üzerinde her türlü
regex, vs. bazlı işlemleri yapabiliriz. Hatta gerekirse başa, sona
istediğimiz metni ekleyebiliriz.

Eğer birçok dosya üzerinde üstteki değişimi yapmak istersek,

```
find .-type f -name "*" -exec python replace.py {} \;
```

Bu tür kullanımda `{} \;` ile alınan dosyalar teker teker Python'a geçiliyor,
dosya ismi `sys.argv[1]` içinde olmak üzere.

Üstteki türden kullanımı daha akıllıca bir `grep` için de
kullanabilirdik.  Mesela her dosya içinde çetrefil bir regex üzerinden
bir arama yapmak istersek bu yapılabilir. Bir kere içinde şu tür
ifadeler içeren dosyaları aramak gerekmişti, "bir satırda tek başına
iki dolar işareti, ardındaki satırda tek başına eşittir işareti olan
tüm dosyalar". Unix'te `find` ile alttaki koda çağrı yaptım,

```python
res = re.findall('^\$\$\s*\n=\s*\n',open(sys.argv[1]).read(), re.MULTILINE)
if len(res)>0:
    print (sys.argv[1], res)
```


Dosyayı kullanan program hangisi

lsof [DOSYA]

Bir dizinden başlayarak özyineli (recursive) olarak o dizin altındaki
en büyük dosyaları listelemek için

```
du -h | sort -hr | head -20
```

Bir makinadaki tüm donanımları listele (Ubuntu)

```
lspci
```

Harici sabit diskte bazı dizinlerimin yedeğini tutuyoruz, ve iki yerel
dizini senkronize etmek için

```
rsync --delete -r /tmp/dira/ /tmp/dirb
```

Üstteki rsynç komutu kaynak dizini "belirleyici (master)" olarak kabul
edecektir, ve /tmp/dira içinde olan her şey, yeni dosya, mevcutun
silinmesi, ekleme işlemi aynen /tmp/dirb'ye yansıtılacaktır. Not: Eğer
/tmp/dirb FAT32, NTFS gibi Windows temelli bir dosya sistemi ise azar
azar güncelleme ise yaramaz, tüm dosyalar her seferinde tekrar
silbaştan yollanır. Bunun sebebi Win temelli dosya sistemlerinin dosya
sahibi, erişim hakları gibi bilgileri -Unix ile uyumlu- tutmuyor
olmasıdır.

Bir dosya içinde mesela her "dropbox" kelimesinden önce gelen 700
karakteri göster

```
grep -E -o ".{700}dropbox" dosya.txt
```

Programın sonuçlarını hem ekrana basmak aynı anda bir log dosyasına yazmak

```
sh script.sh  2>&1 | tee out.log
```

Buyuk Dosyalar

```
sudo du -hx --max-depth=1 .
```

Dizin bazında kullanım raporu verir.

Eğer dosya büyüklüğüne göre sıralı liste istersek (içinde olduğumuz dizinde)

```
sudo du -hb --max-depth=2 . | sort -n
```

