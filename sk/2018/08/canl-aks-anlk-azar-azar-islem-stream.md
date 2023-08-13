# Yerel, Network Dosyasını Canlı, Anlık, Azar Azar Islemek (Stream, Incremental File Processing),  Python

Bir dosyayı hızlı bir şekilde canlı olarak, teker teker satır okuyacak
şekilde işlemek için csv paketi faydalı. Dikkat bu paket standard
Python'un yaptığı gibi önce dosyanın tümünü hafızaya alıp sonra oradan
readlines() ile teker teker satır servis etmiyor. Dosyanın her satırı
hakikaten Python tarafından teker teker alınıyor.

```python
import csv

with open('[DOSYA]') as csvfile:
    rd = csv.reader(csvfile,delimiter=' ')
    headers = {k: v for v, k in enumerate(next(rd))}
    for row in rd: print (row[headers['kolon1']])
```

Değişken headers içinde dosyanın ilk satırından alınan kolon isimleri
var (bu şart değil, atlanabilir) delimiter kolon ayracının ne olduğunu
kontrol eder.

Eger bir zip dosyasi icinden okuma yapmak istiyorsak,

```python
import zipfile, csv, io
zfile = '/filanca/dizin/dosya.zip'
zip_file    = zipfile.ZipFile(zfile)
items_file  = zip_file.open('dosya.csv') # dosya.zip icinde olmali
items_file  = io.TextIOWrapper(items_file)
rd = csv.reader(items_file)
headers = {k: v for v, k in enumerate(next(rd))}
print (headers)
for row in rd: print (row[headers['KOLON1']])
```

Gzip ile sıkıştırılmış tar dosyaları bile satır satır okunabilir,

```pythn
with tarfile.open(dir + "dosya.tar.gz",mode="r:gz") as tar:
    for member in tar:
        if member.isreg():      # Is it a regular file?
            csv_file = io.StringIO(tar.extractfile(member).read().decode('ascii'))
            rd = csv.reader(csv_file)
            headers = {k: v for v, k in enumerate(next(rd))}
            for row in enumerate(rd):
                print (row[headers['KOLON1']])
```

Network üzerinden akış işlemi için `smart_open` var,

```python
from boto.s3.connection import S3Connection
import smart_open
conn = S3Connection('[aws_access_key_id]', '[aws_secret_access_key]')
for line in smart_open.smart_open('s3://bir/dizin/dosya.txt'):
    print (line)
```

Bu kütüphane sadece S3 ile de sınırlı değil. HDFS, HTTP, envai türden
protokol üzerinden okuma yapabiliyor. Eğer yerel dosya ismi verirsek
onu da okuyor. Sıkıştırılmış dosyaları açıp onları anlık olarak
işliyor, vs. Aslında ilk başta bu paketi kullanmak daha iyi herhalde,
pek çok farklı işi yapabiliyor.

https://pypi.org/project/smart_open/






