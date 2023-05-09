# Python ve MySql

Bilimsel hesaplama (scientific computing) icin Python araclarini
isledik. Bunlar SciPy, NumPy ve Matplotlib araclari idi. Isin veri
tarafina bakarsak; Eger hesaplanacak veri Mysql gibi bir tabanda ise,
bu tabandan verileri okuyabilmek gerekir. Bir hesap tipik olarak
gereken tum veriyi hafizaya getirip sonra vektor bazli hesaplara tabi
tutar. Islemcilerin gelecegi cok cekirdekli (multi-core) yonune dogru
gittigine gore, hesaplarin paralelize edilip bir sekilde asenkron
usulde isletmek te mumkun tabii.. Bazi Python araclari burada yardimci
olabilir; En alt seviyede cekirdekler arasinda mesajlasma icin MPI
standardi var mesela, ve burada yardimci olabilecek kutuphaneler
mevcut. Bu konulara ileride girebiliriz.Mysql'e baglanmaya gelelim. Bu
alanda en iyi bilinen MySql-Python paketidir. Kurma yontemi oldukca
standart, paketi indirin, acin, ve sudo python setup.py build ve
install. Bu paketi kullanmanin iyi yontemi var, bir tanesi Mysql
arayuzune bagli olarak, digeri ise standart Python DB-API arayuzu
kullanarak. Biz ikisini de gosterelim, ama bizim tercihimiz "mecbur
olunmadikca" standart arayuzdur, boylece yazilan kodlar diger tabanlar
uzerinde de isler.

```
import _mysql
db=_mysql.connect(host="localhost",user="kullanici",passwd="",db="taban")
db.query("""SELECT count(*) from tablo""")
r=db.store_result()
print r.fetch_row()
```

DB-API usulu

```
import MySQLdb
db=MySQLdb.connect(user="kullanici",passwd="",db="taban")
c=db.cursor()
c.execute("""SELECT count(*) from tablo""")
print c.fetchone()
```




