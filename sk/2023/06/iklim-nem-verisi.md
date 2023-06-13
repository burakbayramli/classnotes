# İklim, Nem Verileri

Havanın nemlilik oranı ay bazlı tarihsel olarak [1] ya da anlık olarak [2]
bağlantısındaki metotlarla alınabilir.

Önce tarihsel verilere bakalım. [1]'den indirilebilecek bazi dosyalar
`HadISDH.landq` ile ya da `HadISDH.landRH` ile baslayan dosyalar,
bunlar sirasiyla bir kg islak hava icinde ne kadar g bazinda buhar
oldugunu gosteren g/kg birimindeki spesifik nemlilik `q`, ya da birim
hacimdeki buharin o sicaklikta tutulabilecek maksimum buhara olan
yuzde olarak oranini gosteren izafi nemlilik `rh`.

Veri indirilip

```python
import csv, pandas as pd, re
fin = open("/tmp/HadISDH.landq.4.4.0.2021f_FLATgridHOM5by5_anoms9120_actuals.dat")
rd = csv.reader(fin)
fout = open("/tmp/q.csv","w")
for i in range(47*12):
    print ('i',i)
    date_line = next(rd)
    date_line = date_line[0].split(" ")
    dt = date_line[1] + "-" + date_line[0]
    dt = pd.to_datetime(dt)
    for j in range(36):
        print ('j',j)
        line = next(rd)
        line = line[0]
        line = re.split('\s*',line)
        line = line[1:]
        line = [str(dt.year), str(dt.month)] + line
        res = ";".join(line)
        res = res.replace("-9999.99","")
        fout.write(res)
        fout.write("\n")
        fout.flush()
    fout.flush()
```

ile işlenebilir. Okunan dosya formatı biraz garip aslında, tarihler ay
olarak bir blok üzerine düz tarih olarak yazılıyor, ve dosyanın en
sonunda her bloğun kolonlarının ve satırlarının tekabül ettiği enlem,
boylam ızgara noktaları verilmiş. Biz Pandas gibi ürünlerle çalışmaya
alışık olduğumuz için her satırda gerekli bilgiyi içeren normalize
edilmiş verilerle iş yapmaya daha alışkınız, bizim ürettiğimiz
çıktının formatında sene/ay bilgisi ayrı kolonlarda, ve her satırda
veriliyor, tarih başlık değil ve en sondaki iki satır atlanıyor.
Böylece sene/ay için filtreleme yapılınca gerekli veri bloğu alınır,
dünyanın belli noktalarını temsil eden bu blok 72 x 36 boyutlu bir
matris olacaktır. O matristeki hücrelerin hangi enlem/boylamlara
tekabül ettiğini kod içine sabit değerler olarak gömebiliriz, veriye
dahil etmeye gerek yok. Enlem değerleri -/+87.5 arasında eşit aralığa
bölünmüş 36 satırda, boylam değerleri ise -+177.5 arasında 72 aralığa
bölünmuş kolonlarda olacak.

```python
import util 
year = 2019; month = 8
df = util.get_pd().read_csv('/tmp/q.csv',sep=';',header=None)
df = df[(df[0] == year) & (df[1] == month)]
df = df.iloc[:,2:]
x,y = np.meshgrid(np.linspace(-177.50,177.50,72),np.linspace(-87.50,87.50,36))
util.get_sm().plot_continents(0, 20, zoom=18, incolor='green', outcolor='white', fill=False)
plt.pcolormesh(x,y,df,cmap='Blues')
plt.savefig('iklim01.jpg',quality=40)
```

![](iklim01.jpg)

2019 Ağustos ayındaki dünya bazlı nem verisi `q` grafi üstte.

Fakat eldeki verininin pek detaylı olmadığını biliyoruz çünkü 72 x 36 boyutlu
bir izgara pek çok alanı atlamış olacaktır. Mesela TR üzerinde odaklanırsak,

```python
year,month = 2019,8
df = util.get_pd().read_csv('/tmp/q.csv',sep=';',header=None)
df = df[(df[0] == year) & (df[1] == month)]
x,y = np.meshgrid(np.linspace(-177.50,177.50,72),np.linspace(-87.50,87.50,36))
df = df.iloc[:,2:]
df = np.array(df).flatten()
xx = x.flatten()[np.isnan(df)==False]
yy = y.flatten()[np.isnan(df)==False]
zz = df[np.isnan(df)==False]

q = util.get_qti()(xx,yy,zz)
interp = np.vectorize(q.interpolate,otypes=[np.float64])
zi = interp(x, y)

util.get_sm().plot_continents(40, 35, zoom=1, incolor='red', outcolor='white', fill=False)
plt.xlim(26,44)
plt.ylim(35,42)
plt.pcolormesh(y,x,zi,cmap='Blues')
plt.savefig('iklim02.jpg',quality=40)
```

![](iklim02.jpg)

gibi bir sonuç alıyoruz.

O gunun verisini kullanarak daha detayli nemlilik verisini OpenWeatherMap
ile alabiliriz [2].



[devam edecek]

Kaynaklar

[1] https://www.metoffice.gov.uk/hadobs/hadisdh/downloadLAND.html

[2] ../../2017/09/meteoroloji-verileri.md-ecmwf-noaa-openweathermap.html

