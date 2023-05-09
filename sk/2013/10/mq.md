# ØMQ

Makinalar, sureclerarasi mesaj gonderimi icin ØMQ (ya da ZeroMQ)
programi tavsiye edilir. Kullanimi soket kullanimina benzeyecek
sekilde yapilmis fakat arka planda mesajlarin hizli gonderilebilmesi
ve iletilmesinden emin olunmasi, tampon bellekte mesajlari biriktirmek
(gerekiyorsa) gibi bir suru ek kodlama yapilmis. Noktadan noktaya TCP
protokolu ile, ayrica UDP ya da multicast ile yayin bazli da
calisabiliyor.

Kurmak icin

```
sudo apt-get install libzmq-dev uuid-dev python-zmq
```

Not: Eger ustteki yeterli olmazsa (mesela bir assertion error
gelirse), kaynaklardan derlemek gerekebilir, libzmq Github'dan kaynak
indirilir, ./autogen.sh, ./configure, make ve sudo make install
gerekebilir. 

TCP kullanan noktadan-noktaya transfer ornegi

```
# alici 
import sys, time, zmqcontext = zmq.Context()
receiver = context.socket(zmq.PULL)receiver.bind("tcp://*:10000")
arr = []while True:
    data = receiver.recv()
    if data == "start":
        print time.time()
        continue
    arr.append(data)
    if data == "end":
        print time.time()
        print len(arr)
        continue

# gonderen
import zmq, timecontext = zmq.Context()
sender = context.socket(zmq.PUSH)
sender.connect("tcp://localhost:10000")
sender.send("start")
for i in range(100000):
    sender.send("01234567890123456789")
sender.send("end")
```

Eger gonderici baska bir makinada ise localhost yerine o makina ismi
(ya da IP adresi) kullanilir.

Uyelik / gonderim (publish, subscribe) kullanimi ve protokol bazinda
yayin kulllanan ornek (ki yayin bazli protokol kullaninca uyelik
kullanmak mecburi)

```
# alici 
import zmqcontext = zmq.Context()socket = context.socket(zmq.SUB)
socket.connect('epgm://wlan0;239.192.1.1:5000')
socket.setsockopt(zmq.SUBSCRIBE, "10001")
while True:
    data = socket.recv()
    print data

# gonderici
import zmq, time
context = zmq.Context()
socket = context.socket(zmq.PUB)
socket.connect('epgm://wlan0;239.192.1.1:5000')
while True:
    socket.send("10001 kjashfkjasdf")
    time.sleep(1)
```

Ornek pgm icin ayni, egpm yerine gpm kullanilir. Yanliz o pgm icin her
iki tarafta da sudo kullanmak gerekti. Bu biraz garip bir durum.

Ayrica egpm (ve pgm) nedir? Bunlar ek protokoller, pgm UDP'nin
guvenilirligini arttirmak icin yazildi. ZeroMQ openpgm uzerinden
bunlari kullanabiliyor. ZeroMQ sayfalarina gore pgm direk multicast,
egpm arka planda UDP kullanir.

UDP yayin bazli bir protokoldur, network'un tek bir adresine (mesela
0.0.0.0) mesajlari basarsiniz, bu mesajlari herkes gorur. Bunun fi
tarihinde bazi faydalari vardi, cunku 'zaten Ethernet (bir alttaki
protokol olmasi baglaminda) zaten mesajlari donanim olarak herkese
goturuyor, bunun ustune yayin bazli kullanim niye daha uygun
olmasin". Irdeleme boyleydi. Bunu yeni network donanimlarinda test
etmek gerekir, zaten pur UDP guvenilir bir protokol degildir (oldugu
gibi kullanilmasini tavsiye etmiyoruz). ZeroMQ her iki protokolu de
pgm uzerinden disari servis ediyor.

Ornekte ilginc bir kullanim goruyoruz, 239.192.1.1 adresi var
mesela. Internet adresleme kurallarina gore 224.0.0.0 ve
239.255.255.255 arasi multicast icin ayrilmistir, bu araliga
baglananlar disari, yani daha genis baglamda Internet'e aktarilmazlar
(routing). Multicast UDP'den daha populer bugunlerde, UDP'nin pabucu
dama atildi bir baglamda. Multicast'te "yayin gruplari" yaratiliyor
(ustteki aralikta bir adres) ve o adrese yazilan her mesaj, o adresi
okuyan herkes tarafindan alinabiliyor. Sistem bakimi (maintenance)
acisindan iyi bir sey, network'e istediginiz kadar makina ekleyin, bu
makinalarin sadece tek bir adresi bilmeleri yeterli.

Isleri biraz daha karistiralim, multicast mesaj gonderimi icin UDP
perde arkasinda UDP kullanabiliyor (fakat bu illa sart degil). Ayrica
bazi pur UDP orneklerinde yayin grubu kullanildigini gorduk.

Ustteki ornekte ayrica filtreleme kullanimi var, sadece 10001 ile
baslayan mesajlari almak istedigimizi belirtmisiz.

wlan0 nedir? Network donaniminin ismidir. Bu ismi ifconfig -a ile
bulmak mumkun.

Test ederken netstat -g ile o anda network'unuzdeki tum yayin
gruplarini gorebilirsiniz.

Not: bazi durumlarda ./configure cagrisini --with-pgm ile yapmak
gerekebilir.

