# Otomatik Program Başlamak, Unix, Linux

### Unix, Servisleri Nasıl Başlatıyor?

Bazı programların bilgisayar açıldığı zaman "otomatik" olarak
başlamasını istiyor musunuz? Mesela, bir Linux makinasını tamamen
Oracle icin ayırdınız, ve Oracle, bilgisayar açılır açılmaz
başlamalı. Sistem idarecisi olarak her seferinde oracle kullanıcısına
girip, kaldır/indir yapmak istemiyorsunuz..

Windows dünyasında (Borg dünyasına benzer), Servisler (Services) denen
kavramın benzeri lazım yâni.. Windows bunlari Unix'ten almıştır tabii,
o yüzden bu kavramın aynısını bulmakta zorluk çekmeyeceğiz..

Unix'te bu işler ayartanım dosyaları ile yapılıyor. Biliyorsunuz
Unix'te bütün idare işleri bir "metin dosyasına" bağlıdır. Böyle
olması çok iyidir çünkü metin değiştirici programlar Perl ve Ruby ile,
sistem idarecileri birçok şeyi aynı anda ve otomatik olarak
yapabilirler.

Ayar dosyalarının nerede olduğuna gelelim..

Unix'te önemli dizin yerleri vardır. Mesela `/etc/` bunlardan
biridir. Çogu Unix versiyonu, `/etc/` altına onemli ayartanım
dosyalarını koyar. Yâni, bir script ile `/etc/` altındaki dosyaları
değiştirirseniz, sistemin işleyişi değişecektir. Dikkatli olun yani.

İndir/kaldır ayar dosyaları `/etc/rc.d` dizini altındadır. Buraya
girip `ls -al` işletirseniz, aşağıdaki tabloyu görebilirsiniz.

```
drwxr-xr-x   10 root     root         4096 Sep 15 01:09 .
drwxr-xr-x   43 root     root         4096 Sep 15 01:23 ..
drwxr-xr-x    2 root     root         4096 Sep  6 15:51 init.d
-rwxr-xr-x    1 root     root         3219 Jul 10  2001 rc
drwxr-xr-x    2 root     root         4096 Apr  4 13:25 rc0.d
drwxr-xr-x    2 root     root         4096 Apr  4 13:25 rc1.d
drwxr-xr-x    2 root     root         4096 Apr  4 13:25 rc2.d
drwxr-xr-x    2 root     root         4096 Apr  4 13:25 rc3.d
drwxr-xr-x    2 root     root         4096 Apr  4 13:25 rc4.d
drwxr-xr-x    2 root     root         4096 Apr  4 13:25 rc5.d
drwxr-xr-x    2 root     root         4096 Apr  4 13:25 rc6.d
-rwxr-xr-x    1 root     root         3200 Sep 15 01:08 rc.local
..
..
```

rc kelimesinden sonra gelen sayı, "başlama seviyesini" belirtir. Bir
Linux makinasını değişik kiplerde (modda) başlatmanız
mümkündür. Meselâ çok önemli bir sistem bakımı gerekiyor ve bu bakım
yapılırken hiçbir kullanıcının sisteme bağlanamaması lâzım. Başlama
seviyesi 0 yada 1, bu iş için kullanılabilir.

Sistemi 4. başlama seviyesinde başlatmak demek, rc4.d altına girip,
oradaki başlatma scriptlerini işletmektir. Şimdi rc4.d altında ne var
görelim.

```
lrwxrwxrwx    1 root     root           14 Apr  2 13:36 K74ntpd
lrwxrwxrwx    1 root     root           16 Apr  2 13:38 K74ypserv
lrwxrwxrwx    1 root     root           16 Apr  2 13:38 K74ypxfrd
lrwxrwxrwx    1 root     root           15 Apr  2 13:34 S05kudzu
lrwxrwxrwx    1 root     root           18 Apr  2 13:34 S08ipchains
lrwxrwxrwx    1 root     root           18 Apr  2 13:34 S08iptables
lrwxrwxrwx    1 root     root           17 Apr  2 13:34 S10network
lrwxrwxrwx    1 root     root           16 Apr  2 13:33 S12syslog
lrwxrwxrwx    1 root     root           17 Apr  2 13:36 S13portmap
lrwxrwxrwx    1 root     root           17 Apr  2 13:37 S14nfslock
lrwxrwxrwx    1 root     root           18 Apr  2 13:33 S17keytable
lrwxrwxrwx    1 root     root           16 Apr  2 13:34 S20random
lrwxrwxrwx    1 root     root           15 Apr  2 13:34 S25netfs
lrwxrwxrwx    1 root     root           14 Apr  2 13:34 S26apmd
```

`ipchains`, `syslog` gibi programlar tanıdık gelebilir. Bu
programların başlatıldıkları yer burası işte! Evet.. işin püf
noktasına iyice yaklaşıyoruz değil mi?

Bir kavram daha kaldı: Bazi scriptlerin "K" ile, ötekilerinin "S" ile
başladığını görüyorsunuz. Bunun sebebi nedir?

`S` "Start" icin `K` "Kill" için kullanılır, yani Başlat ve Durdur
komutları için. Eğer Unix sistemi, rc4.d altındaki servisleri
başlatmak istiyorsa, önce, `rc4.d` altinda `ls S*` benzeri bir komut
işletecektir. Bu komut sadece "S" ile başlayan scriptleri
toplar. Sonra Unix sistemi, bu scriptleri teker teker "start"
kelimesini ekleyerek cağırır.

Aynı şekilde sistem kapanırken, Unix `ls K*` benzeri komut işletip,
durdurmak için gerekli scriptleri toplar, ve onları "stop" kelimesini
ekleyerek cağırır. Bu kadar.

Ayrı bir not: Niye çağirim yaparken "start" ve "stop" eklemek
gerekiyor?

Bunun sebebini script içeriğine baktığımızda göreceğiz.

```
..
..
start() {
     echo -n $"Kaydediciyi baslatiyoruz.. "
..
..
stop() {
     echo -n $"Kaydediciyi durduruyoruz.. "
..
..
case "$1" in
start) ;; eger Unix start kelimesi gondermis ise
     start
     ;;
stop)
     stop eger Unix stop kelimesi gondermis ise
     ;;
status)
     rhstatus
     ;;
restart|reload)
     restart
     ;;
..
..
```

### systemd

Eğer kendi yazdığımız bir program RPi (ya da herhangi bir Ubuntu)
başlayınca otomatik başlasın istiyorsak `systemd` yöntemini
kullanabiliriz [5]. Örnek olarak mesela bir python Flask servisinin
başlatılmasını istiyoruz, program `/home/pi/run.py` altında. Bir
`systemd` ayar dosyası yaratmak lazım, ona `flask` ismi verelim,

```
sudo nano /etc/systemd/system/flask.service
```

İçine sunları yazabiliriz,

```
[Unit]
Description=Flask Service
After=multi-user.target

[Service]
Type=idle
User=pi
ExecStart=/home/pi/myenv/bin/python3 /home/pi/flask.py
Restart=no

[Install]
WantedBy=multi-user.target
```

```
sudo chmod 644 /etc/systemd/system/flask.service
```

```
sudo systemctl daemon-reload
sudo systemctl enable flask.service
```

RPi tekrar başlayınca servisimizin işletildiğini göreceğiz.

Dikkat direk sistem seviyesindeki Python'u değil `/home/pi/myenv`
altındakini işlettik, bu geliştirme ortamı virtualenv ile alakalı,
detayları o yazıda bulabiliriz.

### supervisord

Unix'te süreç (process) isletimi, onların takibi için faydalı bir
program, süpervisord. Bu tür programların en çok aranan
özelliklerinden biri eğer takip ettikleri program çökmüş ise onu
tekrar başlatabilmeleri. Çok uzun süren bir toptan işleyici (batch)
programı mesela tekrar başlatılabilir (restartable) yapabilirsiniz, ve
eğer bir şekilde çökerse programı başlatınca kaldığı yerden devam
eder, ve çöken programı tekrar başlatacak olan şey süpervisord gibi
bir program olacaktır.

http://supervisord.org/index.html

Kurmak için en rahat yol

```
sudo easy_install supervisor
```

Alttaki


```
echo_supervisord_conf
```

komutu ile örnek bir ayar dosyası ürettirebilirsiniz.

Bir ornek

```
[supervisord]logfile=/tmp/supervisord.log ; (main log file;default $CWD/supervisord.log)logfile_maxbytes=50MB
        ; (max main logfile bytes b4 rotation;default 50MB)
logfile_backups=10
           ; (num of main logfile rotation backups;default 10)
loglevel=info
                ; (log level;default info; others: debug,warn,trace)
pidfile=/tmp/supervisord.pid ;
(supervisord pidfile;default supervisord.pid)nodaemon=false[program:mytest]
command=python -u /home/burak/data/scripts/test_supervisord.pyautorestart=trueuser=postgresredirect_stderr=truestdout_logfile=/tmp/test_supervisord.log
```

Bir mytest adında programı başlatıp kontrol ettiriyoruz, log dosyasını
tanımlıyoruz, autorestart=true ile hatalı şekilde süreç çökerse tekrar
başlat diyoruz, ve "hangi kullanıcı üzerinden" alt programın
başlatılmasını gerektiğini bile tanımlayabiliyoruz. Bu durumda
süpervisord root olarak başlatılmalı tabii, yoksa istediği diğer
kullanıcıya geçiş yapamaz.

Baslatmak icin 

```
sudo supervisord
```

komutu yeterli.

Kontrol edilen tüm programları (süpervisord ile beraber) oldurmek için
`supervisord`'nin PİD'ini ps -eaf | grep süpervisord ile bulun, ve

```
sudo kill -SIGTERM [PID]
```

Not: Yukarıdaki program tanımında python'u -u seçeneği ile kullandık,
bu seçenek ile "print" gibi stdout'a çıkan ifadeler derhal "dışarı
atılıyor (flushed)". Bu yapılmazsa çocuk süreç işliyor ve ekrana mesaj
basıyor olsa bile log'da hiç bir şey gözükmeyecektir.


