# supervisord

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
[supervisord]logfile=/tmp/supervisord.log ; (main log file;default $CWD/supervisord.log)logfile_maxbytes=50MB        ; (max main logfile bytes b4 rotation;default 50MB)logfile_backups=10           ; (num of main logfile rotation backups;default 10)loglevel=info                ; (log level;default info; others: debug,warn,trace)pidfile=/tmp/supervisord.pid ; (supervisord pidfile;default supervisord.pid)nodaemon=false[program:mytest]command=python -u /home/burak/data/scripts/test_supervisord.pyautorestart=trueuser=postgresredirect_stderr=truestdout_logfile=/tmp/test_supervisord.log
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

Ekler

Bizim yazdığımız [dand](https://github.com/burakbayramlı/kod/tree/master/dand) adlı
program benzer işlevi gerçekleştiriyor.
