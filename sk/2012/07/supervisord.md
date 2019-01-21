# supervisord


supervisord




Unix'te surec (process) isletimi, onlarin takibi icin faydali bir program, supervisord. Bu tur programlarin en cok aranan ozelliklerinden biri eger takip ettikleri program cokmus ise onu tekrar baslatabilmeleri. Cok uzun suren bir toptan isleyici (batch) programi mesela tekrar baslatilabilir (restartable) yapabilirsiniz, ve eger bir sekilde cokerse programi baslatinca kaldigi yerden devam eder, ve coken programi tekrar baslatacak olan sey supervisord gibi bir program olacaktir.

http://supervisord.org/index.html

Kurmak icin en rahat yol

sudo easy_install supervisor

Alttaki

echo_supervisord_conf

komutu ile ornek bir ayar dosyasi urettirebilirsiniz.

Bir ornek

[supervisord]logfile=/tmp/supervisord.log ; (main log file;default $CWD/supervisord.log)logfile_maxbytes=50MB        ; (max main logfile bytes b4 rotation;default 50MB)logfile_backups=10           ; (num of main logfile rotation backups;default 10)loglevel=info                ; (log level;default info; others: debug,warn,trace)pidfile=/tmp/supervisord.pid ; (supervisord pidfile;default supervisord.pid)nodaemon=false[program:mytest]command=python -u /home/burak/data/scripts/test_supervisord.pyautorestart=trueuser=postgresredirect_stderr=truestdout_logfile=/tmp/test_supervisord.log

Bir
 mytest adinda programi baslatip kontrol ettiriyoruz, log dosyasini 
tanimliyoruz, autorestart=true ile hatali sekilde surec cokerse tekrar 
baslat diyoruz, ve "hangi kullanici uzerinden" alt programin 
baslatilmasini gerektigini bile tanimlayabiliyoruz. Bu durumda 
supervisord root olarak baslatilmali tabii, yoksa istedigi diger 
kullaniciya gecis yapamaz.

Baslatmak icin 

sudo supervisord

komutu yeterli.

Kontrol edilen tum programlari (supervisord ile beraber) oldurmek icin supervisord'nin PID'ini ps -eaf | grep supervisord ile bulun, ve

sudo kill -SIGTERM [PID]

Not: Yukaridaki program taniminda python'u -u secenegi ile kullandik, bu secenek ile "print" gibi stdout'a cikan ifadeler derhal "disari atiliyor (flushed)". Bu yapilmazsa cocuk surec isliyor ve ekrana mesaj basiyor olsa bile log'da hic bir sey gozukmeyecektir.





