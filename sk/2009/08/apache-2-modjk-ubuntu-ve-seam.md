# Apache 2, mod_jk, Ubuntu ve Seam

JBoss onunde bir Apache Web Server programi hem yuk dagiticisi olarak
hem statik icerik saglayici olarak gorev yapsin istiyorsak, Ubuntu
Server uzerinde hem Apache 2 hem de mod_jk kurmak apt-get ile
mumkun. Kurmak ve ayar icin sunlar gerekli:Once

sudo apt-get install apache2 libapache2-mod-jk

Sonra /etc/apache2/conf.d/jk.conf dosyasi yaratin, sunlar
olsun:

```
JkWorkersFile /etc/apache2/jk-workers.properties
JkLogFile /var/log/apache2/mod_jk.log
JkShmFile /var/log/apache2/mod_jk.shm
JkLogLevel warn JkOptions
+ForwardKeySize
+ForwardURICompat
-ForwardDirectories
JkRequestLogFormat "%w %V %T"
JkMount /*seam* ajp13
JkMountCopy all
```

Yukarida *seam* ibaresine dikkat. Sadece *.seam demiyoruz, *seam*
diyoruz cunku Ajax cagrisi amaciyla EJB js kodlari dahil ederken bunu
seam/remoting/../remote.js gibi bir kelime kullanarak yapariz, ve bu
js dosyasi dinamik olarak "uretilen" bir dosya oldugu icin bu cagrinin
app server'a gitmesi lazim, Apache o isi halledemez. Eger sadece
*.seam deseydik, bu tur cagrilar JBoss'a gitmeyecekti.Simdi
/etc/apache2/jk-workers.properties dosyasi yaratin, sunlar
olsun:

```
worker.list=ajp13
worker.ajp13.port=8009
worker.ajp13.host=localhost
worker.ajp13.type=ajp13
worker.ajp13.lbfactor=1
```

Statik iceriklerinizin (imaj, css, html dosyalari gibi) hepsini
/var/www altina kopyalamaniz gerekiyor. Artiksudo /etc/init.d/apache2
restartve JBoss run.sh ile baslatin (-b [IP ADRESI] ibaresine gerek
yok cunku Apache arka planda JBoss ile localhost uzerinden konusuyor
zaten).Artik Seam sayfalarini gorebiliyor olmaniz lazim.Kaynak:





