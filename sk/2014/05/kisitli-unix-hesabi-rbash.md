# Kisitli Unix Hesabi - rbash

Proje icinde arkadaslara buyuk dosyalari dizin makinanizdan scp /
rsync ile almasina izin vermek isteyebilirsiniz; bunun icin Ubuntu'da
yeni bir hesap yaratmaniz yeterli. Fakat bu hesapta her turlu isi
yapmasina izin vermek istemezsiniz belki (makina sizin nihayetinde). O
zaman, mesela kullanici dosyalar adinda bir kullanici olsun,
/etc/passwd altinda bu kullanicinin komut satir programini (shell)
tanimlayan yeri /bin/sh yerine /bin/rbash yaparsiniz.

Rbash, kisitili (restricted) bir shell ortami sunar. Kisi sisteminize
girdikten sonra mesela kendi ev dizininin ustune cikamaz. Boylece daha
guvenli bir ortam saglanir. Ama dosya indirmesi hala mumkun olur,
dersiniz ki

```
scp -r dosyalar@10.1.1.210:/home/dosyalar/filan /tmp
```

ile /home/dosyalar/filan altindaki herseyi alinabilir (sifre de
paylasilir, nasil olsa dosya alma haricinde bu hesap baska bir ise
yaramayacak). Hesap acmak yerine bir web servisi kurulabilirdi, oradan
dosya servis edilebilirdi vs, fakat bu daha uzun is olabilir, ayrica
ustteki yontem ile "sadece farklari / degisiklikleri" indirilmesini
saglayacak rsync kullanilabilir.






