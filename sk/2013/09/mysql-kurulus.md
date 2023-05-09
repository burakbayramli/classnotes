# MySQL Kurulus

MySQL Ubuntu kurulusu

```
sudo apt-get install mysql-server
```

Baglanmak (kendi makinanizdan)

```
mysql --host=localhost --user=root --password=[sifre]
```

Kurulurken root icin sifre vs istenir, fakat bu kullanici disaridan
kullanis sirasinda problem cikartir. Makina ici ve disindan
kullanilacak bir kullanici yaratmak icin (suradan)

```
CREATE USER 'monty'@'localhost' IDENTIFIED BY 'some_pass';GRANT ALL PRIVILEGES ON *.* TO 'monty'@'localhost'
    WITH GRANT OPTION;CREATE USER 'monty'@'%' IDENTIFIED BY 'some_pass';GRANT ALL PRIVILEGES ON *.* TO 'monty'@'%'
    WITH GRANT OPTION;
```

Taban Dizini

Eger veri tabanlarinin tutuldugu dizini degistirmek istiyorsaniz,
/etc/mysql/my.cnf ayar dosyasindaki dataDir dizini degismeli. Hatta
mevcut tabanlarin verisini kaybetmeden sadece dataDir'deki dizini
oldugu gibi yeni yere kopyalayip, yeni dizinden ise devam etmek
mumkun. Dikkat, kopyalanan yeni yerdeki erisim haklari mysql
kullanicisina olmali. Tabii ondan once sudo service mysql stop ile
servisi durdurmayi unutmayin.






