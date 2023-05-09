# Tablo Alanlari ve Postgresql

Eger buyuk bir taban ile calisiyorsaniz, bir sure sonra Postgresql'in
olagan (default) tanimladigi yerde (cogunlukla ana dizin '/' altinda)
bir sure sonra yerinizin bitmeye basladigini farkedebilirsiniz. Cozum,
veri tabanini yaratirken hangi diskin kullanilacagini Postgresql'a
soylemek.

Once bir tablo alani (tablespace) yaratirsiniz,

```
# create tablespace tablo_alan_ismi location '/filanca/dizin'
;

# create database taban_ismi tablespace=tablo_alan_ismi;
```

Artik taban dosyalari /filanca/dizin altina yazilacak. 





