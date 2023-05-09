# Dosya Şifreleme (File Encryption)

### SSH 

SSH kullanarak Unix makinalarına giriş yapabiliyoruz, dosya
kopyalayabiliyoruz. Bu program arka planda açık / kapalı anahtar çifti
kullanıyor, ve yani bir anahtarla şifrelenen  mesaj bir diğeri ile
açılabiliyor, eğer mesajın benden geldiğine birini ikna etmek
istiyorsam o mesajı göndermeden önce gizli anahtarımla şifrelerim,
benim açık anahtarım herkes tarafından bilinir, mesajı alan açık
anahtarımı kullanarak mesajı açar, benden olduğuna emin olur. Ya da
direk bana mesaj göndermek isteyen mesajı göndermeden önce açık
anahtarımla mesajı şifreler, ben gizli anahtarımla mesajı açarım,
gizli anahtarımı kimseyle paylaşmam, bu sebeple bu mesajı benden başka
kimse okuyamaz.

Bu mesajlaşma tekniğini ssh altyapısı üzerinden nasıl kullanırız?
Çünkü programcılar çoğunlukla ssh kurmuş olurlar, `ssh-keygen -t rsa`
ile `$HOME/.ssh` altında açık / kapalı anahtarları vardır
(`id_rsa.pub` ve `id_rsa` dosyaları) başka şeylerle uğraşmasak olmaz
mı?

Bu mümkün. Diyelim ki birinin açık anahtarını biliyorum, bu anahtar bu
kişiye şifreli mesaj gönderilebilmesi için yayınlanır, mesela bana
mesaj gelmesi için siteme koyarım, onu herkes görür. Bu anahtar `mypub`
dosyasında diyelim. Onu `pkcs8` formatına çevirmek lazım,

```
ssh-keygen -f pub1 -e -m pkcs8 > pub1.pkcs8
```

Simdi mesaj `mesaj.txt` icinde diyelim,

```
cat message.txt | openssl rsautl -encrypt -pubin -inkey pub1.pkcs8 > message.enc
```

Böylece şifrelenmiş mesaj `message.enç` içinde olacak. Bu mesajı email ile
artık o kişiye gönderebilirim, çünkü birisi bu mesajı yolda 'yakalaşa' bile
içeriğini çözmesi çok zor olacaktır. 

Mesajı alan kişi onu çözmek için gizli anahtarını kullanabilir,

```
cat message.enc | openssl rsautl -decrypt -inkey ~/.ssh/id_rsa
```

Üstteki komut şifrelenmiş mesajın orijinal içeriğini gösterecektir. 

### Ccrypt

Emacs ile iyi entegre olmuş bir program `ccrypt`. Kurma için Ubuntu
üzerinde `apt install ccrypt` yapılabilir, Emacs için gereken dosya
[2]'deki zip, tar dosyalarında var, `ps-ccrypt.el`. Bu dosyayı alıp
diğer Emacs tanım dosyalarının olduğu dizine koyup ana tanım içine

```
(setq load-path (cons "[DIZIN]" load-path))
(require 'ps-ccrypt "ps-ccrypt.el")
```

eklemek yeterli. Artık `.cpt` ile biten dosyalar ccrypt ile
açılacaktır, sadece ilk seferde bir şifre sorulur, ardından, aynı
dosya Emacs buffer seviyesinde açık olduğu sürece takip eden kaydetme
işlemleri için şifre sorulmaz. Üzerinde çalışılan dosyayı sık kaydeden
kullanıcılar için (benim gibi) bu faydalı bir özellik.

Kaynak

[1] http://krisjordan.com/essays/encrypting-with-rsa-key-pairs

[2] http://ccrypt.sourceforge.net/


