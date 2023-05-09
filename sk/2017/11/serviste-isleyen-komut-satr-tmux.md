# Serviste İşleyen Komut Satırı - tmux

Bir Unix servis makinasına giriş yaptık, uzun süreli bir süreç
(process) isletmek istiyoruz, ama giriş yapılan komut satırını açık
bırakmak istemiyoruz (dizüstü kapatılacak, kalkılıp gidilecek, vs).

İlk seçim komutu arka plana atarak (background process) olarak işletmek, 

```
nohup sh komut.sh > /bir/log/dizini/log.out & 
```

`&` işareti ile süreç arka plana atıldı, çıktılar log dosyasına gidiyor.

Bir diger secim tmux. Kurmak icin

```
sudo apt-get install tmux
```

Tmux ile oturum acmak icin

```
tmux
```

Bu oturumlar makinadan çıkılsa bile açık dururlar, kıyasla ssh, telnet
ile giriş yaptığımızda yeni bir komut ortamına gireriz, çıkınca da
ortam bitirilir, tmux durumunda komut ortamı sürekli açık
kalır. Üstteki komut bizi bir pencereye götürür, orada uzun sürecek
işlemi başlatabiliriz, ve işlem devam ederken 

```
CTRL-b d
```

ile çıkabiliriz. Makinadan da çıkabiliriz, işlem devam edecektir,
makinaya tekrar ssh ile girince

```
tmux list-sessions
```

ile oturumlari listeleyebiliriz. Mesela "0" oturumu var, 

```
tmux attach -t 0
```

ile aynı ekrana bağlanırız, işleyen programı tekrar görürüz.

Diğer komutlar için alttaki kaynağa bakılabilir,

Kaynak

https://www.digitalocean.com/community/tutorials/how-to-install-and-use-tmux-on-ubuntu-12-10--2









