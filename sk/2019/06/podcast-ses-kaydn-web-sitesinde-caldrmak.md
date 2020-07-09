# Podcast, Ses Kaydını Web Sitesinde Çaldırmak

Bir siteden HTML sayfaları üzerinden herhangi bir mp3, wav ya da ogg
ses dosyasını nasıl servis ederiz? Bir podcast servisi verilecek
olabilir, ses kaydı mp3 dosyasında, dosya ise Google Drive üzerinde.

Bu durumda dosyayı GD'a yükleriz. Sağ tıklama ile Share seçilir,
"Anyone on the İnternet can find and view" seçimi yapılır. Bu bir URL
bağlantısı üretecek, bağlantıyı kopyalarız, şu formda olacak,

`https://drive.google.com/file/d/1234xyz/view?usp=sharing`

Bu bağlantıdaki `/view` metnini `/preview` haline getiririz, ve HTML'e

```
<iframe 
  frameborder="0" 
  width="400"     
  height="200"
  src="https://drive.google.com/file/d/1234xyz/preview">    
</iframe>
```

kodunu koyarız. Bu sayfayı ziyaret edince bir çalıcı (player)
göreceğiz, basit bir program, üzerinde Google logosu vs yok, ise
yarıyor, ileri sarma, ara verme gibi özellikleri var.

Peki çalıcının HTML kodu nereden geliyor diye merak edilebilir, sonuca
Javascript dahil etmedik, üstteki kod parçası oldukca basit. Kod
URL'in kendisinden geliyor, `/preview` gerekli HTML'i yaratıyor.

Bu kadar. Dosya Google Drive'da ona referans eden çalıcı herhangi bir
sayfada.

Kaynaklar

https://www.labnol.org/internet/google-drive-mp3-embed/2232/