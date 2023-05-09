# Video, Görüntü ve Ses Kaydını Web Sitesinde HTML ile Çaldırmak, Podcast 

Bir siteden HTML sayfaları üzerinden herhangi bir mp4, mp3, wav ya da
ogg ses ya da görüntü dosyasını nasıl servis ederiz? Bir podcast
servisi verilecek olabilir, ses kaydı mp3 dosyasında, dosya ise Google
Drive üzerinde.

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

HTML5

Bilindiği gibi yeni HTML standart HTML5 içinde sayfaya direk ses, ya
da görüntü gömülmesini ve oradan direk gösterilmesini sağlayan
etiketler var. Alttaki yöntem dosya Internet'te düz dosya olarak
erişilebilen tüm medya kayıtları için kullanılabilir, Dropbox, Google
Drive farketmez. 

Elimizde dosyayının URL bağlantısı olsun, bu bağlantıyı Google Drive
ile video ID alındıktan sonra onun başına

`https://drive.google.com/uc?export=view&id=`

ekleyerek elde edilebilir. Ya da düz dosya ismi vardır `https://site.com/content/dosya.mp4`
gibi. Bu dosyayı göstermek için 

```
<video width="350" controls>
    <source src="https://site.com/content/dosya.mp4" type='video/webm'>
</video>
```

ifadesi yeterli.

Kaynaklar

https://www.labnol.org/internet/google-drive-mp3-embed/2232/
