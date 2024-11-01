# Görüntü Dosyaları, PDF, Birleştirme, Kitap

Kağıt halinde olan dokümanları nasıl birleşmiş, kitaplaşmış bir PDF
haline çeviririz?

Android bazlı telefon ile görüntüler alınacak, fakat paketten çıkan
foto programı istenmeyen, rasgele şekillerde fotoğrafta döndürme
yapabiliyor. Çözüm yeni bir uygulama kullanmak, `HD Camera`.
Görüntüler alındıkça `DCIM/HDCamera` adlı bir dizine yazılır.
Her dosya isminde zaman damgası vardır, bu sebeple listelemede en
son foto dosyası hep en sondadır.

Çekilmiş tüm fotoları dizüstü bilgisayara aktarmak için önce zip
sıkıştırması yapmak lazım, `ZArchiver` kullanalım.

Dizüstü bilgisayara dosyalar kodlayacağımız yükleme (upload) özelliği
ile yapılabilir, ikinci bilgisayarda bir web uygulaması işletiriz,
Flask üzerinden, oradaki özellik dosya gönderimi sağlar [1],

```python
@app.route('/upload', methods = ['GET', 'POST'])
def upload_file():
   if request.method == 'POST':
      f = request.files['file']
      print ("uploading", session['upload_dir'] + "/" + f.filename)
      fout =  session['upload_dir'] + "/" + f.filename
      f.save(fout)     
      return 'file uploaded successfully'
   return "OK"
```

Foto çekimi bitince ZIP dosyası üstteki kodla gönderilir. Son işlem
dizüstü ortamında olur, bir Python script zip içindeki tüm dosyaları
alıp, küçültüp, PDF haline getirir, yeni bir dizine yazar. Ardından
`pdftk` ile birleştirim yapılır.

```python
z    = zipfile.ZipFile('dosya.zip')
files_orig = list(z.namelist())
for x in files_orig:
    if ".jpg" not in x: continue
    print (x)
    f  = z.open(x)
    content = io.BytesIO(f.read())
    with open("/tmp/out.jpg", "wb") as outfile:
        outfile.write(content.getbuffer())
        outfile.flush()        
    cmd = "convert  -rotate 90 -scale 800 /tmp/out.jpg %s.pdf" % (x)
    os.system(cmd)
```

Görüntü işlemek için ImageMagick kullandık, her sayfayı 90 derece sağa
döndürüyoruz (foto programından öyle geldi), gerekirse ek işlemler bu
noktada kodlanabilir. Mesela sayfanın sağında solunda fazla boş
bölgeler varsa `-shave 50x0` gibi bir seçenek o işlemi yapar, resim
üzerinde keskinlik (sharpening) yapılabilir, pek çok diğer kabiliyet
mevcut [3].

Üstteki kodda zip dosyasının içeriğinin gezilmesine dikkat, işlemden
önce tüm zip dosyasını açıp içindeki dosyaları diske yazmıyoruz,
gerekli bilgiyi zip içinde gezerken Python `zipfile` ile
alabiliyoruz. Imagemagick işlemi öncesi ham görüntü dosyası sadece
`/tmp/out.jpg` içinde, yeni görüntü dosyası, ayrı bir dosya olarak
gerekli işlemler bittikten sonra yazılıyor. Böylece yerden muhafaza
ediyoruz.

Yeni görüntüler oluşunca `pdftk` ile birleştirmeyi yapıyoruz [2]. 

Kitaplaşmış PDF dosyası içinde CTRL-F ile arama yapabilmek istiyorsak,
`ocrmypdf` ile bunu da yapabiliriz, gerekli metin bilgisi PDF içine bu
program tarafından eklenecektir. Hatta bu sayede doküman herhangi bir
indeksleyici program tarafından indekslenebilir hale gelir.

Kaynaklar

[1] <a href="../../2024/06/webfilebrowser.html">Flask ile Web Üzerinden Dizin Gezebilmek - webfilebrowser</a>

[2] <a href="../../2011/12/pdftk.html">PDFTK</a>

[3] <a href="../../2010/08/imagemagick.html">ImageMagic</a>



