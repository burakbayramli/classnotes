# Javascript, Frame, Konuşma (Speech), Tekrar Yükleme

Bir sürü numaradan burada bahsediyoruz. Diyelim ki Flask bazlı sayfa
var, bir URL ziyaret edilince gösterilen bilgilerin sesli okunmasını
istiyoruz, ve her güncellemede bilgiler değişiyor. Önce okuma: alttaki
Javascript kodları sesli okumayı yapar, Github deposu içinden
`responsivevoice.js` alınır, kendi projenize koyulur,

https://github.com/ResponsiveVoice/ResponsiveVoice

Örnek kod

```
<script src="/static/responsivevoice.js" type="text/javascript" charset="utf-8"></script>
<script type="text/javascript" charset="utf-8">      
function speak() {
  s1 = 'hello how are you'
  responsiveVoice.speak(s1);
}
</script>
```

Fakat sayfayı tekrar yükleme, bunu rutinsel yapma, bilgi değişimi,
onun JS ile okuması problemli. Sayfa tekrar yüklenince (mesela `meta
.. refresh` tekniği ile) üstteki kod da tekrar yükleniyor, bazı
dengeler bozuluyor; bu sebeple en iyisi tekrar yüklenen sayfayı bir
alt frame içinde tutmak, yükleme, frame içindeki bilgileri okuyup
sesli okuma işini dış sayfa içinde yapmak.

Not: Frame kavramı için HTML kaynaklarına danışılabilir; frame ile bir
dış HTML bir diğeri içine dahil edilir, neredeyse o iç frame'de ayrı
bir tarayıcı işliyormuş gibi olur.

Dış sayfa


```
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <script src="/static/responsivevoice.js" type="text/javascript" charset="utf-8"></script>
    <script type="text/javascript" charset="utf-8">      
      function speak() {
        s = window.frames[0].document.getElementById('distance').innerHTML;
        s1 =  'you are ' + s + ' meters away ';
        responsiveVoice.speak(s1);
      }
      function reload() {        
         window.frames[0].location.reload();
      } 
      
      setInterval( () => reload(), 5000);
      setInterval( () => speak(), 5500);
        
    </script>
  </head>

  <body>

  <a href="#" onClick="speak();">Speak</a>
  <br/>  
  <br/>  
  <br/>  
  <a href="#" onClick="reload();">Reload</a>
  <br/>
  <br/>  
  <br/>  
  
  <iframe frameborder="0" src="/inc" id="sub1">
  </iframe>

  </body>
        
</html>
```

Bu sayfa Flask `/inc` ile yüklenen bir içeriği dahil ediyor. İçerik sayfası

```
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    </meta>
  </head>
   <body>
     <div id='distance'>{{ rnd }}</div>
   </body>  
</html>
```

Dışarıdaki sayfadan frame bilgisine `window.frames[0]` ile erişilir,
gerisi `document` vs ile bildiğimiz Javascript erişim şekli.

Her ~5 saniyede bir yükleme ve okuma olsun diye `setInterval`
kullanıldı, bu zamansal bazlı JS kodu çağırabilen bir yöntem. Peki
niye bu zamansal çağrıyı içinde yükleme ve okuma içeren tek bir metot
için yapmadık? Cevap JS'in asenkron karakterinde.. Yükleme ve okuma
ardı ardına çağrılsa bile okuma hep geride kalıyordu. Ayrı ayrı iki
zamanlama bu işi yaptı.

Flask komutlarını verelim,

```
@app.route('/inc')
def inc():
    return render_template('/inc.html',rnd=np.round(random.random(),3))

@app.route('/tst')
def tst():
    return render_template('/tst.html')
```

İç sayfada her güncellemede rasgele bir sayı üretilir, sayfaya
basılır. Dış sayfa bunu dahil eder, okur ve sesli bir şekilde telafuz
eder. Test etmek için tarayıcıda biz `/tst` URL'ini ziyaret ederiz.

Not: bu kod GPS bazlı harita her bulup yön tarif eden kendi
kullandığım bir kodun kullandığı yaklaşımı; detaylar
[şurada](https://github.com/burakbayramli/kod/tree/master/nomadicterrain).




