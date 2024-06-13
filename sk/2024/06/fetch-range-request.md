# Javascript Fetch, Paralellik, Kapsam İsteği (Range Request)

Çok sayıda URL bağlantısının verisini paralel şekilde almak
gerekebilir.  Fakat tüm URL'lerin hepsi bitmesini beklemek
gerekiyorsa, yani URL kümesi içinde asenkron ama tüm işin bitmesini
beklemek senkron olmak gerektiriyorsa, Javascript'in `await` ve
`Promise.all` kavramlarını bilmek gerekecek.

İki tane basit URL ile başlayalım, bunlar Github'da olan düz metin
dosyaları,

```javascript
const fetchURLs = async (urls) => {

    var promises = urls.map(url => fetch(url));

    const responses = await Promise.all(promises);

    const data = await Promise.all( responses.map(response => response.text()) );

    return data;
    
    console.log('done')
        
}

async function init() {

    const urls = ['https://raw.githubusercontent.com/tj/histo/master/examples/small.txt',
		  'https://raw.githubusercontent.com/tj/histo/master/examples/medium.txt'];
    console.log('start');
    var res = await fetchURLs(urls);
    console.log('end');
       
    console.log(res);
}
```

Üstteki olanları açıklarsak, `promises` listesine fonksiyon objeleri
ekledik, o liste üzerinde `await Promise.all` işleterek tüm işin
bitmesini bekliyoruz. Yanlız dikkat, `await` kullanan her fonksiyonun
kendisi de asenkron olmak zorundadır, `fetchURLs` fonksiyonu bu
sebeple `async` olarak işaretlendi, fakat Web sayfa ortamında bu
problem değil, belki bir düğme tıklanıp üstteki çağrıyı tetiklemiştik,
kullanıcı zaten sonucu bekleyecektir, iş bitince sonuçlar asenkron
şekilde sayfada gösterilir, bu kullanım kullanıcı / sayfa iletişimi
ile uyumludur.

Bir örnek daha: [1] yazısında kapsam istekleri ile Web ortamında büyük
veri dosyalarının tamamını indirmeden "nokta okuyuş" ile azar azar
parçalarını okuyabildiğimizi gördük. GLOBE verisi tam bu tür okumaya
uygundur, dünya kordinatları direk dosya içinde indislere tercüme
edilebilir, ve bu indislere nokta atışı ile gidip o kordinatın verisi
alınabilir.

İki tane indise `fetch` erişimini altta görüyoruz, bu kod aynı şekilde
paralel işletim, senkron bekleme ve ek olarak GLOBE için gereken
`arrayBuffer` ve `Uint16Array` kullanımlarını gösteriyor.

```javascript
async function init() {

    var url = "/vs/vs/data/g10g";

    var promises = [];

    promises.push(
	fetch(url, {
            headers: {
		'content-type': 'multipart/byteranges',
		'range': 'bytes=33681360-33681361',
            },
	})
    );

    promises.push(
	fetch(url, {
            headers: {
		'content-type': 'multipart/byteranges',
		'range': 'bytes=33681362-33681363',
            },
	})
    );
    
    const responses = await Promise.all(promises);
    
    const data1 = await Promise.all( responses.map(response => response.arrayBuffer() ));
    
    const data2 = await Promise.all( data1.map(response => new Uint16Array(response) ));

    data2.forEach(function(x) {
	console.log(x[0]);
    });
    
}
```

Kaynaklar

[1] <a href="../../2019/04/elevation.html">Yükseklik (Elevation) Verileri</a>

