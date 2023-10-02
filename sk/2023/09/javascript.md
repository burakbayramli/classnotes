# Javascript

Tamamen istemci, tarayıcı üzerinde işleyen kodlara ihtiyaç varsa Javascript
kodlaması yaparız. Javascript kodları HTML içine bile gömülebilir, ve
bu kodlar içinde olduğu HTML öğelerine erisebilirler. Bu sayede Uygulama/Web
servisi bağlanana serviste üretilmiş bir HTML göndermiş olsa bile hala
o HTML üzerinde değişiklik yapılabilir. Bir girdi kutusuna girilen bilgi
yeni bir liste yaratılmasını sağlayabilir mesela, her türlü ekleme, çıkarma,
düzeltme işlemi kullanıcı tarafında halledilebilir.

### Geliştirme Ortamı

Javascript geliştirme her zaman bir uygulama servisi gerektirmeyebilir
sonuçta bir HTML kodlanıyor ve bu kodlar tarayıcıda düz dosya olarak
yüklenebilirler, fakat daha ileri özellikler için gene de bir servis
başlatılması iyi olur. Python bilenler için en iyisi Flask, bir `static`
dizini yaratılabilir, ve düz HTML dosyaları buraya koyulur, ve
`http://localhost:8080/static/dosya.html` şeklinde erişilebilir.

Javascript ne zaman, nerede yüklenir, nasıl çağrılır? 

Sablon olarak bir HTML suna benzeyebilir,

```html
<html>
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  </head>  
  <script>
    function foo() {
        document.getElementById("output").innerText = xmlHttp.responseText      
    }
  </script>    
  <body onload="foo()">
    <div id="output">
    </div>  
  </body>
  
</html>
```

Bu dosyada bir JS fonksiyonunu HTML içine gömdük, bu fonksiyon HTML
yüklenir yüklenmez çağrılacaktır. Fonksiyon `foo` yu `body onload`
çengeline takarak bunu yapmış olduk. Çengel derken İngilizce hook
kavramına atıf yapılıyor, yani beni arama ben seni ararım tekniği, biz
çağrılmasını istediğimiz fonksiyonu altyapıya veriyoruz, onun ne zaman
çağrılacağını biz kontrol etmiyoruz, yeri gelince altyapı onu
çağırıyor.

Kodlama direk sayfa içine Javascript gömerek, ya da ayrı bir `js`
dosyasını sayfaya dahil edilerek yapabiliriz. İkinci yöntem kod
idaresi açısından daha rahattır. HTML içine

```
<script src="funcs.js"></script>
```

koyunca `funcs.js` otomatik olarak dahil edilecektir.

Fakat dikkat, eğer önbellekleme (cache) açık ise, ki olağan durum
budur, js dosyasında yapılan değişiklikler HTML tarayıcıda tekrar
yüklense bile etki etmeyebilir, o zaman `Developer tools`, `Network`
ve oradan `Disable cache` seçimi yapılırsa önbelleklenme kapatılmış
olur, kod her seferinde tekrar yüklenir. Tabi Web'de her kullanıcının
bunu yapmasını bekleyemeyiz, o zaman yeni kod sürümü yapacaksak yeni
kod için yeni bir js dosya ismi kullanmak bir çözüm olabilir.

Log

Eğer kod işletimi sırasında bazı değerleri log bırakmak açısından düz metin
olarak basmak istiyorsak, bunu `console.log(..)` ile yapabiliriz. Çıktıları görmek
için Chrome içinde üst sağ köşede tıklama yapıp `More tools` ve `Developer tools`
seçimi yaparız. Bu araç tarayıcının sağ kenarında çıkar, üstteki tab içinde
`Console` seçimi yaparak log çıktılarını görmek mümkündür. 

Bir diğer mesaj basma yöntemi `alert` çağrısı, fakat bu çağrı bir
diyalog kutusu yaratır, tıklama yapıp kapatmak gerektiği için her
yerde kullanılmıyor.

Node

Görsel kodlamada ilerlemeden önemli bir konuya değinelim, çünkü
ileride JS kodlarını test etmek için faydalı olabilir. Node
teknolojisi ünlü, bu servis tarafında Javascript anlamına geliyor. V8
motoruyla beraber JS kodlarının işletimi hızlandı, neredeyse C++
seviyesine geldi, ve bazıları da düşündü ki "ben motoru alıp servis
tarafında koştururum, böylece hem görsel hem servis kodlarını aynı
dilde yazabilmiş olurum". Node buradan çıktı. Biz şu anda servis
tarafı kodlamasını işlemiyoruz, fakat pür istemci için olsa bile bazı
Javascript kodları hala görsel olmayan mantık içerebilir, ve bu
kodları ayırıp, node üzerinden test etmek mümkündür, böylece sürekli
tarayıcıyı açmak gerekmez, komut satırından Javascript test
edebiliriz.

Kurmak için,

```
sudo apt install nodejs
```

Simdi mesela bir `test1.js` icinde

```javascript
function add(a, b){
    return a+b;
}

console.log(add(3,4));
```

Bu kodu

```
node test1.js
```

ile işletince `7` değeri basılacaktır. Dikkat edersek görsel kod içinde kullanılan
aynı `console.log` çağrısı var, ve bu çağrı otomatik olarak komut satırı ekranına
çıktıyı basacağını bildi.

Node kodlarından dosya yüklemek bile mümkün,

```javascript
const fs = require('fs')

file = fs.readFileSync("/home/user1/dir/dosya.json", 'utf8');
const res = JSON.parse(file);
```

Tabii üstteki kod görsel kodlama için uygun değil çünkü tarayıcı içindeki
Javascript yerel dizindeki dosyalara erisemez, İnternet üzerinden dosya
okumak için `XMLHttpRequest` gerekir, o konuya geliyoruz, fakat test
amacıyla üstteki çağrı hala faydalıdır. 

### Çengeller

Javascript fonksiyonları her türlü kullanıcı aksiyonu sayesinde çağrılabilir.
Mesela bir URL bağlantısına tıklanınca bir fonksiyon çağrılsın istiyorsak
`a href='#' onclick='func2()`...`  diyebiliriz. Diğer pek çok çengel noktası,
vardır bunlar bir HTML referansından öğrenilebilir.

### Temel Görsel İşlemler

Javascript icinden HTML sayfasinin gorsel ogelerine erisilebilir demistik,
bu ogelerden bilgi alinabilir, ve geri bilgi yazilip goruntude degisim
yapilabilir.

HTML içinde `id=".."` ile işaretlenen etiketlerin, mesela `<div>`
etiketi olsun, ana objesini kimlik değeri geçerek
`document.getElementById(..)` ile alabiliriz, sonra bu obje üzerinde
`.innerText` ile içerik değişimi yapabiliriz. Sadece düz metin değil
HTML de geçmek mümkündür, bunun için `.innerHTML`.

Eger bir metin giris kutusu var ise, kimligi `myInput` olsun,

```
<form ..>
   <input id="myInput" type="text" name="myCountry" placeholder="Movie"/>
   <button onclick="func2()">Ok</button>
</form>
```

Düğmeye basılınca bir `funç2()` Javascript çağrısı yapılmasını sağlarız, bu
fonksiyon içinden

```javascript
deger = document.getElementById("myInput").value;
```

ile kutudaki değeri okuyabiliriz.

### Temel Programlama Yapilari, Kapsam

Degisken tanimlarken onlarin kapsamini (scope) iyi bilmek lazim, yani
degisken hangi bolgelerde tanimlidir, bir bolumde set edilince diger
yerde gorunebilir mi, deger alimi yapilabilir mi?

Javascript';in olagan davranisi eger degisken kapsami tanimlanmamissa, onun
her yerde (global) kapsami olmasidir. Mesela

```javascript
function func1() {
    console.log('in func1');    
    var1 = "aaaaaa";
}

function func2() {
    console.log('in func2');    
    console.log(var1);
}


func1();

func2();
```

script'i işleyince `var1` değeri basılır. Bir fonksiyon içinde tanımlanan
değer diğerinde görülebilmiştir.

Diğer kapsamlar `let` ve `var` ile yapılır, bunlardan birincisi
değişkeni içinde olduğu kapsama sınırlar, diğeri içinde olduğu
fonksiyona sınırlar. Mesela üstteki kodda `funç1` içinde `let var2 = "bbbbbb"`
tanımlasam ve `funç2` içinden erişmeye uğraşsam hata mesajı alırım. 


dict, liste, for, foreach, if, else, 

```javascript
let header = "Templates Literals";
let tags = ["template literals", "javascript", "es6"];
let html = `<h2>${header}</h2><ul>`;
for (const x of tags) {
  html += `<li>${x}</li>`;
}
html += `</ul>`;
```

### JSON

```javascript
var json1 =  '{ "key1": "val1", "key2": "value2" }';
var json2 =  '[2,3,4,5]';

json1 = JSON.parse(json1);
json2 = JSON.parse(json2);

console.log(json1['key1']);
console.log(json2[2]);

var l1 = [1,2,3,4];
text = "";
for (let i = 0; i < l1.length; i++) {
  text += l1[i] + "<br>";
}

console.log(text);
```


### Cookie





### XMLHttpRequest

Statik Dosya

```javascript
var xmlHttp = new XMLHttpRequest();
xmlHttp.open( "GET", url = 'http://192.168.43.49:5000/static/recom/test2.csv', false ); 
xmlHttp.send( null );
document.getElementById("output").innerText = xmlHttp.responseText
```

Ajax

```python
from flask import Flask, url_for, jsonify, request
import sys, os, sqlite3

app = Flask(__name__)

db = {"a": "ali", "v": "veli"}

@app.route('/get', methods=["PUT", "POST"])
def get():    
    data = request.get_json(force=True)   
    key = data['key']    
    return jsonify({'result': db[key]})

@app.route('/set', methods=["PUT", "POST"])
def set():    
    data = request.get_json(force=True)   
    key = data['key']
    value = data['value']
    db[key] = value
    print ("new db", db)
    return jsonify({'result': "OK"})

if __name__ == "__main__":
    app.run(host="localhost", port=8080)   
```


```html
<html>
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  </head>
  <script>
  function set(key, val) {

    url = "http://localhost:8080/set";
    var xmlhttp = new XMLHttpRequest();   // new HttpRequest instance 
    xmlhttp.open("POST", url, false);
    xmlhttp.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
    xmlhttp.send(JSON.stringify({ "key": key, "value": val }));
  }

  function get(key) {
    url = "http://localhost:8080/get";
    const xhr = new XMLHttpRequest()
    xhr.onload = () => {
      if (xhr.status >= 200 && xhr.status < 300) {
        const response = JSON.parse(xhr.responseText)
        console.log(response)
      }
    }

    xhr.open('POST', url)
    xhr.setRequestHeader('Content-Type', 'application/json')
    xhr.send(JSON.stringify({"key": key}))
  }
  
  function foo() {
      //set("ah","ahmet");
      //get("a");
      get("v");
  }

  
</script>
    
  <body onload="foo()">
  </body>
  
</html>
```


### Girdi Tamamlamak (Autocomplete)

```html
<html>
<head>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>     
<body>

<h2>Autocomplete</h2>

<p>Start typing:</p>

<form autocomplete="off">
  <div class="autocomplete" style="width:300px;">
    <input id="myInput" type="text" name="myCountry" placeholder="Country">
  </div>
  <input type="submit">
</form>

<script src="common.js"></script>

</body>
</html>
```

```javascript
function autocomplete(inp) {

    var arr = ['aa','bb','aaa']
    
    var currentFocus;
    inp.addEventListener("input", function(e) {
	var a, b, i, val = this.value;
	closeAllLists();
	if (!val) { return false;}
	currentFocus = -1;
	a = document.createElement("DIV");
	a.setAttribute("id", this.id + "autocomplete-list");
	a.setAttribute("class", "autocomplete-items");
	this.parentNode.appendChild(a);
	for (i = 0; i < arr.length; i++) {
            if (arr[i].substr(0, val.length).toUpperCase() == val.toUpperCase()) {
		b = document.createElement("DIV");
		b.innerHTML = "<strong>" + arr[i].substr(0, val.length) + "</strong>";
		b.innerHTML += arr[i].substr(val.length);
		b.innerHTML += "<input type='hidden' value='" + arr[i] + "'>";
		b.addEventListener("click", function(e) {
		    inp.value = this.getElementsByTagName("input")[0].value;
		    closeAllLists();
		});
		a.appendChild(b);
            }
	}
    });
    inp.addEventListener("keydown", function(e) {
	var x = document.getElementById(this.id + "autocomplete-list");
	if (x) x = x.getElementsByTagName("div");
	if (e.keyCode == 40) {
            currentFocus++;
            addActive(x);
	} else if (e.keyCode == 38) { //up
            currentFocus--;
            addActive(x);
	} else if (e.keyCode == 13) {
            e.preventDefault();
            if (currentFocus > -1) {
		if (x) x[currentFocus].click();
            }
	}
    });
    function addActive(x) {
	if (!x) return false;
	removeActive(x);
	if (currentFocus >= x.length) currentFocus = 0;
	if (currentFocus < 0) currentFocus = (x.length - 1);
	x[currentFocus].classList.add("autocomplete-active");
    }
    function removeActive(x) {
	for (var i = 0; i < x.length; i++) {
	    x[i].classList.remove("autocomplete-active");
	}
    }
    function closeAllLists(elmnt) {
	var x = document.getElementsByClassName("autocomplete-items");
	for (var i = 0; i < x.length; i++) {
	    if (elmnt != x[i] && elmnt != inp) {
		x[i].parentNode.removeChild(x[i]);
	    }
	}
    }
    document.addEventListener("click", function (e) {
	closeAllLists(e.target);
    });
}

autocomplete(document.getElementById("myInput"));
```
















Kaynaklar

[1] https://www.w3schools.com/howto/howto_js_autocomplete.asp

