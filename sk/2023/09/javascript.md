# Javascript

Tamamen istemci, tarayıcı üzerinde işleyen kodlara ihtiyaç varsa Javascript
kodlaması yaparız. Javascript kodları HTML içine bile gömülebilir, ve
bu kodlar içinde olduğu HTML öğelerine erisebilirler. Bu sayede Uygulama/Web
servisi bağlanana serviste üretilmiş bir HTML göndermiş olsa bile hala
o HTML üzerinde değişiklik yapılabilir. Bir girdi kutusuna girilen bilgi
yeni bir liste yaratılmasını sağlayabilir mesela, her türlü ekleme, çıkarma,
düzeltme işlemi kullanıcı tarafında halledilebilir.

### Gelistirme Ortami

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
çengeline takarak bunu yapmış olduk. 


alert

console.log(text)

Cache

You can click the settings icon on top right corner ... | More Tools |
Developer Tools | Network | Disable cache (while DevTools is open)

Node

```
sudo apt install nodejs
```

```
node code1.js
```

```javascript
const fs = require('fs')

file = fs.readFileSync(path1, 'utf8');
const means = JSON.parse(file);
```


### Temel Gorsel Islemler

document.getElementById

.innerText

.innerHTML

### Strings

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

```html
  <script>

    function add_movie() {
      if (document.cookie.length < 1) {
         document.cookie = "[]"
      }
      res = document.getElementById("myInput").value;
      cook = JSON.parse(document.cookie);
      cook.push(res);
      alert(cook);
      document.cookie = JSON.stringify(cook);
    }
   
</script>

<form autocomplete="off">
  <div class="autocomplete" style="width:300px;">
    <input id="myInput" type="text" name="myCountry" placeholder="Movie">
    <p><button onclick="add_movie()">Add</button></p>
  </div>
</form>
```

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



### XMLHttpRequest

Statik Dosya

```javascript
var xmlHttp = new XMLHttpRequest();
xmlHttp.open( "GET", url = 'http://192.168.43.49:5000/static/recom/test2.csv', false ); 
xmlHttp.send( null );
document.getElementById("output").innerText = xmlHttp.responseText
```

Ajax

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

