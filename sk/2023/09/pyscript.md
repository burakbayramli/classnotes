# Javascript Yerine Tarayıcıda Python, PyScript

Tarayıcı tarafında kullanılan dil Javascript adlı bir dil, Java'nın
daha dinamik, güçlü tipleme gerektirmeyen bir hali denebilir. Bu dil
oldukca güçlüdür, müşteri tarafında gereken neredeyse her işlemi,
hesabı yapabilir. Önemli nokta, Javascript işlediğinde bu kodları
tarayıcı işletilir yani bağlanan kişinin CPU'şu bu kodları
işletmektedir, servis tarafı değil.

Peki tarayıcıda başka diller mümkün mü? Farklı dilleri WebAssembly
altyapısı üzerinen Javascript'e derlemek mümkün. Fakat PyScript
yaklaşımı en temiz, basit, hızlı yöntem, sayfa içinde iki dosya dahil
ediliyor, ve bundan sonra tam tekmilli Python kodlarını sayfa içine
koymak mümkün oluyor.

```html
<html>
  <head>
  <link rel="stylesheet" href="https://pyscript.net/latest/pyscript.css"/>
  <script defer src="https://pyscript.net/latest/pyscript.js"></script>
  </head>

  <body>
    <div id="output"></div>
    
    <py-script>
      def write_to_page():
         inp = Element("search")
         Element("output").element.innerText = inp.element.value
    </py-script>
    
    <p>
     <input type="text" name="search" id="search"/>
    </p>
    <p>
      <button py-click="write_to_page()" id="btn1">OK</button>
    </p>
    
  </body>

</html>
```

```
python -m http.server 8080 --bind 127.0.0.1
```

```
http://localhost:8080/tst1.html
```

pyscript_01.jpg














[devam edecek]

