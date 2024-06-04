# Javascript - 2

### Zip Dosyası Okumak, İşlemek

Statik dosya servisinde duran zip edilmiş bir CSV (düz metin)
dosyasını İnternet'ten çekip, ters zip (unzip) yapıp, içindeki CSV
dosyasını nasıl tararız? Alttaki kod ile,

```javascript
function read_zip() {

    var url = "https://vs.vs.com/dosya.zip";

    fetch(url).then(res => res.arrayBuffer()).then(arrayBuffer => {
	var new_zip = new JSZip();
	new_zip.loadAsync(arrayBuffer)
	.then(function(zip) {
	    var res = zip.file('veri.csv').async('string');
	    return res;
	}).then(function(text) {
	    var lines = text.split('\n');
	    lines.forEach(function(line) {
		console.log('line', line);
		var tokens = line.split(',');
		console.log(tokens.length); 
	    });
	});
    });
}
```

Üstteki kod CSV dosyasının her kaydının ayrı bir satırda ve
kolonlarının virgül ile ayrılmış olduğunu farz ediyor, bu sebeple önce
ters zip yapar, sonra yeni satır `\n` karakteri üzerinden ayırma
(split) yapar, ve en son elde edilen her satırda virgül üzerinden bir
ayrım daha yapar. Bu işlem sonunda her satır işlenirken `tokens`
dizini icinde gerekli kolon bilgileri alinabilecektir.

Not: `JSZip` ve `zip.file` ile yapılan tüm işlemler bellekte olmakta,
diske hiçbir noktada yazım yapılmıyor. Tarayıcı ortamında bunun nasıl
faydalı olacağını görebiliriz, çünkü tarayıcının kullanıcının diskine
yazma izni yoktur, fakat eğer zip çok büyük değilse üstteki kod
tarayıcı üstteki kodla zip indirip, içindeki dosyayı açıp onu rahat
bir şekilde tarayıp, işlemini yapabilir.

### Girdi Tamamlamak (Autocomplete)

HTML sayfasındaki bir giriş kutusuna girilen birkaç kelime sonrası
gerisini tamamlama amaçlı seçenek listesi veren (autocomplete) kodu
[1] altta görülüyor.

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

Javascript kodlari `common.js` icinde

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
