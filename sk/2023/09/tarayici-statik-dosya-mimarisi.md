# Tarayıcı, Statik Dosya Yapısı

Pek cok uygulama, eger sadece okuma gerektiren turden iseler,
tarayicinin statik json dosyalarini azar azar indirip isledigi sekilde
kodlanabilir. Site arama islemini [1]'de bu sekilde kodladik.

```
from requests import get

url = "https://www.rfc-editor.org/rfc/rfc2822.txt"
r = get(url)
print (r.text[:100])
```

```html
<head>
    <script defer src="pyscript.js"></script>
</head>
<body>    
      <py-script>
          from pyodide.http import open_url
          url = 'https://raw.githubusercontent.com/fomightez/pyscript_test/main/turtles.csv'
          df = open_url(url)
          display(df.getvalue())
      </py-script>
</body>
</html>
```









Kaynaklar

[1] statik-web-sitesi-dinamik-arama-full-text-search.html

[2] https://phiresky.github.io/blog/2021/hosting-sqlite-databases-on-github-pages/

