# NoSQL

NoSQL tabanları ilişkisel veri yapısı (SQL ile erişilen türden)
gerektirmeden veri sorgulamayı, eklemeyi destekleyen tabanlardır. Bir
bakıma diske yazılmış Python sözlüğü gibi görülebilirler; tek ihtiyaç
duydukları bir anahtar ve ona tekabül eden değerdir, ki değer herhangi
bir obje olabilir. Mimarinin çekici tarafı veri ünitesini tek objeye
indirdediğimizde dağıtık ortamda çalışmanın kolaylaşması, ölçeklemek
için belli objeleri belli makinalara gönderdiğimizde problem
çözülüyor, çünkü ilişkisel bağlantıları takip etmek gerekmiyor.

Basit, dağıtık çalışabilen bir NoSQL taban tasarımı şöyle olabilir.

- Yükü karşılıyabilmek için ayrı servisler başlatılır. Bağlanan taraf
  istemcide N tane servisten hangisine gidileceğine karar vermek için
  her anahtar üzerinde i = mod N işletir, ve cevaba göre o veri için i
  servisine gidilir. Böylece veri bazlı yük dengesi yapılmış olur.
  Bildiğimiz gibi mod N sonucu N'yi geçemez, ve mod belli sayılar
  arasında gidip gelen, sona geldiğinde başa dönen bir yapıdadır bu
  şekilde dengeli bir dağıtım yapar.
  
- Her serviste ayrı bir Flask süreci işletilir, taban verisini Flask
  REST API'si üzerinden dışarı açarız, burada `get`, `set` çağrıları
  olacaktır. Girdi ve çıktı alış/veriş JSON üzerinden yapılır.
  
- Her Flask süreci kendi içinde seri halde çalışılır (paralel değil),
  bu seri süreç kendi sqlite tabanına yazar ve oradan okur, böylece
  her servisin kendi içindeki eşzamanlılık çakışma problemleri
  engellenmiş olur. Servis tarafı basitleşir.
  
- Bir anahtarla beraber herhangi bir obje, ne olursa olsun, tabana
  yazılıp okunabilmelidir, Python objelerini `pickle` üzerinden string
  haline getirebiliriz, ve arka planda temel depolama sqlite `TEXT`
  kolonunda olur, [1]'de bunun örneğini gördük.

- Listeleme: bu özellik için ayrı bir SQL tablosu yaratırız, bir
  kolonunda liste ismi, diğerinde obje anahtarı olacak şekilde. Liste
  ismi satırlarda tekrarlanabilir böylece çoka bir ilişki
  kuruyoruz. Listeyi istemciden almak için o isim üzerinde `where`
  işletiliriz. Sayfalama özelliği SQL LIMIT üzerinden sağlanır,
  çağıran tarafta liste obje anahtarlarını objeye çevirmek için
  servise tekrar sormak gerekecektir.

### Servis

Basit bir Flask servisi altta,

```python
from flask import Flask, url_for, jsonify, request
import sys, os, sqlite3

app = Flask(__name__)

class OnlyOne(object):
    class __OnlyOne:
        def __init__(self):
            self.conn = None
        def __str__(self):
            return self.val
    instance = None
    def __new__(cls):
        if not OnlyOne.instance:
            OnlyOne.instance = OnlyOne.__OnlyOne()
        return OnlyOne.instance
    def __getattr__(self, name):
        return getattr(self.instance, name)
    def __setattr__(self, name):
        return setattr(self.instance, name)

@app.before_first_request
def start_check_db():
    sno = app.config.get('server_no')
    db = "/opt/Downloads/kvf-%d.db" % int(sno)
    conn = sqlite3.connect(db)
    OnlyOne().conn = conn
    if not os.path.isfile(db): 
        c = OnlyOne().conn.cursor()
        res = c.execute('''CREATE TABLE OBJ (key TEXT PRIMARY KEY, value TEXT); ''')
        c.execute('''INSERT INTO OBJ(key,value) VALUES(?,?)''', ("test1","value1"))
        conn.commit()    
    print ('server', sno)

@app.route('/get', methods=["PUT", "POST"])
def get():    
    data = request.get_json(force=True)   
    key = data['key']
    c = OnlyOne().conn.cursor()
    c.execute("SELECT value FROM OBJ WHERE key = ?", [key])
    res = list(c.fetchall())
    if len(res)==0: 
        return jsonify({'result': "None"})
    return jsonify({'result': res[0][0]})

@app.route('/set', methods=["PUT", "POST"])
def set():    
    data = request.get_json(force=True)   
    key = data['key']
    value = data['value']
    c = OnlyOne().conn.cursor()
    c.execute("insert or replace into OBJ (key,value) values (?,?)", [key,value])
    OnlyOne().conn.commit()
    return jsonify({'result': "OK"})

@app.route('/remove', methods=["PUT", "POST"])
def remove():    
    data = request.get_json(force=True)   
    key = data['key']
    c = OnlyOne().conn.cursor()
    c.execute("delete from OBJ where key = ?", [key])
    OnlyOne().conn.commit()
    return jsonify({'result': "OK"})

if __name__ == "__main__":
    app.config['server_no'] = sys.argv[1]
    app.run(host="localhost", port=8080)   
```

`start_check_db` ile taban yoksa başlangıçta yaratılır.

Tabana tek bir bağlantı vardır, o bağlantı `OnlyOne` tekil obje
(singleton) içinde muhafaza ediliyor. Arkadaki sqlite tabanına
eşzamanlı erişim olmadığı için birden fazla bağlantıya da gerek yok.

Metot `set` içinde `insert or replace into` komutunu kullandık, bu SQL
çağrısı bir satır varsa onu günceller yoksa yeni satır ekler,
İngilizce deyimle insert ve update karışımı olan "upsert" işlemini
yapar. Bu depolama tarzı da yine hafıza sözlük kullanımını andırıyor,
aynen bir Python sözlüğünde `d['key'] = 'değer'` çağrısının önceden
varsa `key` değerlerini ezecek (yoksa ekleyecek) olduğu gibi, taban
bazlı yaklaşım da bu işlevi taklit ediyor.

Not: SQL kullanmayan tabanın arka planda bir SQL tabanı SQLite
kullanıyor olması çelişki gibi durabilir fakat aslında değil; bu
tabanı sadece basit bir depo olarak kullanıyoruz, ve bunu çok basit
düz SQL komutları üzerinden yapıyoruz. Unutmayalım SQL sadece ekleme,
silme gibi "CRUD" operasyonlarından ibaret değil, ilişkisel veriler
literatürü çok daha kapsamlı. NoSQL dışarıya bu tür ilişkisel bir
arayüz vermiyor, arka planda da SQL basit depolama amaçlı olarak
kullanılıyor ve onun ilişkisel özelliklerine de dokunulmuyor.

Servisi başlatırken üstteki script'e komut satırından 0,1,2.. gibi bir
sayı veririz, bu sayı o servisin no'su olur. Bu no taban ismine
eklenecektir (böylece aynı makinada bile farklı taban süreçleri
yanyana işleyebilir). Bu sayıyı servis tarafında bir makina:port
listesi içinden birini seçmesi için de kullanabiliriz. Bağlanan
tarafta anahtar üzerinde mod N işletildikten sonra elde edilen sayı
aynı listeden bir makina:port seçip ona bağlanırdı.

Şimdi REST arayüzüne `curl` ile erişelim, mevcut olan `test1` verisine erişim,

```python
! curl -H "Content-Type: application/json" -d '{"key":"test1"}'  http://localhost:8080/get
```

```text
{"result":"value1"}
```

Olmayan bir anahtara erişim,

```python
! curl -H "Content-Type: application/json" -d '{"key":"asdjflkajsf"}'  http://localhost:8080/get
```

```text
{"result":"None"}
```

Bir değeri set edip onun geri almak,

```python
curl -H "Content-Type: application/json" -d '{"key":"3333", "value":"value333"}'  http://localhost:8080/set
curl -H "Content-Type: application/json" -d '{"key":"3333"}'  http://localhost:8080/get
```

### Python İstemci

Üstteki `curl` tipi ham JSON çağrıları pür Python ile de
yapabilirdik,

```python
import requests
response = requests.post('http://localhost:8080/get', json={"key":"test1"})
print("Status code: ", response.status_code)
print(response.json())
```

```text
Status code:  200
{'result': 'value1'}
```

Şimdi REST çağrılarını üç ayrı fonksiyon ile sarmalayalım, normal
durumda kullanıcıların çağıracağı tek metotlar bunlar olacak.

```python
import requests, pickle, base64

def get(key):
    response = requests.post('http://localhost:8080/get', json={"key":key})
    print("Status code: ", response.status_code)
    res = response.json()
    res = pickle.loads(base64.decodestring(res['result'].encode('utf-8')))
    return res
    
def set(key,value):
    value = base64.encodestring(pickle.dumps(value)).decode()
    response = requests.post('http://localhost:8080/set', json={"key":key,"value":value})
    print("Status code: ", response.status_code)
    res = response.json()
    
def remove(key):
    response = requests.post('http://localhost:8080/remove', json={"key":key})
    print("Status code: ", response.status_code)
    res = response.json()
    

set("2324","33333ddddd3")
o = get("2324")
print (o)
```

```text
Status code:  200
Status code:  200
33333ddddd3
```

Üstteki örnekte değer olarak basit string gönderdik ama tasarım öyle
ki karmaşık objelere de izin veriyor,

```python
m = np.random.rand(2,2)
set("randomId123",m)
```

```text
Status code:  200
```

```python
o = get("randomId123")
print (o)
```

```text
Status code:  200
[[0.21365823 0.42896852]
 [0.31330469 0.03924181]]
```

Bir Numpy matrisi gonderdik NoSQL onu bile tabana yazdi.

Taban içeriğine bakarsak objelerin nasıl yazılmış olduğunu görüyoruz,

```python
import sqlite3
c = sqlite3.connect("/opt/Downloads/kvf-0.db" )
cur = c.cursor()
res = cur.execute("SELECT key,value FROM OBJ")
for x in cur.fetchall():
   print (x)
```

```text
('test1', 'value1')
('2324', 'gANYCwAAADMzMzMzZGRkZGQzcQAu\n')
('randomId123', 'gANjbnVtcHkuY29yZS5tdWx0aWFycmF5Cl9yZWNvbnN0cnVjdApxAGNudW1weQpuZGFycmF5CnEB\nSwCFcQJDAWJxA4dxBFJxBShLAUsCSwKGcQZjbnVtcHkKZHR5cGUKcQdYAgAAAGY4cQiJiIdxCVJx\nCihLA1gBAAAAPHELTk5OSv////9K/////0sAdHEMYolDIPATky0nWcs//JC9Xzh02z+EpHgZLw3U\nP7ATIoOAF6Q/cQ10cQ5iLg==\n')
```

Karmaşık bazı string değerleri görüyoruz, bunlar Python objelerinin kodlanmış hali.

Birkaç servisli durumdan bahsedelim; daha önce değinildiği gibi birden
fazla servise bağlanmak için `key` üzerinde mod kullanabiliriz, mesela

```python
N = 3 # bu kadar servis var
print (hash("test1"))
print ('servis',hash("test1") % N)
print ('servis',hash("111122") % N)
print ('servis',hash("323") % N)
```

```text
-663401633428333736
servis 1
servis 2
servis 0
```

Bu indis değerleri `set` ve `get` içinde kullanılırdı, servise
bağlantı yapılmadan önce hangi URL kullanılacağını bulmak için. Eğer
0 alındıysa mesela,

http://localhost:8080/get

1 alindiysa

http://localhost:8081/get

bağlantısı yapılıyor olabilirdi, bu karar değiştirilmiş `get`, `set`
üzerinde verilecektir.

Kaynaklar

[1] [Obje String Olarak Kodlamak](../../2010/10/encoding-objeleri-yazip-okumak-pickle-base64.html)

[2] [MongoDB](../../2014/05/mongodb.html)




