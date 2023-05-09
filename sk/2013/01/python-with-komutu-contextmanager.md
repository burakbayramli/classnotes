# Python with komutu, ContextManager

Bir bloğun etrafında yapılacak öncesi / sonrası işlemlerini, daha özel
olarak sürekli tekrarlanan rutin temizlik hareketlerini modülerize
hale getirmek için Python with kullanılabilir. Standart örnek

```python
with open('/tmp/workfile', 'r') as f:    read_data = f.read()
```

Örnekteki with şunları yapar; kod bloğuna girmeden önce dosya açılir,
çıktıktan sonra kapanır, bloktan çıkış nasıl olursa olsun. Fakat bu
örnek bazı şeyleri göstermiyor; birincisi öpen komutu özel şekilde
kodlanmıştır ki with ile kullanılabilsin. Daha kısa bir örnek

```python
from contextlibimport contextmanager

@contextmanager
def tag(name):
    print "<%s>" % name
    yield
    print "</%s>" % namewith tag("h1"):
    print "foo"
```

Burada tag komutunun with ile nasil kullanilabilir hale getirildigini
goruyoruz.  tag icindeki yield komutu bir "yer isaretleyici / tutucu
(placeholder)", islem o noktaya gelince with blogunun icindeki
komutlar isletilir, yukaridaki ornekte print komutu. Bir ornek daha

```python
from contextlib import contextmanagerimport os

@contextmanagerdef working_directory(path):
    current_dir = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(current_dir)

with working_directory("data/stuff"):
    # data/stuff icinde bir seyler yap
```

Örnekte working_directory with ile çağrıldığı zaman parametre olarak
verilen string bir dizin olarak kabul edilir, o dizine "gidilir",
sonra with bloğunda tanımlanan şeyler yapılır (yield), ve sonra o
dizinden çıkılır. Çıkış işleminin finally içinde yapılması, işleyiş
sırasında istisna (Exception) olsa / atılsa bile bu işin yapılmasını
garantiler (open kodlamasında da herhalde benzer işler yapılıyor,
açılmış dosyayı kapatmak için).

Kısaca with ile kullanılan bloklar  "çevresinde" hep olacak / beraber
olmasını istediğimiz kod parçalarını biraraya koyabiliriz.

http://docs.python.org/2/tutorial/inputoutput.html

http://docs.python.org/2/library/contextlib.html

http://stackoverflow.com/questions/3012488/what-is-the-python-with-statement-designed-for






