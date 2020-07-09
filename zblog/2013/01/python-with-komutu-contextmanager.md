# Python with komutu, ContextManager


Python with komutu, ContextManager




Bir blogun etrafinda yapilacak oncesi / sonrasi islemlerini, daha ozel olarak surekli tekrarlanan rutin temizlik hareketlerini modulerize hale getirmek icin Python with kullanilabilir. Standart ornek

with open('/tmp/workfile', 'r') as f:    read_data = f.read()

Ornekteki with sunlari yapar; kod bloguna girmeden once dosya acilir, ciktiktan sonra kapanir, bloktan cikis nasil olursa olsun. Fakat bu ornek bazi seyleri gostermiyor; birincisi open komutu ozel sekilde kodlanmistir ki with ile kullanilabilsin. Daha kisa bir ornek

from contextlib import contextmanager@contextmanagerdef tag(name):    print "<%s>" % name    yield    print "</%s>" % namewith tag("h1"):    print "foo"

Burada tag komutunun with ile nasil kullanilabilir hale getirildigini goruyoruz. 
tag icindeki yield komutu bir "yer isaretleyici / tutucu (placeholder)", islem o noktaya gelince with blogunun icindeki komutlar isletilir, yukaridaki ornekte print komutu. Bir ornek daha

from contextlib import contextmanagerimport os@contextmanagerdef working_directory(path):    current_dir = os.getcwd()    os.chdir(path)    try:        yield    finally:        os.chdir(current_dir)

with working_directory("data/stuff"):    # data/stuff icinde bir seyler yap   

Ornekte working_directory with ile cagrildigi zaman parametre olarak verilen string bir dizin olarak kabul edilir, o dizine "gidilir", sonra with blogunda tanimlanan seyler yapilir (yield), ve sonra o dizinden cikilir. Cikis isleminin finally icinde yapilmasi, isleyis sirasinda istisna (Exception) olsa / atilsa bile bu isin yapilmasini garantiler (open kodlamasinda da herhalde benzer isler yapiliyor, acilmis dosyayi kapatmak icin).

Kisaca with ile kullanilan bloklar  "cevresinde" hep olacak / beraber olmasini istedigimiz kod parcalarini biraraya koyabiliriz. 

http://docs.python.org/2/tutorial/inputoutput.html

http://docs.python.org/2/library/contextlib.html

http://stackoverflow.com/questions/3012488/what-is-the-python-with-statement-designed-for





