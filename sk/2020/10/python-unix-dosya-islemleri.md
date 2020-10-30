# Dosya İşlemleri, Unix, Python

Genel ihtiyaca dönük Python'da yapılabilecek dosya işlemleri;

Bir dosya, dizin mevcut mu?

```python
import os

print (os.path.exists("/tmp/filan1"))
```

```text
False
```

Bir dizini yaratmak için

```python
os.mkdir("/tmp/filan1")
print (os.path.exists("/tmp/filan1"))
```

```text
True
```

Dizini altındaki tüm dosyalarla beraber silmek

```python
import shutil
shutil.rmtree("/tmp/filan1")
```

Bir dizin altındaki dosyaları, dizinleri belli bir kalıba uyacak şekilde listelemek. 

```python
import glob

glob.glob("/usr/*")
```

```text
Out[1]: 
['/usr/local',
 '/usr/games',
 '/usr/lib',
 '/usr/share',
 '/usr/src',
 '/usr/libexec',
 '/usr/include',
 '/usr/sbin',
 '/usr/bin']
```

```python
glob.glob("/usr/*bin*")
```

```text
Out[1]: ['/usr/sbin', '/usr/bin']
```

Eğer daha alt seviye dizinleri göstermek istersek, mesela iki seviye
aşağısı için, `/*/*` diyebilirdik.

Fakat üstteki ifade özyineli bir şekilde en üst seviye dizinden tüm
altdızinleri göstermez. Bunun için bir kod yazdık,

```python
def ls(d,ignore_list=[]):
    print ('ls ignore lst', ignore_list)
    dirs = []; files = []
    for root, directories, filenames in os.walk(d):
        for directory in directories:
            path = os.path.join(root, directory)
            do_add = True
            for ignore in ignore_list:
                if ignore in path:
                    print ('ignoring', path); do_add = False
            if do_add: dirs.append(path)
        for filename in filenames: 
            path = os.path.join(root,filename)
            do_add = True
            for ignore in ignore_list:
                if ignore in path: do_add = False
            if do_add: files.append((path, os.path.getsize(path)))
    return dirs, files
```

```python
os.mkdir("/tmp/filan1")
os.mkdir("/tmp/filan1/filan12")
os.mkdir("/tmp/filan1/filan13")
os.mkdir("/tmp/filan1/filan12/filan121")

print (ls("/tmp/filan1"))
```

```text
ls ignore lst []
(['/tmp/filan1/filan12', '/tmp/filan1/filan13', '/tmp/filan1/filan12/filan121'], [])
```















