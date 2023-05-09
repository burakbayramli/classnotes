# Python ve Komut Satırına Erişmek, Popen

Script yazarken bazen ihtiyaçlardan biri python script'i içinde bir
shell script işletebilmek. Çıktısı işlenmeyenler için
`os.system(komut)` yetiyor tabii. Fakat komut çıktısını göstermek
istiyorsak?

```
import os
res = os.popen("ls -al")
```

kullanımı basit - fakat tedavülden kalkacakmış (deprecated), bu yüzden
kalıcı kodları ona bağlı yazmak iyi olmaz. Kütüphane şubprocess
tavsiye ediliyor,

```
import subprocess 
p = subprocess.Popen(['ls','-al'], stdout=subprocess.PIPE)
res = p.stdout.read()
print (res)
```

İş biraz daha zorlaştı ama kullana kullana alışılabilir. 

Bir diğer ihtiyaç komut "işlerken" onun çıktısını iterator ile
gezebilmektir. Belki komut yavaş işleyecektir, beklerken belki o ana
kadar olan çıktıyı işlemek / ekrana basmak ve yan bazı işlerle
uğraşmak isteyebiliriz. Bunu da `subprocess` ile yapabiliyorsunuz,

```
p = subprocess.Popen(['ping','-c','3','localhost'], stdout=subprocess.PIPE)
for line in p.stdout:
    print line
```

Üstteki komut  her satırı ayrı ayrı, Python döngüsü içinde basacak. 

Hata mesajlarını ekrana basmak için `stderr` ile çalışmak lazım, mesela alttaki
komut bir derleme `gcc` komutunu bilerek yanlış işletti, hata mesajı gösterilmesi
lazım ama hiçbir şey görülmüyor,

```python
cmd = ['gcc','-c','dkflakjsdlkas']
p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
res = p.stdout.read()
print (res)
```

```text
b''
```

Hataları görmek için alttaki yaklaşım gerekli,

```python
import subprocess 
process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.STDOUT, shell=False)
(output, _) = process.communicate()
res = str(output).split("\\n")
for x in res: print (x)
```

```text
b'gcc: error: dkflakjsdlkas: No such file or directory
gcc: fatal error: no input files
compilation terminated.
'
```
