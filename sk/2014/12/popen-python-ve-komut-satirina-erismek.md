# Python ve Komut Satırına Erişmek, Popen

Script yazarken bazen ihtiyaçlardan biri python script'i içinde bir
shell script işletebilmek. Çıktısı işlenmeyenler için
`os.system(komut)` yetiyor tabii. Fakat komut çıktısını göstermek
istiyorsak?

```
import os
res = os.popen("ls -al")
```

kullanimi basit - fakat tedavulden kalkacakmis (deprecated), bu yuzden
kalici kodlari ona bagli yazmak iyi olmaz. Kutuphane subprocess
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
uğraşmak isteyebiliriz. Bunu şubprocess ile yapabiliyorsunuz,

```
p = subprocess.Popen(['ping','-c','3','localhost'], stdout=subprocess.PIPE)
for line in p.stdout:
    print line
```

Üstteki komut  her satırı ayrı ayrı, Python döngüsü içinde basacak. 

