# Popen, Python ve Komut Satirina Erismek


Popen, Python ve Komut Satirina Erismek




Script yazarken bazen ihtiyaclardan biri  python script'i icinde bir shell script isletebilmek. Ciktisi islenmeyenler icin os.system(komut) yetiyor tabii. Fakat komut ciktisini gostermek istiyorsak?

import os
res = os.popen("ls -al")

kullanimi basit - fakat tedavulden kalkacakmis (deprecated), bu yuzden kalici kodlari ona bagli yazmak iyi olmaz. Kutuphane subprocess tavsiye ediliyor,

import subprocess 
p = subprocess.Popen(['ls','-al'], stdout=subprocess.PIPE)res = p.stdout.read()print res

Is biraz daha zorlasti ama kullana kullana alisilabilir. 

Bir diger ihtiyac komut "islerken" onun ciktisini iterator ile gezebilmektir. Belki komut yavas isleyecektir, beklerken belki o ana kadar olan ciktiyi islemek / ekrana basmak ve yan bazi islerle ugrasmak isteyebiliriz. Bunu subprocess ile yapabiliyorsunuz,

p = subprocess.Popen(['ping','-c','3','localhost'], stdout=subprocess.PIPE)for line in p.stdout:    print line

Ustteki komut  her satiri ayri ayri, Python dongusu icinde basacak. 





