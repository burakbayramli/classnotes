# Kitap (epub, pdf) Icerigini Cikartmak

Herhangi bir epub, pdf bazli kitap, ya da makale icerigini Python ile
metin (text) haline cevirmek  icin once

```
pip install textract
```

Sonra

def book_extract(file_in, from_perc, to_perc, file_out):
    text = textract.process(file_in,encoding='ascii')
    L = len(text)
    from_l = int((L * from_perc) / 100.0)
    to_l = int((L * to_perc) / 100.0)
    t = str(text[from_l:to_l] )
    fout = codecs.open (file_out,"w","utf-8")
    fout.write(t)
    fout.close()


book_extract(os.environ['HOME'] + "kitap.epub", 10, 11, "out.txt")

from_perc ve to_perc yuzde degerleri.







