# Basit Web Sunucusu - SimpleHTTPServer

Eger hizli bir sekilde, acil ihtiyaclar icin bir Web sunucusu
baslatmak isterseniz, SimpleHTTPServer bu is icin bicilmis
kaftan. Sadece su satirlar ile herhangi bir dizindeki dosyalar servis
edilebilir. Mesela..

```
import sys
import BaseHTTPServer
from SimpleHTTPServer import SimpleHTTPRequestHandler

HandlerClass = SimpleHTTPRequestHandlerServerClass
  = BaseHTTPServer.HTTPServerProtocol
     = "HTTP/1.0"if sys.argv[1:]:
    port = int(sys.argv[1])else:
    port = 8080server_address = ('[IP ADRESI]', port)
HandlerClass.protocol_version = Protocol
httpd = ServerClass(server_address, HandlerClass)
sa = httpd.socket.getsockname()
print "Serving HTTP on", sa[0], "port", sa[1], "..."
httpd.serve_forever() 
```

Bu kadar. Olagan port degeri 8080, ama istenilen baska bir port
numarasi arguman olarak gecilebilir. Eger bu script'i baslattigimiz
dizinde mesela bir index.html var ise, aynen normal Web sunucularinin
yaptigi gibi o dizin ziyaret edilir edilmez otomatik olarak servis
edilecektir.

Bir puf nokta: ustteki script, komut satirindan baslatildiktan sonra,
bunu yapan Unix kullanicisinin sistem disina cikmasi (logout) olayina
hassas, o yuzden python ibaresinin basina nohup komutunun eklenmesi
lazim.

Biz yukaridaki script'i bir ana siteyi bakima aldigimizda (ki site
kendine has apayri baska bir Web altyapisi kullaniyor) hizli bir
sekilde "simdi mesguluz sonra gelin" mesajini gostermek icin
kullaniyoruz. Ana sitenin ayni IP ve port numarasi uzerinden hemen bir
SimpleHTTPServer baslatiyoruz, ve ana siteye gelen kullanicilar bu
basit sunucunun servis ettigi mesaji goruyorlar.






