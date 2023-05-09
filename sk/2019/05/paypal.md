# Paypal, Internet'te Ödeme Almak

Paypal Türkiye'deki hizmetine son vermis fakat ileride açılabilir,
ayrıca yabancı ülkelerde banka hesapları üzerinden hala Paypal
kullanılabilir. Yazının geri kalanı işler bir Paypal hesabı olduğunu
farz ediyor.

Paypal aslında ciddi bir ödeme sistemi, hatta daha ilerisinde bir
"ödeme işleyici (payment processor)". Bu tür şirketler dünya finans
sistemının belkemiğine bağlıdır, bu bağlantıyı kurmak kolay değildir,
mesela Google Pay var, ama GP bir ödeme işleyici değil. Diğer ödeme
işleyicilere bağlanarak işini yapıyor, Paypal gibi.

Eğer normal paypal.com hesabınız varsa, onunla alakalı

developer.paypal.com

hesabınız da olacaktır. Bu adresten Paypal'ın geliştirme ve test
araçlarına erisebiliyorsunuz. Üstteki adrese paypal kullanıcı /
şifremiz ile giriş yapalım. Burada bazı taklit satıcı, müşteri
hesapları yaratacağız. Accounts'a tıklayın orada kum kabı (sandbox)
hesapları yaratabiliriz. "Kum kabı" içinde "kumda kale yapar gibi" bu
taklit hesaplar ile işlem gerçek işlemlere yakın hareketler
yapacağız. Bir tane iş (business) hesabı yaratalim ki bu hesaptan
ödeme kabul edebilelim, diğeri kişisel taklit hesap olabilir, ki ödeme
gonderebilelim. Create Account ile bu hesapları yaratırız. Hesap
yaratırken kullanılan email ve şifre ile şimdi

sandbox.paypal.com

adresine giriş yapabiliriz. Bu adres bizi bu taklit kullanıcıların
"hesabına" götürüyor. Bu kullanıcılara neredeyse tam tekmilli paypal
hesaplariymis gibi, o sayfalar üzerinden erisebilmek önemli böylece bu
hesaplarda oynamalar, değişiklikler yaparak farklı test senaryolarını
deneyebiliriz, ama bu testler gerçek hesabımızı etkilemiyor.

Bir "satın al" butonu yaratalım. Satıcının hesabına girelim, menüden
Tools | Business setup seçelim. Payment setup'tan "on your website"
seçip Continue tıklayalım. Sonraki sayfada ödemeleri nasıl işlemek
istediğimiz soruluyor, sol blok seçilebilir, oradaki Continue
seçilebilir.

Step 1 diyen, bir sonraki ekranda "create payment buttons using HTML".
Düğme tipi için "Buy Now" tipi yeterli, en basiti bu.

Step 2 altında "save button at Paypal" işaretini iptal edelim. Bir
ürün fiyatı girelim, mesela 10 dolar.

Step 3 altında Add advances variables kısmı var, bu kutu önemli,
Paypal ödemeyi aldıktan sonra bizim sitemize bildirimi nasıl yapacak,
onu gösteriyor. Bu kutuya

```python
notify_url=[bizim site ve paypal url]
```

Simdi create button ile düğmeyi üretelim. Düğme bir HTML kodu olarak
bize verilecek, bu kodu sitemizin HTML'i içine koyunca bir ödeme
düğmesi göreceğiz. Sayfayı tarayıcıda ziyaret edelim ve satın alma
düğmesine tıklayaylım, paypal'e gideceğiz, burada taklit alıcı
hesabıyla giriş yapıp ödemeyi yapalım. Ödeme şekillerinde kredi kart,
paypal hesabı gibi seçenekler var, taklit kredi kartı da
kullanabilirdik. Son birkaç rakamı gösterilen kart gerçek değil. Ödeme
düğmesi bizi sandbox alanına götürmüştü, çünkü düğmeyi bu taklit
alanda yaratmıştık.

Artık www.sandbox.paypal.com adresine gidersek burada hesabımızdan 10
dolar eksiltildiğini görürüz. Bu tabii ki taklit para. Şimdi satıcı
hesabına girersek orada 10 dolara yakın bir paranın hesabımıza
geçtiğini görüyoruz (10 dolardan biraz eksik olabilir çünkü paypal
komisyonunu kesti).

Eğer "save button at Paypal" işaretini iptal etmezsek, düğme Paypal
sisteminde kaydedilecektir, ve bize verilen düğme HTML'i bir tür
referans haline gelecektir. Bu referansın ima ettiği düğme (satış
fiyatı, ürün, vs) bilgilerini istersek sonradan Paypal'de
değiştirebiliriz, HTML kodunda hiçbir değişim olmaz. Bu sonuç ortamını
test etmek ve diğer bazı durular için faydalı olabilir, mesela bir
düğmeyi 5 USD ile test ederiz, sonra 30 USD'ye çıkartabiliriz.

Kaydedilmiş düğmeleri görmek için `Tools | All Tools`'dan Paypal
Buttons'a tıklayabiliriz, ya da direk https://paypal.com/buttons
bağlantısına gidebiliriz.

Paypal'den Bilgi Almak

Üstte `notify_url` ile bir URL verdik. Bu URL bizim sitemizde mesela
Flask ile servis ettiğimiz / dinlediğimiz bir adres olacak. Flask'te
şu kodlar kullanılabilir,

```python
from flask import Flask, jsonify, render_template, request
import requests
import urllib.parse
...
VERIFY_URL_PROD = 'https://ipnpb.paypal.com/cgi-bin/webscr'
VERIFY_URL_TEST = 'https://ipnpb.sandbox.paypal.com/cgi-bin/webscr'
VERIFY_URL = VERIFY_URL_TEST
...
@app.route('/paypal_ipn', methods=['POST','GET'])
def paypal_ipn():    
    params = request.form.to_dict()
    params['cmd'] = '_notify-validate'
    print (params)
    headers = {'content-type': 'application/x-www-form-urlencoded',
               'user-agent': 'Python-IPN-Verification-Script'}
    r = requests.post(VERIFY_URL, params=params, headers=headers, verify=True)
    r.raise_for_status()

    if r.text == 'VERIFIED':
        print ("Verified")
    elif r.text == 'INVALID':
        print ("Invalid")
    else:
        print ("some other response")
        print (r.text)       
    return ""
```

Şimdi Paypal'de ödeme yaptıktan sonra Paypal servis tarafı bizim
servisimiz ile bağlantıya geçecek, `bizim site/paypal_ipn`'e
bağlanacak (bu adresi düğme yaratırken vermiş olmamız lazım tabii), ve
log'da bize gönderdiği bilgileri göreceğiz. Müşteri kodu, email'i,
adresi gibi bir sürü bilgi burada var. Bu bilgileri kaydedip bir
kullanıcı girişi için ileride kullanabiliriz.

Bu çağrının oturumsal, asenkronluk durumuna gelelim: mesela kullanıcı
sisteme girmiş (logged in) sonra Paypal ödeme düğmesinden ödeme yapmış
ve Paypal'den çağrıyı üstteki metota almışız. Bu durumda kullanıcı
oturumu ile bu çağrı arasında bağlantı yapabilir miyiz?

Bunu yapamayız. Üstteki geri çağrı (çallback) asenkron şekilde Flask
tarafından işletilecektir, fakat bu farklı bir Thread üzerinde
asenkron şekilde yapılır, çok süreçli ortamlarda çağrıyı işleyen o
çağrıyı tetikleyen kullanıcının üzerinde olduğu makinadan farklı bile
olabilir. Bu durumda bu çağrıyı konumsuz (stateless) bir çağrı olarak
işlemek gerekecektir, yani objesel değil fonksiyonel düşünmek
lazım. Geri çağrıda bize verilen ne bilgi varsa sadece onları
kullanabiliriz, daha fazlasını, mesela "içinde olduğumüz" objenin iç
değişkenleri gibi, kullanamayız. Yani hernangi bir kullanıcı ve o
bilgi arasında eşleştirmeyi veri tabanı üzerinden yapmamız lazım.

Flask Python projemizde gerekli ek paketler `requests`, ve
`urllib3`. Onlar `pip` ile kurulmuş olmalı.

Paypal'den gelen üstte `params` içindeki sözlüğe bakarsak,

``` {'mc_gross': '5.00', 'protection_eligibility': 'Eligible',
'address_status': 'confirmed', 'payer_id': '[ID]', 'address_street':
'1 Main St', 'payment_date': '00:06:13 May 30, 2019 PDT',
'payment_status': 'Completed', 'charset': 'windows-1252',
'address_zip': '95131', 'first_name': 'sdfsdf', 'mc_fee': '0.55',
'address_country_code': 'US', 'address_name': 'sdfsdf asdfasdf',
'notify_version': '3.9', 'custom': '', 'payer_status': 'verified',
'business': '[ALICI EMAIL]', 'address_country': 'United
States', 'address_city': 'San Jose', 'quantity': '1', 'verify_sign':
'[UZUN BIR ID]', 'payer_email': '[EMAIL ADRESI]', 'txn_id':
'[BIR ID]', 'payment_type': 'instant', 'last_name':
'asdfasdf', 'address_state': 'CA', 'receiver_email': '[SATICI EMAIL]',
'payment_fee': '', 'shipping_discount': '0.00', 'insurance_amount':
'0.00', 'receiver_id': 'UBDDHGYZT5F3E', 'txn_type': 'web_accept',
'item_name': '[URUN KODU]', 'discount': '0.00', 'mc_currency': 'EUR',
'item_number': '', 'residence_country': 'US', 'test_ipn': '1',
'shipping_method': 'Default', 'transaction_subject': '',
'payment_gross': '', 'ipn_track_id': '2226091c8006d', 'cmd':
'_notify-validate'} ```

gibi bir çıktı göreceğiz. 

Kaynaklar

[1] https://www.youtube.com/watch?v=NFUdd3gveN8

