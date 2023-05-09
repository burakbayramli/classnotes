# $.getJSON Hata Durumlari

Javascript kodu icinde eger servis tarafina soyle bir $.getJSON Ajax
cagrisi yaptik diyelim:$.getJSON('/bizim/url', {'param1': param1},
function(data){ ... });Eger bu fonksiyon normal sekilde geri donerse
isleyis "function(data)" icine dusecektir. Ama hata dondururse ne
yapariz? O zaman finally mantigina benzer bir kullanim
gerekli:$.getJSON('/bizim/url', {'param1': param1}, function(data){
... }).complete(function() { /* temizlik islemleri */ });Dikkat: Ajax
cagrilari yaparken, cagri dondukten sonra hemen bir sonraki Javascript
kodunun "sirasiyla" isleyecegini farz etmeyelim. Ajax asenkron olarak
isler, ve cagri sonrasi, cagri geri donmeden bile, bir sonraki
Javascript kodu isleme konabilir.





