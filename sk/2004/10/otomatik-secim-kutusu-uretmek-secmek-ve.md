# Otomatik Seçim Kutusu Üretmek, Seçmek ve İşlemek

JSP/Struts bazlı projelerde bazı sayfalarda dinamik olarak seçim
kutusu (checkbox) üretmek gerekiyor. Veri tabanında kayıtlı olan
müşterileri bir ekranda gösterip, seçim kutusu ile seçilen isimleri
veri tabanından silmek gibi bir özellik için bu lazım olabilir. Bu
seçim kutularına programatik olarak isim1, isim2, vs... gibi tekil
no'lar da atanması gerekecektir. Kullanıcı tarafından seçim yapılıp
sayfa form olarak servis'e gönderildiğinde, arka planda id'leri
tanımak için, şöyle bir kod kullanmak gerekir.


```
public void doGet(HttpServletRequest request,
                HttpServletResponse response)
...
...
for (Enumeration e = request.getParameterNames(); e.hasMoreElements() ;) {
  String key = (String)e.nextElement();
  if (key.indexOf("isim") != -1) {
    if (key.length() > 4) {
      String id = key.substring(4, key.length());
      // elde edilen id ile bir islem yap
    }
  }
}
...
```
 
Hepsini Seç Özelliği 
    
Bazen müşterileriniz, eğer ekranda çok fazla seçim kutusu olması
muhtemel ise, "hepsini seç" gibi tek bir seçim kutusu ile bütün seçim
kutularını seçmeyi isteyebilirler. Bunu yapmak için Javascript
kullanmamamız gerekiyor. Aşağıdaki Javascript işlevi, parametre olarak
gönderdiğiniz herhangi bir isim "ile başlayan" bütün seçim kutularını
otomatik olarak seçebiliyor. Bu kodu, lâzım olan JSP sayfalarının
başına koyarak, sayfanın istediğiniz yerinden rahatça
çağırabilirsiniz.

  
```  
<script type="text/javascript" language="javascript">
function check_uncheck_all(grpStr, button) {
var form, el, e, f = 0;
var unbuttoned = (button.value.substring(0,2).toLowerCase() == 'un');
while (form = document.forms[f++]) {
e = 0;
while (el = form.elements[e++])
if (el.type == 'checkbox' && el.name.indexOf(grpStr) != -1)
  el.checked = !unbuttoned;
}
button.value = unbuttoned ? button.value.substring(2) : 'un' + button.value;
}
</script>
```
   
Çağırmak için...   

``` 
   <INPUT type=checkbox name=list value="Tumunu Sec"
   onClick="check_uncheck_all('isim',this)">Tumunu Sec
```

``` 
</font></p>
<p>
<font face="Courier New, Courier, monospace">function check_uncheck_all(grpStr, button) {</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp;var form, el, e, f = 0;</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp;var unbuttoned = (button.value.substring(0,2).toLowerCase() == 'un');</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp;while (form = document.forms[f++]) {</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp; &nbsp;e = 0;</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp; &nbsp;while (el = form.elements[e++])</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp; &nbsp;if (el.type == 'checkbox' &amp;&amp; el.name.indexOf(grpStr) != -1)</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp; &nbsp; &nbsp; el.checked = !unbuttoned;</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp;}</font></p>
<p>
<font face="Courier New, Courier, monospace">&nbsp;button.value = unbuttoned ? button.value.substring(2) : 'un' + button.value;</font></p>
<p>
<font face="Courier New, Courier, monospace">}</font></p>
<p>
<font face="Courier New, Courier, monospace">


Isim 1

Isim 2

Isim 3

Isim 4

onClick="check_uncheck_all('isim',this)">Tumunu Sec
```








