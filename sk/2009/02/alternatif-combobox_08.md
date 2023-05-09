# Alternatif Combobox

Standard HTML birimlerinden olan select..options birimlerinin ne yazik
ki bazi problemleri var; Oncelikle IE 6 uzerinde bu select
etiketlerini stillemek, yani CSS ile bazi goruntu ogelerini
degistirmek mumkun degil. Bunlardan biri border degisimi mesela, siz
CSS'e hangi komutu koyarsaniz koyun, IE 6 uzerinde select o garip uc
boyutumsu golgeli sInIrlI goruntuyu kaybedemiyor.

Bu sebeple klasik select..option yerine, CSS/DHTML/Javascript bazli
alternatif bir Combobox kullanmak daha yerinde olacak. Bu alternatif
kodlar <div> temelli olacak ve bu kodlar Javascript bazli event'leri
kullanarak bir normal combobox isleyisini taklit edecekler.  Bu
birimler direk en temel DHTML ogeleri ile is yaptigi icin, onun
herhangi bir gorsel ogesi ile oynamak serbest olabilecek.

Peki hangi paketi kullanmak en iyisi? Etrafta pek cok Javascript bazli
combobox kodu var. Hem combobox hem de diger veri giris ogelerinin
alternatifleri icin su sayfa faydali olabilir. Benim buldugum ayri bir
paket dhtmlxcombo adinda bir pakettir. Bu paket, duz HTML temelli olan
select'i alip onu combolastiriyor, ya da sadece bos bir <div> blogunu
da alip combo haline getirebiliyor.

Bu kutuphaneyi kullanarak yazdigimiz ornek kodlari altta
bulabilirsiniz. Onemli noktalardan biri combo'nun asagi gosteren ok
imajini bulabilmesi icin `<script>` tagi icinde `window.dhx_globalImgPath="/imaj/dizin/ismi";`
ile ok imajinin yerini vermeniz. test.html icinde bunu gorebilirsiniz.

Dosyalar

`dhtmlxcombo.js` ve dhtmlxcommon.js ustteki kaynaklardan indirilebilir,
ayrica bir blank.gif ve combo_select.gif lazim.

dhtmlxcombo.css

```
.dhx_combo_img{
    position:absolute;
    top:0px;
    right:0px;
    width:17px;
    height:20px;
}

.dhx_combo_option_img{
    position:relative;
    top:1px;
    margin-left:2px;
    left:0px;
    width:18px; height:18px;
}
.dhx_combo_input{
    color:#333333;
    font-family: Arial;
    font-size: 9pt;
    border:0px;
    padding:2px 2px 2px 2px;
    position:absolute;
    top:0px;
}
.dhx_combo_box{
    position:relative;
    text-align:left;
    border:1px solid #C3BBB6;
    height:20px;
    _height:22px;
    overflow:hidden;
    background-color: white;
}

.dhx_combo_list{
    position:absolute;
    z-index:230;
    overflow-y:auto;
    overflow-x:hidden;
    border:1px solid black;
    height:100px;
    font-family: Arial;
    font-size: 9pt;
    background-color: white;
}


.dhx_combo_list div{
    cursor:default;
    padding:2px 2px 2px 2px;
}
.dhx_selected_option{
    background-color:navy;
    color:white;
}


.dhx_combo_img_rtl{
    position:absolute;
    top:0px;
    left:1px;
    width:17px;
    height:20px;
}
.dhx_combo_option_img_rtl{
    float:right;
    margin-right :0px;
    width:18px; height:18px;
}

.dhx_combo_list_rtl{
    direction: rtl;
    unicode-bidi : bidi-override;
    position:absolute;
    z-index:230;
    overflow-y:auto;
    overflow-x:hidden;
    border:1px solid black;
    height:100px;
    font-family: Arial;
    font-size: 9pt;
    background-color: white;
}
.dhx_combo_list_rtl div{
    direction: rtl;
    unicode-bidi : bidi-override;
}
.dhx_combo_list_rtl div div{
    float :right !important;
    cursor:default;
    padding:2px 2px 2px 2px;
}
.dhx_combo_list_rtl div img{
    float :right !important;
}
.dhx_combo_list_rtl div input{
    float :right !important;
}
```


test.html


```
<html>

  <head>

    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />

    <script  src="dhtmlxcommon.js"></script>

    <script  src="dhtmlxcombo.js"></script>

    <link rel="STYLESHEET" type="text/css" href="dhtmlxcombo.css">

    <script>

      window.dhx_globalImgPath="/home/burak/";

    </script>

  </head>



  <body>



    <div id="combo_zone2" style="width:200px; height:30px;"></div>

    <script>

      var z=new dhtmlXCombo("combo_zone2","alfa3",200);

      z.addOption([[1,1111],[2,2222],[3,3333],[4,4444],[5,5555]]);

    </script>


  </body>


</html>
```
