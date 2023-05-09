# Dropdown Secimi Ile Ayri DIV Gostermek

Basit bir prezentasyon teknigi, sukseli bazi raporlari "akilli
dokuman" olarak gondermek icin iyi olabilir. Listeden secilen kaleme
ait olan bilgiler kendi DIV etiketleri icinde tutuluyor, hangi kalem
secilirse onun bilgisi altta hemen basiliyor. 

```
<script>function displayDiv(e){  divs =new Array('ELMA', 'ARMUT', 'PORTAKAL');
  for (var i=0;i<divs.length;i++){
    document.getElementById(divs[i]).style.display = "none";
  }
  document.getElementById(e.innerHTML).style.display = "block";}</script><select name="#" id="#" onChange="displayDiv(this.options[this.selectedIndex]);">
      <option selected="selected"> </option>
      <option >ELMA</option>
      <option >ARMUT</option>
      <option >PORTAKAL</option></select><br/><br/><div id='ELMA'
  style='display:none;'>
  Elma hakkinda seyler buraya...</div><div id='ARMUT'
  style='display:none;'>
  Armut hakkinda seyler buraya...</div><div id='PORTAKAL'
  style='display:none;'>  Portakal hakkinda seyler buraya...
</div>
```
