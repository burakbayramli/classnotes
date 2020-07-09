# Pentaho ve Hesaplanmis Olcutler, Yuzdeler




Pentaho ve Hesaplanmis Olcutler, Yuzdeler





Pentaho icindeki Mondrian OLAP motoruna bir boyut cercevesinde veriyi kesip bicerken hesapli olcut (calculated measure) uzerinden yuzde raporlatmak mumkun. Mesela dellstore orneginde cinsiyet uzerinden veriyi incelerken hem gercek sayi hem de yuzde gormek istiyoruz. Bunun icin analtik kup xml icinde

    <CalculatedMember        name="GenderPercent"        dimension="Measures"        formula="[Measures].[orders] / ([Measures].[orders], [gender].Parent)">      <CalculatedMemberProperty          name="FORMAT_STRING"          value="0.0%"/>    </CalculatedMember>  </Cube>

tanimlariz. Ve cinsiyet (gender) uzerine tiklayinca miktar (amount) olcutu ile beraber hesaplanmis olcut cinsiyet yuzdesinin (gender percent) gosterildigini gorebiliriz. 


 



Kaynak





![](Screenshot+at+2012-05-27+13:35:48.png)
