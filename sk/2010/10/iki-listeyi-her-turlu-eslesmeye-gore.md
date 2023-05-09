# Iki Listeyi Her Turlu Eslesmeye Gore Gezmek

Iki listeyi, bu listedeki elemanlarin her turlu eslesmesinin
olusturdugu ikilileri gorerek gezmek istiyorsak itertools paketi iyi
isler. Mesela a ve b listelerini soyle gezebiliriz:import itertoolsa =
['foo', 'bar', 'baz']b = ['x', 'y', 'z', 'w']for (r,s) in
itertools.product(a, b): print r,sUstteki kod su sonucu verecek:foo
xfoo yfoo zfoo w...baz ybaz zbaz wYani iki listenin her turlu
kombinasyonu tek bir satirla gezilebilmis oluyor.





