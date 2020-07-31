# Liste Gezerken Eleman Silmek

Python ile bir listeyi gezdigimiz anda gezdigimiz listeden eleman
silersek, gezme islemi negatif sekilde etkilenmis olacaktir. Mesela
soyle bir kod dusunelim:list = [["1","1"], ["2","2"]]for item in list:
print item if ["1","1"] in list: list.remove(["1","1"])Bu kod sadece
['1', '1'] sonucunu basacaktir, cunku gezme sirasinda bir eleman
(['1', '1']) silinmistir, ve bu listeyi kucultmustur.Eger silme
isleminin gezme islemini etkilememesini istiyorsak, o zaman listenin
bir "kopyasi" uzerinde gezinti yapmamiz lazim. Python'da kopya
cikartmak icin clone, copy gibi cagrilar yerine bir operator
kullaniliyor; bu operator [:] operatoru. O zaman:list = [["1","1"],
["2","2"]]for item in list[:]: print item if ["1","1"] in list:
list.remove(["1","1"])istedigimiz sonucu verecektir.




