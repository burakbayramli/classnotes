# Operatorler


Operatorler



Bir class tipi uzerinde islemesini istedigimiz operatorler icin o tipin class'i icindeki "ozel" fonksiyon isimlerini kullanmamiz gerekir. Mesela X adli bir tip icin kucuktur isaretini tanimlamak isteseydik, __lt__ fonksiyonunu tanimlamamiz gerekirdi. Ornek:class X:   def __init__(self, x):       self.x = x   def __lt__(self, other):       if (other < self.x):           return True       return Falsexx = X(10)yy = X(20)if (xx < yy): print "kucuk"if (xx > yy): print "degil"




