# izip


izip



Python gezinme (iteration) araclarindan izip, birden fazla dizinin ayni anda gezilmesini saglar. Her dizinde yapilan gezinme hareketi, otekilerde ayni sekilde yapilir.from itertools import izipaa = [1,2,3]bb = [5,6,7]for a, b in izip(aa, bb):   print a, bprogrami su sonucu verir:1 52 63 7




