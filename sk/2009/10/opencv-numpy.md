# OpenCv -> Numpy

OpenCv uzerinden okunan bir imaji Numpy ile isleyebilmek icin veriyi
Numpy'in isleyebilecegi bir array tipine cevirmek gerekiyor. Bu
ceviriyi yapmak icin gereken kod OPENCV/interfaces/swig/python dizini
altindaki adaptors.py kodu icinde. Once, eger yapmadiysaniz, tum bu
dizini "make; sudo make install" ile derleyip kurmak lazim. Adaptor
kodunu soyle kullaniyoruzfrom opencv import adaptorsdef dotrack(img):
..  numpy_arr = adaptors.Ipl2NumPy(img)Geri gelen numpy_arr uc boyutlu
bir vektor. X ve Y kordinatlar ve bu indislerin kullanimiyla gelen 3
elemanli "ufak array" icinde ise sirasiyla R, G, B degerleri olacak,
yani kirmizi (red), yesil (green) ve mavi (blue) renklerinin degerleri
olacak. Bu array uzerinde degisim yapilirsa geri OpenCv imajina
cevirmek icin NumPy2Ipl cagrisi kullanilabilir. Array boyutlarina
print len(numpy_arr..) ile bakabilirsiniz, erisim normal array
erisimi, numpy_arr[100][100][0] kullanimi 100x100 kordinatindaki
kirmizi degeri verecek mesela. Kamera goruntusu indislere [y][x][renk]
olarak yansiyor.





