# Numpy Loadtxt ve Converter

CSV dosyasi diye bilinen bosluk, virgul, vs. ile ayrilmis satirsal,
duz text bazli dosyalari Loadtxt ile yukluyoruz. Eger bu yukleme
sirasinda mesela bir float olmasi gereken bir hucrede 'NA' gibi bir
string bazli deger varsa, loadtxt bu degeri float yapamadigi icin
sikayet edecektir. NA bilindigi gibi "deger yok" anlamina gelen "not
applicable" kelimesinden geliyor; Numpy icinde buna tekabul eden bir
float degeri var (Nump.nan diye erisiliyor), o zaman String gorunce bu
degere donusumu bizim yapmamiz lazim.Alttaki kod nes.dat adli bir
dosyayi okur, 1. satiri atlar, ve sadece 11 ve 32. kolonlarini cekip
cikartir, bu kolonlarin her ikisinde de bazen NA metni gorulurse, onu
converter() fonksiyonu uzerinden numpy.nan degerine cevirir.import
numpydef converter(x): if x == 'NA': return numpy.nan else: return
float(x)nes = numpy.loadtxt("nes.dat", skiprows=1, usecols = (11,32),
converters={11:converter, 32:converter})





