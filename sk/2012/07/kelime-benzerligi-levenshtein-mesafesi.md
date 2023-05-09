# Kelime Benzerligi - Levenshtein Mesafesi

Kelime benzerliğini bulmak harfleme (spelling) hataları düzeltmekten,
benzer kelimeleri gruplamaya kadar pek çok alanda kullanışlı
olabilir. Gruplama algoritmaları çoğunlukla grupladıkları "şeyler"
arasındaki mesafeyi bir şekilde hesaplayan bir fonksiyona ihtiyaç
duyarlar. Levenshtein mesafesi kelimeler için bu ölçütlerden
biridir. Aslında fikir basit, "en az kaç değiştirme operasyonu ile
birinci kelimeden ikinciye ulaşabiliriz". Kaç tane operasyon
kullanıldıysa, mesafe o'dur. Mesela pizza kelimesinden piazzo
kelimesine 2 basamakta gelinebilir. Önce 'a' silinir sonra 'o' harfi
'a' yapılır.

Levenshtein ıcın en hızlı kodlardan (arka planda Ç ile kodlanmış) şurası var

https://code.google.com/p/pylevenshtein/

Kurulur, test için

```
from Levenshtein import *print distance("pizza", "piazzo")
```

Pur Python ile

```
def printMatrix(m):
    print ' '
    for line in m:
        spTupel = ()
        breite = len(line)
        for column in line:
            spTupel = spTupel + (column, )
        print "%3i"*breite % spTupel

def levenshtein(s1, s2):
  l1 = len(s1)  l2 = len(s2)
  matrix = [range(l1 + 1)] * (l2 + 1)
  for zz in range(l2 + 1):
    matrix[zz] = range(zz,zz + l1 + 1)
  for zz in range(0,l2):
    for sz in range(0,l1):
      if s1[sz] == s2[zz]:
        matrix[zz+1][sz+1] = min(matrix[zz+1][sz] + 1, matrix[zz][sz+1] + 1, matrix[zz][sz]) 
     else:
        matrix[zz+1][sz+1] = min(matrix[zz+1][sz] + 1, matrix[zz][sz+1] + 1, matrix[zz][sz] + 1)
  print "That's the Levenshtein-Matrix:"
  printMatrix(matrix)
  return matrix[l2][l1]if __name__ == "__main__":
    s1 = "pizza"
    s2 = "pioazza"
   
    distance = levenshtein(s1, s2)
   
   
    print 'The Levenshtein-Distance of ',s1, ' and ', s2, ' is ', distance
    s1 = "hamburger"    s2 = "haemmurger"
   
    distance = levenshtein(s1, s2)
   
   
    print 'The Levenshtein-Distance of ',s1, ' and ', s2, ' is ', distance
    
```




