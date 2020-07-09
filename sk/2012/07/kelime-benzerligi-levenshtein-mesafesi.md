# Kelime Benzerligi - Levenshtein Mesafesi


Kelime Benzerligi - Levenshtein Mesafesi




Kelime benzerligini bulmak harfleme (spelling) hatalari duzeltmekten, benzer kelimeleri gruplamaya kadar pek cok alanda kullanisli olabilir. Gruplama algoritmalari cogunlukla grupladiklari "seyler" arasindaki mesafeyi bir sekilde hesaplayan bir fonksiyona ihtiyac duyarlar. Levenshtein mesafesi kelimeler icin bu olcutlerden biridir. Aslinda fikir basit, "en az kac degistirme operasyonu ile birinci kelimeden ikinciye ulasabiliriz". Kac tane operasyon kullanildiysa, mesafe o'dur. Mesela pizza kelimesinden piazzo kelimesine 2 basamakta gelinebilir. Once 'a' silinir sonra 'o' harfi 'a' yapilir.

Levenshtein icin en hizli kodlardan (arka planda C ile kodlanmis) surasi var

https://code.google.com/p/pylevenshtein/

Kurulur, test icin

from Levenshtein import *print distance("pizza", "piazzo")

Pur Python ile

''' Calculates the Levenshtein distance of 2 strings'''def printMatrix(m):    print ' '    for line in m:        spTupel = ()        breite = len(line)        for column in line:            spTupel = spTupel + (column, )        print "%3i"*breite % spTupeldef levenshtein(s1, s2):  l1 = len(s1)  l2 = len(s2)  matrix = [range(l1 + 1)] * (l2 + 1)  for zz in range(l2 + 1):    matrix[zz] = range(zz,zz + l1 + 1)  for zz in range(0,l2):    for sz in range(0,l1):      if s1[sz] == s2[zz]:        matrix[zz+1][sz+1] = min(matrix[zz+1][sz] + 1, matrix[zz][sz+1] + 1, matrix[zz][sz])      else:        matrix[zz+1][sz+1] = min(matrix[zz+1][sz] + 1, matrix[zz][sz+1] + 1, matrix[zz][sz] + 1)  print "That's the Levenshtein-Matrix:"  printMatrix(matrix)  return matrix[l2][l1]if __name__ == "__main__":     s1 = "pizza"    s2 = "pioazza"        distance = levenshtein(s1, s2)            print 'The Levenshtein-Distance of ',s1, ' and ', s2, ' is ', distance    s1 = "hamburger"    s2 = "haemmurger"        distance = levenshtein(s1, s2)            print 'The Levenshtein-Distance of ',s1, ' and ', s2, ' is ', distance    




