# CVS'e GIF, JPG ve JAR Nasıl Eklenir?


CVS'e GIF, JPG ve JAR Nasıl Eklenir?



 CVS'te en sık karşılanan problemlerden biri, .gif, .jpg ve .jar gibi ikisel (binary) dosyaları CVS'e ekleyip, geri alınca bu dosyaların bozulduğunu görmektir!               Bunun sebebi, CVS'in bu dosyaları metin bazlı dosyalar gibi görmesi ve üzerinde değişim yaparken dosyayı bozmasıdır.              Bu hatayı düzetlmek için, CVS'i kurduğunuz makine ve dizine girip şu değişikliği yapın. ENÜSTCVSDİZİNİ/CVSROOT/cvswrappers adlı bir dosya bulacaksınız. Metin bazlı dosyadan daha değişik muamele görmesini istediğiniz dosyaları, şu şekilde cvswrappers içine ekleyin.                *.gif -k 'b'*.jpg -k 'b'*.jar -k 'b'..... vs.              Bu yeterli olacaktır. CVS'i tekrar başlatmanıza bile gerek yoktur. Şimdi yeni ekleyeceğiniz dosyalar doğru muamele ediliyor olacaklar.              Eğer daha önceden eklenmiş dosyaları düzeltmek istiyorsanız, cvs remove, cvs commit ve arkasından cvs add ve cvs commit ile bu dosyaların düzgün hâlini tekrar ekleyin.




