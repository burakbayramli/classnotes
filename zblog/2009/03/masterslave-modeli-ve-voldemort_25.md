# Master/Slave Modeli ve Voldemort


Master/Slave Modeli ve Voldemort



Master/Slave bazli calisan yapilar, okumalari N sayida slave'e, yazmalari tek master'a gonderirler, ve okumalarin yazmadan cok oldugu durumlarda sistemi olcekleyebiliyorlar.Voldemort gibi bir sistem, hem okuma, hem yazma icin ayni anda olceklenebilen bir sistemdir. Ortada bir master, slave yoktur - her islem ne olursa olsun, anahtar bazli olarak bir "bolume yonlendirilir" ve gerekli makinaya gider. Ayrica, M/S cozumunde kullanilmasi gereken master ve slave arasindaki replikasyon suresi, bir VM kumesinde problem degildir cunku replikasyon kullanilmamaktadir. Her verinin yeri bellidir. (Replikasyon kullanildiginda yedekleme amaciyla yapilir, ama bu replikasyonun kac baska makinaya dogru olacagi sistem tarafindan kontrol edilir, hic bir kurulum, ek efor, kodlama gerektirmez).Bu da akilda tutulmasi gereken bir husus.Bir ek not: Tek makinadan cok makinaya gectiginizde transaction problem olabilir - bu durumda VM cozumu, versiyon bilgisi eklenmis anahtar-deger eslerinin once-sonra hallerini kontrol etmektir. Bu uygulama programinin seviyesinde yapilir. 2 asamali commit (two-phase commit)  bazli cozumlerin istenen seviyede olceklenmesi imkansiz oldugu icin VM'e dahil edilmemistir.




