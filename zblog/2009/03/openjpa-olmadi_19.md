# OpenJPA: Olmadi


OpenJPA: Olmadi



OpenJPA testlerini yaparken baytkodlari tekrar islemden gecirdigini (enhance) ve bunu derleme zamaninda yapmamizi bekledigini farkettim. Hibernate bunu islem aninda (dinamik olarak) yapiyor. Bu enhance isi bana bir zamanlarin JDO'su ve Kodo'yu hatirlatti; baktim bu projelerden adamlar OpenJPA'ye gecmisler. Isin asli belli oldu. OpenJPA: OUT. Slice Hibernate ile kullanilabilirse ne ala (pek zannetmiyorum), yoksa Shards'i JPA usulu kullanmanin bir yolunu bulacagiz. Aslinda Shards projesi bunun farkinda ve uzerinde calisiyor. Doc'ta soyle demisler:"Many of you will quickly realize that the configuration mechanism we've provided won't work if you're configuring your SessionFactory via JPA. It's true. We expect this deficiency to be addressed shortly."Bu durumda bizim yapacagimiz, obje mimarimizi, semamizi sharding kavramina hazir hale getirmek ve Shards JPA destegini ekler eklemez ona gecmek. Teknolojinin en uc noktasinda (bleeding edge) olmak boyle bir sey. Hareketli bir hedefle calismayi ogrenmek gerekiyor.




