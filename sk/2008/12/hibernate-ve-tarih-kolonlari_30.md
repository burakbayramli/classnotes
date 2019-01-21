# Hibernate ve Tarih Kolonlari


Hibernate ve Tarih Kolonlari



Eger icinde tarih olan tablolari okurken Hibernate (ve JDBC) soyle bir hata veriyorsa,java.sql.SQLException: Value '0000-00-00' can not be represented as java.sql.DateBunun basit bir cozumu persistence.xml deki JDBC URL'e zeroDateTimeBehavior=convertToNull ibaresini eklemektir.<property name="hibernate.connection.url" value="jdbc:mysql://localhost:3306/DB?zeroDateTimeBehavior=convertToNull">Bunun sonucunda eger Hibernate'in map ettigi kolonda 0000-00-00 gibi bir deger var ise, bu otomatik olarak NULL'a cevirilecektir. Bu degerin oraya konulmasinin sebebi MySQL seviyesinde sema yaratilirken olagan degerin 0000-00-00 birakilmis olmasi cogunlukla, boylece tarih tanimlanmayan INSERT otomatik olarak bu garip degeri atiyor. Cozum DB seviyesinde de yapilabilir muhakkak, ama orada kontrolunuz yok ise, ustteki cozum ise yarar.




