# Logger ve Serilestirme

JBoss (EJB3) ortaminda calisirken EJB'lerimiz icinde Logger class'inda
tanimli objeler var ise, app server objemizi serilestirmeye calisirken
hata mesajlari gelebilir. Problem EJB icindeki Logger nesnesinin
serilestirilemez olmasidir. Hata icinde su ibareyi
gorursunuz:InstantiationException: org.apache.log4j.LoggerBu durumda
hatadan kurtulmak icin yapilmasi gereken Logger deklarasyonunun basina
"transient" kelimesini eklemektir. Yani

Logger log = Logger.getLogger("logger")ifadesi

transient Logger log = Logger.getLogger("logger")

haline gelecek.





