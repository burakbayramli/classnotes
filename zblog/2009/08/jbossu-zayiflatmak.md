# JBoss'u Zayiflatmak


JBoss'u Zayiflatmak



JBoss server dizini altinda belli "kurulus tipleri" oldugunu biliyoruz. Bunlar minimal, default gibi kurulus duzenleridir; run.sh -c minimal diyerek mesela JBoss'u minimal haliyle, o dizin altindaki tanimlari kullanarak baslatabiliriz. Biz default ayarlarini kullaniyoruz; fakat bu dizin altinda hala isimize yaramayan, fazla bazi ekler var. Bu yazida bu gereksiz ekleri cikartarak JBoss'u "zayiflatma (slimming)" teknigini gorecegiz.Bundan once kullanim amacimizi ortaya koyalim: Hedefimiz JBoss'un sadece Web sayfalarini dinamik olarak ureten, veri erisimini Schemafree (ya da diger bir anahtar/deger paketini -demek istedigimiz kap servislerini kullanmadan-) yapan bir Seam uygulamasini barindirmasidir. Bu kadar. Seam arka planda local EJB kullanir, onlar da bir sekilde dahil olacak. Bu durumda ise yaramayanlar JMS, takvimleme (scheduling) gibi ekler.. Bu, uygulamaniz JMS'e hic ihtiyac duymayacak anlamina gelmez; sadece standart tipik bir Web JBoss'u hazirlamak icin (ki bizim ihtiyacimiz buydu) gerekenlere isaret ediyoruz.Alttaki Python script bir default kurulusu alip, gereksiz bilesenleri otomatik olarak siliyor ve onu inceltiyor. Kullandigimiz JBoss versiyonu 4.2.2 GA. Kaynak bolumundeki yazida silinecekler listesi vardi, orada bazi degisiklikler yapmak gerekti; yani alttaki script en guncel zayiflatma script'i su anda.import osa = '''/server/default/deploy/jms/server/default/deploy/jmx-console.war/server/default/deploy/snmp-adaptor.sar/server/default/deploy/management/server/default/deploy/mail-service.xml/server/default/deploy/monitoring-service.xml/server/default/deploy/schedule-manager-service.xml/server/default/deploy/scheduler-service.xml/server/default/deploy/hibernate-deployer-service.xml/server/default/deploy/http-invoker.sar'''JB = "[JBOSS DIZININIZ]"for token in a.split():  f = JB + token  os.system("rm -rf " + f)Kaynak




