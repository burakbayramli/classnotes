# @Out ve @Scope


@Out ve @Scope



Seam programlamasinda oturum uzerinde veri saklamak zararli degildir, hatta bu tur tasarimlar Seam yaraticisi tarafindan cesaretlendirilmektedir. Ne de olsa, modern Web mimarisi, yapiskan oturum kavramina sahiptir, yuk dagiticisi (HAProxy, Apache) bir kere bir oturumu bir makinaya yonlendirdiginde o oturum altindaki tum diger istekleri ayni makinaya gondermesi gerektigini bilir. Boylece ayni app server icinde Java objeleri rahatca paylasilabilir.Oturum bazli objeleri islem yapan Session Bean'lere enjekte ederek veriyoruz, de-enjekte ederek dis dunya ile paylasiyoruz. Eger tipik bir kullanimda bir Person objesi bir aksiyona veriliyor, orada islem gorup sonra disari veriliyorsa,@In @OutPerson person;gibi bir sekilde tanimlanir. Simdi dikkat; gelen ayni objenin disari gitmesini istiyorsak, o zaman bu objeyi class bazinda SESSION (oturum) olarak tanimlamaliyiz. Yani soyle;@Name("person")@Scope(SESSION)public class Person implements Serializable { ...}




