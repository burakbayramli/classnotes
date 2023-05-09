# Yararlı Solaris Araçları

Solaris işletim sistemi, süreçlerinizin (process) performansını takip
edebilmeniz için bazı ek araçlar sağlamaktadır. Bu yazıda gösterilen
bütün komutlar izledikleri süreçlere başlangıçta çok az ek bir ek yük
getirirler, o yüzden canlı bir sistem üzerinde, özellikle tekrar eden
bir şekilde kullanılmaları uygun olmayabilir. UNIX man proc
sayfasından bu ve öteki komutlar hakkında daha fazla bilgi
edinebilirsiniz.

Problem araştırme ve inceleme için aşağıdaki programlar yararlı
olacaktır.

```
ps -eo pid,pcpu,args | sort +1n
```

CPU kullanım yüzdesine (utilization) göre ps çıktısı almak (en cok
kullanıma sahip olan en altta olmak üzere) için kullanilan ps
komutudur. Bu

komut bizi ayrı ayrı ps ve top kullanmaktan kurtaracaktır.

```
/usr/proc/bin/pstack
```

Bu komut size Java sürecinizin içindeki lwp yığıtını gösterir. Her
Java Thread'i koşmaya başlamadan önce bir lwp'ye (hafif
süreç/lightweight process) bağlanmak zorundadır, ve bu komutu ile
hangi Thread'in hangi sistem çağrısı üzerinde beklediğini raporda
görebilirsiniz.

Bütün Thread'leri bir soketten okumada, ya da bir yerel çağırım
üzerinden JDBC sürücünüzü beklediğini görebilirsiniz. Bâzen bu gibi
bir bilgi yeterli olmayabilir, çünkü hangi Thread'in hangi soket
no'sunu beklediğini bu araç göstermez.

```
/usr/proc/bin/pldd
```

Bu komut hangi yerel kodun (native code) JVM'inize yüklenmiş olduğunu
göstermesi açısından yararlıdır. Komut, hangi .so dosyasının JVM
sürecine dinamik link edilmiş olduğunu göstererek çalışır.

```
/usr/proc/bin/pflags
```

Bu komut, her süreç içindeki her lwp'nin işaretlerini (flags) ve
bekleme durumunu (wait state) rapor eder. Bu komutu kaç tane thread
context değtirimi (switch) olduğunu anlamak için
kullanabilirsiniz. Eğer her JVM için birden fazla CPU var ise, context
değiştirimi büyük bir ihtimalle yüksek sayıda olacaktır.

```
/usr/proc/bin/pfiles
```

Pfiles, herhangi bir sürecin açık tuttuğu dosyaları gösterir, böylece
dosyaların kapanmaması ile alakalı olan problemleri takip etmenize
yarar.  lsof Bir Unix sürecinin açık tuttuğu dosyaları görmek için
lsof'u kullanabilirsiniz. lsof, pfiles'a benzer, fakat daha yararlı
bir çıktı verir. Daha fazla bilgi için lsof'u şurada ziyaret
edebilirsiniz.

```
truss -c -p
```

Bu komut, bir sürece karşı başlatılması ve kesilmesi (interrupt)
arasında süreç tarafından yapılmış olan sistem çâğrılarını rapor
eder. Örnek:

```
[root@balina /]# psPID TTY TIME CMD11218 pts/5 0:00
ps11211 pts/5 0:00 bash[root@balina /]

# truss -c -p 11211^C <
```





