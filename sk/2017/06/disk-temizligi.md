# Disk Temizliği

Arada sırada Ubuntu / Unix'imizde "bahar temizliği" iyi olabilir; ardı
ardına kurulan paketler, işletilen programların yarattığı onbellek
(cache) dosyaları arkada kalıp diski bitirebilir. Temizlik için bazı
programlar:

En basiti komut satırında du,

```
sudo du -hx --max-depth=1 /
```

Dizin bazında kullanım raporu verir.

Eğer dosya büyüklüğüne göre sıralı liste istersek

```
sudo du -hb --max-depth=2 /home | sort -n
```

Benzer bir raporu görsel araç üzerinden dosya idarecisi Nemo ile
görebiliriz, herhangi bir dizin üzerinde sağ tıklama ve Open With |
Disk Usage Analyzer.

Otomatik temizleme icin bleachbit: Kurulum apt-get install bleachbit
ile, gksudo ile başlatıp solda temizlenecek şeyler seçilir. APT
temizliği iyi oluyor, diğer bir sürü seçenek te var.






