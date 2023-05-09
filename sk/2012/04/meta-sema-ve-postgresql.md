# Meta, Sema ve Postgresql

Tablolar hakkinda sema, meta bilgisi almak istiyorsaniz, ve pg_dump
komutu problem cikartiyorsa, tablo, kolon, tip bilgisi icin

```
select column_name,udt_name from information_schema.columns where table_name = '[tablo]'
  order by ordinal_position
```

gibi bir sorgu gerekli bilgiyi alir. Siralamayi ordinal_position
uzerinden yapmamiz iyi olur, cunku bu siralama o tablo uzerinde
"select *" islettigimizde kolonlarin gelecegi sira. Tabandan veri
cekerken genellikle (basitlik amaciyla) select * kullanildigi icin
semanin da ayni siralamaya uyacak sekilde alinmasi iyi olur.

Not: information_schema.columns uzerinde baska ilginc kolonlar da var.

Eger indis bilgisi almak istiyorsak,

```
SELECT tablename, indexdef FROM pg_catalog.pg_indexes
```

Bu sorgu indisi uretmek icin gerekli komutu bile gosterecektir.

Otomatik olarak uzaktaki bir semayi verisiyle indisleriyle beraber
yerel bir tabana aktarmak icin su script'ler kullanilabilir.

run_csv.sh - uzaktaki makina uzerinde sql isletip ciktiyi bir csv
olarak kaydeder,

```
psql [.. uzaktaki makina .. ] -c "COPY ($1) TO stdout with delimiter ',' CSV HEADER "
```

run.sh - yerel taban uzerinde argumanda verilen sql dosyasini isletir

```
psql [.. yerel ..]  < $1
```

csv_schema.py - sema ciktisini alip sql komutlarina donusturur

```
import csv, sys
# argv 1: csv file name
# argv 2: table name
schema = csv.reader(open(sys.argv[1], 'rb'), delimiter=',')
schema.next() # skip header
print "drop TABLE if exists %s ;" % sys.argv[2]
print "CREATE TABLE %s (" % sys.argv[2]
i = 0list = list(schema)
l = len(list)-1
for line in list:
    if i < l:
        print "%s %s, " % (line[0], line[1])
    else:
        print "%s %s " % (line[0], line[1])
    i+=1
print ");\n"
```

schema.sh - python script'i kullanarak semayi doker, ve yerel tablo uzerinde isletir. 

sh run_csv.sh "select column_name,udt_name from information_schema.columns where table_name = '[tablo]'" > $LH_DATA_DIR/$1/[tablo].outpython csv_schema.py DATA_DIR/$1/[tablo].out [tablo] > $LH_DATA_DIR/$1/[tablo].sqlsh run.sh DATA_DIR/$1/[tablo].sql

dump.sh - tablolarin verisini db dosyalarina kaydeder

```
sh run_csv.sh "select * from [tablo]" > DATA_DIR/$1/[tablo].db
```

load.sh - db dosyalarini yerel tabloya yukler

```
psql [..yerel..] -c "COPY [tablo] FROM 'DATA_DIR/$1/[tablo].db' with csv header delimiter ',' ";
```

indexes.sh - indeksleri uzaktaki makinadan alarak yerelde isletir

```
sh run_csv.sh "SELECT indexdef FROM pg_catalog.pg_indexes where tablename = '[tablo]' " > /tmp/idx
perl -pi -e 's/\"//sg' /tmp/idx
perl -pi -e 's/\n/;\n/sg' /tmp/idx
tail -n+2 /tmp/idx > $LH_DATA_DIR/$1/[tablo]_idx.sql
sh run.sh $LH_DATA_DIR/$1/[tablo]_idx.sql
```