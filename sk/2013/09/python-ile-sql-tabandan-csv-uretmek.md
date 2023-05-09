# Python ile SQL Tabandan CSV Uretmek

Alttaki kod SQL ile kursor (cursor) acarak veriyi satir satir alir ve
CSV dosyasina yazar.  Oracle icin

```
import csvimport cx_Oracle as odb 

oraclepass = [SIFRE]
user = [KULLANICI]
conn = odb.connect("%s/%s@TABAN" % (user,oraclepass) )
cursor = conn.cursor()
query = """select vs,vs from vs""" cursor.execute(query)
csv_writer = csv.writer(open("/tmp/out.csv", "wt"),delimiter=';')
csv_writer.writerow([i[0]
for i in cursor.description])
   csv_writer.writerows(cursor)
del csv_writer 
```






