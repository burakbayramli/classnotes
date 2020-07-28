# SQL

```python
import sqlite3
DB = '/tmp/chinook.db'
def runsql(sql):
    conn = sqlite3.connect(DB)
    c = conn.cursor()
    rows = c.execute(sql)
    for row in rows: print (row)
    conn.close()
```


Hangi ulkenin musteri en cok odeme yapti? (Chinook-SQL-Exercise/top_country.sql)


```python
sql = """
SELECT "Country", MAX("Total Sales For Country") as "Total Spent"
FROM 
  (SELECT BillingCountry as "Country" , SUM(Total) as "Total Sales For Country"
FROM Invoice 
GROUP BY BillingCountry);
"""

```

```text
('USA', 523.0600000000003)
```














![](chinook_er.jpeg)









Referans

[psycopg2](../../2012/06/psycopg2-python-ile-api-bazli-postgresql-erisimi.md)

[sqlite](../../2018/03/sqlite-basit-sekilde-hzl-diske-deger-yazma.md)

https://github.com/Olamiotan/PythonStarter

https://database.guide/2-sample-databases-sqlite/

https://data-xtractor.com/knowledgebase/chinook-database-sample/

https://github.com/KAPrueved/Chinook-SQL-Exercise















