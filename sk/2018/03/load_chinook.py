import sqlite3, re

db = sqlite3.connect('/tmp/chinook.db')
cursor = db.cursor()
content = open("/tmp/Chinook_Sqlite.sql").read()

sqls = re.findall("\n(.*?);", content, re.DOTALL)
for sql in sqls:
    print (sql)
    cursor.execute(sql)
    db.commit()
db.close()
