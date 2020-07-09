# Oracle Bağlantılarını Listeleyin


Oracle Bağlantılarını Listeleyin



 Son projemizde, toptan işlem yapan bir programdan şöyle bir hata aldık.                SQLException: ORA-01000: maximum open cursors  exceeded              Hataların hangi SQL'den geldiğini anlamak için Oracle'da aşağıdaki sorguyu  kullanabilirsiniz.               SQL> select sql_text, count(*) from v$open_cursor group by sql_text;              Sonuç olarak aşağıdaki gibi bir çıktı gelecektir.               SELECT SYSDATE FROM DUAL                                              109SELECT flag_1, flag_2  FROM TABLO                                     150SELECT rowid, "KULLANICI"."ACCOUNT".* FROM "KULLANICI"."HESAP"  Wher  1Select /*+ CHOOSE */ cols.column_name as Name, nullable,  da          1              Bu komut o anki açık/işlemde olan sorguları gösterir. 2. satırdaki sorgunun 150 açık cursor'u olduğunu görüyoruz. Bu tabii iyi değil. Tamir için, bu sorguları kullanan PreparedStatement'lara (.close()) cağrısını eklemek gerekir. Bu eklemeden sonra sorun düzelecektir.




