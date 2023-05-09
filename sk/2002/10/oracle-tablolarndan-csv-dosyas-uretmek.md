# Oracle Tablolarından CSV Dosyası Üretmek

Oracle'daki bir tablonun verilerini metin bazlı bir dosyaya almak
istiyorsanız, bunun için aşağıdaki gibi bir betik yardımcı olabilir.

SET TERMOUT OFFSET HEADING OFFSET SHOWMODE OFFSET LINESIZE 2000SET
FEEDBACK OFFSET VERIFY OFFSET ECHO OFFSET TIMING OFFSET NEWPAGE 1SET
WRAP ONSET LONG 32767SET ARRAYSIZE 1SET HEADING OFFspool
c:/temp/cikti.cvsselect KOLON1||','||KOLON2||','||... from TABLO;exit;
$ sqlplus kulanici/sifre@SID @dump.sql ..

olarak isletildikten sonra, bir CSV dosyasi c:/temp/cikti.csv olarak
üretilecektir.





