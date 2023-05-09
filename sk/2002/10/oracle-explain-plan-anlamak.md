# Oracle Explain Plan'i Anlamak

SQL TRACE ve tkprof programları birden fazla işlenen sorgular için
yararlı istatistikler verse bile, "tek bir" sorgu için harcanacak
zamanı hesaplamak için değişik bir yöntem kullanmanız daha yararlı
olur. EXPLAIN PLAN komutunu sorguyu işletmenize gerek kalmadan,
Oracle'ın işlem planını hesaplaması için kullanabilirsiniz. İşlem
planı (execution plan) Oracle'in herhangi bir sorguyu nasıl işletmeyi
planladığını gösterir.  İşletmeden sorgu analiz edebilme özelliği,
uzun süreli sorgular için çok işinize yarayacak. Uzun süren derken,
mesela 30 dakika işleyebilecek bir sorgudan bahsediyoruz. Bu sorguyu,
çalıştırmadan analiz edebilme, her seferinde 30 dakika beklemekten
sizi kurtaracaktır.  Explain Plan'i masıl kullanacağız? İlk önce plan
çizelgesi denilen bir çizelgeyi yaratmamız lazım. Bu çizelge, plan
çıktısını depolamak için kullanılıyor.

Bu çizelgeyi otomatik yaratan betik Unix üzerinde
$ORACLE_HOME/rdbms/admin/utxlplan.sql dosyasındadır. Script işledikten
sonra, hangi şematikten işlettiyseniz orada PLAN_TABLE adlı bir
çizelge göreceksiniz.

Evet hazırız. Şimdi, analiz etmek istediğiniz sorgu üzerinde EXPLAIN
PLAN işletin: SQL> EXPLAIN PLAN 2> SET STATEMENT_ID='SORGU_1' 3> FOR
4> SELECT EMPLOYEE_ID 5> FROM EMPLOYEES 6> WHERE EMPLOYEE_ID=243218;
Bu komut, analizi yaptı, ve sonucu plan çizelgesinde sakladı. Sonucu
almak için, şunu işletebilirsiniz.  SQL> SELECT OPERATION, OPTIONS,
OBJECT_NAME, ID, PARENT_ID, POSITION 2> FROM PLAN_TABLE 3> WHERE
STATEMENT_ID='SORGU_1' 4> ORDER BY ID;

Bu komut, analizi düz bir tablo halinde gösterecek. Eğer daha düzgun
ve girintili bir sonuç istiyorsanız, şunu işletin.

SQL> SELECT LPAD(' ',2*LEVEL[nd]1) | | OPERATION | | ' ' | | OPTIONS |
| ' ' | |OBJECT_NAME | | ' ' | | DECODE(ID, 0, 'COST= '| | POSITION)
"QUERY PLAN" 2> FROM PLAN_TABLE 3> START WITH ID=0 AND
STATEMENT_ID='SORGU_1' 4> CONNECT BY PRIOR ID=PARENT_ID;

Büyük işlem planları için bu şekilde bir çıktı daha yararlı olacaktır.
Sonucu Analiz Etmek Amaç, sorgularda indis kullanımını en üst düzeye
getirmektir. İndisler, CREATE INDEX komutu ile yaratılırlar, ve
çizelgeye erişimi hızlandırırlar. Amacımız, EXPLAIN PLAN komutundan
gelen sonuçlarda INDEX RANGE SCAN ibaresini mümkün olduğunca çok
görmektir. Eğer FULL TABLE SCAN ibaresini birden fazle kere
görüyorsanız sorgu çizelgeye satır satır erişiliyor demektir ve bu çok
kötüdür. İndis ekleyerek, ya da mevcut olan indislerin kullanılmasını
Oracle'a 'hatırlatmak' için sorgularınız içinde tiyo vermeye
başlayabilirsiniz.





