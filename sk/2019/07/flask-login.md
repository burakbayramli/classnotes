# Flask, Kullanıcı Giriş (Login), Şifre, Oturum İdaresi, Flask-User

Kullanıcının şifresini almak, unuttuysa hatırlatmak, email üzerinden
kaydetmek, oturum açıp oturumu hatırlamak, hangi sayfaya izin var
hangisine yok kontrol etmek, vs. gibi işlerlerle gayet "Pythonic"
şekilde başedebilmemizi sağlayan bir paket Flask-User. Kodları,

https://github.com/lingthio/Flask-User/

dan indirebiliriz. Gerekli paketleri 

```
pip install -r requirements.txt
```

ile kurarız. Örnek bir kod ([1] baz alınmıştır), `basic_app.py` diyelim,

```python
import datetime, os, util
from flask import Flask, request, render_template
from flask_babelex import Babel
from flask_sqlalchemy import SQLAlchemy
from flask_user import current_user, login_required
from flask_user import roles_required, UserManager, UserMixin

class ConfigClass(object):
    """ Flask application config """

    SECRET_KEY = 'This is an INSECURE secret!! DO NOT use this in production!!'

    SQLALCHEMY_DATABASE_URI = 'sqlite:///basic_app.sqlite' 
    SQLALCHEMY_TRACK_MODIFICATIONS = False   

    MAIL_SERVER = 'smtp.gmail.com'
    MAIL_PORT = 587
    MAIL_USE_SSL = False
    MAIL_USE_TLS = True
    MAIL_USERNAME = '[kaynak email@gmail.com]'
    MAIL_PASSWORD = '[sifre]'
    MAIL_DEFAULT_SENDER = '"MyApp" <noreply@example.com>'

    USER_APP_NAME = "Flask-User Basic App"
    USER_ENABLE_EMAIL = True       
    USER_ENABLE_USERNAME = False  
    USER_EMAIL_SENDER_NAME = USER_APP_NAME
    USER_EMAIL_SENDER_EMAIL = "noreply@example.com"


def create_app():
    """ Flask application factory """
    
    app = Flask(__name__)
    app.config.from_object(__name__+'.ConfigClass')

    babel = Babel(app)

    db = SQLAlchemy(app)

    class User(db.Model, UserMixin):
        __tablename__ = 'users'
        id = db.Column(db.Integer, primary_key=True)
        active = db.Column('is_active', db.Boolean(),
	                   nullable=False, server_default='1')

        email = db.Column(db.String(255, collation='NOCASE'),
	                  nullable=False, unique=True)
        email_confirmed_at = db.Column(db.DateTime())
        password = db.Column(db.String(255), nullable=False, server_default='')

        first_name = db.Column(db.String(100, collation='NOCASE'),
	                       nullable=False, server_default='')
        last_name = db.Column(db.String(100, collation='NOCASE'),
	                      nullable=False, server_default='')

        roles = db.relationship('Role', secondary='user_roles')

    class Role(db.Model):
        __tablename__ = 'roles'
        id = db.Column(db.Integer(), primary_key=True)
        name = db.Column(db.String(50), unique=True)

    class UserRoles(db.Model):
        __tablename__ = 'user_roles'
        id = db.Column(db.Integer(), primary_key=True)
        user_id = db.Column(db.Integer(),
	                    db.ForeignKey('users.id', ondelete='CASCADE'))
        role_id = db.Column(db.Integer(),
	                    db.ForeignKey('roles.id', ondelete='CASCADE'))

    user_manager = UserManager(app, db, User)

    db.create_all()

    if not User.query.filter(User.email == 'member@example.com').first():
        user = User(
            email='member@example.com',
            email_confirmed_at=datetime.datetime.utcnow(),
            password=user_manager.hash_password('Password1'),
        )
        db.session.add(user)
        db.session.commit()

    if not User.query.filter(User.email == 'admin@example.com').first():
        user = User(
            email='admin@example.com',
            email_confirmed_at=datetime.datetime.utcnow(),
            password=user_manager.hash_password('Password1'),
        )
        user.roles.append(Role(name='Admin'))
        user.roles.append(Role(name='Agent'))
        db.session.add(user)
        db.session.commit()

    @app.route('/')
    def home_page():
        return render_template("home.html")

    @app.route('/members')
    @login_required    # Use of @login_required decorator
    def member_page():
        return render_template("/member_page.html")

    @app.route('/admin')
    @roles_required('Admin')    # Use of @roles_required decorator
    def admin_page():
        return render_template("/admin.html")

    return app

if __name__ == '__main__':
    app = create_app()
    app.run(host='0.0.0.0', port=5000, debug=True)

```

Sayfalar `templates` alt dizininde,

admin.html

[Link](t1.txt)

home.html

[Link](t2.txt)

member_page.html

[Link](t3.txt)

Bu kod basit bir şekilde `python basic_app.py` diye başlatılır. Email,
sifre girilir, ve konfirmasyon için email gelir, ona tıklanır,
kullanıcı doğrulanmış olur.

Sayfaları daha kendimize göre tasarlamak istersek, mesela `extends`
ibaresini çıkartıp daha pür HTML kodları kullanmak istersek, sistem
mesajlarını görmeyebiliriz, bu mesajlar 'kullanıcıya konfirme için
email gönderildi', vs. gibi mesajlardır, o mesajları görmek için

[Link](t4.txt)

kodunu sayfalarımıza ekleyebiliriz. Bu durumda

```python
from flask import flash
...
flash('Invitation has been sent.', 'success')
```

ile gönderilen mesajlar üstteki kodun olduğu sayfalarda bir kereliğine
gösterilecektir. Aynı sayfaya bir daha gidersek mesaj görülmez.

Eğer bir sayfayı (onun metoduna daha doğrusu) site giriş yapmış
olanlar için kısıtlamak istersek o metotu `@login_required` ile
işaretleriz.

Kullanıcılar çok basit bir sqlite veri tabanında tek bir dosya içinde
kaydedilirler, dosya ismi `SQLALCHEMY_DATABASE_URI` ile tanımlı.

SQLAlchemy aslında pek çok farklı taban ile çalışabilir. Ben
Postgresql'e geçmek istediğimde bu rahat bir şekilde oldu,
`SQLALCHEMY_DATABASE_URI` icin `sqlite:///basic_app.sqlite` yerine

```
postgresql+psycopg2://{user}:{pw}@{url}/{db}'.format(user="[kullanici]",pw="[sifre]",url="[makina]",db="[taban]")
```

kullanınca taban yaratma, ona erişme, vs her şey işledi.

Giriş yapmak, oturum kapatmak, şifre ile ilgili envai türden işlem
üstteki örnek ile yapılabiliyor.

SQL

Flask-User içinde SQLAlchemy kullanılıyor, bu bir tür ORM
(ilişkisel-obje eşlemesi), Java dünyasındaki Hibernate gibi. Fakat siz
direk SQL ile iş yapmak isterseniz,

```python
res = db.engine.execute('select * from users')
for x in res: print (x)
```

kullanımı olur. `db` referansı `db = SQLAlchemy(app)` ifadesinden geliyor. 

Eğer `INSERT`, `UPDATE`, `DELETE` gibi sorgular işletmek istersek,

```python
connection = db.engine.connect()
trans = connection.begin()
db.engine.execute(" ...
...
trans.commit()
```

Roller

Bazı kullanıcılara bakıcı, "Admin" rolü atamak isteyebiliriz. Bu rolü
ben spesifik bir email adresine atamak için Flask üzerinden bir metot içinde 

```
insert into roles (id,name) values (1,'Admin')
insert into user_roles select 1, id, 1 from users where email = '[BENIM EMAIL]'
```

sorgularını işlettim. Bende üstteki kodun başında görülen kullanıcı ve
rolleri tabana ekleyen bölüm yok. Onları direk SQL içinden kendimiz yapıyoruz.

Bir kullanıcı Admin olduktan sonra (ya da herhangi bir başka rol), bu
rolü metot girişinde kontrol edebiliriz.

```python
@app.route('/onemli_admin_komutu')
@roles_required('Admin')
def onemli_admin_komutu():
...
```

Kullanıcı Durumuna Göre Sayfa

Eğer kullanıcı sisteme şifre ile girmiş ise farklı, girmemiş ise
farklı içerik göstermek istersek sayfa içinde
`current_user.is_authenticated` kullanabiliriz. Mesela

[Link](t5.txt)

Kullanıcı `user` objesine çağrıldığı anda hesaplanan yeni öğeler
(property) ekleyebiliriz, `@property` ile bu mümkün. Diyelim ki o
günün tarihini, kullanıcının tabandaki son erisebileceği tarih ile
karşılaştırıp doğru ve yanlış cevabı verebilen `is_member` (üye mi)
adlı bir öğe istiyoruz,

```python
@property
def is_member(self):
    nd = ... # gunun tarihi
    ld = ..  # son kullanim
    return ld < nd
```

Artık bu öğeyi aynen üstteki gibi `current_user.is_member` ile çağırıp
sayfa mantığı içinde kullanabiliriz.

Temel Sayfaları Değiştirmek

Dikkat edersek sisteme giriş, kayıt sayfaları Flask-User projesinin
içinden geliyor. Bu sayfalar için kendi istediğimiz sayfaları
kullanmak istersek, `Flask-User / flask_user / templates / flask_user`
dizinini olduğu gibi alıp kendi projemizin `templates` dizinine
kopyalayabiliriz, ve ilgili sayfaları artık kendi yerel projemizde
değiştirebiliriz. Flask-User artık otomatik olarak kurulan paket
içindeki değil bu yerel proje altındaki sayfalara gidecektir.

Email, SMTP

Email, Gmail SMTP ile ilgili problem olursa bir [diğer
yazı](/2012/06/python-ile-mail-göndermek-smtp-gmail.html). Ya da başka
bir email servisi için
[şurası](/2019/05/sendgrid-smtp-email-servisi.html).  `MAIL_USE_SSL`
kullanımı ile `MAIL_USE_TLS` kullanımı arasında ya o ya bu türünden
bir durum var, SSL ya da TLS seçimi SMTP servisinde hangi port'u
kullandığımızı etkiler.


Kaynaklar

[1] https://github.com/lingthio/Flask-User/blob/master/example_apps/basic_app.py
