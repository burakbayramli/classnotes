# Python Ile Mail - Metin Eklentileri (Text Attachement)


Python Ile Mail - Metin Eklentileri (Text Attachement)




Python script icinden otomatik email gondermek ve mesaja txt, csv dosyasi eklemek icin alttaki mail.py adli script kullanilabilir. Script SMTP servisi olarak localhost, yani yerel makinada bir SMTP servisi oldugunu farz ediyor. Bu servisin yeri degistirilebilir, mesela bir GMail hesabi ve SMTP servisi de kullanilabilir. 

import smtplib, osfrom email.MIMEMultipart import MIMEMultipartfrom email.MIMEBase import MIMEBasefrom email.MIMEText import MIMETextfrom email.Utils import COMMASPACE, formatdatefrom email import Encodersdef send_mail(send_from, send_to, subject, text, files=[], server="localhost"):    assert type(send_to)==list    assert type(files)==list    msg = MIMEMultipart()    msg['From'] = send_from    msg['To'] = COMMASPACE.join(send_to)    msg['Date'] = formatdate(localtime=True)    msg['Subject'] = subject    msg.attach( MIMEText(text) )    for f in files:        part = MIMEBase('application', "octet-stream")        part.set_payload( open(f,"rb").read() )        Encoders.encode_base64(part)        part.add_header('Content-Disposition', 'attachment; filename="%s"' % os.path.basename(f))        msg.attach(part)    smtp = smtplib.SMTP(server)    smtp.sendmail(send_from, send_to, msg.as_string())    smtp.close()

Kullanimi

import datetimeimport mailimport sysstart = datetime.datetime.now()date = datetime.datetime.today().strftime('%Y-%m-%d')mail.send_mail('nereden@filan.com',               ['nereye@falan.com'],               '%s tarihinde gonderilen mesaj' % date,               'Mesaj basligi .. ',               ['dosya1.csv','dosya2,csv'])






