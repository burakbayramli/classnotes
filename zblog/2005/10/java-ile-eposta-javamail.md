# Java İle Eposta (JavaMail)


Java İle Eposta (JavaMail)



 Java ile e-posta göndermek oldukça basit. Referans olması açısından aşağıdaki kod parçasını paylasıyoruz.               import java.util.*;import java.io.*;import javax.mail.*;import javax.mail.internet.*;import javax.activation.*;public class Postaci{ public static void main(String args[]) throws Exception {     Properties props = System.getProperties();     props.put("mail.smtp.host", "mail.sizinsirket.com");     Session session = Session.getDefaultInstance(props, null);       Message msg = new MimeMessage(session);       msg.setFrom(new InternetAddress("ben@orada.com"));       InternetAddress[] tos =         InternetAddress.parse("alici-eposta-adresi@sizinsirket.com");       msg.setRecipients(Message.RecipientType.TO,tos);       msg.setSubject("Java Eposta testi");     msg.setSentDate(new Date());     msg.setText("Bu mesaj Java'dan gönderilmistir...");     Transport.send(msg);   }}              Java Mail'in çalışması için Java Tetikleyici Altyapısı (Javabeans Activation Framework) gerekiyor. Aşağıda gerekli bağlantıları bulabilirsiniz.          Kaynaklar          * Java Eposta (JavaMail)   * Java Tetikleyici Altyapısı (Javabeans Activation Framework)




