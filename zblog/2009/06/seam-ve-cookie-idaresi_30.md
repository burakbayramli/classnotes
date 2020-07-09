# Seam ve Cookie Idaresi


Seam ve Cookie Idaresi



Seam uygulamamizin cookie (cerez) uzerinden kullanici makinasina bilgi birakmasi gerekiyorsa (kullanici kimligi gibi), o zaman javax.servlet paketlerini kullanarak cookie arayuzlerini kullanmak gerekiyor. Import edilmesi gereken kodlarimport javax.servlet.http.Cookie;import javax.servlet.http.HttpServletRequest;import javax.servlet.http.HttpServletResponse;import javax.faces.context.FacesContext;Bu kodlar Seam action class'inizda bulunmali, ya da FacesContext'e erisebildiginiz herhangi bir yerde. Simdi cookie yazmak icin soyle bir kod gerekliFacesContext context = FacesContext.getCurrentInstance();Cookie cookie = new Cookie("[cookie id]", "[deger]");cookie.setMaxAge(3 * 30 * 24 * 60 * 60);cookie.setPath("/");HttpServletResponse res = (HttpServletResponse)context.getExternalContext().getResponse();res.addCookie(cookie);Ustte [cookie id] yazan yer programci tarafindan secilen bir cookie kimligidir. Mesela "userId" gibi bir isim secilebilir, daha sonra [deger] yazan yere veri tabanindan, vs. gelen deger yazilacaktir. Boylece kullanicinin her zaman kim oldugunun hatirlanmasi saglanacaktir. Yazdigimiz degere sonradan erismek istiyorsak soyle bir kod lazimFacesContext context = FacesContext.getCurrentInstance();String cookieName = null;Cookie[] cookies = ((HttpServletRequest)context.getExternalContext().getRequest()).getCookies();if(cookies != null && cookies.length > 0) {  for (int i = 0; i < cookies.length;i++) {    Cookie c = cookies[i];                 ...            }} getCookies() ile tum cookie'leri almisiz, ve teker teker onlara bakiyoruz; c.getName() ile cookie ismi, c.getValue() ile degerini okuyabilecegiz. Aradigimiz ismi bu notada filtreleyerek bulmamiz mumkun. Degeri aldiktan sonra @Out ile de-enjekte ederek (mesela) Seam uygulamamizin diger kisimlarinin da  ayni degere erismesini saglayabiliriz.Firefox eklentisi olarak Web Developer eklentisini tavsiye ederim; Cookie'lere, CSS'leri incelemek gibi pek cok guzel ozelligi var.




