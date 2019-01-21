# JBoss Seam ve Tarih Gostermek


JBoss Seam ve Tarih Gostermek



Seam/JSF ile programlarken tarihleri basmak icin h:outputText ekrana bir seyler basacaktir. Fakat goruntude saat kismi da gozukecek (12:34:00 EST vs seklinde) ki bu istedigimiz bir goruntu olmayabilir. Bu goruntuyu istedigimiz gibi cevirttirmek icin Seam/JSF bir Converter tanimlamamiza izin veriyor.import javax.faces.component.UIComponent;import javax.faces.context.FacesContext;import javax.faces.convert.ConverterException;import org.jboss.seam.annotations.Name;import org.jboss.seam.annotations.intercept.BypassInterceptors;import org.jboss.seam.annotations.faces.Converter;import java.util.Date;@Name("dateConverter")@BypassInterceptors@Converterpublic class DateConverter implements javax.faces.convert.Converter{  public Object getAsObject(javax.faces.context.FacesContext context,                            javax.faces.component.UIComponent component,                            java.lang.String str) {    return DateUtil.stringToDate(str);  }  public String getAsString(javax.faces.context.FacesContext context,                            javax.faces.component.UIComponent component,                            java.lang.Object object)  {    final Date date = (Date) object;    return DateUtil.dateToString(date);  }}DateUtil.dateToString cagrisi formatlamayi istedigimiz gibi yapmamizi saglayan yardimci metottur. Bu kadar, bu class @Convertor annotation'i kullandigi icin XML karin agrilarina girmeye gerek yok. Kullanmak icin herhangi bir xhtml sayfasinda alttaki kullanim yeterli.<h:outputText value="#{obj.someDateColumn}"><f:converter converterId="dateConverter"/></h:outputText>




