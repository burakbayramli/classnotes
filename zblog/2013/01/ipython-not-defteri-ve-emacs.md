# iPython, Not Defteri ve Emacs




iPython, Not Defteri ve Emacs 




iPython Not Defteri uygulamasini ipython shell icinden kullanabilirsiniz, ya da Emacs icinden baglanti kurulursa direk Emacs'te kullanim mumkun. Bu isi halleden eklenti ein adinda bir eklenti, kurmak icin bir suru tarif var, fakat bizim Emacs ayarlarinin tamami surada, o ayarlar ile beraber ein baglantisi otomatik olarak gelecek. Eklentiye girilmeden once ipython not defteri servisi baslatilir

ipython notebook --pylab=inline

Ve Emacs icinden M-x ein:notebooklist-open ile not defterine girilir. Ein icinden python + not defteri kullaniminin bazi ek faydalari var, bunlari ileride isleriz. Fakat cok kuvvetli bir veri analiz, yayin, bilgi iletim sistemine kavusmak mumkun.




"Hucreler" icinde Python yazilir, ve bu kodlari isletmek ve sonuclarini (eger istenmis ise, print vs gibi komutlarla) altta gormek icin CTRL - Enter'a basilir. Eger grafik cizilmesine dair bir komut verilmisse, bu grafik aynen metinsel sonuclarda oldugu gibi, ayni dokuman icinde (hemen altta) gosterilecektir.

Emacs C-x C-s artik not defteri kaydi yapacaktir, kaydedilen hedef formati JSON.

iPython matematiksel sembolleri cizmek icin MathJax kullanir. Bu bir Javascript kutuphanesidir ve not defteri bu js kodlarini uzaktaki bir adresten almaya calisir. Eger bu kodlar yerel islesin istersek, ipython icinden

from IPython.external.mathjax import install_mathjax
install_mathjax()

komutlari gerekli.

Eger not defterini pdf'e cevirmek istersek, bunu LaTeX uzerinden yapabiliriz.

https://github.com/ipython/nbconvert

adresinden kod alinir. Kurmak icin dizini acmak yeterli, kullanmadan once bazi ek paketler lazim,

sudo pip install markdown 
sudo pip install -U docutils
sudo pip install pygments
sudo pip install nose
sudo apt-get install pandoc

Kullanmak, 

python [DIZIN]/nbconvert.py dosya.ipynb -f latex

Sonucta dosya.tex uretilecek. Bu dosyayi pdflatex dosya.tex ile PDF'e cevirebilirsiniz. Eger uretilen LaTeX'in sartlarini kontrol etmek istersek, [DIZIN]/converters/latex.py icinde bunu yapabiliriz, mesela 113. satiri

       r'\documentclass[%s,fleqn]{%s}' % (self.documentclass_options,

131. satirdan itibaren

        # Include document body        final.append('\setlength{\mathindent}{0pt}')        final.append('\setlength{\parindent}{0pt}')        final.append('\setlength{\parskip}{8pt}')

 ve o metodun sonunda

        # Return value must be a string        str = '\n'.join(final)        str=str.replace('\n\\[','\\[')        str=str.replace('\\]\n','\\]')        return str


ekleri yapilirsa, uretilen LaTeX buna gore olur. 




![](Screenshotfrom2013-01-06152909.png)
