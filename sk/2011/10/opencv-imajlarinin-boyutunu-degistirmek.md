# OpenCV Imajlarinin Boyutunu Degistirmek (Resize)

Eger OpenCV Resize cagrisi problem yaratiyorsa, bu islemi Python IPL
icinden yapabiliriz. Ipl2PIL ile bir OpenCV video karesini PIL imajina
cevirebiliyoruz. Bu imaj uzerinde resize(..) cagrisi
yapilabilir. Hatta np.array(..) ile bu imaj Numpy dizinine de
cevirilebilir. Boyle bir dizin elde edilince her turlu Uygulamali
Matematik teknigi bu dizin uzerinde uygulanabilir. PIL imaji OpenCV'ye
PIL2Ipl ile geri cevirilebilir.Alttaki ornek bir kareyi ucte birine
indirgiyor.

```python
__scale__ = 3

frame = cvQueryFrame(capture)

gray = cvCreateImage ((frame.width, frame.height), 8, 1)

cvCvtColor( frame, gray, CV_BGR2GRAY )

ipl = Ipl2PIL(gray)

ipl = ipl.resize((int(frame.width/__scale__),
                  int(frame.height/__scale__)),
                 Image.ANTIALIAS)nimg = np.array(ipl) ..
ipl2 = PIL2Ipl(ipl)..cvShowImage("Example2", ipl2)
```



