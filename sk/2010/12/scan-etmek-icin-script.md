# Scan Etmek icin Script

Otomatik olarak numaralandirarak sayfalari scan etmek icin basit bir
script. 01'den baslayarak birer birer artirarak scan edip imaj
dosyalarini yaratir, her scan sonrasi durur ve [enter] dugmesine
basilmasini bekler, kullanici o sirada sayfa degistirebilir.  Scanner
parametreleri icin scanimage -L, mesela plustek:libusb:005:002 gibi
bir sonuc.import os, re, sysdef run_command(command): result = [] f =
os.popen(command, "r") sys.stdout.flush() for l in f.xreadlines():
result.append(l) return resultdef two_digitize(i): if i < 10: return
"0" + str(i) return str(i)i = int(sys.argv[1])while True: print
two_digitize(i) run_command("scanimage --resolution 200 -x 200 -y 100
-d [SCANNER PARAMETRELERI] \ --format=tiff > " + two_digitize(i) +
".tiff") i += 1 r = raw_input(">")





