import random, scipy.io.wavfile, io
import sounddevice as sd, time

zip = '/home/burak/Downloads/goog_voice_test.zip'
import zipfile
with zipfile.ZipFile(zip, 'r') as z:
    files = z.namelist()
    fout = open ("label.txt", "a")     
    while (True):
        f = random.choice(files)
        print f
        def play():
            wav = io.BytesIO(z.open(f).read())
            v = scipy.io.wavfile.read(wav)
            print 'playing'
            sd.play(v[1], 16000)
            time.sleep(1)
        c = None
        while True:
            play()        
            c = raw_input()
            print 'key is', c
            if c!='r':
                fout.write("%s,%s\n" % (f,c))
                fout.flush()
                break            
        








