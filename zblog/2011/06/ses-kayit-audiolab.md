# Ses Kayit, audiolab


Ses Kayit, audiolab



Audiolab kurulusu surada. Python audiolab ile beyaz ses (white noise) yaratmak icinimport numpy as npfrom scikits.audiolab import play# output one second of stereo gaussian white noise at 48000 hzdata = np.random.randn(2, 480000)play(0.001 * data)Uc dakikalik bir beyaz ses toplamini dosyaya kaydetmek icinfrom scikits.audiolab import Format, Sndfileimport numpy as npfilename = 'foo.wav'data = 0.001 * np.random.randn(48000, 2)format = Format('wav')f = Sndfile(filename, 'w', format, 2, 48000)for i in range(60*3):     f.write_frames(data)f.close()Audiolab ne yazik ki MP3 formatinda yazamiyor. Wav dosyalari komut satirindan cevirmek icinsudo apt-get install shntoolsudo apt-get install lamelame -V2 foo.wav foo.mp3




