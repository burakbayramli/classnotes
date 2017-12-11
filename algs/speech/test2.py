import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io
import matplotlib.pyplot as plt

zip = '/home/burak/Downloads/goog_voice_train.zip'
with zipfile.ZipFile(zip, 'r') as z:
     training_files = z.namelist()

random.seed(0)

def get_minibatch(batch_size):
    res = np.zeros((batch_size, 16000))
    with zipfile.ZipFile(zip, 'r') as z:
        for i in range(batch_size):
            f = random.choice(training_files)
     	    wav = io.BytesIO(z.open(f).read())
     	    v = scipy.io.wavfile.read(wav)
            print f, v[1].shape
	    res[i, 0:len(v[1])] = v[1]
    return res

tmp = get_minibatch(20)
print tmp.shape

