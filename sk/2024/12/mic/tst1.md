
https://stackoverflow.com/questions/62172398/convert-audiobuffer-to-arraybuffer-blob-for-wav-download


```python
import scipy.io.wavfile
tmp, wav = scipy.io.wavfile.read('../2024-12-27T09_02_31.076Z.wav')
print (wav[0:200])
```













