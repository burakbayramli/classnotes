

```python
import scipy.io.wavfile, zipfile
import io, time, os, random, re
fs = 16000
train_dir = '/home/burak/Downloads/train/audio'
labels = ['down','go','left','no','off','on','right','stop','up','yes']
all_labels = labels + ['unknown','silence']
```

```python
f = train_dir + '/sheila/019fa366_nohash_0.wav'
wav = io.BytesIO(open(f).read())
v = scipy.io.wavfile.read(wav)
print v[1]
vol_multiplier = np.mean(np.abs(v[1])) / 500.
print vol_multiplier
vnew = v[1].astype(float) / vol_multiplier
vnew = vnew.astype(np.int16)
scipy.io.wavfile.write('/tmp/tmp.wav', fs, vnew)    
```

```text
[0 3 5 ..., 4 4 3]
1.786834
```

```python
plt.plot(vnew)
plt.savefig('test1_1.png')
```


```python

all_train_files = []
for d, r, f in os.walk(train_dir):
    for filename in f:
    	all_train_files.append(os.path.join(d,filename))
noise_files = [x for x in all_train_files if "_background_noise_" in x and ".wav" in x]
train_files = []
unknown_files = []
for x in all_train_files:
    if ".wav" in x: 
       label = re.findall(".*/(.*?)/.*?.wav",x)[0]
       if label not in labels and x not in noise_files: unknown_files.append(x)
       elif x not in noise_files: train_files.append(x)
```

```python
print all_train_files[:10]
print train_files[:10]
print unknown_files[:10]
```

```text
['/home/burak/Downloads/train/audio/_background_noise_/pink_noise.wav', '/home/burak/Downloads/train/audio/_background_noise_/README.md', '/home/burak/Downloads/train/audio/_background_noise_/white_noise.wav', '/home/burak/Downloads/train/audio/_background_noise_/exercise_bike.wav', '/home/burak/Downloads/train/audio/_background_noise_/running_tap.wav', '/home/burak/Downloads/train/audio/_background_noise_/dude_miaowing.wav', '/home/burak/Downloads/train/audio/_background_noise_/doing_the_dishes.wav', '/home/burak/Downloads/train/audio/on/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/on/c103a2d5_nohash_0.wav', '/home/burak/Downloads/train/audio/on/36050ef3_nohash_3.wav']
['/home/burak/Downloads/train/audio/on/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/on/c103a2d5_nohash_0.wav', '/home/burak/Downloads/train/audio/on/36050ef3_nohash_3.wav', '/home/burak/Downloads/train/audio/on/f953e1af_nohash_2.wav', '/home/burak/Downloads/train/audio/on/126a31d2_nohash_0.wav', '/home/burak/Downloads/train/audio/on/6414258b_nohash_0.wav', '/home/burak/Downloads/train/audio/on/2c7c33e8_nohash_0.wav', '/home/burak/Downloads/train/audio/on/39999a0f_nohash_0.wav', '/home/burak/Downloads/train/audio/on/763188c4_nohash_3.wav', '/home/burak/Downloads/train/audio/on/340c8b10_nohash_0.wav']
['/home/burak/Downloads/train/audio/happy/3efef882_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/19b05529_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/9080f6d3_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/126a31d2_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/c392e01d_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/fffcabd1_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/d750966e_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/340c8b10_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/fb24c826_nohash_0.wav']
```


```python

def get_minibatch(batch_size, silence_percent=0.10, unknown_percent=0.15):
    res = np.zeros((batch_size, fs))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
        if random.choice(range(int(1/silence_percent))) == 0:	   
           f = random.choice(noise_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
           chunks = int(len(v[1]) / fs) - 1
           chosen_chunk = random.choice(range(chunks))
           fr = int(chosen_chunk * fs)
           to = int((chosen_chunk+1)*fs)
           chunk_byte = v[1][fr:to]
	   res[i, :] = chunk_byte
	   y[i, all_labels.index('silence')] = 1.0 # silence
        elif random.choice(range(int(1/unknown_percent))) == 0:	   
           f = random.choice(unknown_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
	   res[i, 0:len(v[1])] = v[1]
	   y[i, all_labels.index('unknown')] = 1.0 # unknown
	else:
	   f = random.choice(train_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
	   if i==0: scipy.io.wavfile.write('/tmp/tmp1.wav', fs, v[1])	   
	   res[i, 0:len(v[1])] = v[1]
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	   y[i, labels.index(label)] = 1.0
	   
    return res, y
    
x,y = get_minibatch(20)
print y
```

```text
[[ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 1.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  1.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  1.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.  0.]
 [ 0.  1.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.]
 [ 0.  1.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  0.]
 [ 0.  0.  0.  1.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.  0.]
 [ 0.  0.  0.  1.  0.  0.  0.  0.  0.  0.  0.  0.]]
```


```python
vv = x[5,:].astype(np.int16)
scipy.io.wavfile.write('/tmp/tmp2.wav', fs, vv)
vv = x[8,:].astype(np.int16)
scipy.io.wavfile.write('/tmp/tmp3.wav', fs, vv)
i = 8
plt.specgram(x[i,:], Fs=fs, NFFT=1024)
print y[i]
plt.savefig('test1_2.png')
```

```text
[ 0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.]
```


```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf

init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [None, 16000])

print data

stfts = tf.contrib.signal.stft(data, frame_length=300, frame_step=100, fft_length=512)

spec = tf.abs(stfts)

from tensorflow.python.ops import random_ops
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(spec, feed_dict={data: x[0].reshape(1,fs) })  
print res.shape
```

```text
Tensor("Placeholder_24:0", shape=(?, 16000), dtype=float32)
(1, 158, 257)
```

```python
print help(plt.specgram)
```

```text
Help on function specgram in module matplotlib.pyplot:

specgram(x, NFFT=None, Fs=None, Fc=None, detrend=None, window=None, noverlap=None, cmap=None, xextent=None, pad_to=None, sides=None, scale_by_freq=None, mode=None, scale=None, vmin=None, vmax=None, hold=None, data=None, **kwargs)
    Plot a spectrogram.
    
    Call signature::
    
      specgram(x, NFFT=256, Fs=2, Fc=0, detrend=mlab.detrend_none,
               window=mlab.window_hanning, noverlap=128,
               cmap=None, xextent=None, pad_to=None, sides='default',
               scale_by_freq=None, mode='default', scale='default',
               **kwargs)
    
    Compute and plot a spectrogram of data in *x*.  Data are split into
    *NFFT* length segments and the spectrum of each section is
    computed.  The windowing function *window* is applied to each
    segment, and the amount of overlap of each segment is
    specified with *noverlap*. The spectrogram is plotted as a colormap
    (using imshow).
    
    Parameters
    ----------
    x : 1-D array or sequence
        Array or sequence containing the data.
    
    Fs : scalar
        The sampling frequency (samples per time unit).  It is used
        to calculate the Fourier frequencies, freqs, in cycles per time
        unit. The default value is 2.
    
    window : callable or ndarray
        A function or a vector of length *NFFT*. To create window
        vectors see :func:`window_hanning`, :func:`window_none`,
        :func:`numpy.blackman`, :func:`numpy.hamming`,
        :func:`numpy.bartlett`, :func:`scipy.signal`,
        :func:`scipy.signal.get_window`, etc. The default is
        :func:`window_hanning`.  If a function is passed as the
        argument, it must take a data segment as an argument and
        return the windowed version of the segment.
    
    sides : [ 'default' | 'onesided' | 'twosided' ]
        Specifies which sides of the spectrum to return.  Default gives the
        default behavior, which returns one-sided for real data and both
        for complex data.  'onesided' forces the return of a one-sided
        spectrum, while 'twosided' forces two-sided.
    
    pad_to : integer
        The number of points to which the data segment is padded when
        performing the FFT.  This can be different from *NFFT*, which
        specifies the number of data points used.  While not increasing
        the actual resolution of the spectrum (the minimum distance between
        resolvable peaks), this can give more points in the plot,
        allowing for more detail. This corresponds to the *n* parameter
        in the call to fft(). The default is None, which sets *pad_to*
        equal to *NFFT*
    
    NFFT : integer
        The number of data points used in each block for the FFT.
        A power 2 is most efficient.  The default value is 256.
        This should *NOT* be used to get zero padding, or the scaling of the
        result will be incorrect. Use *pad_to* for this instead.
    
    detrend : {'default', 'constant', 'mean', 'linear', 'none'} or callable
        The function applied to each segment before fft-ing,
        designed to remove the mean or linear trend.  Unlike in
        MATLAB, where the *detrend* parameter is a vector, in
        matplotlib is it a function.  The :mod:`~matplotlib.pylab`
        module defines :func:`~matplotlib.pylab.detrend_none`,
        :func:`~matplotlib.pylab.detrend_mean`, and
        :func:`~matplotlib.pylab.detrend_linear`, but you can use
        a custom function as well.  You can also use a string to choose
        one of the functions.  'default', 'constant', and 'mean' call
        :func:`~matplotlib.pylab.detrend_mean`.  'linear' calls
        :func:`~matplotlib.pylab.detrend_linear`.  'none' calls
        :func:`~matplotlib.pylab.detrend_none`.
    
    scale_by_freq : boolean, optional
        Specifies whether the resulting density values should be scaled
        by the scaling frequency, which gives density in units of Hz^-1.
        This allows for integration over the returned frequency values.
        The default is True for MATLAB compatibility.
    
    mode : [ 'default' | 'psd' | 'magnitude' | 'angle' | 'phase' ]
        What sort of spectrum to use.  Default is 'psd', which takes
        the power spectral density.  'complex' returns the complex-valued
        frequency spectrum.  'magnitude' returns the magnitude spectrum.
        'angle' returns the phase spectrum without unwrapping.  'phase'
        returns the phase spectrum with unwrapping.
    
    noverlap : integer
        The number of points of overlap between blocks.  The
        default value is 128.
    
    scale : [ 'default' | 'linear' | 'dB' ]
        The scaling of the values in the *spec*.  'linear' is no scaling.
        'dB' returns the values in dB scale.  When *mode* is 'psd',
        this is dB power (10 * log10).  Otherwise this is dB amplitude
        (20 * log10). 'default' is 'dB' if *mode* is 'psd' or
        'magnitude' and 'linear' otherwise.  This must be 'linear'
        if *mode* is 'angle' or 'phase'.
    
    Fc : integer
        The center frequency of *x* (defaults to 0), which offsets
        the x extents of the plot to reflect the frequency range used
        when a signal is acquired and then filtered and downsampled to
        baseband.
    
    cmap :
        A :class:`matplotlib.colors.Colormap` instance; if *None*, use
        default determined by rc
    
    xextent : [None | (xmin, xmax)]
        The image extent along the x-axis. The default sets *xmin* to the
        left border of the first bin (*spectrum* column) and *xmax* to the
        right border of the last bin. Note that for *noverlap>0* the width
        of the bins is smaller than those of the segments.
    
    **kwargs :
        Additional kwargs are passed on to imshow which makes the
        specgram image
    
    Notes
    -----
        *detrend* and *scale_by_freq* only apply when *mode* is set to
        'psd'
    
    Returns
    -------
    spectrum : 2-D array
        Columns are the periodograms of successive segments.
    
    freqs : 1-D array
        The frequencies corresponding to the rows in *spectrum*.
    
    t : 1-D array
        The times corresponding to midpoints of segments (i.e., the columns
        in *spectrum*).
    
    im : instance of class :class:`~matplotlib.image.AxesImage`
        The image created by imshow containing the spectrogram
    
    Examples
    --------
    .. plot:: mpl_examples/pylab_examples/specgram_demo.py
    
    See Also
    --------
    :func:`psd`
        :func:`psd` differs in the default overlap; in returning the mean
        of the segment periodograms; in not returning times; and in
        generating a line plot instead of colormap.
    
    :func:`magnitude_spectrum`
        A single spectrum, similar to having a single segment when *mode*
        is 'magnitude'. Plots a line instead of a colormap.
    
    :func:`angle_spectrum`
        A single spectrum, similar to having a single segment when *mode*
        is 'angle'. Plots a line instead of a colormap.
    
    :func:`phase_spectrum`
        A single spectrum, similar to having a single segment when *mode*
        is 'phase'. Plots a line instead of a colormap.
    
    .. note::
        In addition to the above described arguments, this function can take a
        **data** keyword argument. If such a **data** argument is given, the
        following arguments are replaced by **data[<arg>]**:
    
        * All arguments with the following names: 'x'.

None
```




















