# Ses Komut Tanıma, Dikkat (Attention) Modeli

Ses komut tanıma için ünlü bir veri Kaggle'da paylaşılmıştır [4], bir
saniyelik ses komutları wav dosyalarında var, ve farklı arka plan
gürültüleri, kişiler tarafından kaydedilmiş. Bu veri bir Kaggle yarışması
için de kullanılmıştı.

Ses tanıma temel bir yapay zeka alanı. Amazon, Google, MS gibi
şirketlerin ses tanıma servisleri var, fakat [2] araştırmasına göre bu
yazılımlar çok yer tutuyor, çok fazla kaynağa ihtiyaç duyuyor. Bu
sebeple onların kullandığı türden ses tanıma mekanizması küçük
bilgisayarlarda kullanılamaz. Halbuki bu tür bir yazılım çok faydalı
olacaktır, İnternet bağlantısı olmadan ses komut tanıma
yapabilmeliyiz.

Bu tür bir Keras / Tensorflow modeli [1]'de var. Araştırmacılar en son
dil işleme mimarilerinde ilerleme sağlayan "dikkat bölgeleri
(attention)" mekanizmasını kullanarak ses tanımayı daha verimli
işletebilmeyi başarmışlar.

YSA'yı kullanmak için tekrar eğitim yapmaya gerek yok, hazır
pişirilmiş YSA modellerinin ağırlıkları kullanılabilir, 2.7 MB civarı
yer tutuyor ve bir .h5 dosyasından hızlı bir şekilde
yüklenebiliyor. Önce [1] projesini indirelim, `git clone` ya da farklı
bir yöntemle olabilir. Kodlar eğitilmiş bir ses tanıma YSA ağını
içerir, `model-attRNN.h5` dosyasında.  Github projesinin
`HOME/Documents/repos/SpeechCmdRecognition` altında olduğunu, ve [3]
dosyasının `/opt/Downloads/skdata/voice_cmd` dizininde olduğunu farzedersek,
alttaki kodu işletebiliriz.

```python
import numpy as np, tensorflow as tf, sys, os
from tensorflow.keras.models import Model, load_model
proj_src = os.environ['HOME'] + "/Documents/repos/SpeechCmdRecognition"
sys.path.append(proj_src)
import librosa, SpeechModels, zipfile, io

sr = 16000
nCategs = 36
model = SpeechModels.AttRNNSpeechModel(nCategs,
                                        samplingrate = sr,
                                        inputLength = None) 

model.compile(optimizer='adam',
              loss=['sparse_categorical_crossentropy'],
              metrics=['sparse_categorical_accuracy'])
model.summary()
model.load_weights(proj_src + '/model-attRNN.h5')
print("Model loaded successfully and weights loaded.")

audio_file_path = "/opt/Downloads/skdata/voice_cmd/wav/dog/fcb25a78_nohash_0.wav"
print(f"\nProcessing audio file: {audio_file_path}")

audio, current_sr = librosa.load(audio_file_path, sr=sr)
print(f"   Original sampling rate of audio: {current_sr} Hz")
if current_sr != sr:
    print(f"   Resampling audio from {current_sr} Hz to {sr} Hz (model's expected SR).")

if len(audio) == 0:
    raise ValueError("Audio file is empty or could not be loaded properly.")

target_length = sr * 1 

if len(audio) > target_length:
    audio = audio[:target_length] # Truncate if too long
elif len(audio) < target_length:
    padding = target_length - len(audio)
    audio = np.pad(audio, (0, padding), 'constant')

processed_audio = np.expand_dims(audio, axis=0) # Shape: (1, 16000) for a 1-second clip

print(f"   Processed audio shape for prediction: {processed_audio.shape}")

predictions = model.predict(processed_audio)

predicted_class_index = np.argmax(predictions[0])
confidence = np.max(predictions[0])

category_labels = ['nine', 'yes', 'no', 'up', 'down', 'left',
                   'right', 'on', 'off', 'stop', 'go', 'zero', 'one', 'two',
                   'three', 'four', 'five', 'six', 'seven', 'eight',
                   'backward', 'bed', 'bird', 'cat', 'dog', 'follow',
                   'forward', 'happy', 'house', 'learn', 'marvin', 'sheila',
                   'tree', 'visual', 'wow']    

if predicted_class_index < len(category_labels):
    predicted_label = category_labels[predicted_class_index-1]
    print(f"Predicted command: '{predicted_label}' with confidence: {confidence:.4f}")
```

```
________
 Layer (type)                Output Shape                 Param #   Connected to                  
==================================================================================================
 input (InputLayer)          [(None, None)]               0         []                            
                                                                                                  
 normalized_spectrogram_mod  (None, None, 80)             0         ['input[0][0]']               
 el (Functional)                                                                                  
                                                                                                  
 tf.expand_dims (TFOpLambda  (None, None, 80, 1)          0         ['normalized_spectrogram_model
 )                                                                  [0][0]']                      
                                                                                                  
 conv2d (Conv2D)             (None, None, 80, 10)         60        ['tf.expand_dims[0][0]']      
                                                                                                  
 batch_normalization (Batch  (None, None, 80, 10)         40        ['conv2d[0][0]']              
 Normalization)                                                                                   
                                                                                                  
 conv2d_1 (Conv2D)           (None, None, 80, 1)          51        ['batch_normalization[0][0]'] 
                                                                                                  
 batch_normalization_1 (Bat  (None, None, 80, 1)          4         ['conv2d_1[0][0]']            
 chNormalization)                                                                                 
                                                                                                  
 squeeze_last_dim (Lambda)   (None, None, 80)             0         ['batch_normalization_1[0][0]'
                                                                    ]                             
                                                                                                  
 bidirectional (Bidirection  (None, None, 128)            74240     ['squeeze_last_dim[0][0]']    
 al)                                                                                              
                                                                                                  
 bidirectional_1 (Bidirecti  (None, None, 128)            98816     ['bidirectional[0][0]']       
 onal)                                                                                            
                                                                                                  
 lambda (Lambda)             (None, 128)                  0         ['bidirectional_1[0][0]']     
                                                                                                  
 dense (Dense)               (None, 128)                  16512     ['lambda[0][0]']              
                                                                                                  
 dot (Dot)                   (None, None)                 0         ['dense[0][0]',               
                                                                     'bidirectional_1[0][0]']     
                                                                                                  
 attSoftmax (Softmax)        (None, None)                 0         ['dot[0][0]']                 
                                                                                                  
 dot_1 (Dot)                 (None, 128)                  0         ['attSoftmax[0][0]',          
                                                                     'bidirectional_1[0][0]']     
                                                                                                  
 dense_1 (Dense)             (None, 64)                   8256      ['dot_1[0][0]']               
                                                                                                  
 dense_2 (Dense)             (None, 32)                   2080      ['dense_1[0][0]']             
                                                                                                  
 output (Dense)              (None, 36)                   1188      ['dense_2[0][0]']             
                                                                                                  
==================================================================================================
Total params: 201247 (786.12 KB)
Trainable params: 201225 (786.04 KB)
Non-trainable params: 22 (88.00 Byte)
__________________________________________________________________________________________________
Model loaded successfully and weights loaded.

Processing audio file: /opt/Downloads/skdata/voice_cmd/wav/dog/fcb25a78_nohash_0.wav
   Original sampling rate of audio: 16000 Hz
   Processed audio shape for prediction: (1, 16000)
WARNING: All log messages before absl::InitializeLog() is called are written to STDERR
W0000 00:00:1752147986.896920  100346 op_level_cost_estimator.cc:699] Error in PredictCost() for the op: op: "Softmax" attr { key: "T" value { type: DT_FLOAT } } inputs { dtype: DT_FLOAT shape { unknown_rank: true } } device { type: "CPU" vendor: "AuthenticAMD" model: "248" frequency: 2096 num_cores: 12 environment { key: "cpu_instruction_set" value: "AVX SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2" } environment { key: "eigen" value: "3.4.90" } l1_cache_size: 32768 l2_cache_size: 524288 l3_cache_size: 8388608 memory_size: 268435456 } outputs { dtype: DT_FLOAT shape { unknown_rank: true } }
1/1 [==============================] - 2s 2s/step
Predicted command: 'dog' with confidence: 1.0000
```

Test için bir örnek wav kullandık, içinde `dog` kelimesi söyleniyordu,
ve üstteki test doğru sonuç bulundu. [3] verisindeki tüm wav
dosyalarının test edilmesi için gerekli kod `voice1.py` içinde. 500
kusur dosya üzerinden 90% başarı elde ediyor.

Not:

Üstteki kodu [1] projesine eklenmesi için göndermiştik [5], ve proje
idarecisi bu eklemeyi yapmış, projenin son halini alanlar direk wav
dosyasını işleyen kodu projenin kendisinden de alabilir.

Kodlar

[voice1.py](voice1.py)

Kaynaklar

[1] [Github](https://github.com/douglas125/SpeechCmdRecognition)

[2] de Andrade, *A neural attention model for speech command recognition*,
    [ArXiv](https://arxiv.org/abs/1808.08929)

[3] [Veri](https://www.dropbox.com/scl/fi/7bjyicydyyurizi314qp8/google_voice_small.zip?rlkey=l5ibbx480jld79exvkwih3szr&st=ehyr58nt&raw=1)

[4] [Kaggle](https://www.kaggle.com/datasets/neehakurelli/google-speech-commands)

[5] Issues, https://github.com/douglas125/SpeechCmdRecognition/issues/20
