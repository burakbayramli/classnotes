# Ses Komut Tanıma, Dikkat (Attention) Modeli

Ses tanıma için hazır pişirilmiş bir model.

```python
import numpy as np, tensorflow as tf, sys, os
from tensorflow.keras.models import Model, load_model
proj_src = os.environ['HOME'] + "/Documents/repos/SpeechCmdRecognition"
sys.path.append(proj_src)
import librosa, SpeechModels, zipfile, io

# --- Model Loading (from your provided code) ---
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

# --- 🎤 Audio File Preprocessing and Prediction ---

# Define the path to your sample audio file
audio_file_path = "/opt/Downloads/voice_cmd/wav/dog/fcb25a78_nohash_0.wav"
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

Kodlar

[voice1.py](voice1.py)

Kaynaklar

[1] https://github.com/douglas125/SpeechCmdRecognition

[2] de Andrade, *A neural attention model for speech command recognition*,
    [ArXiv](https://arxiv.org/abs/1808.08929)

[3] https://www.dropbox.com/scl/fi/7bjyicydyyurizi314qp8/google_voice_small.zip?rlkey=l5ibbx480jld79exvkwih3szr&st=ehyr58nt&raw=1

