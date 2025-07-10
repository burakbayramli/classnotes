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

model.load_weights(proj_src + '/model-attRNN.h5')
print("✅ Model loaded successfully and weights loaded.")

category_labels = ['nine', 'yes', 'no', 'up', 'down', 'left',
                   'right', 'on', 'off', 'stop', 'go', 'zero', 'one', 'two',
                   'three', 'four', 'five', 'six', 'seven', 'eight',
                   'backward', 'bed', 'bird', 'cat', 'dog', 'follow',
                   'forward', 'happy', 'house', 'learn', 'marvin', 'sheila',
                   'tree', 'visual', 'wow']

zfile_path = "/opt/Downloads/google_voice_small.zip"

pos = 0; neg = 0
with zipfile.ZipFile(zfile_path, 'r') as z:
    files_in_zip = z.namelist()
    
    for file_path_in_zip in files_in_zip:
        if file_path_in_zip.endswith(".wav"):
            try:
                expected_label = os.path.basename(os.path.dirname(file_path_in_zip))
                print(f"\nProcessing file: {file_path_in_zip}")
                print(f"   Expected label: {expected_label}")
                with z.open(file_path_in_zip) as f_in_zip:
                    audio_bytes = io.BytesIO(f_in_zip.read())
                audio, current_sr = librosa.load(audio_bytes, sr=sr)                
                print(f"   Original sampling rate of audio: {current_sr} Hz (librosa resampled to {sr} Hz)")
                if len(audio) == 0:
                    print("⚠️ Warning: Audio file is empty or could not be loaded properly.")
                    continue # Skip to the next file
                target_length = sr * 1
                if len(audio) > target_length:
                    audio = audio[:target_length]
                elif len(audio) < target_length:
                    padding = target_length - len(audio)
                    audio = np.pad(audio, (0, padding), 'constant')

                processed_audio = np.expand_dims(audio, axis=0) # Add batch dimension (1, 16000)
                predictions = model.predict(processed_audio)
                predicted_class_index = np.argmax(predictions[0])
                confidence = np.max(predictions[0])

                predicted_label = "UNKNOWN/BACKGROUND" # Default for special cases
                if predicted_class_index > 0 and predicted_class_index <= len(category_labels):
                    adjusted_index = predicted_class_index - 1
                    predicted_label = category_labels[adjusted_index]
                elif predicted_class_index == 0:
                    pass # Keep as UNKNOWN/BACKGROUND or similar placeholder

                print(f"📈 Predicted command: '{predicted_label}' with confidence: {confidence:.4f}")
                if predicted_label.lower() != expected_label.lower():
                    neg += 1
                    print(f"❌ Mismatch! Predicted '{predicted_label}' but expected '{expected_label}'")
                else:
                    pos += 1
                    print(f"✅ Match! Predicted '{predicted_label}' as expected.")
                
            except Exception as e:
                print(f"❌ Error processing {file_path_in_zip}: {e}")
                continue # Continue to the next file even if one fails

print (pos, neg, pos/(pos+neg))
