# Derin Öğrenim ile Text Üretmek, RNN, LSTM

Derin Öğrenim her yerde; yapay sinir ağları sağlam bir dönüş yaptılar,
ve ardı ardına ilerlemeler, ve ilginç kullanımlar haberleri
görülebiliyor. Bunlardan biri A. Karpathy'nin Kendini Tekrarlayan
Yapay Sinir Ağları (Recurrent Neural Network, -RNN-) hakkında
yazdıkları. Arkadaş Shakespeare'in uzun bir oyununu RNN'e okutup sonra
bu ağdan Shakespeare ürettirmiş. Sonuçlar fena değil.

Öteki yandan alttaki Y. Goldberg adlı birisi sadece önceki N
karakterden sonra her karakterin kaç kere geldiğini "sayarak" ve
frekansları kullanarak metin üretmiş. Burada derin öğrenim filan yok,
sadece basit sayım var. Sonuçlar onda da iyi. Ama nasıl oldu bu iş?
Derin YSA hiçbir iş yapmıyor mu yani? Fakat Goldberg'in dediği gibi
aslında RNN, LSTM gibi derin modelleri daha farklı bir şey
yapıyorlar. Shakespeare örneğinde her iki yaklaşım da Shakespeare'imsi
bir şeyler üretiyor, fakat RNN, LSTM yaklaşımında bazı gramer yapıları
öğrenilmeye başlanıyor.

Mesela parantez açıp bir blok sonra parantez kapamak frekans modelleri
için zor, fakat DO bunu başarıyor. Goldberg "N sayısını arttırıp 10,20
seviyesine getirsem belki bunları frekans modeli de yapardı ama
hafızam yetmezdi" diye bir yorum da yapmış, ve bu nokta DO / YSA'nın
önemli bir avantajını yakalamış. Frekans modeli mesela 9 harf geriye
bakıp 10. harfi tahmin için, mumkun tum kombinasyonlar icin 20 harflik
alfabede 20**10 öğeli bir sözlük / listeye sahip olmalıdır, bu 10240
milyar öğe demektir. O tüm kombinasyonlar tabii ki girdi verisi içinde
olmayabilir, fakat az bir kısmı olsa bile bu müthiş büyük bir
sayıdır. Halbuki derin öğrenim, her ne kadar modeli derinleştirse de,
her seviye sadece bir sonraki seviyeyle iletişimde olduğu için yer
israf etmiyor. Üstelik bu katmanlı yaklaşımı ile her katmanın belli
alanlara odaklaşmasını teşvik ediyor, böylece model
genelleştirebiliyor. Frekans modeli ezberci, derin öğrenim böyle
değil.

Su yazıyı baz alarak mevcut olan bir DO'yu (LSTM) yükleyip biz de
metin üretme yaptırdık, kurulum için

```python
sudo pip install tensorflow
sudo pip install h5py
```

Kullanılan DO programı Keras. Kaynak indirilip python setup.py build
ve install ile kurulabilir.

Model

http://www.cs.virginia.edu/~vicente/recognition/notebooks/weights-vicente.hdf5

Alttaki kodu arka arkaya isletince 

a black cat is drinking into a bowl on the side of a road
a bike sitting next to a stick is taking his lapkay
a person is standing on a restaurant

gibi cümleler üretiliyor. Kelimelerin çoğunun mantıklı olması bir yana, gramer yapısı da fena değil.

Kod

```python
import tensorflow as tf
import numpy as np
from keras.models import Sequential
from keras.layers import Dense, Activation, LSTM
from keras.optimizers import RMSprop
from keras.layers.wrappers import TimeDistributed

# Read captions into a python list.
maxSamples = 10000
captions = []
fopen = open('captions_train.txt', 'r')
iterator = 0
for line in fopen:
    if iterator < maxSamples:
        captions.append(line.lower().strip())
        iterator += 1
fopen.close()
    
# Compute a char2id and id2char vocabulary.
char2id = {}
id2char = {}
charIndex = 0
for caption in captions: 
    for char in caption:
        if char not in char2id:
            char2id[char] = charIndex
            id2char[charIndex] = char
            charIndex += 1

# Add a special starting and ending character to the dictionary.
char2id['S'] = charIndex; id2char[charIndex] = 'S'  # Special sentence start character.
char2id['E'] = charIndex + 1; id2char[charIndex + 1] = 'E'  # Special sentence ending character.
            
# Place captions inside tensors.
maxSequenceLength = 1 + max([len(x) for x in captions])
# inputChars has one-hot encodings for every character, for every caption.
inputChars = np.zeros((len(captions), maxSequenceLength, len(char2id)), dtype=np.bool)
# nextChars has one-hot encodings for every character for every caption (shifted by one).
nextChars = np.zeros((len(captions), maxSequenceLength, len(char2id)), dtype=np.bool)
for i in range(0, len(captions)):
    inputChars[i, 0, char2id['S']] = 1
    nextChars[i, 0, char2id[captions[i][0]]] = 1
    for j in range(1, maxSequenceLength):
        if j < len(captions[i]) + 1:
            inputChars[i, j, char2id[captions[i][j - 1]]] = 1
            if j < len(captions[i]):
                nextChars[i, j, char2id[captions[i][j]]] = 1
            else:
                nextChars[i, j, char2id['E']] = 1
        else:
            inputChars[i, j, char2id['E']] = 1
            nextChars[i, j, char2id['E']] = 1

print("input:")
print(inputChars.shape)  # Print the size of the inputCharacters tensor.
print("output:")
print(nextChars.shape)  # Print the size of the nextCharacters tensor.
print("char2id:")
print(char2id)  # Print the character to ids mapping.

trainCaption = inputChars[25, :, :]  # Pick some caption
labelCaption = nextChars[25, :, :]  # Pick what we are trying to predict.

def printCaption(sampleCaption):
    charIds = np.zeros(sampleCaption.shape[0])
    for (idx, elem) in enumerate(sampleCaption):
        charIds[idx] = np.nonzero(elem)[0].squeeze()
    print(np.array([id2char[x] for x in charIds]))

printCaption(trainCaption)
printCaption(labelCaption)

print('Building training model...')
hiddenStateSize = 128
hiddenLayerSize = 128
model = Sequential()
# The output of the LSTM layer are the hidden states of the LSTM for every time step. 
model.add(LSTM(hiddenStateSize, return_sequences = True, input_shape=(maxSequenceLength, len(char2id))))
# Two things to notice here:
# 1. The Dense Layer is equivalent to nn.Linear(hiddenStateSize, hiddenLayerSize) in Torch.
#    In Keras, we often do not need to specify the input size of the layer because it gets inferred for us.
# 2. TimeDistributed applies the linear transformation from the Dense layer to every time step
#    of the output of the sequence produced by the LSTM.
model.add(TimeDistributed(Dense(hiddenLayerSize)))
model.add(TimeDistributed(Activation('relu'))) 
model.add(TimeDistributed(Dense(len(char2id))))  # Add another dense layer with the desired output size.
model.add(TimeDistributed(Activation('softmax')))
# We also specify here the optimization we will use, in this case we use RMSprop with learning rate 0.001.
# RMSprop is commonly used for RNNs instead of regular SGD.
# See this blog for info on RMSprop (http://sebastianruder.com/optimizing-gradient-descent/index.html#rmsprop)
# categorical_crossentropy is the same loss used for classification problems using softmax. (nn.ClassNLLCriterion)
model.compile(loss='categorical_crossentropy', optimizer = RMSprop(lr=0.001))

print(model.summary()) # Convenient function to see details about the network model.

# Test a simple prediction on a batch for this model.
print("Sample input Batch size:"),
print(inputChars[0:32, :, :].shape)
print("Sample input Batch labels (nextChars):"),
print(nextChars[0:32, :, :].shape)
outputs = model.predict(inputChars[0:32, :, :])
print("Output Sequence size:"),
print(outputs.shape)

inference_model = Sequential()
# Two differences here.
# 1. The inference model only takes one sample in the batch, and it always has sequence length 1.
# 2. The inference model is stateful, meaning it inputs the output hidden state ("its history state")
#    to the next batch input.
inference_model.add(LSTM(hiddenStateSize, batch_input_shape=(1, 1, len(char2id)), stateful = True))
# Since the above LSTM does not output sequences, we don't need TimeDistributed anymore.
inference_model.add(Dense(hiddenLayerSize))
inference_model.add(Activation('relu'))
inference_model.add(Dense(len(char2id)))
inference_model.add(Activation('softmax'))

inference_model.load_weights("weights-vicente.hdf5")

# Given the start Character 'S' (one-hot encoded), predict the next most likely character.
startChar = np.zeros((1, 1, len(char2id)))
startChar[0, 0, char2id['S']] = 1
nextCharProbabilities = inference_model.predict(startChar)

# print the most probable character that goes next.
print(id2char[nextCharProbabilities.argmax()])

inference_model.reset_states()  # This makes sure the initial hidden state is cleared every time.

startChar = np.zeros((1, 1, len(char2id)))
startChar[0, 0, char2id['S']] = 1

res = []
for i in range(0, 100):
    nextCharProbs = inference_model.predict(startChar)
    
    # In theory I should be able to input nextCharProbs to np.random.multinomial.
    nextCharProbs = np.asarray(nextCharProbs).astype('float64') # Weird type cast issues if not doing this.
    nextCharProbs = nextCharProbs / nextCharProbs.sum()  # Re-normalize for float64 to make exactly 1.0.
    
    nextCharId = np.random.multinomial(1, nextCharProbs.squeeze(), 1).argmax()
    res.append(id2char[nextCharId]) # The comma at the end avoids printing a return line character.
    startChar.fill(0)
    startChar[0, 0, nextCharId] = 1
print ''.join(res)
```

