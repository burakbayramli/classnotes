import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' 
os.environ['CUDA_VISIBLE_DEVICES'] = '-1'
import keras_ocr

pipeline = keras_ocr.pipeline.Pipeline()

images = [
    keras_ocr.tools.read(url) for url in [
        'street1.jpg'
    ]
]

prediction_groups = pipeline.recognize(images)
ps = prediction_groups[0]
for p in ps: print (p[0])
