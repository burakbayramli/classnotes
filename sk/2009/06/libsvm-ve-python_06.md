# LIBSVM ve Python

Iyi bir SVM siniflayici (classifier) icin LIBSVM programi
kullanilabilir. Bu siniflayici icin Python arayuzleri yazilmis. Kurmak
icin indirin ve en ust dizinden python altdizinini bulun. Oradan
"make" ve "sudo python setup.by install" isi hallediyor. Test icin
svm_test.py adli programi isleyebilirsiniz. Daha ufak bir ornek:from

```
svm import *labels = [0, 1, 1, 2]

samples = [[0, 0], [0, 1], [1, 0], [1, 1]]

problem = svm_problem(labels, samples);

size = len(samples)

kernels = [LINEAR, POLY, RBF]

kname = ['linear','polynomial','rbf']

param = svm_parameter(C = 10,nr_weight = 2,weight_label = [1,0],

weight = [10,1])param.kernel_type = kernels[0]

model = svm_model(problem,param)

prediction = model.predict([0, 1])

probability = model.predict_probability

print predictionSonucun 1.0
```

cikmasi lazim.





