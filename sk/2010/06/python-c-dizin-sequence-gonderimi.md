# Python -> C Dizin (Sequence) Gonderimi

Bir Python script icinden C fonksiyonuna dizin gondermek icin

```
static PyObject* py_dizin(PyObject* self, PyObject* args){
  PyObject* seq;
  if(!PyArg_ParseTuple(args, "O", &seq))
  return 0;
  seq = PySequence_Fast(seq, "argument must be iterable");
  int seqlen = PySequence_Fast_GET_SIZE(seq);
  float* dizin = new VL::float[seqlen];
  double *dbar = new double[seqlen];
  for(int i=0; i < seqlen; i++) {
     PyObject *fitem; PyObject *item = PySequence_Fast_GET_ITEM(seq, i);
     fitem = PyNumber_Float(item);
     dizin[i] = PyFloat_AS_DOUBLE(fitem);
     printf("%f\n",dizin[i]); // ekrana bas Py_DECREF(fitem);
  }
Py_DECREF(seq);
delete[] pic; // is bitince dizin burada silinir
return Py_BuildValue("d", 0); // geri sifir dondur}Python icinden test
etmek icin dizin([10.,20.,30.]) gibi bir cagri yapilabilir.
```






