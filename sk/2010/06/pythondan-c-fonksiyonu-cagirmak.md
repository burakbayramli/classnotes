# Python'dan C Fonksiyonu Cagirmak

Python script icinden C fonksiyonu cagirmak icin sunlar gerekli. Kodun
testmod.c dosyasinda oldugunu farzedelim; once bir shared library
olusturmamiz lazim. make.sh adinda bir derleme script'i soyle
olabilir:

```
#!/bin/sh
gcc -shared -o testmod.so -I/usr/include/python2.5 -lpython2.5  testmod.c
```

C fonksiyonu soyle bir dosyada:
#include "Python.h"

```
static PyObject* py_myFunction(PyObject* self, PyObject* args)
{
 char *s = "Hello from C!";
 return Py_BuildValue("s", s);
}

static PyObject* py_myOtherFunction(PyObject* self, PyObject* args)
{
 double x, y;
 PyArg_ParseTuple(args, "dd", &x, &y);
 return Py_BuildValue("d", x*y);
}

static PyMethodDef myModule_methods[] = {
 {"myFunction", py_myFunction, METH_VARARGS},
 {"myOtherFunction", py_myOtherFunction, METH_VARARGS},
 {NULL, NULL}
};

void inittestmod()
{
 (void) Py_InitModule("testmod", myModule_methods);
}
```

Usttekileri make.sh ile derledikten sonra, bir Python script icinden
sunlar isletilebilir:

```
from testmod import *

print "Result from myFunction:", myFunction()
print "Result from myOtherFunction(4.0, 5.0):", myOtherFunction(4.0, 5.0)
```

Kodlarin tamami altta

Not: Eger gcc yerine g++ kullanirsaniz, o zaman tum fonksiyonlarin
extern "C" { } ile sarilmasi gerekli, cunku g++ derleyicileri
derlerken fonksiyon isimlerini degisime ugratiyorlar (mangling), ve
Python yorumlayicisi bekledigi cagri isimlerini bulamayinca hata
veriyor. Surada.

Not: Bir .so dosyasi, shared library icindeki sembolleri, fonksiyon
cagrilarini listelemek icin "nm dosya.so" komutu kullanilabilir.

Kaynak

make.sh

```
#!/bin/sh
gcc -shared -o testmod.so -g `pkg-config opencv --cflags --libs glib-2.0` -I/usr/include/python2.5 -lpython2.5  testmod.c
```


testmod.c

```
#include "Python.h"

/*

 * Function to be called from Python

 */

static PyObject* py_myFunction(PyObject* self, PyObject* args)

{

 char *s = "Hello from C!";

 return Py_BuildValue("s", s);

}



/*

 * Another function to be called from Python

 */

static PyObject* py_myOtherFunction(PyObject* self, PyObject* args)

{

 double x, y;

 PyArg_ParseTuple(args, "dd", &x, &y);

 return Py_BuildValue("d", x*y);

}



/*

 * Bind Python function names to our C functions

 */

static PyMethodDef myModule_methods[] = {

 {"myFunction", py_myFunction, METH_VARARGS},

 {"myOtherFunction", py_myOtherFunction, METH_VARARGS},

 {NULL, NULL}

};



/*

 * Python calls this to let us initialize our module

 */

void inittestmod()

{

 (void) Py_InitModule("testmod", myModule_methods);

}
```

testmod.py


```
from testmod import *

print "Result from myFunction:", myFunction()

print "Result from myOtherFunction(4.0, 5.0):", myOtherFunction(4.0, 5.0)
```













