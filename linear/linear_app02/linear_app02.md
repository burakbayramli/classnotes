# Matris Tersi, Cholesky Ayrıştırması

Eger kovaryans matrisleri ile ugrasiyorsak bazen bu matrislerin
tersini almamiz gerekebiliyor. Eger elimizdeki kovaryans matrisi essiz
(singular) degil ise (kovaryans matrisi zaten pozitif kesin olur) o
zaman matris tersini alma islemini Cholesky ayristirmasi ile
yapabiliriz.

Diyelim ki $\boldsymbol{\Sigma}$ bir pozitif kesin kovaryans matrisi.
O zaman Cholesky ayrıştırması

$$\boldsymbol{\Sigma} = \mathbf{L}\mathbf{L}^T$$

ki $\mathbf{L}$ bir alt üçgensel (lower triangular) matristır, ve
dikgendeki öğeler pozitiftir. Bulmak istediğimiz
$\boldsymbol{\Sigma}^{-1}$. Bu işlemi ayrıştırmaya uygularsak,

$$\boldsymbol{\Sigma} = \mathbf{L}\mathbf{L}^T$$

İki tarafın tersini alalım,

$$\boldsymbol{\Sigma}^{-1} = (\mathbf{L}\mathbf{L}^T)^{-1}$$

Şu özelliği kullanalım, $(\mathbf{A}\mathbf{B})^{-1} =
\mathbf{B}^{-1}\mathbf{A}^{-1}$:

$$\boldsymbol{\Sigma}^{-1} = (\mathbf{L}^T)^{-1}\mathbf{L}^{-1}$$

Ya da

$$\boldsymbol{\Sigma}^{-1} = \mathbf{L}^{-T}\mathbf{L}^{-1}$$

Ya da

$$
\boldsymbol{\Sigma}^{-1} = (\mathbf{L}^{-1})^T \mathbf{L}^{-1}
$$

ki $\mathbf{L}^{-T}$ büyüklüğü şu ile aynı $(\mathbf{L}^T)^{-1}$.

Bu hesap hesapsal açıdan daha verimlidir çünkü,

1. $L$ hesabı: Cholesky'nin hesapsal yükü $O(n^3/3)$ (dikkat bölü 3
bir sabit fakat gerçek dünya uygulamalarında bu fark yaratıyor).

2. $L$ tersi: $\mathbf{L}$ iş üçgensel olduğu için $\mathbf{L}^{-1}$
işlemi geriye değer sokma (back-substitution) ile  $O(n^3/3)$, genel
matris tersini alma işleminden daha hızlı.

```python
A = np.random.rand(3,3)
B = A @ A.T

L = np.linalg.cholesky(B)
Linv = np.linalg.inv(L)
Binv1 = Linv.T @ Linv
print ('Cholesky ile')
print (Binv1,'\n')

print (u'Klasik yöntem')
Binv2 = np.linalg.inv(B)
print (Binv2)
```

```text
Cholesky ile
[[ 404.6037679  -313.9944771  -106.74225974]
 [-313.9944771   258.50401095   65.98449623]
 [-106.74225974   65.98449623   48.13729976]] 

Klasik yöntem
[[ 404.6037679  -313.9944771  -106.74225974]
 [-313.9944771   258.50401095   65.98449623]
 [-106.74225974   65.98449623   48.13729976]]
```
