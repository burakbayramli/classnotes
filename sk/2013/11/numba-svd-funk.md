# Numba ile SVD Hızlandırma

Numba ve Funk SVD

Eğer Numba [2] kullanırsak, SVD kodunu çok daha hızlı işletebiliriz. Ayrıca
Funk'ın kodlaması (ki alttaki kodu onu temel alacak) biraz daha ilginç, mesela
en dış döngü özellikler (feature) geziyor, onun içindeki birkaç yüz kez yine
kendi içinde olan tahmin/hata hesabını yapıyor, tüm veri seti üzerinde. Bunun
için Movielens 100k verisi lazım, ardından [data_m100k.py](data_m100k.py) ile veri
yaratılır [1],

```python
# Requires Movielens 100k data 
from scipy.io import mmread, mmwrite
import numpy as np, time, sys
from numba import jit
import os

def create_user_feature_matrix(review_matrix, NUM_FEATURES):
    num_users = review_matrix.shape[0]
    user_feature_matrix = 1./NUM_FEATURES * \
        np.random.randn(NUM_FEATURES, num_users).astype(np.float32)
    return user_feature_matrix

def create_movie_feature_matrix(review_matrix, NUM_FEATURES):
    num_movies = review_matrix.shape[1]
    movie_feature_matrix = 1./NUM_FEATURES * \
        np.random.randn(NUM_FEATURES, num_movies).astype(np.float32)
    return movie_feature_matrix

@jit(nopython=True)
def predict_rating(user_id, movie_id, user_feature_matrix, movie_feature_matrix):
    rating = 1.
    for f in range(user_feature_matrix.shape[0]):
        rating += \
            user_feature_matrix[f, user_id] * \
            movie_feature_matrix[f, movie_id]
    if rating > 5: rating = 5
    elif rating < 1: rating = 1
    return rating

@jit(nopython=True)
def sgd_inner(feature, A_row, A_col, A_data,
              user_feature_matrix, movie_feature_matrix,
              NUM_FEATURES):
    K = 0.015
    LEARNING_RATE = 0.001
    squared_error = 0
    for k in range(len(A_data)):
        user_id = A_row[k]
        movie_id = A_col[k]
        rating = A_data[k]
        p = predict_rating(user_id,
                           movie_id,
                           user_feature_matrix,
                           movie_feature_matrix)
        err = rating - p            
        squared_error += err ** 2
        user_feature_value = user_feature_matrix[feature, user_id]
        movie_feature_value = movie_feature_matrix[feature, movie_id]
        user_feature_matrix[feature, user_id] += \
            LEARNING_RATE * (err * movie_feature_value - K * user_feature_value)
        movie_feature_matrix[feature, movie_id] += \
            LEARNING_RATE * (err * user_feature_value - K * movie_feature_value)

    return squared_error

def calculate_features(A_row, A_col, A_data,
                       user_feature_matrix, movie_feature_matrix,
                       NUM_FEATURES):
    MIN_IMPROVEMENT = 0.0001
    MIN_ITERATIONS = 200
    rmse = 0
    last_rmse = 0
    print (len(A_data))
    num_ratings = len(A_data)
    for feature in xrange(NUM_FEATURES):
        iter = 0
        while (iter < MIN_ITERATIONS) or  (rmse < last_rmse - MIN_IMPROVEMENT):
            last_rmse = rmse
            squared_error = sgd_inner(feature, A_row, A_col, A_data,
                                      user_feature_matrix, movie_feature_matrix,
                                      NUM_FEATURES)
            rmse = (squared_error / num_ratings)
            iter += 1
        print (('Squared error = %f' % squared_error))
        print ('RMSE = %f' % rmse)
        print ('Feature = %d' % feature)
    return last_rmse

def main():
    LAMBDA = 0.02
    NUM_FEATURES = 30

    A = mmread('%s/Downloads/A_m100k_train' % os.environ['HOME'])

    user_feature_matrix = create_user_feature_matrix(A, NUM_FEATURES)
    movie_feature_matrix = create_movie_feature_matrix(A, NUM_FEATURES)

    A = A.tocoo()

    rmse = calculate_features(A.row, A.col, A.data,
                              user_feature_matrix, movie_feature_matrix,
                              NUM_FEATURES )
    print ('rmse', rmse)

    np.savetxt("/tmp/user_feature_matrix2.dat", user_feature_matrix)
    np.savetxt("/tmp/movie_feature_matrix2.dat", movie_feature_matrix)

if __name__ == "__main__": 
    main()
```

Üstteki script'i işlettikten sonra bazı tavsiyeleri gösterebiliriz,

```python
import pandas as pd, os
items_file = '%s/Downloads/ml-100k/u.item' % os.environ['HOME']
item_df = pd.read_csv(items_file, sep='|',header=None)
item_df['idx'] = item_df[0] - 1
item_df = item_df.set_index('idx')

from scipy.io import mmread, mmwrite
import numpy as np, time, sys, os
import funk2, pandas as pd

user_feature_matrix = np.loadtxt("/tmp/user_feature_matrix2.dat")
movie_feature_matrix = np.loadtxt("/tmp/movie_feature_matrix2.dat")

preds = []
user_id = 110
for movie_id in range(1682):
    pred = funk2.predict_rating(user_id, movie_id, user_feature_matrix, movie_feature_matrix)
    preds.append([movie_id, pred])

preds_df = pd.DataFrame(preds,columns=['movie','score'])
preds_df.sort_index(by='score',ascending=False,inplace=True)
preds_df['movie_name'] = item_df[1]
print (preds_df.head(10))
```

```
      movie     score                                         movie_name
1448   1448  4.600873                             Pather Panchali (1955)
407     407  4.538450                              Close Shave, A (1995)
168     168  4.424078                         Wrong Trousers, The (1993)
482     482  4.406603                                  Casablanca (1942)
99       99  4.402690                                       Fargo (1996)
11       11  4.362673                         Usual Suspects, The (1995)
319     319  4.323309  Paradise Lost: The Child Murders at Robin Hood...
49       49  4.315158                                   Star Wars (1977)
113     113  4.308009  Wallace & Gromit: The Best of Aardman Animatio...
172     172  4.288836                         Princess Bride, The (1987)
```

Bu kişinin seyrettiği ve en çok beğendiği filmler altta

```python
A = mmread('%s/Downloads/A_m100k_train' % os.environ['HOME']).tocsc()
movies = A[user_id,:].nonzero()[1]
ratings = A[user_id,A[user_id,:].nonzero()[1]]
ratings = np.ravel(ratings.todense())
likes_df = pd.DataFrame()
likes_df['movie'] = movies; likes_df['rating'] = ratings
likes_df = likes_df.set_index('movie')
likes_df.sort_index(by='rating',ascending=False,inplace=True)
likes_df['movie_name'] = item_df[1]
print (likes_df.head(10))
```

```
       rating                     movie_name
movie                                       
301         5       L.A. Confidential (1997)
314         5               Apt Pupil (1998)
257         4                 Contact (1997)
285         4    English Patient, The (1996)
303         4           Fly Away Home (1996)
310         4  Wings of the Dove, The (1997)
353         4     Wedding Singer, The (1998)
302         3             Ulee's Gold (1997)
320         3                  Mother (1996)
304         2          Ice Storm, The (1997)
```

Kaynaklar

[1] Bayramli, Lineer Cebir, Yaklaşıksal SVD ile Tavsiye Sistemleri

[2] Bayramlı, *Numba, LLVM, ve SVD*, 
     [https://burakbayramli.github.io/dersblog/sk/2014/09/numba-llvm-ve-svd.html](https://burakbayramli.github.io/dersblog/sk/2014/09/numba-llvm-ve-svd.html)







