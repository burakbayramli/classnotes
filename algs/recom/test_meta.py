import matplotlib.pyplot as plt
from pathlib import Path
from zipfile import ZipFile
from keras.layers import Input, Embedding, Flatten, merge, Dense, Dropout, Lambda
from keras.models import Model
import keras.backend as K
import pandas as pd, zipfile
from sklearn.metrics import mean_squared_error, mean_absolute_error

ML_100K_FOLDER = Path('ml-100k')
icols = ["name", "date", "genre", "url"]
icols += ["f" + str(x) for x in range(19)]  # unused feature names
rcols  = ["user_id", "item_id", "rating", "timestamp"]
with zipfile.ZipFile('ml-100k.zip', 'r') as z:
    all_ratings = pd.read_csv(z.open('ml-100k/u.data'), sep='\t', names=rcols)
    items = pd.read_csv(z.open('ml-100k/u.item'), sep='|',names=icols, encoding='latin-1')
    
items.fillna(value="01-Jan-1997", inplace=True)

from sklearn.model_selection import train_test_split

ratings_train, ratings_test = train_test_split(
    all_ratings, test_size=0.2, random_state=0)

user_id_train = ratings_train['user_id']
item_id_train = ratings_train['item_id']
rating_train = ratings_train['rating']

user_id_test = ratings_test['user_id']
item_id_test = ratings_test['item_id']
rating_test = ratings_test['rating']

max_user_id = all_ratings['user_id'].max()
max_item_id = all_ratings['item_id'].max()

# transform the date (string) into an int representing the release year
parsed_dates = [int(film_date[-4:])
                for film_date in items["date"].tolist()]

items['parsed_date'] = pd.Series(parsed_dates, index=items.index)
max_date = max(items['parsed_date'])
min_date = min(items['parsed_date'])

from sklearn.preprocessing import scale

items['scaled_date'] = scale(items['parsed_date'].astype('float64'))
item_meta_train = items["scaled_date"][item_id_train]
item_meta_test = items["scaled_date"][item_id_test]

# For each sample we input the integer identifiers
# of a single user and a single item
user_id_input = Input(shape=[1])
item_id_input = Input(shape=[1])
meta_input = Input(shape=[1])

embedding_size = 32
user_embedding = Embedding(output_dim=embedding_size, input_dim=max_user_id + 1,
                           input_length=1)(user_id_input)
item_embedding = Embedding(output_dim=embedding_size, input_dim=max_item_id + 1,
                           input_length=1)(item_id_input)


user_vecs = Flatten()(user_embedding)

item_vecs = Flatten()(item_embedding)

input_vecs = merge([user_vecs, item_vecs, meta_input], mode='concat')

x = Dense(64, activation='relu')(input_vecs)
x = Dropout(0.5)(x)
x = Dense(32, activation='relu')(x)
y = Dense(1)(x)

model = Model(input=[user_id_input, item_id_input, meta_input], output=y)
model.compile(optimizer='adam', loss='mae')

initial_train_preds = model.predict([user_id_train, item_id_train, item_meta_train])

history = model.fit([user_id_train, item_id_train, item_meta_train], rating_train,
                    batch_size=64, nb_epoch=15, validation_split=0.1,
                    shuffle=True)

test_preds = model.predict([user_id_test, item_id_test, item_meta_test])
print("Final test Loss: %0.3f" % mean_squared_error(test_preds, rating_test))
print("Final test Loss: %0.3f" % mean_absolute_error(test_preds, rating_test))
