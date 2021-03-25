'''
Creates a new matrix from Movielens data with users as rows and items
as columns with necessary fields one-hot-encoded
'''
from sklearn.feature_extraction import DictVectorizer
import pandas as pd
import os

def one_hot_dataframe(data, cols, replace=False):
    vec = DictVectorizer()
    mkdict = lambda row: dict((col, row[col]) for col in cols)
    vecData = pd.DataFrame(vec.fit_transform(data[cols].to_dict(outtype='records')).toarray())
    vecData.columns = vec.get_feature_names()
    vecData.index = data.index
    if replace is True:
        data = data.drop(cols, axis=1)
        data = data.join(vecData)
    return (data, vecData, vec)

rnames = ['user_id', 'movie_id', 'rating', 'timestamp']
ratings = pd.read_table('ratings.dat', sep='::', header=None,names=rnames)
df2 = ratings.pivot_table('rating', rows='user_id',cols='movie_id')
df2.to_csv("/tmp/out1.csv",sep=';')

import pandas as pd
unames = ['user_id', 'gender', 'age', 'occupation', 'zip']
users = pd.read_table('users.dat', sep='::', header=None,names=unames)
users = users.drop('zip',axis=1)

users['occupation'] = users['occupation'].map(lambda x: str(x))
df2, _, _ = one_hot_dataframe(users, ['gender','occupation'],True)
df2.to_csv("/tmp/out2.csv",sep=';',index=None)

df1 = pd.read_csv("/tmp/out1.csv",sep=';')
df2 = pd.read_csv("/tmp/out2.csv",sep=';')
df3 = pd.merge(df1,df2)

df3.to_csv("%s/Downloads/movielens.csv" % os.environ['HOME'],sep=';',index=None)
