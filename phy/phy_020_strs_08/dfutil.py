import pandas as pd, numpy as np
def expand_dataframe(df, new_cols):

    res = df.copy()
    old_cols = list(df.columns)
    addn_vars = [x for x in new_cols if x not in old_cols]
    res.index = df.columns
    for x in addn_vars:
        res[x] = np.nan
        res.loc[x] = pd.Series(res.columns)
    res = res[new_cols]
    res = res.reindex(new_cols).fillna(0)
    return res
def drop_col_row(df, var_list):
    res = df.copy()
    for x in var_list:
        res = res.drop(x,axis=1)
        res = res.drop(x,axis=0)
    return res
