"""
    Mac hakkindaki ham veriyi tahmin icin kullanabilecegimiz
    ozelliklere donusturur. Tarihi verideki birkac maci birlestirip
    birlesik / ozetsel bazi hesaplar uret ki boylece sonraki maci
    tahmin edebilelim.
"""

import pandas as pd

import match_stats

def get_wc_features(history_size):
    return pd.read_csv('results-20140714-123022.csv',sep=',')

def get_features(history_size):
    return pd.read_csv('results-20140714-123519.csv',sep=',')

def get_game_summaries():
    return pd.read_csv('results-20140714-124014.csv',sep=',')

