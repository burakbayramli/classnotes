import pandas as pd
import numpy as np

Q = 1.0 ; T = 1.0

class Game:

    def __init__(self,df):
        self.df = df.copy()
        self.df_orig = self.df.copy()
        # dictionaries of df variables - used for speedy access
        self.df_capability = df.Capability.to_dict()    
        self.df_position = df.Position.to_dict()    
        self.df_salience = df.Salience.to_dict()    
        self.max_pos = df.Position.max()
        self.min_pos = df.Position.min()

    def weighted_median(self):
        df = self.df.copy()
        df['w'] = df.Capability*df.Salience
        df = df.sort_index(by='Position',ascending=True)
        df['w'] = df['w'] / df['w'].sum()
        df['w'] = df['w'].cumsum()       
        return float(df[df['w']>=0.5].head(1).Position)

    def mean(self):
        return (self.df.Capability*self.df.Position*self.df.Salience).sum() / \
               (self.df.Capability*self.df.Salience).sum()

