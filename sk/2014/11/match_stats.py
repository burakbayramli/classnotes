def get_non_feature_columns():
    """ 
    Ozellikler dataframe'inin kolon isimlerini dondur. Bu dataframe
    tahmin icin kullanilmayan bir dataframe; kolonlari metaveri
    aslinda (yani veri hakkinda veri), mesela takim ismi gibi, ya da
    regresyon icin kaynak degil hedef veri iceren kolonlar. Hedef kolonunu
    kullanmak istemiyoruz cunku bir macin verisini kullanarak ayni maci tahmin
    ettigimiz bir durumda olmak istemeyiz.
    """
    return ['teamid', 'op_teamid', 'matchid', 'competitionid', 'seasonid',
            'goals', 'op_goals', 'points', 'timestamp', 'team_name', 
            'op_team_name']

def get_feature_columns(all_cols):
    """
    Tahminde kullanilmasi gereken tum kolonlari geri dondurur, mesela
    mesela dataframe'de olan ama features.get_non_feature_column() icinde
    olmayan tum ogeler (kolonlar)
    """
    return [col for col in all_cols if col not in get_non_feature_columns()]

