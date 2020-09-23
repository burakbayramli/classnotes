"""
Futbol takimlarin gecmiste oynadiklari maclara gore bir guc indisi atayarak
derecelendirir.
"""

import numpy as np
from numpy.linalg import LinAlgError
import pandas as pd

import world_cup

def _build_team_matrix(data, target_col):
    """
    Verili bir mac dataframe'ine gore bir seyrek guc matrisi yaratir.
    Girdi verisinde her mac icin arka arkaya iki tane satir kaydi olmasini
    bekliyoruz. Ilk satir deplasman takimi hakkinda bilgi tasiyacak,
    ikinci satir konuk takim hakkinda. Hesap yapilan matriste takimlar kolonlarda
    maclar ise satirlarda olacak. Her mac icin deplasman takiminin
    kolonunda pozitif bir deger olacak. Konuk takim icin negatif bir deger
    olacak. Futbolde deplasman avantaji cok onemli oldugu icin
    bu deplasmanda oynayan takimdan biraz avantaj cikartiyoruz. Tabii
    burada dikatli olmak ta lazim, cunku dunya kupasi icin is_home (deplasman mi?)
    kolonu evet/hayir formatinda degil (0.0 ile 1.0 arasinda reel degerler).
    Guc matrisindeki en son kolon bir puan matrisi, bu kolona
    deplasman takimi ile konuk takimin hedef kolonlari arasindaki fark
    yazilmis.
    """
    teams = {}
    nrows = len(data) / 2
    for teamid in data['teamid']:
        teams[str(teamid)] = pd.Series(np.zeros(nrows))

    result = pd.Series(np.empty(nrows))
    teams[target_col] = result

    current_season = None
    current_discount = 2.0

    for game in xrange(nrows):
        home = data.iloc[game * 2]
        away = data.iloc[game * 2 + 1]
        if home['seasonid'] != current_season:
            # Discount older seasons.
            current_season = home['seasonid']
            current_discount *= 0.6
            print "New season %s" % (current_season,)

        home_id = str(home['teamid'])
        away_id = str(away['teamid'])
        points = home[target_col] - away[target_col]

        # Discount home team's performance.
        teams[home_id][game] = (1.0 + home['is_home'] * .25) / current_discount
        teams[away_id][game] = (-1.0 - away['is_home'] * .25) / current_discount
        result[game] = points

    return pd.DataFrame(teams)


def _build_power(games, outcomes, coerce_fn, acc=0.0001, alpha=1.0, snap=True):
    """ Birbiri ile alakali bir kume mac uzerinden bir guc modeli
        hazirlar (tum bu maclar ayni turnuvadan, musabakadan olmalidir mesela).
        Bu maclar ve onlarin sonucunu alarak bu fonksiyon bir lojistik
        regresyon modeli kurar, ve bu model tum takimlarin birbirine
        olan izafi bir siralamasini hesaplar. Geriye takim id'si ve
        o takimin 0 ve 1 arasindaki bir guc indisini dondurur. Eger
        snap degiskeni True ise, indisler ceyreklere bolunur. Bu faydali
        cunku siralama tahmini oldukca kabaca yapilan bir tahmin ve
        tek bir numaradan elde edilecek bir tur "asiri spesifiklik" 
        bizi yaniltabilirdi. 
    """
    outcomes = pd.Series([coerce_fn(val) for val in outcomes])
    games.to_csv('/tmp/games.csv',index=None)
    outcomes.to_csv('/tmp/outcomes.csv',index=None)
    model = world_cup.build_model_logistic(outcomes, games, 
        acc=acc, alpha=alpha)

    #print model.summary()
    params = np.exp(model.params)
    del params['intercept']
    params = params[params != 1.0]
    max_param = params.max()
    min_param = params.min()
    param_range = max_param - min_param
    if len(params) == 0 or param_range < 0.0001:
        return None
    
    params = params.sub(min_param)
    params = params.div(param_range)
    qqs = np.percentile(params, [20, 40, 60, 80])
    def _snap(val): 
        """ Snaps a value to a quartile. """
        for idx in xrange(len(qqs)):
            if (qqs[idx] > val):
                return idx * 0.25
        return 1.0
      
    if snap:
        # Snap power data to rough quartiles.
        return params.apply(_snap).to_dict()
    else:
        return params.to_dict()


def _get_power_map(competition, competition_data, col, coerce_fn):
    """
       Verili bir musabakadaki maclar ve sonuclarini iceren hedef kolonlarini
       kullanarak tum bu takimlarin guc siralamasini hesapla. Uyum biraz
       gevsek olacaktir, evet, bu sebeple uydurma islemini farkli
       regularizasyon ve alpha parametreleri ile birkac kez denememiz
       gerekebilir, ki boylece uydurmanin yakinsamasini (converge)
       elde edebilmis oluruz. Geriye takim id ve guc siralamsini dondurur.
    """
    acc = 0.000001
    alpha = 0.5
    while True:
        if alpha < 0.1:
            print "Skipping power ranking for competition %s column %s" % (
                competition, col)
            return {}
        try:
            games = _build_team_matrix(competition_data, col)
            outcomes = games[col]
            del games[col]
            competition_power = _build_power(games, outcomes, coerce_fn, acc,
                                             alpha, snap=False)
            if not competition_power:
                alpha /= 2
                print 'Reducing alpha for %s to %f due lack of range' % (
                    competition, alpha)
            else:
                return competition_power
        except LinAlgError, err:
            alpha /= 2  
            print 'Reducing alpha for %s to %f due to error %s' % (
                competition, alpha, err)


def add_power(data, power_train_data, cols):
    """
        Dataframe'e bazi guc kolonlari ekliyor. Egitim power_train_data
        verisini musabakalara boler (cunku bu parcalar birbirinden farkli
        guc istatistigine sahip olacaktir; mesela EPL takimlari MLS takimlari
        ile normal lig maclarinda oynamazlar), hangi takimin daha fazla ya da
        az guclu olup olmadigini bu maclar bazinda anlamak faydali olmazdi.

        cols icinde bir kolon ismi olacak, ki bu kolon tahmin etmek
        icin kullanilacak, bir fonksiyon ki onceden bahsedilen kolondaki
        farki irdelemekle gorevli, ve sonucun yazilacagi hedef kolon.

        Geriye bir dataframe dondurur, bu dataframe 'data' parametresiyle
        ayni boyutlardadir, sadece ekl olarak guc istatistigi eklemis olacaktir.
    """   
    data = data.copy()
    competitions = data['competitionid'].unique()
    for (col, coerce_fn, final_name) in cols:
        power = {}
        for competition in competitions:
            competition_data = power_train_data[
                power_train_data['competitionid'] == competition]
            power.update(
                _get_power_map(competition, competition_data, col, coerce_fn))

        names = {}
        power_col = pd.Series(np.zeros(len(data)), data.index)
        for index in xrange(len(data)):
            teamid = str(data.iloc[index]['teamid'])
            names[data.iloc[index]['team_name']] = power.get(teamid, 0.5)
            power_col.iloc[index] = power.get(teamid, 0.5)
        print ['%s: %0.03f' % (x[0], x[1])
               for x in sorted(names.items(), key=(lambda x: x[1]))]
        data['power_%s' % (final_name)] = power_col
    return data
