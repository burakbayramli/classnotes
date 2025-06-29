import numpy as np, math
import matplotlib.pyplot as plt

import world_cup
import features
import match_stats
import pandas as pd

history_size = 3

game_summaries = features.get_game_summaries()
data = features.get_features(history_size)

club_data = data[data['competitionid'] != 4]
# Show the features latest game in competition id 4, which is the world cup.
print (data[data['competitionid'] == 4].iloc[0])

import pandas as pd
print (pd.crosstab(
    club_data['goals'], 
    club_data.replace(
        {'points': {
            0: 'lose', 1: 'tie', 3: 'win'}})['points']))

import world_cup

import match_stats
pd.set_option('display.width', 80)

# Don't train on games that ended in a draw, since they have less signal.
train = club_data.loc[club_data['points'] != 1] 
# train = club_data

(model, test) = world_cup.train_model(
     train, match_stats.get_non_feature_columns())
print ("Rsquared: %0.03g" % model.prsquared)

def print_params(model, limit=None):    
    params = model.params.copy()
    params.sort_values(ascending=False)
    del params['intercept']
    
    if not limit:
        limit = len(params)

    print("Pozitif ozellikler")
    params.sort_values(ascending=False)
    print (np.exp(params[[param > 0.001 for param in params]]).sub(1)[:limit])

    print("\nAtilan ozellikler")
    print (params[[param  == 0.0 for param in params]][:limit])

    print("\nNegatif ozellikler")
    params.sort_values(ascending=True)
    print (np.exp(params[[param < -0.001 for param in params]]).sub(1)[:limit])

print_params(model, 10)

results = world_cup.predict_model(model, test, match_stats.get_non_feature_columns())

predictions = world_cup.extract_predictions(results.copy(), results['predicted'])

print ('Dogru tahminler:')
print (predictions[(predictions['predicted'] > 50) & (predictions['points'] == 3)][:5])

print ('Yanlis tahminler:')
print (predictions[(predictions['predicted'] > 50) & (predictions['points'] < 3)][:5])

baseline = (sum([yval == 3 for yval in club_data['points']]) 
            * 1.0 / len(club_data))
y = [yval == 3 for yval in test['points']]
world_cup.validate(3, y, results['predicted'], baseline, 
                   compute_auc=True)
plt.savefig('stat_worldcup_01.png')

import power
def points_to_sgn(p):
  if p > 0.1: return 1.0
  elif p < -0.1: return -1.0
  else: return 0.0
power_cols = [
  ('points', points_to_sgn, 'points'),
]

power_data = power.add_power(club_data, game_summaries, power_cols)
power_train = power_data.loc[power_data['points'] != 1] 

# power_train = power_data
(power_model, power_test) = world_cup.train_model(
    power_train, match_stats.get_non_feature_columns())
print ("\nRsquared: %0.03g, Power Coef %0.03g" % (
    power_model.prsquared, 
    math.exp(power_model.params['power_points'])))

power_results = world_cup.predict_model(power_model, power_test, 
    match_stats.get_non_feature_columns())
power_y = [yval == 3 for yval in power_test['points']]
world_cup.validate(3, power_y, power_results['predicted'], baseline, 
                   compute_auc=True, quiet=False)

print_params(power_model, 8)

plt.plot([0, 1], [0, 1], '--', color=(0.6, 0.6, 0.6), label='Luck')
# Add the old model to the graph
world_cup.validate('old', y, results['predicted'], baseline, 
                   compute_auc=True, quiet=True)
plt.legend(loc="lower right")
plt.savefig('world_cup_02.png')

import world_cup
import features
wc_data = world_cup.prepare_data(features.get_wc_features(history_size))
wc_labeled = world_cup.prepare_data(features.get_features(history_size))
wc_labeled = wc_labeled[wc_labeled['competitionid'] == 4]
wc_power_train = game_summaries[game_summaries['competitionid'] == 4].copy()

# Guc verisiyle egitirken, kupa birden fazla maca yayildigi icin 
# is_home'u 0.5'e set et. Yoksa 2010'daki kupa maclarina baktigimizda
# Guney Afrika yerine Brezilya'nin hala ev sahibi olduugnu zannedebilirdik.

wc_power_train['is_home'] = 0.5
wc_power_data = power.add_power(wc_data, wc_power_train, power_cols)

wc_results = world_cup.predict_model(power_model, wc_power_data, 
    match_stats.get_non_feature_columns())


["Australia: 0.000", "Serbia: 0.016", "USA: 0.017", "Cameroon: 0.035",
"Iran: 0.081", "Croatia: 0.180", "Nigeria: 0.204", "Côte d'Ivoire:0.244", 
"Costa Rica: 0.254", "Algeria: 0.267", "Paraguay: 0.277",
"Honduras: 0.279", "Slovakia: 0.281", "Greece: 0.284", "Switzerland:0.291",
 "Ecuador: 0.342", "Uruguay: 0.367", "Sweden: 0.386",
 "Japan: 0.406", "Mexico: 0.409", "Chile: 0.413", "Colombia: 0.438",
 "England: 0.460", "Belgium: 0.467", "Ukraine: 0.470", "Portugal: 0.487",
 "Ghana: 0.519", "South Korea: 0.532", "France: 0.648", "Spain: 0.736",
 "Argentina: 0.793", "Italy: 0.798", "Brazil: 0.898", "Netherlands: 0.918",
 "Germany: 1.000"] 

games = pd.read_csv('/tmp/games.csv')
outcomes = pd.read_csv('/tmp/outcomes.csv')

print ('mac', games[100:101])
print ('sonuc', outcomes[100:101])

raw_games = pd.read_csv('results-20140714-124014.csv')
tmp = raw_games[(raw_games['teamid'] == 369) & (raw_games['op_teamid'] == 494)]
tmp = tmp[['teamid','team_name','op_team_name','is_home','points']]
print (tmp)

pd.set_option('display.max_rows', 5000)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

wc_with_points = wc_power_data.copy()
wc_with_points.index = pd.Index(
    zip(wc_with_points['matchid'], wc_with_points['teamid']))
wc_labeled.index = pd.Index(
    zip(wc_labeled['matchid'], wc_labeled['teamid']))
wc_with_points['points'] = wc_labeled['points']

wc_pred = world_cup.extract_predictions(wc_with_points, 
                                        wc_results['predicted'])

# Reverse our predictions to show the most recent first.
wc_pred.reindex(index=wc_pred.index[::-1])
# Show our predictions for the games that have already happenned.
print (wc_pred)

"""
    Futbol maclarinin sonucunu lojistik regresyon kullanarak tahmin eder.
"""

import random
import math

import numpy as np
random.seed(987654321)
np.random.seed(987654321)
import pandas as pd
import pylab as pl
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
import statsmodels.api as sm


def _drop_unbalanced_matches(data):
    """
    Mac sirasinda her iki takim hakkinda elimide veri olmadigi icin,
    tarihi veride bir macta oynayan iki takimin ikisininde hakkinda veri
    yoksa o iki takimi egitim verisinden at. Bu durum eger bir takim hakkinda
    10 mactan daha az veri var ise ortaya cikabilir.
    """
    keep = []
    index = 0
    data = data.dropna()
    while index < len(data) - 1:
        skipped = False
        for col in data:
            if isinstance(col, float) and math.isnan(col):
                keep.append(False)
                index += 1
                skipped = True
             
        if skipped:
            pass
        elif data.iloc[index]['matchid'] == data.iloc[index+1]['matchid']:
            keep.append(True)
            keep.append(True)
            index += 2
        else:
            keep.append(False)
            index += 1
    while len(keep) < len(data):
        keep.append(False)
    results = data[keep]
    if len(results) % 2 != 0:
        raise Exception('Unexpected results')
    return results


def _swap_pairwise(col):
    """ 0 ile 1, 2 ile 2, vs.. seklinda satir degis tokusu yap """
    col = pd.np.array(col)
    for index in range(0, len(col), 2):
        val = col[index]
        col[index] = col[index + 1]
        col[index+1] = val
    return col


def _splice(data):
    """ Bir maci temsil eden iki satiri tek bir satir olacak sekilde birlestir """
    data = data.copy()
    opp = data.copy()
    opp_cols = ['opp_%s' % (col,) for col in opp.columns]
    opp.columns = opp_cols
    opp = opp.apply(_swap_pairwise)
    del opp['opp_is_home']
    
    return data.join(opp)


def split(data, test_proportion=0.4):
    """
    Bir dataframe'i egitim set'i ve test set'i olarak ikiyi ayirir.
    Dikkatli olmak lazim cunku dataframe icinde bir macin satirlari ardi
    ardina gelmeli, bu sebeple bir macin tum verileri ya tamamen
    egitim ya da tamamen test set'inde olmali.
    """
    
    train_vec = []
    if len(data) % 2 != 0:
        raise Exception('Unexpected data length')
    while len(train_vec) < len(data):
        rnd = random.random()
        train_vec.append(rnd > test_proportion) 
        train_vec.append(rnd > test_proportion)
            
    test_vec = [not val for val in train_vec]
    train = data[train_vec]
    test = data[test_vec]
    if len(train) % 2 != 0:
        raise Exception('Unexpected train length')
    if len(test) % 2 != 0:
        raise Exception('Unexpected test length')
    return (train, test)


def _extract_target(data, target_col):
    """
    Egitimde hedef olarak kullanilan kolonu dataframe'den cikart.
    Geriye verilen dataframe eksi o kolonu dondur.
    """
    target = data[target_col]
    train_df = data.copy()
    del train_df[target_col]
    return target, train_df


def _check_eq(value): 
    """
    Geriye oyle bir fonksiyon dondur ki gecilen tamsayi degerine (value) uyup
    uymadigini kontrol etsin. Dikkat, geriye gecilen degeri kontrol eden
    _fonksiyon_ donduruyoruz, o degeri burada kontrol etmiyoruz.
    """
    return lambda x: int(x) == int(value)


L1_ALPHA = 16.0
def build_model_logistic(target, data, acc=0.00000001, alpha=L1_ALPHA):
    """
    Bir lojistik regresyon modelini egitir. target parametresi
    hedef, yani etiket. target vektorunun satir sayisi data icindeki
    egitim verisinin satir sayisi kadar olmali.
    """
    data = data.copy()
    data['intercept'] = 1.0
    target = np.array(target)
    if np.any(target==-1): target[target==-1] = 0
    logit = sm.Logit(target, data, disp=False)
    return logit.fit_regularized(maxiter=1024, alpha=alpha, acc=acc, disp=False)


def validate(label, target, predictions, baseline=0.5, compute_auc=False,
             quiet=True):
    """
    Ikisel tahminleri kontrol et, karisiklik matrisi (confusion matrix) ve
    AUC hesaplar.

    Verili bir tahmin vektoru ve gercek degerlere bakarak tahminde ne kadar
    basarili oldugumuzu hesaplar.

    Argumanlar:

    label: kontrol ettigimiz seyin etiketi
    target: gercek degerlerin vektoru
    predictions: tahmin edilen degerleri - bu bir olasilik vektoru
       olabilir, ki bu durumda onu siralayip (sort) en emin olunan
       degerleri aliriz, emin olmak bir esik degerine gore hesaplanir.
       Tabii tahmin 1 ya da 0 ise direk dogru ya da yanlis sonucuna
       varabiliriz.
    compute_auc: Eger dogru ise tahminler icin bir AUC hesaplar.
       Bu arguman dogru ise tahminlerin de bir olasilik vektoru
       olmasi gerekir. 
    """

    if len(target) != len(predictions):
        raise Exception('Length mismatch %d vs %d' % (len(target), 
                                                      len(predictions)))
    if baseline > 1.0:
        # Baseline number is expected count, not proportion. Get the proportion.
        baseline = baseline * 1.0 / len(target)

    zipped = sorted(zip(target, predictions), key=lambda tup: -tup[1])
    expect = len(target) * baseline
    
    (true_pos, true_neg, false_pos, false_neg) = (0, 0, 0, 0)
    for index in range(len(target)):
        (yval, prob) = zipped[index]
        if float(prob) == 0.0:
            predicted = False
        elif float(prob) == 1.0:
            predicted = True
        else:
            predicted = index < expect
        if predicted:
            if yval:
                true_pos += 1
            else:
                false_pos += 1 
        else:
            if yval:
                false_neg += 1
            else:
                true_neg += 1
    pos = true_pos + false_neg
    neg = true_neg + false_pos
    # P(1 | predicted(1)) and P(0 | predicted(f))
    pred_t = true_pos + false_pos
    pred_f = true_neg + false_neg
    prob1_t = true_pos * 1.0 / pred_t if pred_t > 0.0 else -1.0
    prob0_f = true_neg * 1.0 / pred_f if pred_f > 0.0 else -1.0
              
    # Lift = P(1 | t) / P(1)
    prob_1 = pos * 1.0 / (pos + neg)
    lift = prob1_t / prob_1 if prob_1 > 0 else 0.0
              
    accuracy = (true_pos + true_neg) * 1.0 / len(target)
              
    if compute_auc:
        y_bool =  [True if yval else False for (yval, _) in zipped]
        x_vec = [xval for (_, xval) in zipped]
        auc_value = roc_auc_score(y_bool, x_vec)
        fpr, tpr, _ = roc_curve(y_bool, x_vec)
        pl.plot(fpr, tpr, lw=1.5,
            label='ROC %s (area = %0.2f)' % (label, auc_value))
        pl.xlabel('False Positive Rate', fontsize=18)
        pl.ylabel('True Positive Rate', fontsize=18)
        pl.title('ROC curve', fontsize=18)
        auc_value = '%0.03g' % auc_value
    else:
        auc_value = 'NA'

    print ('(%s) Lift: %0.03g Auc: %s' % (label, lift, auc_value))
    if not quiet:
        print ('    Base: %0.03g Acc: %0.03g P(1|t): %0.03g P(0|f): %0.03g' % (
            baseline, accuracy, prob1_t, prob0_f))
        print ('    Fp/Fn/Tp/Tn p/n/c: %d/%d/%d/%d %d/%d/%d' % (
            false_pos, false_neg, true_pos, true_neg, pos, neg, len(target)))
    

def _coerce_types(vals):
    """ Bir liste icindeki tum degerlerin float (reel sayi) oldugunu kontrol et """
    return [1.0 * val for val in vals]


def _coerce(data):
    """
    Dataframe icindeki tum degerleri float'a cevir ve degerleri
    standardize et
    """
    return _standardize(data.apply(_coerce_types))


def _standardize_col(col):
    """ Tek bir kolonu standardize et (ortalamayi cikar, standart sapma
        ile bol
    """
    std = np.std(col)
    mean = np.mean(col)
    if abs(std) > 0.001:
        return col.apply(lambda val: (val - mean)/std)
    else:
        return col


def _standardize(data):
    """ Tum dataframe'i standardize et. Tum kolonlar sayisal olmali """
    return data.apply(_standardize_col)


def _clone_and_drop(data, drop_cols):
    """ Icinde belli bazi kolonlarin atildigi bir Dataframe'in kopyasini dondur """
    clone = data.copy()
    for col in drop_cols:
        if col in clone.columns:
            del clone[col]
    return clone


def _normalize(vec):
    """ Listeyi normalize et ki toplami 1 olsun """
    total = float(sum(vec))
    return [val / total for val in vec]


def _games(data):
    """ Tek sayili satirlari at. Bu fonksiyon faydali cunku bazen
        ardi ardina ayni mac hakkinda iki satira ihtiyacimiz olmuyor
        bir tanesi yeterli.
    """
    return data[[idx % 2 == 0 for idx in range(len(data))]] 
  

def _team_test_prob(target):
    """ A takiminin B takimini, ve B takiminin A takimini yenme olasiliklarini
        hesapliyoruz. Her iki yondeki olasiliklari kullanarak genel
        bir olasilik hesabi yap.
    """
    results = []
    for idx in range(len(target)/2):
        game0 = float(target.iloc[idx*2])
        game1 = float(target.iloc[idx*2+1])
        results.append(game0/(game0+game1))
    return results


def extract_predictions(data, predictions):
    """
         Uyum verileri ve tahminleri iceren Dataframe'leri birlestir.
         Geriye dondurulen DF icinde takim isimleri, tahminler, ve
         (eger mevcut ise) puan olarak sonuc. 
    """
    probs = _team_test_prob(predictions)
    teams0 = []
    teams1 = []
    points = []
    for game in range(len(data)/2):
        if data['matchid'].iloc[game*2] != data['matchid'].iloc[game*2+1]:
            raise Exception('Unexpeted match id %d vs %d', (
                               data['matchid'].iloc[game * 2],
                               data['matchid'].iloc[game * 2 + 1]))
        team0 = data['team_name'].iloc[game * 2]
        team1 = data['op_team_name'].iloc[game * 2]
        if 'points' in data.columns: 
            points.append(data['points'].iloc[game * 2])
        teams0.append(team0)
        teams1.append(team1)
    results =  pd.DataFrame(
        {'team_name': pd.Series(teams0), 
         'op_team_name': pd.Series(teams1),
         'predicted': pd.Series(probs).mul(100)},
         columns = ['team_name', 'op_team_name', 'predicted'])

    expected_winner = []
    for game in range(len(results)):
        row = results.iloc[game]
        col = 'team_name' if row['predicted'] >= 50 else 'op_team_name' 
        expected_winner.append(row[col])

    results['expected'] = pd.Series(expected_winner)

    if len(points) > 0:
        winners = []
        for game in range(len(results)):
            row = results.iloc[game]
            point = points[game]
            if point > 1.1:
                winners.append(row['team_name'])
            elif point < 0.9:
                winners.append(row['op_team_name'])
            elif point > -0.1:
                winners.append('draw')
            else:
                winners.append('NA')
        results['winner'] = pd.Series(winners)
        results['points'] = pd.Series(points)
    return results


def _check_data(data):
    """ Dataframe'i gez ve her seyin iyi durumda olup olmadigini kontrol et """
    i = 0
    if len(data) % 2 != 0:
        raise Exception('Unexpeted length')
    matches = data['matchid']
    teams = data['teamid']
    op_teams = data['op_teamid']
    while i < len(data) - 1:
        if matches.iloc[i] != matches.iloc[i + 1]:
            raise Exception('Match mismatch: %s vs %s ' % (
                            matches.iloc[i], matches.iloc[i + 1]))
        if teams.iloc[i] != op_teams.iloc[i + 1]:
            raise Exception('Team mismatch: match %s team %s vs %s' % (
                            matches.iloc[i], teams.iloc[i], 
                            op_teams.iloc[i + 1]))
        if teams.iloc[i + 1] != op_teams.iloc[i]:
            raise Exception('Team mismatch: match %s team %s vs %s' % (
                            matches.iloc[i], teams.iloc[i + 1], 
                            op_teams.iloc[i]))
        i += 2


def prepare_data(data):
    """ birbiri ile uyan ama iki takim hakkinda veri olmadigi durumda o satirlari at"""
    data = data.copy()
    data = _drop_unbalanced_matches(data)
    _check_data(data)
    return data


def train_model(data, ignore_cols):
    """
    Veri uzerinde bir lojistik regresyon modeli egitir. ignore_cols icinde
    gecilen kolonlar dikkate alinmaz. 
    """
    # Validate the data
    data = prepare_data(data)
    target_col = 'points'
    (train, test) = split(data)
    train.to_csv('/tmp/out3.csv')
    (y_train, x_train) = _extract_target(train, target_col)
    x_train2 = _splice(_coerce(_clone_and_drop(x_train, ignore_cols)))

    y_train2 = [int(yval) == 3 for yval in y_train]
    model = build_model_logistic(y_train2, x_train2, alpha=8.0)
    return (model, test)


def predict_model(model, test, ignore_cols):
    """
    Bir takimin kazanip kazanmayacaginin tahmin eden basit bir algoritma
    """
      
    x_test = _splice(_coerce(_clone_and_drop(test, ignore_cols)))
    x_test['intercept'] = 1.0
    predicted =  model.predict(x_test)
    result = test.copy()
    result['predicted'] = predicted
    return result

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

    for game in range(nrows):
        home = data.iloc[game * 2]
        away = data.iloc[game * 2 + 1]
        if home['seasonid'] != current_season:
            # Discount older seasons.
            current_season = home['seasonid']
            current_discount *= 0.6
            print ("New season %s" % (current_season,))

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
        for idx in range(len(qqs)):
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
            print ("Skipping power ranking for competition %s column %s" % (
                competition, col))
            return {}
        try:
            games = _build_team_matrix(competition_data, col)
            outcomes = games[col]
            del games[col]
            competition_power = _build_power(games, outcomes, coerce_fn, acc,
                                             alpha, snap=False)
            if not competition_power:
                alpha /= 2
                print ('Reducing alpha for %s to %f due lack of range' % (
                    competition, alpha))
            else:
                return competition_power
        except LinAlgError as err:
            alpha /= 2  
            print ('Reducing alpha for %s to %f due to error %s' % (
                competition, alpha, err))


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
        for index in range(len(data)):
            teamid = str(data.iloc[index]['teamid'])
            names[data.iloc[index]['team_name']] = power.get(teamid, 0.5)
            power_col.iloc[index] = power.get(teamid, 0.5)
        print (['%s: %0.03f' % (x[0], x[1])
               for x in sorted(names.items(), key=(lambda x: x[1]))])
        data['power_%s' % (final_name)] = power_col
    return data

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
