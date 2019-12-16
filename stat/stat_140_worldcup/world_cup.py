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
    for index in xrange(0, len(col), 2):
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
    return lambda (x): int(x) == int(value)


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
    for index in xrange(len(target)):
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

    print '(%s) Lift: %0.03g Auc: %s' % (label, lift, auc_value)
    if not quiet:
        print '    Base: %0.03g Acc: %0.03g P(1|t): %0.03g P(0|f): %0.03g' % (
            baseline, accuracy, prob1_t, prob0_f)
        print '    Fp/Fn/Tp/Tn p/n/c: %d/%d/%d/%d %d/%d/%d' % (
            false_pos, false_neg, true_pos, true_neg, pos, neg, len(target))
    

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
    return data[[idx % 2 == 0 for idx in xrange(len(data))]] 
  

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
    for game in xrange(len(data)/2):
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
    for game in xrange(len(results)):
        row = results.iloc[game]
        col = 'team_name' if row['predicted'] >= 50 else 'op_team_name' 
        expected_winner.append(row[col])

    results['expected'] = pd.Series(expected_winner)

    if len(points) > 0:
        winners = []
        for game in xrange(len(results)):
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

