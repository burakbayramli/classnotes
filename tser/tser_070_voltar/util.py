from scipy.optimize import minimize
import pandas as pd, random
import numpy as np, datetime
import scipy.stats

FLAG_BAD_RETURN=-99999.0
CALENDAR_DAYS_IN_YEAR = 365.25
BUSINESS_DAYS_IN_YEAR = 256.0
ROOT_BDAYS_INYEAR = BUSINESS_DAYS_IN_YEAR**.5
WEEKS_IN_YEAR = CALENDAR_DAYS_IN_YEAR / 7.0
ROOT_WEEKS_IN_YEAR = WEEKS_IN_YEAR**.5
MONTHS_IN_YEAR = 12.0
ROOT_MONTHS_IN_YEAR = MONTHS_IN_YEAR**.5
ARBITRARY_START=pd.datetime(1900,1,1)

DEFAULT_CAPITAL = 1.0
DEFAULT_ANN_RISK_TARGET = 0.16

contract_month_codes = ['F', 'G', 'H', 'J', 'K', 'M','N', 'Q', 'U', 'V', 'X', 'Z']
contract_month_dict = dict(zip(contract_month_codes,\
                           range(1,len(contract_month_codes)+1)))

def shift(lst,empty):
    res = lst[:]
    temp = res[0]
    for index in range(len(lst) - 1): res[index] = res[index + 1]         
    res[index + 1] = temp
    res[-1] = empty
    return res

def stitch_prices(dfs, price_col, dates):
    res = []
    datesr = list(reversed(dates))
    dfsr = list(reversed(dfs))    
    dfsr_pair = shift(dfsr,pd.DataFrame())
        
    for i,v in enumerate(datesr):
        tmp1=float(dfsr[i].ix[v,price_col])
        tmp2=float(dfsr_pair[i].ix[v,price_col])
        dfsr_pair[i].loc[:,price_col] = dfsr_pair[i][price_col] + tmp1-tmp2

    dates.insert(0,'1900-01-01')
    dates_end = shift(dates,'2200-01-01')
    
    for i,v in enumerate(dates):
        tmp = dfs[i][(dfs[i].index > dates[i]) & (dfs[i].index <= dates_end[i])]
        res.append(tmp.Settle)
    return pd.concat(res)

def which_contract(contract_list, cycle, offset, expday, expmon):
    assert len(contract_list) > 0
    start_date = contract_list[contract_list.keys()[0]].head(1).index[0] # first dt of first contract
    end_date = contract_list[contract_list.keys()[-1]].tail(1).index[0] # last date of last contract
    delta = end_date - start_date
    dates = []
    for i in range(delta.days + 1):
    	day = start_date + datetime.timedelta(days=i)
	if day.weekday() < 5: dates.append(day)
    df = pd.DataFrame(index=dates)    
    def closest_biz(d): # get closest biz day
    	diffs = np.abs((d - df.index).days)
    	return df.index[np.argmin(diffs)]
    cycle_d = [contract_month_dict[x] for x in cycle]
    df['effcont'] = np.nan
    for year in np.unique(df.index.year):
    	for c in cycle_d:
	    v = "%d%02d" % (year,c)
	    exp_d = datetime.datetime(year, c, expday)
            if expmon=="prev": exp_d = exp_d - datetime.timedelta(days=30)
	    df.loc[closest_biz(exp_d),'effcont'] = v
            
    df = df.fillna(method='bfill')
    df['effcont'] = df.effcont.shift(-int(offset*2/3 + 3))

    return df.fillna(method='ffill')

def create_carry(df, offset, contract_list):
    df2 = df.copy()
    df2['effcont'] = df2.effcont.astype(str)
    def offset_contract(con):
    	s = pd.to_datetime(con + "15", format='%Y%m%d')
    	ss = s + datetime.timedelta(days=30*offset)
    	return "%d%02d" % (int(ss.year), int(ss.month)) 
    df2['carrycont'] = df2.effcont.map(offset_contract)
    df2['effprice'] = df2.apply(lambda x: contract_list.get(x.effcont).s.get(x.name) if x.effcont in contract_list else np.nan,axis=1)
    df2['carryprice'] = df2.apply(lambda x: contract_list.get(x.carrycont).s.get(x.name) if x.carrycont in contract_list else np.nan,axis=1)
    return df2

def ccy_returns(price, forecast):
    base_capital = DEFAULT_CAPITAL
    daily_risk_capital = DEFAULT_CAPITAL * DEFAULT_ANN_RISK_TARGET / ROOT_BDAYS_INYEAR        
    ts_capital=pd.Series([DEFAULT_CAPITAL]*len(price), index=price.index)        
    ann_risk = ts_capital * DEFAULT_ANN_RISK_TARGET
    daily_returns_volatility = robust_vol_calc(price.diff())
    multiplier = daily_risk_capital * 1.0 * 1.0 / 10.0
    numerator = forecast *  multiplier
    positions = numerator.ffill() /  daily_returns_volatility.ffill()
    cum_trades = positions.shift(1).ffill()
    price_returns = price.diff()
    instr_ccy_returns = cum_trades.shift(1)*price_returns 
    instr_ccy_returns=instr_ccy_returns.cumsum().ffill().reindex(price.index).diff()
    return instr_ccy_returns
    
def skew(price, forecast): 
    base_capital = DEFAULT_CAPITAL
    pct = 100.0 * ccy_returns(price, forecast) / base_capital
    return scipy.stats.skew(pct[pd.isnull(pct) == False])

def sharpe(price, forecast):
    instr_ccy_returns = ccy_returns(price, forecast)
    tval,pval = scipy.stats.ttest_1samp(instr_ccy_returns.dropna(), 0)
    mean_return = instr_ccy_returns.mean() * BUSINESS_DAYS_IN_YEAR
    vol = instr_ccy_returns.std() * ROOT_BDAYS_INYEAR
    return mean_return / vol, tval, pval

def ewma(price, slow, fast):
    fast_ewma = pd.ewma(price, span=slow)
    slow_ewma = pd.ewma(price, span=fast)
    raw_ewmac = fast_ewma - slow_ewma
    vol = robust_vol_calc(price.diff())
    return raw_ewmac /  vol 

def bollinger(df,col,lev):
    signals = pd.DataFrame(index=df.index) 
    signals['signal'] = np.nan
    middle = pd.rolling_mean(df[col], 40, min_periods=1) 
    std = pd.rolling_std(df[col], 40, min_periods=1)
    df['middle'] = middle
    df['top'] = middle+2*std
    df['bottom'] = middle-2*std
    signals['signal'] = np.where(df[col] > middle+2*std, -1, np.nan) 
    signals['signal'] = np.where(df[col] < middle-2*std, 1, np.nan)
    signals['signal'] = signals['signal'].fillna(method='ffill')
    df['ret'] = df[col].pct_change() * signals['signal'].shift(1)
    ret = df.ret.dropna() * lev
    return ret

def crossover(df,col,lev):
    signals = pd.DataFrame(index=df.index) 
    signals['signal'] = 0 
    short_ma = pd.rolling_mean(df[col], 40, min_periods=1) 
    long_ma = pd.rolling_mean(df[col], 100, min_periods=1) 
    signals['signal'] = np.where(short_ma > long_ma, 1, 0) 
    df['signal'] = signals['signal'].shift(1) 
    df['ret'] = df[col].pct_change() * df['signal']
    ret = df.ret.dropna()  * lev
    return ret

def carry(daily_ann_roll, vol, diff_in_years, smooth_days=90):
    ann_stdev = vol * ROOT_BDAYS_INYEAR
    raw_carry = daily_ann_roll / ann_stdev
    smooth_carry = pd.ewma(raw_carry, smooth_days) / diff_in_years
    return smooth_carry.fillna(method='ffill')

def estimate_forecast_scalar(x, window=250000, min_periods=500):
    target_abs_forecast = 10.
    x=x.abs().iloc[:,0]
    avg_abs_value=x.mean()
    return target_abs_forecast/avg_abs_value  

def vol_equaliser(mean_list, stdev_list):
    if np.all(np.isnan(stdev_list)):
        return (([np.nan]*len(mean_list), [np.nan]*len(stdev_list)))
    avg_stdev=np.nanmean(stdev_list)

    norm_factor=[asset_stdev/avg_stdev for asset_stdev in stdev_list]
    
    norm_means=[mean_list[i]/norm_factor[i] for (i, notUsed) in enumerate(mean_list)]
    norm_stdev=[stdev_list[i]/norm_factor[i] for (i, notUsed) in enumerate(stdev_list)]
    
    return (norm_means, norm_stdev)

def apply_with_min_periods(xcol, my_func=np.nanmean, min_periods=0):
    not_nan=sum([not np.isnan(xelement) for xelement in xcol])    
    if not_nan>=min_periods:    
        return my_func(xcol)
    else:
        return np.nan

def vol_estimator(x, using_exponent=True, min_periods=20, ew_lookback=250):
    vol=x.apply(apply_with_min_periods,axis=0,min_periods=min_periods, my_func=np.nanstd) 
    stdev_list=list(vol)    
    return stdev_list

def mean_estimator(x, using_exponent=True, min_periods=20, ew_lookback=500):
    means=x.apply(apply_with_min_periods,axis=0,min_periods=min_periods, my_func=np.nanmean)
    mean_list=list(means)    
    return mean_list

def str2Bool(x):
    if type(x) is bool:
        return x
    return x.lower() in ("t", "true")

def correlation_single_period(data_for_estimate, 
                              using_exponent=True, min_periods=20, ew_lookback=250,
                              floor_at_zero=True):
    ## These may come from config as str
    using_exponent=str2Bool(using_exponent)
            
    if using_exponent:
        ## If we stack there will be duplicate dates
        ## So we massage the span so it's correct
        ## This assumes the index is at least daily and on same timestamp
        ## This is an artifact of how we prepare the data
        dindex=data_for_estimate.index
        dlenadj=float(len(dindex))/len(set(list(dindex)))
        ## Usual use for IDM, FDM calculation when whole data set is used
        corrmat=pd.ewmcorr(data_for_estimate, span=int(ew_lookback*dlenadj), min_periods=min_periods)
        
        ## only want the final one
        corrmat=corrmat.values[-1]
    else:
        ## Use normal correlation
        ## Usual use for bootstrapping when only have sub sample
        corrmat=data_for_estimate.corr(min_periods=min_periods)
        corrmat=corrmat.values

    if floor_at_zero:
        corrmat[corrmat<0]=0.0
    
    return corrmat

def fix_mus(mean_list):
    def _fixit(x):
        if np.isnan(x):
            return FLAG_BAD_RETURN
        else:
            return x    
    mean_list=[_fixit(x) for x in mean_list]    
    return mean_list

def fix_sigma(sigma):
    def _fixit(x):
        if np.isnan(x):
            return 0.0
        else:
            return x    
    sigma=[[_fixit(x) for x in sigma_row] for sigma_row in sigma]    
    sigma=np.array(sigma)    
    return sigma

def addem(weights):
    ## Used for constraints
    return 1.0 - sum(weights)

def neg_SR(weights, sigma, mus):
    ## Returns minus the Sharpe Ratio (as we're minimising)
    estreturn=(np.matrix(weights)*mus)[0,0]
    std_dev=(variance(weights,sigma)**.5)    
    return -estreturn/std_dev
    
def variance(weights, sigma):
    ## returns the variance (NOT standard deviation) given weights and sigma
    return (np.matrix(weights)*sigma*np.matrix(weights).transpose())[0,0]

def un_fix_weights(mean_list, weights):
    def _unfixit(xmean, xweight):
        if xmean==FLAG_BAD_RETURN:
            return np.nan
        else:
            return xweight
    
    fixed_weights=[_unfixit(xmean, xweight) for (xmean, xweight) in zip(mean_list, weights)]    
    return fixed_weights

    
def optimise( sigma, mean_list):
    
    ## will replace nans with big negatives
    mean_list=fix_mus(mean_list)
    
    ## replaces nans with zeros
    sigma=fix_sigma(sigma)
    
    mus=np.array(mean_list, ndmin=2).transpose()
    number_assets=sigma.shape[1]
    start_weights=[1.0/number_assets]*number_assets
    
    ## Constraints - positive weights, adding to 1.0
    bounds=[(0.0,1.0)]*number_assets
    cdict=[{'type':'eq', 'fun':addem}]
    ans=minimize(neg_SR, start_weights, (sigma, mus), method='SLSQP', bounds=bounds, constraints=cdict, tol=0.00001)

    ## anything that had a nan will now have a zero weight
    weights=ans['x']
    
    ## put back the nans
    weights=un_fix_weights(mean_list, weights)    
    return weights

def sigma_from_corr_and_std(stdev_list, corrmatrix):
    stdev=np.array(stdev_list, ndmin=2).transpose()
    sigma=stdev*corrmatrix*stdev
    return sigma
    
def markosolver(period_subset_data):
    mean_list=mean_estimator(period_subset_data)
    corrmatrix=correlation_single_period(period_subset_data)
    stdev_list=vol_estimator(period_subset_data)
    
    (mean_list, stdev_list)=vol_equaliser(mean_list, stdev_list)    
    sigma=sigma_from_corr_and_std(stdev_list, corrmatrix)    
    unclean_weights=optimise( sigma, mean_list)
    weights=unclean_weights    
    diag=dict(raw=(mean_list, stdev_list), sigma=sigma, mean_list=mean_list, 
              unclean=unclean_weights, weights=weights)    
    return (weights, diag)

def bootstrap_portfolio(subset_data, monte_runs=100, bootstrap_length=50):

    all_results=[bs_one_time(subset_data, bootstrap_length) for unused_index in range(monte_runs)]
        
    ### We can take an average here; only because our weights always add
    ### up to 1. If that isn't true then you will need to some kind
    ### of renormalisation

    weightlist=np.array([x[0] for x in all_results], ndmin=2)
    diaglist=[x[1] for x in all_results]
         
    theweights_mean=list(np.mean(weightlist, axis=0))
    
    diag=dict(bootstraps=diaglist)
    
    return (theweights_mean, diag)

def bs_one_time(subset_data, bootstrap_length):

    ## choose the data    
    bs_idx=[int(random.uniform(0,1)*len(subset_data)) for notUsed in range(bootstrap_length)]
    
    returns=subset_data.iloc[bs_idx,:] 
    
    (weights, diag)=markosolver(returns)

    return (weights, diag)

def robust_vol_calc(x, days=35, min_periods=10, vol_abs_min=0.0000000001, vol_floor=True,
                    floor_min_quant=0.05, floor_min_periods=100,
                    floor_days=500):
    """
    Robust exponential volatility calculation, assuming daily series of prices
    We apply an absolute minimum level of vol (absmin);
    and a volfloor based on lowest vol over recent history

    :param x: data
    :type x: Tx1 pd.Series

    :param days: Number of days in lookback (*default* 35)
    :type days: int

    :param min_periods: The minimum number of observations (*default* 10)
    :type min_periods: int

    :param vol_abs_min: The size of absolute minimum (*default* =0.0000000001) 0.0= not used
    :type absmin: float or None

    :param vol_floor Apply a floor to volatility (*default* True)
    :type vol_floor: bool
    :param floor_min_quant: The quantile to use for volatility floor (eg 0.05 means we use 5% vol) (*default 0.05)
    :type floor_min_quant: float
    :param floor_days: The lookback for calculating volatility floor, in days (*default* 500)
    :type floor_days: int
    :param floor_min_periods: Minimum observations for floor - until reached floor is zero (*default* 100)
    :type floor_min_periods: int

    :returns: pd.DataFrame -- volatility measure


    """

    # Standard deviation will be nan for first 10 non nan values
    vol = pd.ewmstd(x, span=days, min_periods=min_periods)

    vol[vol < vol_abs_min] = vol_abs_min

    if vol_floor:
        # Find the rolling 5% quantile point to set as a minimum
        vol_min = pd.rolling_quantile(
            vol, floor_days, floor_min_quant, floor_min_periods)
        # set this to zero for the first value then propogate forward, ensures
        # we always have a value
        vol_min.set_value(vol_min.index[0], 0.0)
        vol_min = vol_min.ffill()

        # apply the vol floor
        vol_with_min = pd.concat([vol, vol_min], axis=1)
        vol_floored = vol_with_min.max(axis=1, skipna=False)
    else:
        vol_floored = vol

    return vol_floored

def ewmac(price, Lfast, Lslow):
    price=price.resample("1B", how="last")
    fast_ewma = pd.ewma(price, span=Lfast)
    slow_ewma = pd.ewma(price, span=Lslow)
    raw_ewmac = fast_ewma - slow_ewma
    return raw_ewmac.PRICE / robust_vol_calc(price.diff()).vol
    
