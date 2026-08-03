import datetime
import numpy as np
import pandas as pd
import pandas_datareader.data as web

def fetch_and_prepare_quarterly_data(start_year=1990):
    today = datetime.datetime.now()
    start = datetime.datetime(start_year, 1, 1)
    end = datetime.datetime(today.year, today.month, today.day)
    
    cols = [
        "ARGBCAGDPBP6",       # Balance of Payments (BOP % GDP)
        "CRDQARBPUBIS",       # Domestic Credit
        "MKTGDPARA646NWDB",   # Nominal GDP
        "RBARBIS",            # Real Effective Exchange Rate (XCH)
        "GGNLBAARA188N",      # Government Net Borrowing/Lending (% GDP)
        "MYAGM2ARM189N",      # Money Supply M2
        "SLUEM1524ZSARG"      # Unemployment Rate
    ]
    
    print("Fetching FRED macroeconomic data...")
    df = web.DataReader(cols, 'fred', start, end)
    df.columns = ["bop", "domcred", "gdp", "xch", "govdebt", "m2", "unemploy"]
    df = df[df.index <= '2025-12-31'].copy()

    # Load External Annual Interest Rates & forward fill
    print("Loading interest rate dataset (arg_real_ir.csv)...")
    df_ir = pd.read_csv('arg_real_ir.csv')
    df['real_ir_anchor'] = np.nan
    for _, row in df_ir.iterrows():
        yr = int(row['year'])
        anchor_date = f"{yr}-07-01"
        if anchor_date in df.index:
            df.loc[anchor_date, 'real_ir_anchor'] = row['real_ir']

    df['real_ir'] = df['real_ir_anchor'].ffill().bfill()
    #df = df.interpolate(method='polynomial', order=4)
    df = df.interpolate(method='cubic', order=4)

    # Resample to Quarterly Frequency
    df_q = df.resample('Q').mean()

    # Load Annual Inflation and linearly interpolate across quarterly index
    print("Loading arg_inf.csv and linearly interpolating quarterly inflation...")
    df_inf = pd.read_csv('arg_inf.csv', header=None, names=['year', 'inf_rate'])
    df_inf['date'] = pd.to_datetime(df_inf['year'].astype(str) + '-12-31')
    df_inf = df_inf.set_index('date')
    
    df_q['inf_rate'] = df_inf['inf_rate'].reindex(df_q.index).interpolate(method='linear').bfill().ffill()

    # Quarterly Transformations (% change & first differences)
    print("Calculating quarterly transformations...")
    df_q['m2_diff']  = np.log(df_q['m2'] / df_q['m2'].shift(1)) * 100.0
    df_q['xch_diff'] = -np.log(df_q['xch'] / df_q['xch'].shift(1)) * 100.0  # Depreciation rate
    df_q['bop_diff'] = df_q['bop'].diff()
    df_q['ir_diff']  = df_q['real_ir'].diff()
    df_q['inf_diff'] = df_q['inf_rate']

    # Clean NAs
    export_cols = ['bop_diff', 'xch_diff', 'ir_diff', 'inf_diff', 'm2_diff']
    df_clean = df_q[export_cols].dropna().copy()
    return df_clean

if __name__ == "__main__":
    df_clean = fetch_and_prepare_quarterly_data()
    df_clean.to_csv('arg_quarterly_final.csv')
    print("Quarterly dataset exported to 'arg_quarterly_final.csv'.")
