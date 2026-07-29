import datetime
import numpy as np
import pandas as pd
import pandas_datareader.data as web

def fetch_fred_data(start_year=1990):
    today = datetime.datetime.now()
    start = datetime.datetime(start_year, 1, 1)
    end = datetime.datetime(today.year, today.month, today.day)
    
    cols = [
        "ARGBCAGDPBP6",       # Balance of Payments (BOP % GDP)
        "ARGCPALTT01GPM",     # CPI Index
        "CRDQARBPUBIS",       # Domestic Credit
        "MKTGDPARA646NWDB",   # Nominal GDP
        "RBARBIS",            # Real Effective Exchange Rate (XCH)
        "GGNLBAARA188N",      # Government Net Borrowing/Lending (% GDP)
        "MYAGM2ARM189N",      # Money Supply M2
        "SLUEM1524ZSARG"      # Unemployment Rate
    ]
    
    df = web.DataReader(cols, 'fred', start, end)
    df.columns = ["bop", "cpi", "domcred", "gdp", "xch", "govdebt", "m2", "unemploy"]
    return df

print("Fetching FRED macroeconomic data...")
df = fetch_fred_data(1990)

# Filter out future projection ramps (cap at end of 2025)
df = df[df.index <= '2025-12-31'].copy()

# -----------------------------------------------------------------------------
# 2. Load External Datasets & Anchor Annual Series
# -----------------------------------------------------------------------------
print("Loading external annual datasets (arg_inf.csv & arg_real_ir.csv)...")

# Load Annual Inflation (arg_inf.csv)
# Assuming no header: column 0 = year, column 1 = annual_inflation
df_inf = pd.read_csv('arg_inf.csv', header=None, names=['year', 'annual_inflation'])

# Load Annual Interest Rates (arg_real_ir.csv)
df_ir = pd.read_csv('arg_real_ir.csv')  # Contains 'year' and 'real_ir'

# Initialize empty NaN columns in the monthly dataframe
df['annual_inflation_anchor'] = np.nan
df['real_ir_anchor']           = np.nan

# Place anchor points on July 1st (mid-year point) for every annual observation
for _, row in df_inf.iterrows():
    yr = int(row['year'])
    anchor_date = f"{yr}-07-01"
    if anchor_date in df.index:
        df.loc[anchor_date, 'annual_inflation_anchor'] = row['annual_inflation']

for _, row in df_ir.iterrows():
    yr = int(row['year'])
    anchor_date = f"{yr}-07-01"
    if anchor_date in df.index:
        df.loc[anchor_date, 'real_ir_anchor'] = row['real_ir']

# -----------------------------------------------------------------------------
# 3. Monthly Interpolation & Variable Construction
# -----------------------------------------------------------------------------
print("Performing smooth monthly linear interpolation...")

# Interpolate the anchored annual metrics linearly across monthly timestamps
df['inflation'] = df['annual_inflation_anchor'].interpolate(method='linear')
df['real_ir']   = df['real_ir_anchor'].interpolate(method='linear')

# Interpolate missing values in monthly/quarterly FRED series (e.g. bop, xch, govdebt)
df_interp = df.interpolate(method='linear')

# Derived economic metrics
# Exchange Rate Depreciation (log change): Positive value = Peso weakening
df_interp['xch_deprec'] = -np.log(df_interp['xch'] / df_interp['xch'].shift(1)) * 100.0

# Fiscal Deficit definition (negative net lending/borrowing balance)
df_interp['deficit'] = -df_interp['govdebt']

# GDP Growth (% YoY)
df_interp['growth'] = (df_interp['gdp'] - df_interp['gdp'].shift(12)) / df_interp['gdp'].shift(12) * 100.0

# -----------------------------------------------------------------------------
# 4. Clean & Export Final Dataset
# -----------------------------------------------------------------------------
# Drop initial NaN rows created by lag/shift operations
df_clean = df_interp.dropna(subset=['bop', 'xch_deprec', 'real_ir', 'inflation']).copy()

# Keep relevant columns for exporting
export_cols = ['bop', 'xch', 'xch_deprec', 'real_ir', 'inflation', 'deficit', 'm2', 'growth', 'unemploy']
df_clean = df_clean[export_cols]

print("\n--- Pipeline Summary ---")
print(f"Dataset start date : {df_clean.index.min().strftime('%Y-%m-%d')}")
print(f"Dataset end date   : {df_clean.index.max().strftime('%Y-%m-%d')}")
print(f"Total observations : {len(df_clean)} months")
print("\nFirst 5 rows preview:")
print(df_clean[['bop', 'xch_deprec', 'real_ir', 'inflation']].head())

# Save clean dataset for PyMC MCMC sampling
df_clean.to_csv('arg_final.csv')
