import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import util as u, pandas as pd

pd.set_option('display.max_columns', None)

year = 2010
gold = u.get_yahoo_ticker(year, "GC=F").iloc[:, 0]
nominal_ir = u.get_fred(year, "FEDFUNDS").iloc[:, 0]
cpi = u.get_fred(year, "CPIAUCSL").iloc[:, 0]
inflation_yoy = cpi.pct_change(12) * 100
calculated_real_ir = nominal_ir - inflation_yoy
df = pd.DataFrame(
    {
        "Gold_Price": gold,
        "Calculated_Real_IR": calculated_real_ir,
    }
)
df = df.ffill().dropna()
df.to_csv("gold_ir.csv")
