from simplegeomap.util import QuadTreeInterpolator as QTI
import requests, json, os, datetime
import simplegeomap as sm
import csv, pandas as pd, re
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def get_pd(): return pd

def get_sm(): return sm

def get_qti(): return QTI

coords = [[41.66660574372402, 27.597843572891335], \
          [41.10544172227507, 26.87900032596917], \
          [40.08181141046265, 27.043813700278797],
          [39.63294027301523, 28.244287215746702], \
          [41.06599274892219,29.66055095815541], \
          [39.34050135345396, 29.412982819576886],\
          [38.33363654892914, 27.998420567536638], \
          [38.20291238994359, 29.595007071785744], \
          [37.16486717203693, 29.193429286181665], \
          [41.12477920291042, 31.368540191220983], \
          [39.86384290403438, 31.55907189445102], \
          [38.64538008154757, 31.191561816717517],
          [37.04326030609023, 32.4259631272564], \
          [38.58854322775805, 32.44059912087414], \
          [39.94583099847231, 33.26287131444977],
          [41.37620636980841, 33.29635164278608], \
          [41.11621018103262, 35.539850699822445], \
          [39.38065573240239, 35.2927234646545],
          [37.82469944956659, 35.790272714207866], \
          [37.45498178774245, 37.705777759964526], \
          [39.38435121353959, 37.76633905444692], \
          [40.72426225990410, 37.70282797822432], \
          [40.60176394952222, 39.95220427021634], \
          [39.34987890250865, 40.452825755886174],
          [37.85921516631271, 40.517476278586855], \
          [38.03594817780898, 43.29193505198768], \
          [39.93075475946135, 43.184987971740945]]

def get_latest():
    base_url = 'http://api.openweathermap.org/data/2.5/weather?'
    params = json.loads(open(os.environ['HOME'] + "/.nomterr.conf").read())
    n = datetime.datetime.now()
    ns = n.strftime("%Y-%m-%d")
    hums = []; temps = []
    for i in range(len(coords)):
        print (i)
        payload = { 'units': 'metric', 'lat': str(coords[i][0]), 'lon': str(coords[i][1]),'appid': params['weatherapi'] }
        r = requests.get(base_url, params=payload) 
        res = [json.loads(x.decode()) for x in r.iter_lines()]
        hums.append(str(res[0]['main']['humidity']))
        temps.append(str(res[0]['main']['temp']))

    hline = ns + "," + ",".join(hums)
    tline = ns + "," + ",".join(temps)
    
    hout = open("trhumid.csv","a")
    hout.write(hline)
    hout.write("\n")
    tout = open("trtemp.csv","a")
    tout.write(tline)
    tout.write("\n")
    
    hout.close()    
    tout.close()    

def plot_latest():
    get_sm().plot_continents(40, 35, zoom=1, incolor='red', outcolor='white', fill=False)
    cs = np.array(coords)
    df = get_pd().read_csv('trhumid.csv',header=None)
    #df = get_pd().read_csv('trtemp.csv',header=None)
    df = df.tail(1)
    x = cs[:,0]
    y = cs[:,1]
    z = np.array(df[list(range(1,28))])[0]

    xi,yi = np.meshgrid(np.linspace(35,42,20),np.linspace(26,44,20))

    q = get_qti()(x,y,z)
    interp = np.vectorize(q.interpolate,otypes=[np.float64])
    zi = interp(xi, yi)
    plt.xlim(26,44)
    plt.ylim(35,42)
    plt.pcolormesh(yi,xi,zi,cmap='Blues')
    plt.show()
    
if __name__ == "__main__":
    
    #get_latest()
    plot_latest()
