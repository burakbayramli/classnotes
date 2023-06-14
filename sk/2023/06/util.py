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
          [40.081811410462656, 27.043813700278797],
          [39.63294027301523, 28.244287215746702], \
          [41.06599274892219,29.66055095815541], \
          [39.340501353453966, 29.412982819576886],\
          [38.33363654892914, 27.998420567536638], \
          [38.20291238994359, 29.595007071785744], \
          [37.164867172036935, 29.193429286181665], \
          [41.12477920291042, 31.368540191220983], \
          [39.86384290403438, 31.55907189445102], \
          [38.64538008154757, 31.191561816717517],
          [37.04326030609023, 32.4259631272564], \
          [38.58854322775805, 32.44059912087414], \
          [39.94583099847231, 33.26287131444977],
          [41.376206369808415, 33.29635164278608], \
          [41.11621018103262, 35.539850699822445], \
          [39.380655732402396, 35.2927234646545],
          [37.82469944956659, 35.790272714207866], \
          [37.454981787742454, 37.705777759964526], \
          [39.38435121353959, 37.76633905444692], \
          [40.7242622599041, 37.70282797822432], \
          [40.60176394952222, 39.95220427021634], \
          [39.34987890250865, 40.452825755886174],
          [37.859215166312715, 40.517476278586855], \
          [38.03594817780898, 43.29193505198768], \
          [39.93075475946135, 43.184987971740945]]


def hadisdh_process():
    fin = open("/tmp/HadISDH.landq.4.4.0.2021f_FLATgridHOM5by5_anoms9120_actuals.dat")
    rd = csv.reader(fin)
    fout = open("/tmp/q.csv","w")
    for i in range(47*12):
        print ('i',i)
        date_line = next(rd)
        date_line = date_line[0].split(" ")
        dt = date_line[1] + "-" + date_line[0]
        dt = pd.to_datetime(dt)
        for j in range(36):
            print ('j',j)
            line = next(rd)
            line = line[0]
            line = re.split('\s*',line)
            line = line[1:]
            line = [str(dt.year), str(dt.month)] + line
            res = ";".join(line)
            res = res.replace("-9999.99","")
            fout.write(res)
            fout.write("\n")
            fout.flush()
        fout.flush()
        
def get_humidity():
    base_url = 'http://api.openweathermap.org/data/2.5/weather?'
    params = json.loads(open(os.environ['HOME'] + "/.nomterr.conf").read())
    n = datetime.datetime.now()
    ns = n.strftime("%Y-%m-%d")
    hums = []
    for i in range(len(coords)):
        print (i)
        payload = { 'lat': str(coords[i][0]), 'lon': str(coords[i][1]),'appid': params['weatherapi'] }
        r = requests.get(base_url, params=payload) 
        res = [json.loads(x.decode()) for x in r.iter_lines()]
        hums.append(str(res[0]['main']['humidity']))

    line = ns + "," + ",".join(hums) 
    fout = open("trall.csv","a")
    fout.write(line)
    fout.write("\n")
    fout.close()    

def plot_latest():
    get_sm().plot_continents(40, 35, zoom=1, incolor='red', outcolor='white', fill=False)
    cs = np.array(coords)
    df = get_pd().read_csv('trall.csv',header=None)
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
    
    #get_humidity()
    plot_latest()
