import pandas as pd, folium, numpy as np, json
from pygeodesy.sphericalNvector import LatLon

m = folium.Map(location=[40,33], tiles='Stamen Terrain', zoom_start=7)
base = "/opt/Downloads/trgeo"

def average(coords):
    b = (LatLon(lat,lon) for lat,lon in coords)
    nvecs = np.array([a.toNvector() for a in b])
    mid = nvecs.mean().toLatLon()
    return mid.lat,mid.lon
        
def camp():
    df = pd.read_csv(base + '/kamp/kampyerleri.csv',sep=';')
    for index, row in df.iterrows():
        lat,lon = (float(x) for x in row['location'].split(","))
        folium.CircleMarker(location=[lon,lat],
                            tooltip=row['description'],
                            radius=6,
                            color='yellow',
                            weight=4).add_to(m)                
        
        
    df = pd.read_csv(base + '/kamp/trkamp.csv',sep=';')
    for index, row in df.iterrows():
        lat,lon = (float(x) for x in row['location'].split(","))
        folium.CircleMarker(location=[lat,lon],
                            tooltip=row['name'],
                            radius=6,
                            color='blue',
                            weight=4).add_to(m)                

def park():        
    files = ['/millipark/milli_parklar.json',
             '/millipark/tabiat_anitlari.json',
             '/millipark/ozel_cevre_koruma_alanlari.json',
             '/millipark/tabiati_koruma_alanlari.json',
             '/millipark/tabiat_parklari.json',
             '/millipark/yaban_hayati_gelistirme_sahalari.json',
             '/millipark/sulak_alanlar.json']

    for f in files:
        print (f)
        with open(base + f, encoding='utf-8') as fh:
            data = json.load(fh)
        for name in data.keys():
            data1 = data[name]
            data1 = np.array([[float(x[0]),float(x[1])] for x in data1])
            if len(data1) < 3: continue
            clat,clon = average(data1)
            desc = name + " " + f.replace("/millipark/","").replace(".json","")
            folium.CircleMarker(location=[clat,clon],
                                tooltip=desc,
                                radius=3,
                                color='green',
                                weight=4).add_to(m)                
            folium.Polygon(locations=data1,weight=2,color = 'blue').add_to(m)

camp()
park()
            
m.save(base + "/trgeo.html")
