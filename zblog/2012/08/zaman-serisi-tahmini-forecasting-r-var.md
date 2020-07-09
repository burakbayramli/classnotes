# Zaman Serisi Tahmini (Forecasting), R, VAR, rpy2


Zaman Serisi Tahmini (Forecasting), R, VAR, rpy2




Zaman serisi modellemesinde VAR modelleri bugunlerde en populer olanlari. VAR, vector autoregression kelimelerinden geliyor, yani tek bir zaman serisi degil, birkac tanesine birden, ayni anda modellemeye ugrasiyoruz.

Autregression, zaman serisinin kendi kendisiyle regresyona sokulmasidir; bilindigi gibi regresyon y = f(x) modellemesinde gurultulu elde edilen y ile, x degerleri arasindaki baglantinin bulunmasina yardim eder (eger f(x) lineer ise iyi sonuclar da bulur). Tek boyutlu zaman serisi modellemesi icin soyle bir numara kullanilir, serinin kopyasi alinir, bir geri kaydirilir, x bu kaydilirilmis seri, y esas seri olur, bu ikili regresyona sokulur. Boylece zaman serisinin kendisini ve regresyon mekanizmasi kullanilarak zaman serisi tahmini yapilabilir. 

VAR ise bunu cok boyutlu yapar. Her seriyi hem kendisi, hem de diger tum serilerin p kadar gecmis degeri goz onune alinir. Oldukca guclu bir metottur.

Bu alanda unlu isimlerden Sims'i bilmek gerekir, 1980 yilinda yazdigi ve kendi alanini elestirdigi bir makalede makroekonomide yapisal modeller yerine, ciplak veriye bakmak gerektigini, ve bunu yapmak icin her zaman serilerine  tek baslarina degil tum diger serilere de baglantilarini goz onune alarak incelemek gerektigini soyler. VAR matematigi buradan cikmistir. Granger ismi de vardir, VAR modellemesi sonrasi serilerin "birbirine ne kadar etki ettigini" hesaplayan "Granger istatistigi" mesela ona aittir. 

Isin matematigine sonra daha detayli girebiliriz, simdilik kodlama acisindan ornekleri verelim. Bu alanda R kodculari cok aktif, o yuzden bir R paketi vars kullanacagiz, ve onu Python uzerinden cagiracagiz.

Diyelim ki bir predict-1.csv icinde bir ulkenin GDP ve tuketim verileri (cons) var, 1959-2009 arasi icin (bu oldukca standart bir veri seti). Once R kurulur


sudo apt-get install r-base-dev r-base python-rpy2

Sonra R'ye girilir


> install.packages("vars")

Simdi su R kodu kullanilabilir

library("vars")file = "predict-1.csv"a <- read.csv(file, header = TRUE, sep = ",", na.strings="")impute.med <- function(x) {    z <- median(x, na.rm = TRUE)    x[is.na(x)] <- z    return(x)}a2 <- sapply(a, function(x){    if(is.numeric(x) & any(is.na(x))){            impute.med(x)        } else {            x        }    })out <- VAR(a2, p = 2, type = "const")out.prd <- predict(out, n.ahead = 30, ci = 0.95)

Bunu Python'dan cagirmak icin rpy2 kullaniriz,

import os, sysimport numpy as npimport rpy2.robjectsfrom datetime import date, timedeltaf = file("predict.R")code = ''.join(f.readlines())result = rpy2.robjects.r(code)res = [['gdp','cons']]for i in range(30):    res.append([str(result[0][0][i]),str(result[0][1][i]) ] )res = np.array(res)np.savetxt('predict-2.csv',res,delimiter=",",fmt='%s')

Python isledikten sonra sonuc predict-2.csv icinde olacak. Sonuclar 2009 sonrasi 30 sene sonrasi icin gdp ve tuketim rakamlarini tahmin edecek.

Eger pur Python kullanmak isteseydik, scikits statsmodels adinda bir paketi de kullanabilirdik. Bu durumda hic R kodlamasi olmayacak, kurmak icin

https://github.com/statsmodels/statsmodels

Bu kod

import osimport numpy as npimport statsmodels.api as smfrom statsmodels.tsa.api import VAR  def pad(data):    bad_indexes = np.isnan(data)    good_indexes = np.logical_not(bad_indexes)    good_data = data[good_indexes]    interpolated = np.interp(bad_indexes.nonzero()[0], good_indexes.nonzero()[0], good_data)    data[bad_indexes] = interpolated    return datadata = np.genfromtxt("predict-1.csv", skip_header=1, delimiter=',')data = np.apply_along_axis(pad, 0, data)model = VAR(data)res = model.fit(2)f = res.forecast(data[-2:], 30)np.savetxt('predict-3.csv',f,delimiter=",",fmt='%s')

Ustteki kod sonuclar predict-3.csv icine yazar.

VAR ile zaman serisi tahminlerinde onemli bazi konular incelenen verinin (zaman serisinin) duragan (stationary), ve beraber entegre (co-integrated) olup olmadigidir -- bu durumlarda bazi ek numaralar kullanmak gerekebilir, mesela duragan bir veri seti yoksa serinin farklarini kullanmak gibi.. 

Ana veri

gdp,cons2710.349,1707.42778.801,1733.72775.488,1751.82785.204,1753.72847.699,1770.52834.390,1792.92839.022,1785.82802.616,1788.22819.264,1787.72872.005,1814.32918.419,1823.12977.830,1859.63031.241,1879.43064.709,1902.53093.047,1917.93100.563,1945.13141.087,1958.23180.447,1976.93240.332,2003.83264.967,2020.63338.246,2060.53376.587,2096.73422.469,2135.23431.957,2141.23516.251,2188.83563.960,2213.03636.285,2251.03724.014,2314.33815.423,2348.53828.124,2354.53853.301,2381.53884.520,2391.43918.740,2405.33919.556,2438.13950.826,2450.63980.970,2465.74063.013,2524.64131.998,2563.34160.267,2611.54178.293,2623.54244.100,2652.94256.460,2669.84283.378,2682.74263.261,2704.14256.573,2720.74264.289,2733.24302.259,2757.14256.637,2749.64374.016,2802.24398.829,2827.94433.943,2850.44446.264,2897.84525.769,2936.54633.101,2992.64677.503,3038.84754.546,3110.14876.166,3167.04932.571,3165.44906.252,3176.74953.050,3167.44909.617,3139.74922.188,3150.64873.520,3163.64854.340,3117.34795.295,3143.44831.942,3195.84913.328,3241.44977.511,3275.75090.663,3341.25128.947,3371.85154.072,3407.55191.499,3451.85251.762,3491.35356.131,3510.65451.921,3544.15450.793,3597.55469.405,3618.55684.569,3695.95740.300,3711.45816.222,3741.35825.949,3760.25831.418,3758.05873.335,3794.95889.495,3805.05908.467,3798.45787.373,3712.25776.617,3752.05883.460,3802.06005.717,3822.85957.795,3822.86030.184,3838.35955.062,3809.35857.333,3833.95889.074,3847.75866.370,3877.25871.001,3947.95944.020,3986.66077.619,4065.76197.468,4137.66325.574,4203.26448.264,4239.26559.594,4299.96623.343,4333.06677.264,4390.16740.275,4464.66797.344,4505.26903.523,4590.86955.918,4600.97022.757,4639.37050.969,4688.77118.950,4770.77153.359,4799.47193.019,4792.17269.510,4856.37332.558,4910.47458.022,4922.27496.600,5004.47592.881,5040.87632.082,5080.67733.991,5140.47806.603,5159.37865.016,5182.47927.393,5236.17944.697,5261.78027.693,5303.38059.598,5320.88059.476,5341.07988.864,5299.57950.164,5284.48003.822,5324.78037.538,5345.08069.046,5342.68157.616,5434.58244.294,5466.78329.361,5527.18417.016,5594.68432.485,5617.28486.435,5671.18531.108,5732.78643.769,5783.78727.919,5848.18847.303,5891.58904.289,5938.79003.180,5997.39025.267,6004.39044.668,6053.59120.684,6107.69184.275,6150.69247.188,6206.99407.052,6277.19488.879,6314.69592.458,6366.19666.235,6430.29809.551,6456.29932.672,6566.010008.874,6641.110103.425,6707.210194.277,6822.610328.787,6913.110507.575,7019.110601.179,7088.310684.049,7199.910819.914,7286.411014.254,7389.211043.044,7501.311258.454,7571.811267.867,7645.911334.544,7713.511297.171,7744.311371.251,7773.511340.075,7807.711380.128,7930.011477.868,7957.311538.770,7997.811596.430,8052.011598.824,8080.611645.819,8122.311738.706,8197.811935.461,8312.112042.817,8358.012127.623,8437.612213.818,8483.212303.533,8555.812410.282,8654.212534.113,8719.012587.535,8802.912683.153,8865.612748.699,8888.512915.938,8986.612962.462,9035.012965.916,9090.713060.679,9181.613099.901,9265.113203.977,9291.513321.109,9335.613391.249,9363.613366.865,9349.613415.266,9351.013324.600,9267.713141.920,9195.312925.410,9209.212901.504,9189.012990.341,9256.0







