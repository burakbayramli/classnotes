import numpy as np

SHR_CONST_TKFRZ = 273.15

def QSat_2(T_k, p_t):

    lambd_a = 3.504    	# Inverse of Heat Capacity
    alpha = 17.67 	    # Constant to calculate vapour pressure
    beta = 243.5		# Constant to calculate vapour pressure
    epsilon = 0.6220	# Conversion between pressure/mixing ratio
    es_C = 6.112		# Vapour Pressure at Freezing STD (mb)
    vkp = 0.2854		# Heat Capacity
    y0 = 3036		    # constant
    y1 = 1.78		    # constant
    y2 = 0.448		    # constant
    Cf = SHR_CONST_TKFRZ	# Freezing Temp (K)
    refpres = 1000	    # Reference Pressure (mb)

    p_tmb = p_t*0.01
    tcfbdiff = T_k - Cf + beta
    es_mb = es_C * np.exp(alpha*(T_k - Cf)/(tcfbdiff))
    dlnes_mbdT = alpha * beta/((tcfbdiff)*(tcfbdiff))
    pminuse = p_tmb - es_mb
    de_mbdT = es_mb * dlnes_mbdT
    d2e_mbdT2 = dlnes_mbdT * (de_mbdT - 2*es_mb/(tcfbdiff))


    ndimpress = (p_tmb/refpres)**vkp
    p0ndplam = refpres * ndimpress**lambd_a
    rs = epsilon * es_mb/(p0ndplam - es_mb + np.spacing(1)) #eps)
    prersdt = epsilon * p_tmb/((pminuse)*(pminuse))
    rsdT = prersdt * de_mbdT
    d2rsdT2 = prersdt * (d2e_mbdT2 -de_mbdT*de_mbdT*(2/(pminuse)))

    rsy2rs2 = rs + y2*rs*rs
    oty2rs = 1 + 2.0*y2*rs
    y0tky1 = y0/T_k - y1
    goftk = y0tky1 * (rs + y2 * rs * rs)
    gdT = - y0 * (rsy2rs2)/(T_k*T_k) + (y0tky1)*(oty2rs)*rsdT
    d2gdT2 = 2.0*y0*rsy2rs2/(T_k*T_k*T_k) - 2.0*y0*rsy2rs2*(oty2rs)*rsdT + \
        y0tky1*2.0*y2*rsdT*rsdT + y0tky1*oty2rs*d2rsdT2

    foftk = ((Cf/T_k)**lambd_a)*(np.abs(1 - es_mb/p0ndplam))**(vkp*lambd_a)* \
        np.exp(-lambd_a*goftk)
    fdT = -lambd_a*(1.0/T_k + vkp*de_mbdT/pminuse + gdT)
    d2fdT2 = lambd_a*(1.0/(T_k*T_k) - vkp*de_mbdT*de_mbdT/(pminuse*pminuse) - \
        vkp*d2e_mbdT2/pminuse - d2gdT2)

    rs[rs>1]=np.nan
    rs[rs<0]=np.nan

    return es_mb,rs,de_mbdT,dlnes_mbdT,rsdT,foftk,fdT

def WetBulbImp(TemperatureC,Pressure,Humidity):
    # https://github.com/smartlixx/WetBulb/blob/master/WetBulb.py
    TemperatureK = TemperatureC + SHR_CONST_TKFRZ

    constA = 2675 	 # Constant used for extreme cold temparatures (K)
    grms = 1000 	 # Gram per Kilogram (g/kg)
    p0 = 1000   	 # surface pressure (mb)

    kappad = 0.2854	 # Heat Capacity

    C = SHR_CONST_TKFRZ		# Freezing Temperature
    pmb = Pressure*0.01   	# pa to mb
    T1 = TemperatureK		# Use holder for T

    es_mb,rs = QSat_2(TemperatureK, Pressure)[0:2] # first two returned values

    relhum = Humidity                # relative humidity (%)
    qin = rs * relhum * 0.01         # specific humidity
    vapemb = es_mb * relhum * 0.01   # vapor pressure (mb) 

    mixr = qin * grms          # change specific humidity to mixing ratio (g/kg)

    pnd = (pmb/p0)**(kappad)
    D = 1.0/(0.1859*pmb/p0 + 0.6512)
    k1 = -38.5*pnd*pnd + 137.81*pnd - 53.737
    k2 = -4.392*pnd*pnd + 56.831*pnd - 0.384

    tl = (1.0/((1.0/((T1 - 55))) - (np.log(relhum/100.0)/2840.0))) + 55.0

    theta_dl = T1*((p0/(pmb-vapemb))**kappad) * ((T1/tl)**(mixr*0.00028))

    epott = theta_dl * np.exp(((3.036/tl)-0.00178)*mixr*(1 + 0.000448*mixr))
    Teq = epott*pnd			 # Equivalent Temperature at pressure
    X = (C/Teq)**3.504

    invalid = (Teq > 600) + (Teq < 200)
    hot = (Teq > 355.15)
    cold = ((X>=1) * (X<=D))
    X[invalid==1] = np.nan 
    Teq[invalid==1] = np.nan

    es_mb_teq,rs_teq,de_mbdTeq, dlnes_mbdTeq, rsdTeq, foftk_teq, fdTeq = QSat_2(Teq, Pressure)
    wb_temp = Teq - C - ((constA*rs_teq)/(1 + (constA*rs_teq*dlnes_mbdTeq)))
    sub=np.where(X<=D)
    wb_temp[sub] = (k1[sub] - 1.21 * cold[sub] - 1.45 * hot[sub] - (k2[sub] - 1.21 * cold[sub]) * X[sub] + (0.58 / X[sub]) * hot[sub])
    wb_temp[invalid==1]=np.nan

    maxiter = 3
    iter = 0
    delta = 1e6*np.ones_like(wb_temp)

    while (np.max(delta)>0.01) and (iter<=maxiter):
        es_mb_wb_temp,rs_wb_temp,de_mbdwb_temp, dlnes_mbdwb_temp, rsdwb_temp, foftk_wb_temp, fdwb_temp = QSat_2(wb_temp + C, Pressure)
        delta = (foftk_wb_temp - X)/fdwb_temp  #float((foftk_wb_temp - X)/fdwb_temp)
        delta = np.where(delta<10., delta, 10.) #min(10,delta)
        delta = np.where(delta>-10., delta, -10.) #max(-10,delta)
        wb_temp = wb_temp - delta
        wb_temp[invalid==1] = np.nan
        Twb = wb_temp
        iter = iter+1

    convergence = 0.00001
    maxiter = 20000

    es_mb_wb_temp,rs_wb_temp,de_mbdwb_temp, dlnes_mbdwb_temp, rsdwb_temp, foftk_wb_temp, fdwb_temp = QSat_2(wb_temp + C, Pressure)
    delta = (foftk_wb_temp - X)/fdwb_temp  
    subdo = np.where(np.abs(delta)>convergence) 

    iter = 0
    while (len(subdo)>0) and (iter<=maxiter):
        iter = iter + 1

        wb_temp[subdo] = wb_temp[subdo] - 0.1*delta[subdo]

        es_mb_wb_temp,rs_wb_temp,de_mbdwb_temp, dlnes_mbdwb_temp, rsdwb_temp, foftk_wb_temp, fdwb_temp = QSat_2(wb_temp[subdo]+C, Pressure[subdo])
        delta = 0 * wb_temp
        delta[subdo] = (foftk_wb_temp - X[subdo])/fdwb_temp 
        subdo = np.where(np.abs(delta)>convergence)
    #end

    Twb = wb_temp
    if any(map(len,subdo)):
        print(len(subdo))
        Twb[subdo] = TemperatureK[subdo]-C
        for www in subdo[0]:
            print('WARNING-Wet_Bulb failed to converge. Setting to T: WB, P, T, RH, Delta: %0.2f, %0.2f, %0.1f, %0.2g, %0.1f'%(Twb[www], Pressure[www], \
                TemperatureK[www], relhum[www], delta[www]))

    return Twb,Teq,epott

def wet_bulb(temp, pres, hum):
    tempC = np.array([temp])
    Pres  = np.array([pres])
    relHum = np.array([hum])
    Twb,Teq,epott = WetBulbImp(tempC,Pres,relHum)
    return Twb[0]

if __name__ == "__main__":
    print (wet_bulb(46, 1009*100, 50.))
