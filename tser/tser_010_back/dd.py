import numpy as np

def calculateMaxDD(cumret):
    highwatermark=np.zeros(len(cumret))
    drawdown=np.zeros(len(cumret))
    drawdownduration=np.zeros(len(cumret))
    for t in range(1,len(cumret)):
        highwatermark[t]=np.max([highwatermark[t-1], cumret[t]])
        drawdown[t]=(1+cumret[t])/(1+highwatermark[t])-1
        if (drawdown[t]==0):
            drawdownduration[t]=0
        else:
            drawdownduration[t]=drawdownduration[t-1]+1
    return np.min(drawdown), np.max(drawdownduration)
