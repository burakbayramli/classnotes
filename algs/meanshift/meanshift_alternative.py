from numpy import *
from numpy import linalg as la

def mean_shift(dataPts, bandWidth):
    dataPts = asarray( dataPts )
    bandWidth = float( bandWidth )
    plotFlag = False    
    
    numDim, numPts = dataPts.shape
    numClust        = 0
    bandSq          = bandWidth**2
    initPtInds      = arange( numPts )
    #biggest size in each dimension 
    maxPos          = dataPts.max(0)
    #smallest size in each dimension                       
    minPos          = dataPts.min(0)
    #bounding box size
    boundBox        = maxPos-minPos
    #indicator of size of data space        
    sizeSpace       = la.norm(boundBox)
    #when mean has converged
    stopThresh      = 1e-3*bandWidth
    #center of clust
    clustCent       = []
    #track if a points been seen already
    beenVisitedFlag = zeros( numPts, dtype = uint8 )
    #number of points to possibly use as initilization points
    numInitPts      = numPts
    #used to resolve conflicts on cluster membership
    clusterVotes    = []
    
    while numInitPts:
        print "numInitPts",numInitPts
        rand = random.rand()
        #pick a random seed point
        tempInd         = int(floor( (numInitPts-1e-6)*rand ))
        #use this point as start of mean
        stInd           = initPtInds[ tempInd ]               
        # intilize mean to this points location
        myMean          = dataPts[ :, stInd ]                 
        # points that will get added to this cluster                 
        myMembers       = []                                   
        #used to resolve conflicts on cluster membership
        thisClusterVotes    = zeros( numPts, dtype = uint16 )  
        
        while True:   
            #dist squared from mean to all points still active            
            sqDistToAll = ( (myMean[:,newaxis] - dataPts )**2 ).sum(0)
            #points within bandWidth
            inInds      = where(sqDistToAll < bandSq)
            #add a vote for all the in points belonging to this cluster
            thisClusterVotes[ inInds ] = thisClusterVotes[ inInds ]+1 
            
            #save the old mean                        
            myOldMean   = myMean
            #compute the new mean
            myMean      = mean( dataPts[ :, inInds[0] ], 1 )
            #add any point within bandWidth to the cluster
            myMembers.extend( inInds[0] )                   
            #mark that these points have been visited
            beenVisitedFlag[myMembers] = 1                  
                        
            if la.norm(myMean-myOldMean) < stopThresh:                
                #check for merge posibilities
                mergeWith = None
                for cN in xrange( numClust ):
                    #distance from possible new clust max to old clust max
                    distToOther = la.norm( myMean - clustCent[ cN ] )    
                    #if its within bandwidth/2 merge new and old
                    if distToOther < bandWidth/2:                    
                        mergeWith = cN
                        break

                # something to merge                                
                if mergeWith is not None:
                    #record the max as the mean of the two merged (I know biased twoards new ones)
                    clustCent[ mergeWith ] = 0.5*( myMean + clustCent[ mergeWith ] )
                    #add these votes to the merged cluster
                    clusterVotes[ mergeWith ]    += thisClusterVotes   
                else:
                    #increment clusters
                    numClust                    = numClust+1
                    #record the mean  
                    clustCent.append( myMean )
                    clusterVotes.append( thisClusterVotes )
    
                break
        
        initPtInds      = where(beenVisitedFlag == 0)[0]
        numInitPts      = len(initPtInds)
    
    data2cluster = asarray( clusterVotes ).argmax(0)                

    return clustCent, data2cluster


def test():
    print '=== beginning test ==='
    dataPts = asarray([[1],[2],[3],[9],[9],[9],[10]]).T
    bandwidth = 2
    print 'data points:', dataPts
    print 'bandwidth:', bandwidth
    clustCent, data2cluster = mean_shift(dataPts, 2)
    print 'cluster centers:', sorted( asarray( clustCent ).squeeze().tolist() )
    print 'data2cluster:', data2cluster
    assert len( clustCent ) == 2
    assert sorted( asarray( clustCent ).squeeze().tolist() ) == [ 2., 9.25 ]
    print '=== passed test ==='

def test2():
    from pandas import *
    data = read_csv("synthetic.txt",names=['a','b'],sep="   ")
    print data.shape
    data = np.array(data)
    clustCent, data2cluster = mean_shift(asarray(data.T), 5000)

    
def main():
    test2()

if __name__ == '__main__': main()
