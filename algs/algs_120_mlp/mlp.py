from numpy import *

class mlp:
    def __init__(self,inputs,targets,nhidden,beta=1,momentum=0.9):
        self.nin = shape(inputs)[1]
        self.nout = shape(targets)[1]
        self.ndata = shape(inputs)[0]
        self.nhidden = nhidden
        self.beta = beta
        self.momentum = momentum
        self.weights1 = (random.rand(self.nin+1,self.nhidden)-0.5)*2/sqrt(self.nin)
        self.weights2 = (random.rand(self.nhidden+1,self.nout)-0.5)*2 / \
                        sqrt(self.nhidden)

    def earlystopping(self,inputs,targets,valid,validtargets,eta,niterations=100):
    
        valid = concatenate((valid,-ones((shape(valid)[0],1))),axis=1)        
        old_val_error1 = 100002
        old_val_error2 = 100001
        new_val_error = 100000
        
        count = 0
        while (((old_val_error1 - new_val_error) > 0.001) or \
               ((old_val_error2 - old_val_error1)>0.001)):
            count+=1
            self.mlptrain(inputs,targets,eta,niterations)
            old_val_error2 = old_val_error1
            old_val_error1 = new_val_error
            validout = self.mlpfwd(valid)
            new_val_error = 0.5*sum((validtargets-validout)**2)
            
        print "Stopped", new_val_error,old_val_error1, old_val_error2
        return new_val_error
    	
    def mlptrain(self,inputs,targets,eta,niterations):
        inputs = concatenate((inputs,-ones((self.ndata,1))),axis=1)
        change = range(self.ndata)    
        updatew1 = zeros((shape(self.weights1)))
        updatew2 = zeros((shape(self.weights2)))
        for n in range(niterations):    
            self.outputs = self.mlpfwd(inputs)
            error = 0.5*sum((targets-self.outputs)**2)
            #if (n % 100) ==0 : print "Iteration: ",n, " Error: ",error    
            deltao = (targets-self.outputs)/self.ndata            
            deltah = self.hidden*(1.0-self.hidden)*\
                     (dot(deltao,transpose(self.weights2)))
            updatew1 = eta*(dot(transpose(inputs),deltah[:,:-1])) + \
                       self.momentum*updatew1
            updatew2 = eta*(dot(transpose(self.hidden),deltao)) + \
                       self.momentum*updatew2
            self.weights1 += updatew1
            self.weights2 += updatew2                

            random.shuffle(change)
            inputs = inputs[change,:]
            targets = targets[change,:]
            
    def mlpfwd(self,inputs):
        self.hidden = dot(inputs,self.weights1);
        self.hidden = 1.0/(1.0+exp(-self.beta*self.hidden))
        self.hidden = concatenate((self.hidden,-ones((shape(inputs)[0],1))),axis=1)
        outputs = dot(self.hidden,self.weights2);
        normalisers = sum(exp(outputs),axis=1)*ones((1,shape(outputs)[0]))
        return transpose(transpose(exp(outputs))/normalisers)

