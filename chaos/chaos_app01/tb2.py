"""3BodyProb.py
 
This program solves the differeintial equation of the three-body
problem with the use of odeint from scipy.integrate.  The plots show
the solutions of different initial conditions.  The Hamiltonian used
for the cyclotron (none relativisic) is -->
H=1/2(p_x^2+p_y^2)-V_mu(x)+y*p_x-x*P_y (with mu=0.2 as default)

"""

__date__ = "$Date: 4/01/2012 $"
 
from numpy import exp,array, arange, sqrt, linspace, sin, pi, cos, arctan2
from scipy.integrate import odeint
from pylab import * # plot, axis, show, subplot, xlabel, ylabel, label, legend
 
"""
Globally define t_0,t_1,t_range!!!
@param t_0: starting time of integration
@param t_1: end time of integration
"""
t_0=0
t_1=10
t_range=10000
 
def func(init,t):
    """
    function returning [q'_x,q'_y,p'_x,p'_y] which are calculated through the given hamiltonian of the !!!none!!! relativistic Cylotron
    H=1/2*(p_x^2+p_y^2)+x*p_y-y*p_x+alpha*x*cos(omwga*t)
    @param init initial condition for [q_x,q_y,p_x,p_y]; has to be a list of four real numbers
    """
    global mu
    qx=init[0]
    qy=init[1]
    px=init[2]
    py=init[3]
    """if(mu==1.0):
        return[px+qy,
            py-qx,
            py-(qx)/((qx**2.+qy**2.)**1.5),
            -px-qy*(1.0/(((qx)**2.+qy**2.)**1.5))]"""
 
    return [px+qy,
            py-qx,
            py-(qx+mu)*(1.-mu)/(((qx+mu)**2.+qy**2.)**1.5)-(mu*(qx-1.+mu))/(((qx-1.+mu)**2.+qy**2.)**1.5),
            -px-qy*(((1.-mu)/((qx+mu)**2.+qy**2.)**1.5)+mu/(((qx-1.+mu)**2.+qy**2.)**1.5))]
 
if __name__ == '__main__':
 
    T=linspace(t_0,t_1,t_range)
    """ #this part is not working properly, it is thought to be used to set initial conditions as well as mu through the user.
    print("initial conditions for qx,qy and px, py")
    init=input()
    print init
    print len(init)
    if(len(init)!=4):
        init=[1,0,0,0]
        print("the number of initial conditions is not correct, has to be 4; will now use")
        for i in range(len(init)):
            print init[i]
    print("give a value for mu such that 0<mu>2")
    mu=input()
    if(mu<=0 or mu>2):
        mu=0.4
        print"Value does not mach the requirements will use",mu"""
 
    #different initial conditions   
    init=[0,0,0,1]
    mu=0.2
    res1=odeint(func, init, T)
 
    init=[1,0,0,0]
    mu=0.2
    res2=odeint(func, init, T)
 
    init=[0,1,0,0]
    mu=0.2
    res3=odeint(func, init, T)
 
    init=[0,0,1,0]
    mu=0.2
    res4=odeint(func, init, T)
 
    #ploting the results
    subplot(2,2,1)
    plot(res1[:,0],res1[:,1],'r',label='init=[0,0,0,1]')
    plot(res2[:,0],res2[:,1],'y',label='init=[1,0,0,0]')
    xlabel("q_x")
    ylabel("q_y")
    legend(loc='upper right')
 
    subplot(2,2,2)
    plot(res1[:,2],res1[:,3],'r',label='init=[0,0,0,1]')
    plot(res2[:,2],res2[:,3],'y',label='init=[1,0,0,0]')
    xlabel("p_x")
    ylabel("p_y")
    #legend(loc='upper left')
 
    subplot(2,2,3)
    plot(res3[:,0],res3[:,1],'b', label='init=[0,1,0,0]')
    plot(res4[:,0],res4[:,1],'g',label='init=[0,0,1,0]')
    xlabel("q_x")
    ylabel("q_y")
    legend(loc='upper right')
 
    subplot(2,2,4)
    plot(res3[:,2],res3[:,3],'b', label='init=[0,1,0,0]')
    plot(res4[:,2],res4[:,3],'g', label='init=[0,0,1,0]')
    xlabel("p_x")
    ylabel("p_y")
    #legend(loc='upper left')
 
    show()

