function [w1, w2] = amnesic(time, t1, t2, c, m)
%[w1, w2] = amnesic(time, t1, t2, c, m)
%This function returns the amnesic averaging weights given the time.
%
%Amnesic averaging dynamically increases the learning rate in order 
%to "forget" old samples and allows plasticity of the first order
%statistics (mean).
%
%Use the returned weights as follows:
%new average = (w1 * previous average) + (w2 * current sample)
%
%
%            time - 1 - parameter(U)               1 + parameter(U)
% new avg = ------------------------ * prev avg +  ----------------- * curr 
%                    time                               time
%
%           |---------w1-----------|               |-------w2------|
%
%
% There are two defined break points - t1,t2 - which define the value of U:
% If t < t1 then, U = 0
% If t >= t1 and t < t2 then, U = c * (time-t1) / (t2-t1);
% If t >= t2 then,  U = c + ((time-t2) / m);
%
%amnesic average switch points (t1,t2)
%--at these times, the weight of the previous data will change--
%--from 0 to t1, straight average is computed
%
%c is the learning rate increase over normal average -
%if c is 2, each new sample will be weighted from 2 to 3
%times more in the second interval
%
%when time is greater than t2, 
%the parameter grows at a rate of 1/m 
%--for every m samples, the sample is weighted
%as one more over normal average
%
%so m should be based on how long your
%simulation will take or how quickly the input changes
%
%starter values:
%t1=20;
%t2=200;
%c = 1;
%m=1000;
%
%M. Luciw

if time < t1   
    U = 0;
elseif time >= t1 & time < t2
    U = (c * (time-t1)) / (t2-t1);
else 
    U = c + ((time-t2) / m);
end

w1= (time-1-U) ./ time;    
w2= (1+U)     ./ time;



