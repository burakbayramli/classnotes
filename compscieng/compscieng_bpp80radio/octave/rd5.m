pkg load signal
y=loadFile('FMcapture1.dat');
size(y)
y_shifted=y.*transpose(exp(-j*2*pi*0.178E6*[1:1:length(y)]/2.5E6));  
d = decimate(y_shifted,8,'fir');  
[y_FM_demodulated] = FM_IQ_Demod(d); %d is the decimated signal
df = decimate(y_FM_demodulated,10,'fir');
sound(df,2.5E6/8/10);
