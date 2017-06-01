pkg load signal
y=loadFile('FMcapture1.dat');
y_shifted=y.*transpose(exp(-j*2*pi*0.178E6*[1:1:length(y)]/2.5E6));  
d = decimate(y_shifted,8,'fir');  
[y_FM_demodulated] = FM_IQ_Demod(d); %d is the decimated signal
plot_FFT_IQ(y_FM_demodulated,1,.05*2.5E6/8,2.5/8,0,'Spectrum of demodulated signal');
print ('out3.png')
