pkg load signal
y=loadFile('FMcapture1.dat');
y_shifted=y.*transpose(exp(-j*2*pi*0.178E6*[1:1:length(y)]/2.5E6));  
d = decimate(y_shifted,8,'fir');  
plot_FFT_IQ(d,1,.002*2.5E6/8,2.5/8,100.122,'Spectrum of decimated signal'); 
print ('out3.png');

