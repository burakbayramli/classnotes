pkg load signal
y=loadFile('FMcapture1.dat');
y_shifted=y.*transpose(exp(-j*2*pi*0.178E6*[1:1:length(y)]/2.5E6));  
plot_FFT_IQ(y_shifted,1,.002*2.5E6,2.5,101.1,'Spectrum of shifted signal');
print ('out2.png');

