function [y_FM_demodulated] = FM_IQ_Demod(y)
%This function demodualtes an FM signal. It is assumed that the FM signal
%is complex (i.e. an IQ signal) centered at DC and occupies less than 90%
%of total bandwidth. 


b = firls(30,[0 .9],[0 1],'differentiator'); %design differentiater 
d=y./abs(y);%normalize the amplitude (i.e. remove amplitude variations) 
rd=real(d); %real part of normalized siganl. 
id=imag(d); %imaginary part of normalized signal.  
y_FM_demodulated=(rd.*conv(id,b,'same')-id.*conv(rd,b,'same'))./(rd.^2+id.^2); %demodulate! 

end
