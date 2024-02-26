function plot_FFT_IQ(x,n0,nf,fs,f0,title_of_plot)

%
%  plot_FFT_IQ(x,n0,nf)
%
%  Plots the FFT of sampled IQ data
%
%                x  -- input signal
%                fs -- sampling frequency [MHz] 
%                f0 -- center frequency [MHz]
%                n0 -- first sample (start time = n0/fs)
%                nf -- block size for transform (signal duration = nf/fs)
%                title_of_plot--title of plot (string) (optional)
%
%-This extracts a segment of x starting at n0, of length nf, and plots the FFT.
%


x_segment=x(n0:(n0+nf-1)); %extracts a small segment of data from signal

p=fftshift(fft(x_segment)); %find FFT
z = 20*log10(abs(p)/max(abs(p))); %normalize

Low_freq=(f0-fs/2); %lowest frequency to plot
High_freq=(f0+fs/2); %highest frequency to plot

N=length(z);
freq=[0:1:N-1]*(fs)/N+Low_freq;

plot(freq,z);
axis tight
xlabel('Freqency [MHz]','FontSize', 14)
ylabel('Relative amplitude [dB down from max]','FontSize', 14)
grid on
set(gcf,'color','white');

if nargin==6
    title(title_of_plot,'FontSize', 14)
else
    title({'Spectrum',['Center frequency = ' num2str(f0) ' MHz'] },'FontSize', 14)
end
title({'Spectrum',['Center frequency = ' num2str(f0) ' MHz'] },'FontSize', 14)

%Add vertical line
y1=get(gca,'ylim');
hold on;
plot([f0 f0],y1,'r-','linewidth',2);
hold off;