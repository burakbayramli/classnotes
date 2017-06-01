function y = loadFile(filename)
%  y = loadFile(filename)
%
% reads  complex samples from the rtlsdr file
%

fid = fopen(filename,'rb');
y = fread(fid,'uint8=>double');

y = y-127.5;
y = y(1:2:end) + i*y(2:2:end);
