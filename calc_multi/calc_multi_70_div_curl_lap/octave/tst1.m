% taken from Introduction to MATLAB ACM11 Spring 2015, Chern book
close all
clear
clc
M = imread('cameraman.gif');
U = double(M)/255;
U_noisy = add_noise(U,0.3);

[m,n] = size( U_noisy );
isNoise = (U_noisy == 0) | (U_noisy == 1);
indNoise = find( isNoise ); % list of linear indices for noisy pixels
indClean = setdiff( 1 : m*n , indNoise );

% build Laplacian
L = Laplacian(m,n);
L_NN = L(indNoise,indNoise);
L_NC = L(indNoise,indClean);
U_noisy1 = U_noisy(:); % column-vector-vesion of U noisy
rhs = -L_NC * U_noisy1(indClean);
U_denoise1 = U_noisy1;
U_denoise1(indNoise) = L_NN\rhs;
U_denoise = reshape( U_denoise1, m, n );

figure
%imshow([U_noisy,U_denoise])
imwrite (U_noisy, "/tmp/out-noisy.gif")
imwrite (U_denoise, "/tmp/out-denoised.gif")

