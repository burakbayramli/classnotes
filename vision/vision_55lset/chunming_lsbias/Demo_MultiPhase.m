
% This code demonstrates the three-phase formulation of the level set evolution (LSE) 
% and bias field estimation proposed in the following paper:
%      C. Li, R. Huang, Z. Ding, C. Gatenby, D. N. Metaxas, and J. C. Gore, 
%      "A Level Set Method for Image Segmentation in the Presence of Intensity
%      Inhomogeneities with Application to MRI", IEEE Trans. Image Processing, 2011
%
% Note: 
%    This code implements the three-phase formulation of the model in the above paper.
%    The three-phase formulation is used to segment an image into three regions. 
%    The code for four-phase or other multi-phase formulation will be released later
%    in the website below.
%
%    All rights researved by Chunming Li, who formulated the model, designed and 
%    implemented the algorithm in the above paper.
%
% E-mail: lchunming@gmail.com
% URL: http://www.engr.uconn.edu/~cmli/
% Copyright (c) by Chunming Li
% Author: Chunming Li


close all;clear all;


Img=imread('myBrain_axial.bmp');  

Iter_outer = 100;
Iter_inner = 10;
sigma = 4;  % scale parameter
timestep = .1;
mu = 0.1/timestep;
A=255;
nu = 0.001*A^2;  % weight of length term
c0 = 1;
epsilon = 1;

Img = double(Img(:,:,1));
Img=normalize01(Img)*A;  % rescale the image intensity to the interval [0,A]
Mask=(Img>5);


[nrow,ncol] = size(Img);

numframe=0;

figure;
imagesc(Img,[0 255]);colormap(gray);hold on; axis off;axis equal;

%%% initialization of bias field and level set function
b=ones(size(Img));
initialLSF(:,:,1) = randn(size(Img));  % randomly initialize the level set functions
initialLSF(:,:,2) = randn(size(Img));  % randomly initialize the level set functions
initialLSF(:,:,1)= Mask;   % remove the background outside the mask
u = sign(initialLSF);

[c,h] = contour(u(:,:,1),[0 0],'r');
[c,h] = contour(u(:,:,2),[0 0],'b');

hold off

Ksigma=fspecial('gaussian',round(2*sigma)*2+1,sigma); % Gaussian kernel
% disk_radius = 7; 
% Ksigma=fspecial('disk',disk_radius); % an alternative kernel as a truncated uniform function
KONE=conv2(ones(size(Img)),Ksigma,'same');


pause(0.1)


totaltime =0
for n = 1:Iter_outer
   
    t0=cputime;
    [u, b, C]=  lse_bfe_3Phase(u,Img,b,Ksigma,KONE, nu,timestep,mu,epsilon,Iter_inner);
    t1=cputime;
    totaltime = totaltime + t1-t0;
    
    if(mod(n,3) == 0)
        pause(0.01);
        imagesc(Img,[0 255]);colormap(gray);hold on; axis off;axis equal;
        [c,h] = contour(u(:,:,1),[0 0],'r');
        [c,h] = contour(u(:,:,2),[0 0],'b');
        iterNum=[num2str(n), ' iterations'];
        title(iterNum);
        hold off;
            
    end
    
end
totaltime

H1 =  Heaviside(u(:,:,1),epsilon );
H2 =  Heaviside(u(:,:,2),epsilon );
M1=H1.*H2;
M2=H1.*(1-H2);
M3=(1-H1);

Img_seg=C(1)*M1+C(2)*M2+C(3)*M3;  % three regions are labeled with C1, C2, C3
figure;imagesc(Img_seg); axis off; axis equal;title('Segmented regions');
colormap(gray);

figure;
imagesc(Img,[0 255]);colormap(gray);hold on; axis off;axis equal;
[c,h] = contour(u(:,:,1),[0 0],'r','LineWidth',1);
[c,h] = contour(u(:,:,2),[0 0],'b','LineWidth',1); 

figure; imagesc(Img, [0,255]);colormap(gray);hold on; axis off; axis equal;
title('Original image');
 
img_corrected = normalize01(Mask.*Img./(b+(b==0)))*255;
figure,imagesc(img_corrected, [0,255]);colormap(gray);hold on; axis off; axis equal;
title('Bias corrected image');

figure;
imshow(uint8(Mask.*normalize01(b)*200),[0 255]);colormap(gray);hold on; axis off; axis equal
title('Estimated bias field (on the mask)');

figure;
title('Histogram');
subplot(1,2,1)
[N,X]=hist(Img(:),30); plot(X,N,'b');title('Histogram of original image');% text(50,2000,'Histogram of original image','FontSize',[10],'Color', 'k');
subplot(1,2,2)
[N,X]=hist(img_corrected(:),30); plot(X,N,'b'); title('Histogram of bias corrected image');%text(50,2000,'Histogram of bias corrected image','FontSize',[10],'Color', 'k');

