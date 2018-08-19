% spMillikan.m

% 171004
% Ian Cooper  School of Phyiscs  University of Sydney
% http://www.physics.usyd.edu.au/teach_res/mp/mscripts/

% Calculations for Millikan's Oil Drop Experiment
%  Using actual data where the oil drop is held srtationary
% S.I. units used for all physical quantities


clear all
close all
clc

% DATA =================================================================
   m = 5.7e-16;
   d = 4e-3;
   g = 9.81;
   e = 1.602e-19;

   c = 1:30;
   
V = [-30 28.8 -28.4 30.6 -136.2 -134.3 82.2 28.7 -39.9 54.3 -126.3 -83.9 ...
    -44.6 -65.5 -139.1 -64.5 -28.8 -30.7 26.1 -140.8 -31.5 -66.8 41.5 ...
    -34.8 -44.3 -143.6 77.2 -39.9 -57.9 42.3];

   q = (m*g*d)./V;


% GRAPHICS ==============================================================
figure(1)
   set(gcf,'units','normalized','position',[0.01 0.2 0.3 0.2]);
   fs = 14;
   xP = abs(q)*1e19;
   yP = -0.5.*ones(1,length(q));
   hPlot = plot(xP,yP,'o');
   set(hPlot,'MarkerFaceColor','b');
   set(gca,'yLim',[-1 1]);
   hold on
   
   xP = 1.602 .* [1 2 3 4 5 6];
   yP = 0.5 .* ones(1,length(xP));
   hPlot = plot(xP,yP,'or');
   set(hPlot,'MarkerFaceColor','r');
   
   xlabel('| q | x10^{-19}  [C]','fontsize',fs);
   set(gca,'fontsize',fs);
   set(gca,'YTickLabel',[]);
   grid on


figure(2)
   set(gcf,'units','normalized','position',[0.01 0.5 0.3 0.2]);
   fs = 14;
   xP = abs(q)/e;
   yP = zeros(1,length(q));
   hPlot = plot(xP,yP,'o');
     
   
   set(hPlot,'MarkerFaceColor','b');
   
   hold on
   
   xP = [1 2 3 4 5 6];
   yP = 0.5 .* ones(1,length(xP));
   hPlot = plot(xP,yP,'or');
   set(hPlot,'MarkerFaceColor','r');
   set(gca,'yLim',[-1 1]);
   xlabel('| q | / e  ','fontsize',fs);
   set(gca,'fontsize',fs);
   set(gca,'YTickLabel',[]);
  grid on

  