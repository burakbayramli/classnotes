% spMillikan1.m

% 171003
% Ian Cooper  School of Phyiscs  University of Sydney
% http://www.physics.usyd.edu.au/teach_res/mp/mscripts/

% Calculations for Millikan's Oil Drop Experiment
%  from experimental data
% S.I. units used for all physical quantities

clear all
close all
clc

% KNOWN PARAMETERS ======================================================
  % separation distance between capacitor plates  [m]
    d = 8.00e-3; 
  % acceleration due to gravity  [m/s^2]  
    g = 9.81; 
  % oil drop displacement for terminal velocity  [m]  
    s = 8.30e-4;        
  % density of air  [kg.m^3]
    rho_A = 1.225;
  % corrected viscoisty of air  [N.s/m]  
    eta = 1.60e-5;      
  % density of oil  [kg.m^3] 
    rho = 839;          
  % elementary charge  [C]
    e = 1.602e-19; 

    
% MEASUREMENTS ==========================================================
  % voltage between plates of capacitro  [V]
    V = 968;            
  % time intervals for falling oil drop:  E = 0   [s]
    tD = [15.2 15.0 15.1 15.0 14.9 15.1 15.1 15.0 15.2 15.2];   
  % time intervals for rising oil drop: E <> 0   [s]
    tU = [6.4  6.3  6.1 24.4 24.2 3.7 3.6 1.8 2.0 1.9];         

    
% CALCULATIONS ==========================================================
  % uniform electric field between plates of capacitor [V/m]
    E = V / d;        
  % mean time interval for falling oil drop  [s]
    tD_avg = mean(tD);  
  % terminal velocity of falling oil drop  [m/s]
    vD = s / tD_avg;       
  % radius and volume of oil drop  [m] [m^3]
    R = sqrt(9*eta*vD / (2*g*(rho - rho_A)));   
    Vol = (4/3)*pi*R^3;           
  % mass of oil drop  [kg]
    m = Vol * rho;    
  % terminal velocity rising oil drops  [m/s]
    vU = s ./ tU;          
  % gravitational force  [N]
    FG = m*g;  
  % bouyancy force  
    FB = -Vol*rho_A*g; 
  % resisitive force
    FR = (6*pi*eta*R).*vU;
  
  % charge  [C]
    q = (FG + FB + FR) ./ E;
  
  % Average charge per group
    q_avg(1) = mean(q(4:5));
    q_avg(2) = mean(q(1:3));
    q_avg(3) = mean(q(6:7));
    q_avg(4) = mean(q(8:10));
    
 %  Averages for elementary charge for each group   
    eN(1) = q_avg(1)/1;
    eN(2) = q_avg(2)/2;
    eN(3) = q_avg(3)/3;
    eN(4) = q_avg(4)/5;

% Estimate for eleementary charge
    e_avg = mean(eN);

    e_avg
   
    
 % GRAPHICS =============================================================   
  figure(1)
   set(gcf,'units','normalized','position',[0.01 0.5 0.3 0.15]);
   fs = 14;
   xP = q;
   yP = zeros(1,length(xP));
   hPlot = plot(xP,yP,'+b');
   set(hPlot,'MarkerSize',10);
   grid on
   set(gca,'YTickLabel',[]);
   xlabel('q  ','fontsize',fs);
   set(gca,'fontsize',fs);
   
figure(2)
   set(gcf,'units','normalized','position',[0.32 0.5 0.3 0.15]);
   fs = 14;
   xP = q;
   yP = zeros(1,length(xP));
   hPlot = plot(xP,yP,'+b');
   set(hPlot,'MarkerSize',10);
   hold on
   xP = q_avg;
   yP = zeros(1,length(xP));
   hPlot = plot(xP,yP,'or');
   set(hPlot,'MarkerSize',5);
   set(hPlot,'MarkerFaceColor','r');
   grid on
   set(gca,'YTickLabel',[]);
   xlabel('q  ','fontsize',fs);
   set(gca,'fontsize',fs);

   figure(3)
   set(gcf,'units','normalized','position',[0.65 0.5 0.3 0.15]);
   fs = 14;
   xP = q./e;
   yP = zeros(1,length(xP));
   hPlot = plot(xP,yP,'+b');
   set(hPlot,'MarkerSize',10);
   hold on
   xP = [1 2 3 5];
   yP = zeros(1,length(xP));
   hPlot = plot(xP,yP,'or');
   set(hPlot,'MarkerSize',5); 
   set(hPlot,'MarkerFaceColor','r');
   
   
   grid on
   set(gca,'YTickLabel',[]);
   xlabel('q/e  ','fontsize',fs);
   set(gca,'fontsize',fs);
