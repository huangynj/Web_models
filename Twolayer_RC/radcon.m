% Three-layer radiative-convective equilibrium with clouds
%
% This program solves the time-dependent problem to find steady-state solutions
%
% 's' denotes the surface, and '1', '2', and '3' denote succesively higher layers
%
% PARAMETERS
%
Te=255;			% Effective emission temperature in absence of clouds
flux=5.67e-8.*Te.^4;
%
f1=0.15;
f2=0.1;			% Fraction of clouds in each layer
f3=0.6;
%
ac1=0.8;
ac2=0.65;		% Cloud albedos in each layer
ac3=0.4;
%
ecloud1=0.9;
ecloud2=0.85;	% Cloud emissivities in each layer
ecloud3=0.8;
%
ec3=0.55;       % Background clear sky emissivity 
%
stoptime=100;	% Nondimensional time that integration stops
dt=0.001;		% Nondimensional time step
al=500;			% Convective relaxation parameter
asf=0.2;        % Asselin filter for time stepping
%
dtt=2.*dt;
clear Tgraph Time
%-------------------------------------------------------------------------------------
%
%	Calculate effective emissivity of layer 3
%
e3=(1-f3).*ec3+f3.*ecloud3;
%
ftemp=(1-ac1.*f1).*(1-ac2.*f2).*(1-ac3.*f3);
ftemp2=1-ftemp;
%
%  Initial guess
%
T31=0.75;
T21=T31+.147;
T11=T21+0.088;
Ts1=T11+0.056;
%
T31=T31.^4;
T21=T21.^4;
T11=T11.^4;
Ts1=Ts1.^4;
%
T32=T31;
T22=T21;
T12=T11;
Ts2=Ts1;
%
% Loop in time until solutions are stable
%
Tgraph=zeros(1,stoptime/dt);
Time=zeros(1,stoptime/dt);
Sprod=zeros(1,stoptime/dt);
%
for j=1:stoptime/dt
   %
   T3=T31.^0.25;
   T2=T21.^0.25;
   T1=T11.^0.25;
   Ts=Ts1.^0.25;
%
%  Re-calculate clear-sky and total emissivities taking into account 
%  variable water vapor. Added according to notes by Malte Jansen, 10/2007
%
   Tsc=Te.*Ts-273.15;
   T1c=Te.*T1-273.15;
   T2c=Te.*T2-273.15;
   T3c=Te.*T3-273.15;
   % Saturation vapor pressures in layers 1 and 2
   es1=6.112.*exp(17.67.*T1c./(243.5+T1c));
   es2=6.112.*exp(17.67.*T2c./(243.5+T2c));
   % Clear sky emissivities in layers 1 and 2
   ec1=1-(1-ec3).*exp(-0.21.*es1);
   ec2=1-(1-ec3).*exp(-0.41.*es2);
   % Effective emissivities in layers 1 and 2
   e1=(1-f1).*ec1+f1.*ecloud1;
   e2=(1-f2).*ec2+f2.*ecloud2;   
%
%  end mods
%
   Tgraph(j)=Te.*Ts;
   Time(j)=dt.*j;
%
%  Introduce temperature-dependent moist adiabatic lapse rates.  11/2007.   
%
% Saturation vapor pressure at surface
%
   ess=6.112.*exp(17.67.*Tsc./(243.5+Tsc));
   es3=6.112.*exp(17.67.*T3c./(243.5+T3c));
%   
% Saturation specific humidities
%
   qs=0.622.*ess./1000;
   q1=0.622.*es1./800;
   q2=0.622.*es2./600;
   q3=0.622.*es3./400;
%
% Moist adiabatic temperature differences across layers
%
   gam1=0.1.*(1+8.7e3.*(qs+q1)./(Te.*(Ts+T1)))./(1+1.36e7.*2.*(qs+q1)./(Te.*(Ts+T1)).^2);
   gam2=0.12.*(1+8.7e3.*(q1+q2)./(Te.*(T1+T2)))./(1+1.36e7.*2.*(q1+q2)./(Te.*(T1+T2)).^2);
   gam3=0.14.*(1+8.7e3.*(q2+q3)./(Te.*(T2+T3)))./(1+1.36e7.*2.*(q2+q3)./(Te.*(T2+T3)).^2);
%  
% Convective heat fluxes
%
   fcon1=al.*max(0,(Ts-T1-gam1));
   fcon2=al.*max(0,(T1-T2-gam2));
   fcon3=al.*max(0,(T2-T3-gam3));
%
%  Mods 10/17/2008
%
     f3=1-exp(-5.*fcon3);
     ecloud3=1-exp(-2.8.*f3);
     e3=(1-f3).*ec3+f3.*ecloud3; 
     ftemp=(1-ac1.*f1).*(1-ac2.*f2).*(1-ac3.*f3);
     ftemp2=1-ftemp;
%
% Calculate entropy production
%
   Sprod(j)=fcon1.*(1./T1-1./Ts)+fcon2.*(1./T2-1./T1)+fcon3.*(1./T3-1./T2);
%   
%
%  End mods
%
   dts=(1-ftemp2)+e1.*T12+e2.*(1-e1).*T22+e3.*(1-e2).*(1-e1).*T32-Ts1-fcon1;
   dt1=e1.*(Ts2+e2.*T22+(1-e2).*e3.*T32-2.*T11)+fcon1-fcon2;
   dt2=e2.*(e1.*T12+(1-e1).*Ts2+e3.*T32-2.*T21)+fcon2-fcon3;
   dt3=e3.*((1-e1).*(1-e2).*Ts2+(1-e2).*e1.*T12+e2.*T22-2.*T31)+fcon3;
   %
   Ts3=Ts1+dtt.*dts;
   T13=T11+dtt.*dt1;
   T23=T21+dtt.*dt2;
   T33=T31+dtt.*dt3;
%
%  Apply Asselin filter 
%
   Ts1=Ts2+asf.*(Ts1+Ts3-2.*Ts2);
   T11=T12+asf.*(T11+T13-2.*T12);
   T21=T22+asf.*(T21+T23-2.*T22);
   T31=T32+asf.*(T31+T33-2.*T32);
%
   Ts2=Ts3;
   T12=T13;
   T22=T23;
   T32=T33;
end
%
%   Plots
%
subplot(2,2,1), h=plot(Time,Tgraph);
set(gca,'fontweight','bold','fontsize',12)
xlabel('Nondimensional time','fontweight','bold')
ylabel('Surface temperature (K)','fontweight','bold')
Surface_temperature=num2str((Te.*Ts));
disp(strcat('Surface Temperature =',Surface_temperature))
z=[0 1 2 3];
tz=Te.*[Ts T1 T2 T3];
subplot(2,2,2), plot(tz,z);
set(gca,'fontweight','bold','fontsize',12)
xlabel('Temperature (K)','fontweight','bold')
ylabel('Level','fontweight','bold')
set(gca,'YTick',0:1:3)
hold on
scatter(tz,z,'filled');
hold off
zc=[0 0.5 1.5 2.5 3];
fcon=[0 fcon1 fcon2 fcon3 0];
subplot(2,2,3), plot(fcon,zc);
set(gca,'fontweight','bold','fontsize',12)
xlabel('Convective flux (fraction of net solar flux)','fontweight','bold')
ylabel('Level','fontweight','bold')
set(gca,'YTick',0:1:3)
hold on
gg=scatter(fcon,zc,'filled');
hold off
rad1=e1.*(Ts2+e2.*T22+(1-e2).*e3.*T32-2.*T11);
rad2=e2.*(e1.*T12+(1-e1).*Ts2+e3.*T32-2.*T21);
rad3=e3.*((1-e1).*(1-e2).*Ts2+(1-e2).*e1.*T12+e2.*T22-2.*T31);
cq1=fcon1-fcon2;
cq2=fcon2-fcon3;
cq3=fcon3;
fac=flux.*24.*3600./3e6;
radh=fac.*[rad1 rad2 rad3];
ch=fac.*[cq1 cq2 cq3];
zh=[1 2 3];
subplot(2,2,4),g=plot(radh,zh,'b',ch,zh,'r');
legend('Radiative','Convective',1)
set(gca,'fontweight','bold','fontsize',12)
xlabel('Heating rate (K/day)','fontweight','bold')
ylabel('Level','fontweight','bold')
set(gca,'YTick',1:1:3)



   
   



