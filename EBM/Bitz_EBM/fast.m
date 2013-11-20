
 % Acknowledgment: modified from seasonal energy balance model on 
 % website of Cecilia Bitz

 % Energy balance model using implicit trapezoidal 
 % timestepping for long timestep and fast evaluation

 %%%%%%%%%%%%%
 % Parameters

 % number of gridpoints
 num_grid_points = 151;

 % heat capacity (W*year/m^2/K)
 % (e.g., value of 0.5 for land and 10 for mixed-layer ocean)
 heat_capacity = 0.2; % something small to make it equilibriate quickly
 
 % time step in fraction of year
 delta_t = 1./50;
 max_timesteps = 1000; 

 if (exist('scaleQ')==0); scaleQ=1.; end
 
 % OLR constant (W/m^2)
 if ~exist('A'); A=203.3; end
 
 % OLR coefficient (W/m^2/K)
 if ~exist('B'); B=2.09; end
 
 % heat diffusion coefficient (W/m^2/K)
 if ~exist('Dmag'); Dmag = 0.44; end
 
 % cold start parameter
 Toffset=0.;
 if exist('coldstartflag') 
  if coldstartflag==1 
   Toffset = -40
  end
 end
   
 % simulate Hadley Cell with Lindzen and Farrell plan
 if ~exist('hadleyflag'); hadleyflag = 0.; end
 
 % remove albedo feedback
 if ~exist('albedoflag'); albedoflag = 0.; end
 
 %%%%%%%%%%%%%
 % Computation
 
 % set up equal-area latitude (phi) array, and x = sin(phi)
 delta_x = 2.0/num_grid_points; % grid spacing
 x = [-1.0+delta_x/2:delta_x:1.0-delta_x/2]';
 phi = asin(x)*180/pi;

 % Legendre polynomial approximation of mean annual insolation
 Q = 338.5; % W/m^2 (0.25 times the solar constant)
 S = Q*(1-0.241*(3*x.^2-1)); 
 S = scaleQ*S;
 S = S(:);
 
 % set up inital temperature distribution
 T     = 20*(1-2*x.^2); % temperature is in degrees Celsius
 T     = T(:);
 T     = T+Toffset;
 Tinit = T;
 
 % set up D(x) if simulating the Hadley Cell
 % and calculate the matrix M and inv_operator.
 if (hadleyflag)
   xmp=[-1:delta_x:1];
   D=Dmag*(1+9*exp(-(xmp/sin(25*pi/180)).^6));
   D=D(:);
 else
   D=Dmag*ones(num_grid_points+1,1);
 end

 
 % boundary conditions
 alb  = albedo(T,num_grid_points,x,albedoflag);

 % M, dT_dt_fixed, and initial dT_dt
 [inv_operator,M]=setup_matrices(delta_x,num_grid_points,D,B,heat_capacity,delta_t);
 dT_dt_fixed  = (1-alb).*S/heat_capacity-A/heat_capacity; 
 dT_dt_fixed  = dT_dt_fixed(:);
 dT_dt        = M*T+dT_dt_fixed; % note the matrix multiplication
 
 % global mean temperature
 Tglob=mean(T);
 
 % timestepping loop
 for n=1:max_timesteps

    Tglob_prev = Tglob;
     
    % calculate dT_dt_fixed for this loop.
    alb=albedo(T,num_grid_points,x,albedoflag);
    dT_dt_fixed=((1-alb).*S-A)/heat_capacity; 
    dT_dt_fixed=dT_dt_fixed(:);
 
    % calculate new T.
    T = inv_operator*(0.5*(dT_dt+dT_dt_fixed)+T/delta_t);
 
    % calculate h for next loop.
    dT_dt = M*T+dT_dt_fixed; % note the matrix multiplication
 
    % check to see if global mean temperature has converged
    Tglob=mean(T);
    Tchange = Tglob-Tglob_prev;
    if (abs(Tchange) < 1.0e-12)
     break 
    end

 end

 if (abs(Tchange) >= 1.0e-12)
  error('did not converge')
 end

 
 
 % compute meridional heat flux and its convergence
 a = 6.37e6; % Earth radius in meters
 [inv_operator,M]=setup_matrices(delta_x,num_grid_points,D,0.,1.0,delta_t);
 D_smaller_grid=0.5*(D(2:num_grid_points+1)+D(1:num_grid_points));
 % flux divergence
 divF=M*T;
 % flux 
 F=-2*pi*a^2*sqrt(1-x.^2).*D_smaller_grid.*gradient(T,delta_x);
 
 
 figure; 
 subplot(3,1,1);
 plot(phi,T,'.-','linewidth',1.5)
 ylabel('Temperature'); xlabel('latitude');
 set(gca,'position',[0.1300    0.71    0.7750    0.21]);
 title(['Global mean temperature is ',num2str(Tglob,'%7.2f')]);
 grid on;
 
 subplot(3,1,2);
 plot(phi,F*1e-15,'.-','linewidth',1.5)
 ylabel('Poleward Heat Flux (10^{15} W)'); xlabel('latitude');
 set(gca,'position',[0.1300    0.41    0.7750    0.21]);
 grid on;
 
 subplot(3,1,3);
 plot(phi,divF,'.-',phi,(1-alb).*S,'o',phi,A+B*T,'.','linewidth',1.5)
 ylabel('Energy Balance Terms (W m^{-2})'); xlabel('latitude');
 set(gca,'position',[0.1300    0.130    0.7750    0.21]);
 legend('\nabla F','SWd','LWu',4);
 grid on;
 u=axis; pos=u(3)-0.4*(u(4)-u(3));
 text(-90,pos,['D = ',num2str(Dmag,'%7.2f'),...
        ',    Q/Qo = ',num2str(scaleQ,'%7.3f'),...
        ',    A = ',num2str(A,'%7.1f'),...
        ',    B = ',num2str(B,'%7.1f'),...
        ',    Toffset = ',num2str(Toffset,'%7.1f')] );
 
