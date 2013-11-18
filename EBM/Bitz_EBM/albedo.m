function alb=albedo(T, num_grid_points, x, albedoflag);

% calculate albedo distribution

 alb=ones(num_grid_points,1)*0.3;
 
 if albedoflag
  % fixed albedo distribution
  k=find(abs(x)>=0.95);
 else
  % ice albedo for temperatures less than 10degC
  k=find(T<=-10);
 end
 
 alb(k)=0.6;

