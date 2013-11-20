function [inv_operator,M]=setup_matrices(delta_x,num_grid_points,D,B,heat_capacity,delta_t);

 %set up lambda array.
 lam=(1-[-1:delta_x:1]'.^2);
 lam=D(:).*lam(:);
 
 % find an operator for the part of dT_dt that varies with temperature
 M = zeros(num_grid_points,num_grid_points);
 M(1,1) = -B - lam(2)/delta_x^2;
 M(1,2) = lam(2)/delta_x^2;
 
 M(num_grid_points,num_grid_points-1) = lam(num_grid_points)/delta_x^2;
 M(num_grid_points,num_grid_points)   = -B - lam(num_grid_points)/delta_x^2;
 
 for j=2:num_grid_points-1
   M(j,j-1) = lam(j)/delta_x^2;
   M(j,j)   = -B - (lam(j+1)+lam(j))/delta_x^2;
   M(j,j+1) = lam(j+1)/delta_x^2;
 end
 
 % include inverse factor of the heat capacity
 M = M/heat_capacity;

 % also calculate the inverse matrix operator
 operator=-0.5*M;
 for j=1:num_grid_points
   operator(j,j)=operator(j,j) + 1./delta_t;
 end
 inv_operator = inv(operator);

