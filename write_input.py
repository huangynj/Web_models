#!/usr/bin/python

#######################################################################
def write_params_in(form,dirname):
# This function writes the input file 'params.in'
# It has one input containing all the values from the html form
         
	# Extract each paramater from the input object ################

	# Program control
        restart    = 'y' if "restart" in form else 'n'
	days       = form["days"].value; 		input_error('simulation length',days,'int',1,2000)
	time_step  = form["time_step"].value; 		input_error('time step',time_step,'int',1,30)
	avg_time   = form["avg_time"].value; 		input_error('average time',avg_time,'int',1,2000)
	graph_time = form["graph_time"].value; 		input_error('graphics frequency',graph_time,'int',1,240)
     

	# Radiation parameters
	rad_type = form["rad_type"].value

	month    = form["month"].value;			input_error('month',month,'int',1,12)
	day      = form["day"].value;			input_error('day',day,'int',1,31)
	hour     = form["hour"].value;			input_error('hour',hour,'float',0,24)

	rad_freq = form["rad_freq"].value;  		input_error('radiation calling frequency',rad_freq,'int',1,24)

        rad_wv = 'n' if "rad_wv" in form else 'y'
	rad_cld  = form["rad_cld"].value;


        S0       = form["S0"].value;                	input_error('Solar Constant',S0,'float',500,2000)
	theta    = form["theta"].value;			input_error('Latitude',theta,'float',-90,90)

	# Atmospheric composition
	co2      = form["co2"].value;  			input_error('CO2 concentration',co2,'float',1,10000)
	ch4      = form["ch4"].value;  			input_error('CH4 concentration',co2,'float',0,1000)
	n2o      = form["n2o"].value;  			input_error('N2O concentration',co2,'float',0,10000)
	cfc11    = form["cfc11"].value;  		input_error('CFC11 concentration',co2,'float',0,10000)
	cfc12    = form["cfc12"].value;  		input_error('CFC12 concentration',co2,'float',0,10000)

	# parameterization
        dry_conv    = 'y' if "dry_conv"   in form else 'n'
        moist_conv  = 'y' if "moist_conv" in form else 'n'
        turb_flux   = 'y' if "turb_flux"  in form else 'n'

	# Surface parameters
	water_frac  = form["water_frac"].value;  	input_error('Fraction of surface covered by water',water_frac,'float',0,1)
        ugust       = form["ugust"].value;		input_error('Surface wind',ugust,'float',0.01,100)
        ml_depth    = form["ml_depth"].value;		input_error('Mixed layer depth',ml_depth,'float',0.05,1000)
        albedo      = form["alpha"].value;		input_error('Albedo',albedo,'float',0,1)

	surf_int    = 'n' if "surf_int"    in form else 'y'
	calc_albedo = 'y' if "calc_albedo" in form else 'n'

	# Forcing
        w_cubic     = 'y' if "w_cubic" in form else 'n'

        w_max       = form["w_max"].value;		input_error('extreme value of omega ',w_max,'float',-100,100)
        w_T         = form["w_T"].value;		input_error('period of omega forcing',w_T,'float',0,10000)
        w_top       = form["w_top"].value;		input_error('p at which omega=0 (top) ',w_top,'float',0,1000)
        w_bot       = form["w_bot"].value;		input_error('p at which omega=0 (bot)',w_bot,'float',0,1000)
        w_p         = form["w_p"].value;		input_error('p at which omega=extr. value',w_p,'float',0,1000)

	# Weak temperature gradient
        wtg         = 'y' if "wtg" in form else 'n'
        p_pbl       = form["p_pbl"].value;		input_error('p above which sounding is fixed',p_pbl,'float',0,1000)

	# Write the file ##############################################
	# Replace the given parameter with %s, and add the variable name
        # at the end of the write call
        # I have written this out explicitly which makes for long tedious
        # code, but will ease any changes later
	f = open(dirname+'/params.in', 'w')

	f.write( " Value           Units                              Parameter                        \n" )
	f.write( " -------         ------                          ---------------                     \n" )
	f.write( " Program Control                                                                     \n" )
	f.write( "    %s          (y or n)                    Restart from end of last integration?    \n" 			% restart )
	f.write( "    %s          days                        Length of integration                    \n" 			% days    )
	f.write( "    %s          minutes                     Time step                                \n"                      % time_step)
	f.write( "    %s          days                        Composite averaging time at end of integration\n" 		% avg_time)
	f.write( "    %s          hours                       Frequency of graphics output to time series   \n"			% graph_time)
	f.write( " -------------------------------                                                     \n" )
	f.write( " Radiation parameters                                                                \n" )
	f.write( "    y           (y or n)                   Interactive radiation?                   \n" )
	f.write( "    %s           (0/y/n)                   Interactive cloud?                       \n" 			% rad_cld   )
	f.write( "    %s           (y or n)                   Interactive surface temperature          \n" 			% surf_int )
	f.write( "    %s           hours                      Frequency of radiation calls (if interactive)\n"                  % rad_freq )
	f.write( "    %s         Watts/m^2                     SCON = Solar constant                    \n"			% S0       )
        f.write( "    %s        Degrees latitude              Latitude of column                       \n"                      % theta    )
	f.write( "    %s           (1-12)                      Starting month                           \n" 			% month)
	f.write( "    %s          (1-31)                      Starting day                             \n" 			% day)
	f.write( "    %s          (0.0-24.0)                  Starting hour                            \n" 			% hour)
	f.write( "    %s          0-4                         RADFLAG: flag for radiation time-dependence \n" 			% rad_type )
	f.write( "    %s           (y or n)                    Calculate surface albedo over water?     \n" 			% calc_albedo)
	f.write( "    %s                                      Ocean surface albedo (if n above)        \n" 			% albedo   )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Radiatively active gases                                                            \n" )
	f.write( "   %s           ppm                         CO2 content                              \n" 			% co2      )
	f.write( "   %s           ppm                         CH4 content                              \n" 			% ch4)
	f.write( "   %s           ppb                         N20 content                              \n" 			% n2o)
	f.write( "   %s           ppt                         CFC11 content                            \n" 			% cfc11)
	f.write( "   %s           ppt                         CFC12 content                            \n" 			% cfc12)
	f.write( "    %s          (y or n)                    Interactive water vapor?                 \n" 			% rad_wv   )
	f.write( "    1.0                                     Radiation H20 multiplier                 \n" )
	f.write( "    1.0                                     Radiation O3 multiplier                  \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Convection                                                                          \n" )
	f.write( "    %s                                       Dry adiabatic adjustment                \n" 			% dry_conv)
	f.write( "    %s                                       Moist convection                        \n" 			% moist_conv)
	f.write( " -------------------------------                                                     \n" )
	f.write( " Surface                                                                             \n" )
	f.write( "    %s                                       Turbulent fluxes                        \n" 			% turb_flux)
	f.write( "    %s                                     Fraction of surface covered by water      \n" 			% water_frac)
	f.write( "    %s          meters                      Assumed ocean mixed layer thickness      \n" 			% ml_depth)
	f.write( "    %s           m/s                        Surface wind speed                       \n" 			% ugust    )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Forcing                                                                             \n" )
	f.write( "    %s                                       cubic profile of omega?                  \n" 			% w_cubic)
	f.write( "    %s           mb/hour                    extreme value of omega                   \n" 			% w_max)
	f.write( "    %s           days                       Period of omega                          \n" 			% w_T)
	f.write( "    %s           hPa                        p at which omega=0 (top)                 \n" 			% w_top)
	f.write( "    %s           hPa                        p at which omega=0 (bottom)              \n" 			% w_bot)
	f.write( "    %s           hPa                        p at which omega=extr. value             \n" 			% w_p)
	f.write( " -------------------------------                                                     \n" )
	f.write( " Weak Temperature Gradient                                                           \n" )
	f.write( "    %s                                     apply weak-temperature-gradient approx?  \n" 			% wtg)
	f.write( "    %s          hPa                        p above which sounding fixed             \n" 			% p_pbl)
	
	f.close()
        return
#######################################################################



#######################################################################
def write_sounding_in(form,dirname):
# This function writes the input file 'sounding.in'
# It has one input containing all the values from the html form
# In future we will want to:

	# Modules
	import numpy as np

	# Extract each paramater from the input object ################
	SSTi = form["SSTi"].value;		input_error('Initial SST',SSTi,'float',-50,100)
        SSTi = float(SSTi)

	# Initial relative humidity
        RHi = 0.8
	
	# Load pressure and O3 vector from file
        p,O3 = np.loadtxt("O3.in", skiprows=3, unpack=True)
    
	# Create omega vector
        Omega = np.zeros(len(p))

	# create Qcool vector
        Rcool = np.zeros(len(p))

	# Create Temp and specific humidity using idealized profile
        T,q = calc_idealized_profile(SSTi,p[0],RHi,p)


	# Write the sounding.in file ##################################    
        # This file has to be created very carefully, as the Fortran format specifiers are primitive
	f = open(dirname+'/sounding.in', 'w')

        f.write( "  N levels =  %3i          CBMF=     0.000     SST=   %5.2f C            \n" %(len(p),SSTi) )
	f.write( "  \n" )
	f.write( "  Pressure   Temp.      Spec. humid.   O3 mr   Omega   Rad. Cooling    \n" )
	f.write( "    (mb)      (C)          (g/kg)      10**-6 (mb/hr)   (deg/day)     \n" )  
	f.write( "  -------    -----      ------------   -----   ------  ------------   \n" )
	for i in range(0,len(p)):
		f.write( "   %6.1f  %8.3f     %10.6f    %6.3f  %6.2f   %7.3f  \n" %(p[i],T[i],q[i],O3[i],Omega[i],Rcool[i]) )

	f.close()


#######################################################################
def calc_idealized_profile(T_surf,p_surf,RH,p):
# This function calculates idealized temp and humidity profile 
# for initialization of RC model. Uses a specified lapse rate
# ans surface temperature, with a specified relative humidity
# Stratosphere is isothermal at 200 K. 

	# Modules
	import numpy as np

	# Constants
	epsilon = 0.622

	# lapse-rate and scale-height
	gamma = 0.0065
	scale = 7000

	# Convert to K
	T_surf = T_surf + 273.15

	# initialize T and q vectors
	T = np.zeros(len(p))
	q = np.zeros(len(p))

	# Calculate temperature and humidity 
	T = T_surf -2.0  + gamma*scale*np.log(p/p_surf)

	e= RH*6.112*np.exp( 17.67*(T-273.15) / (T-273.15+243.5) )
        q = epsilon*e / (p - (1-epsilon)*e)

	# Dry stratosphere at 200 K
        q[T<200] = 0.0
        T[T<200] = 200

	# convert to deg C
        T = T-273.15

	# Convert to g/kg
        q = q*1000
        

	return T,q

#######################################################################
def calc_adiabat(T_surf,p_surf,RH_surf,p):
# This function calculates a moist adiabat
# from surface values of temperature pressure and humidity

	# Will include some sort of moist adiabat claculation
	return


#######################################################################
def input_error(input_var_name,input_var,input_type,minimum=float('-inf'),maximum=float('inf')):
# This function calculates a moist adiabat
# from surface values of temperature pressure and humidity

        input_types = {'float':float,'int':int,}

        try:
		var = input_types[input_type](input_var)

		if var > maximum or var < minimum:
			exit()


	except:
		print '<html><h2>Input error</h2>'
		print 'The input value for: <b>"'+input_var_name+'"</b> produced an error. <br>'
		print 'This input should be of type: <b>'+input_type+' </b> in the range <b> ['+str(minimum)+','+str(maximum)+'] </b><br>'
		print 'The value given was: <b>"'+input_var+'"</b> <br> </html>'
		exit()
	return






