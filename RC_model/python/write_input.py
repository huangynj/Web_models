#!/usr/bin/python

#######################################################################
def write_params_in(form,dirname):
# This function writes the input file 'params.in'
# It has one input containing all the values from the html form
         
	# Extract each paramater from the input object ################

	# Program control
        restart    = 'y' if "restart" in form else 'n'
	days       = form["days"].value; 	  err = input_error('simulation length',days,'float',1,5000);           
        if err['err'] == 'fatal': return err
	time_step  = form["time_step"].value; 	  err = input_error('time step',time_step,'int',1,30);
        if err['err'] == 'fatal': return err
	avg_time   = form["avg_time"].value; 	  err = input_error('average time',avg_time,'int',1,2000);
        if err['err'] == 'fatal': return err
	graph_time = form["graph_time"].value; 	  err = input_error('graphics frequency',graph_time,'int',1,240);
        if err['err'] == 'fatal': return err
     

	# Radiation parameters
	rad_type = form["rad_type"].value

	month    = form["month"].value;		  err = input_error('month',month,'int',1,12);
        if err['err'] == 'fatal': return err
	day      = form["day"].value;		  err = input_error('day',day,'int',1,31);
        if err['err'] == 'fatal': return err
	hour     = form["hour"].value;		  err = input_error('hour',hour,'float',0,24);
        if err['err'] == 'fatal': return err

	rad_freq = form["rad_freq"].value;  	  err = input_error('radiation calling frequency',rad_freq,'int',1,24);
        if err['err'] == 'fatal': return err

        rad_wv = 'n' if "rad_wv" in form else 'y'
	rad_cld  = form["rad_cld"].value;


        S0       = form["S0"].value;              err = input_error('Solar Constant',S0,'float',100,5000);
        if err['err'] == 'fatal': return err
	theta    = form["theta"].value;		  err = input_error('Latitude',theta,'float',-90,90);
        if err['err'] == 'fatal': return err

	# Atmospheric composition
	co2      = form["co2"].value;  		  err = input_error('CO2 concentration',co2,'float',1,10000);
        if err['err'] == 'fatal': return err
	ch4      = form["ch4"].value;  		  err = input_error('CH4 concentration',co2,'float',1,10000);
        if err['err'] == 'fatal': return err
	n2o      = form["n2o"].value;  		  err = input_error('N2O concentration',co2,'float',1,10000);
        if err['err'] == 'fatal': return err
	cfc11    = form["cfc11"].value;  	  err = input_error('CFC11 concentration',co2,'float',1,10000);
        if err['err'] == 'fatal': return err
	cfc12    = form["cfc12"].value;  	  err = input_error('CFC12 concentration',co2,'float',1,10000);
        if err['err'] == 'fatal': return err

	# parameterization
        dry_conv    = 'y' if "dry_conv"   in form else 'n'
        moist_conv  = 'y' if "moist_conv" in form else 'n'
        turb_flux   = 'y' if "turb_flux"  in form else 'n'

	# Surface parameters
	water_frac  = form["water_frac"].value;   err = input_error('Fraction of surface covered by water',water_frac,'float',0,1);
        if err['err'] == 'fatal': return err
        ugust       = form["ugust"].value;	  err = input_error('Surface wind',ugust,'float',0.01,100);
        if err['err'] == 'fatal': return err
        ml_depth    = form["ml_depth"].value;	  err = input_error('Mixed layer depth',ml_depth,'float',0.01,1000);
        if err['err'] == 'fatal': return err
        albedo      = form["alpha"].value;	  err =	input_error('Albedo',albedo,'float',0,1);
        if err['err'] == 'fatal': return err

	surf_int    = 'n' if "surf_int"    in form else 'y'
	calc_albedo = 'y' if "calc_albedo" in form else 'n'

	# Forcing
        w_cubic     = 'y' if "w_cubic" in form else 'n'

        w_max       = form["w_max"].value;	  err = input_error('extreme value of omega ',w_max,'float',-100,100);
        if err['err'] == 'fatal': return err
        w_T         = form["w_T"].value;	  err = input_error('period of omega forcing',w_T,'float',0,10000);
        if err['err'] == 'fatal': return err
        w_top       = form["w_top"].value;	  err = input_error('p at which omega=0 (top) ',w_top,'float',0,1000);
        if err['err'] == 'fatal': return err
        w_bot       = form["w_bot"].value;	  err = input_error('p at which omega=0 (bot)',w_bot,'float',0,1000);
        if err['err'] == 'fatal': return err
        w_p         = form["w_p"].value;	  err = input_error('p at which omega=extr. value',w_p,'float',0,1000);
        if err['err'] == 'fatal': return err

	# Weak temperature gradient
        wtg         = 'y' if "wtg" in form else 'n'
        p_pbl       = form["p_pbl"].value;		input_error('p above which sounding is fixed',p_pbl,'float',0,1000);
        if err['err'] == 'fatal': return err

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
        return err
#######################################################################



#######################################################################
def write_sounding_in(form,dirname,O3in):
# This function writes the input file 'sounding.in'
# It has one input containing all the values from the html form
# In future we will want to:

	# Modules
	import numpy as np

	# Extract each paramater from the input object ################
	SSTi = form["SSTi"].value;		err = input_error('Initial SST',SSTi,'float',-50,100)
        if err['err'] == 'fatal': return err

        SSTi = float(SSTi)

	# Initial relative humidity
        RHi = 0.8
	
	# Load pressure and O3 vector from file
        p,O3 = np.loadtxt(O3in, skiprows=3, unpack=True)
    
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
        return err

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
# Checks if the input is the right format

        input_types = {'float':float,'int':int,}
        error = {}
        try:
		var = input_types[input_type](input_var)

		if var > maximum or var < minimum:
			raise 

                error['err'] = 'none'

	except:
                error['err'] = 'fatal'
		error['var'] = input_var_name
		error['type'] = input_type
		error['value'] = input_var
		error['min'] = str(minimum)
		error['max'] = str(maximum)

	return error 






