#!/usr/bin/python

#######################################################################
def write_params_in(form):
# This function writes the input file 'params.in'
# It has one input containing all the values from the html form
# In future we will want to:
# a) add error handling so we don't try to run the model for "dog" number of days
         

	import numpy as np
 
	keys = form.keys()
	print "<p>"+str(len(keys))+"</p>"
	print keys
        #for key in keys
	#	print "<html><p>"+"bahh"+"</p></html>"       

	# Extract each paramater from the input object ################

	# Basic parameters
	days = form["days"].value
	co2  = form["co2"].value

 
	# Radiation parameters
	rad_type = form["rad_type"].value
	if rad_type == "diurnal":
		rad_int = 'y'
		perpetual = 'n'
		time_dep = 'y'
	elif rad_type == "avg":
		rad_int = 'y'
		perpetual = 'y'
		time_dep = 'n'
	elif rad_type == "fixed":
		rad_int = 'n'
		perpetual = 'y'
		time_dep = 'n'

	S0 = form["S0"].value

	theta = form["theta"].value
        costheta = str(np.cos(float(theta)))
 
        if "rad_wv" in form:
		rad_wv = 'n'
	else:
		rad_wv = 'y'
	
	if "rad_cl" in form:
		rad_cl = 'n'
	else:
		rad_cl = 'y'

        albedo = form["alpha"].value

	# Surface parameters
        ugust = form["ugust"].value

	if "surf_int" in form:
		surf_int = 'n'
	else:
		surf_int = 'y'

	# Write the file ##############################################
	# Replace the given parameter with %s, and add the variable name
        # at the end of the write call
        # I have written this out explicitly which makes for long tedious
        # code, but will ease any changes later

	f = open('./params.in', 'w')

	f.write( " Value           Units                              Parameter                        \n" )
	f.write( " -------         ------                          ---------------                     \n" )
	f.write( " Program Control                                                                     \n" )
	f.write( " ------------------------                                                            \n" )
	f.write( "     n          (y or n)                    Restart from end of last integration?    \n" )
	f.write( "    %s          days                        Length of integration                    \n" 			% days    )
	f.write( "    5.0         minutes                     Time step                                \n" )
	f.write( " ------------------------------                                                      \n" )
	f.write( " Domain specification                                                                \n" )
	f.write( " ------------------------------                                                      \n" )
	f.write( "   1.0          Thousands of kilometers     Length of domain                         \n" )
	f.write( "   0.0          Degrees latitude            Latitude of first column                 \n" )
	f.write( "   0.0          Degrees latitude            Latitude of last column                  \n" ) 
	f.write( "   0.0          Degrees longitude           Longitude of first column                \n" )
	f.write( "   0.0          Degrees longitude           Longitude of last column                 \n" )
	f.write( "   1                                        Column number of land-sea boundary (sea to left)\n" )
	f.write( "   r            (r or p)                    Rigid or periodic boundaries             \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Radiation parameters                                                                \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( "    %s           (y or n)                    Interactive radiation?                   \n" 			% rad_int  )
	f.write( "   1.0           hours                      Frequency of radiation calls (if y above)\n" )
	f.write( "    %s           (y or n)                    Interactive surface temperature?         \n" 			% surf_int )
	f.write( "    %s          (y or n)                    Interactive water vapor?                 \n" 			% rad_wv   )
	f.write( "    %s           (y or n)                    Interactive cloud?                       \n" 			% rad_cl   )
	f.write( "   %s         Watts/m^2                   SCON = Solar constant                       \n"			% S0       )
	f.write( "   %s         (0-1)                       COSZETA = Cosine of zenith angle to use if PERPETUAL=y \n" 		% costheta )
	f.write( "   %s           ppm                         CO2 content                              \n" 			% co2      )
	f.write( "    3           (1-12)                      Starting month                           \n" )
	f.write( "   21           (1-31)                      Starting day                             \n" )
	f.write( "   0.0          (0.0-24.0)                  Starting hour                            \n" )
	f.write( "    %s           (y or n)                    Time-dependent radiation?                \n" 			% time_dep )
	f.write( "    n           (y or n)                    Date-dependent radiation?                \n" )
	f.write( "    n           (y or n)                    Diurnally averaged radiation?            \n" )
	f.write( "    n           (y or n)                    Annual-average radiation?                \n" )
	f.write( "    %s           (y or n)                   do perpetual sun? uses SCON and COSZETA above; overrides other radiation options \n" % perpetual)
	f.write( "    n           (y or n)                    Calculate surface albedo over water?     \n" )
	f.write( "    %s                                      Ocean surface albedo (if n above)        \n" 			% albedo   )
	f.write( "  0.15                                      Land surface albedo                      \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Surface fluxes                                                                      \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( "    n           (y or n)                    Wind-dependent surface fluxes?           \n" )
	f.write( "   0.0           m/s                        Assumed background surface wind across section \n" )
	f.write( "   0.0           m/s                        Assumed background surface wind along section  \n" )
	f.write( "    %s           m/s                        Gust factor for surface fluxes           \n" 			% ugust    )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Soil parameters                                                                     \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( "    n           (y or n)                    Interactive soil moisture?               \n" )
	f.write( "   1.00                                     Evaporative fraction if n above          \n" )
	f.write( "  10.0          days                        Soil response time scale                 \n" )
	f.write( "  10.0          days                        Runoff time scale (assumed infinite if > 100) \n" )
	f.write( "   0.3                                      Sensitivity of evaporation fraction to soil moisture \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Ocean parameters                                                                    \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( "   5.0          meters                      Assumed ocean mixed layer thickness      \n" )
	f.write( "   0.0                                      Coupling parameter                       \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Miscellaneous parameters                                                            \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( " 150.0          hPa                         Pressure scale over which PBL momentum fluxes converge \n" )
	f.write( "  60.0          days                        Internal damping time scale              \n" )
	f.write( " 120.0          minutes                     Top sponge layer damping time scale      \n" )
	f.write( "   0.0          hPa                         Pressure above which sounding held fixed at initial condition \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( " Output                                                                              \n" )
	f.write( " -------------------------------                                                     \n" )
	f.write( "  3.0          hours                       Frequency of graphics output to time series   \n" )
	f.write( " 500.0          days                        Composite averaging time at end of integration\n" )
	f.write( "   0.0          m/s                         Composite translation speed              \n" )
	f.write( "    y          (y or n)                     Write out average cloud and water vapor at end of integration? \n" )


	f.close()
        return
#######################################################################



#######################################################################
def write_sounding_in(form):
# This function writes the input file 'sounding.in'
# It has one input containing all the values from the html form
# In future we will want to:

	# Modules
	import numpy as np

	# Extract each paramater from the input object ################
	SSTi = form["SSTi"].value
        SSTi = float(SSTi)
        #SSTi = 28

	# Initial relative humidity
        RHi = 0.8
	
	# Load pressure and O3 vector from file
        p,O3 = np.loadtxt("O3.in", skiprows=3, unpack=True)
    
	# Create u vector
        u = np.zeros(len(p))

	# create Qcool vector
        Rcool = np.zeros(len(p))

	# create Psi vector
        Psi = np.zeros(len(p))

	# Create Temp and specific humidity using idealized profile
        T,q = calc_idealized_profile(SSTi,p[0],RHi,p)


	# Write the sounding.in file ##################################    
        # This file has to be created very carefully, as the Fortran format specifiers are primitive 
	f = open('./sounding.in', 'w')

        f.write( "  N levels =  %3i        Q= 0.00 m/s           SST=   %5.2f C            \n" %(len(p),SSTi) )
	f.write( "  \n" )
	f.write( "  Pressure   Temp.      Spec. humid.   O3 mr    Psi     Rcool     U   \n" )
	f.write( "    (mb)      (C)          (g/kg)      10**-6 E4 kg/m/s (K/dy)   m/s  \n" )  
	f.write( "  -------    -----      ------------   -----   -----    ------    -   \n" )
	for i in range(0,len(p)):
		f.write( "   %6.1f  %8.3f     %10.6f    %6.3f  %6.2f   %7.3f %5.1f  \n" %(p[i],T[i],q[i],O3[i],Psi[i],Rcool[i],u[i]) )
	f.close()


#######################################################################
def calc_idealized_profile(T_surf,p_surf,RH,p):
# This function calculates idealized temp and humidity profile 
# for initialization of RC model

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
	T = T_surf + gamma*scale*np.log(p/p_surf)

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


