#!/usr/bin/python
import cgi
import os
import subprocess 

os.environ['MPLCONFIGDIR'] = '/tmp/'
import matplotlib
matplotlib.use('agg')
from pylab import *

########################################################################################################################
# Passing in parameters to the model:
# Currently This python scripts writes a params.in file from scratch
# substituting values passed to it from the html form
# To pass a parameter in 3 steps must be taken
#
# 1. Add the parameter to the html form
# 2. Read the parameter into the python script
# 3. Write the parameter to the file
#
# In future we will want to 
# a) add error handling so we don't try to run the model for "dog" number of days
# b) Perhaps write some functions so this is not just a big block of code


# Step 1: Here is where we read the values from the html form. Make sure they are all there!
form = cgi.FieldStorage()

# Step 2: Extract each paramater from the Fieldstorage object
days = form["days"].value
co2  = form["co2"].value

# Step 3: Write the file
# Replace the given parameter with %s, and add the variable name at the end of the write call

# I have written this out explicitly which makes for long tedious code.
# The real reason for this is I don't really know how the model gets the parameters from the input file
# but I think it may actually be nice for readbility so we will go with that...

f = open('./params.in', 'w')

f.write( " Value           Units                              Parameter                        \n" )
f.write( " -------         ------                          ---------------                     \n" )
f.write( " Program Control                                                                     \n" )
f.write( " ------------------------                                                            \n" )
f.write( "     n          (y or n)                    Restart from end of last integration?    \n" )
f.write( "    %s          days                        Length of integration                    \n" 			% days )
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
f.write( "    y           (y or n)                    Interactive radiation?                   \n" )
f.write( "   1.0           hours                      Frequency of radiation calls (if y above)\n" )
f.write( "    y           (y or n)                    Interactive surface temperature?         \n" )
f.write( "    y           (y or n)                    Interactive water vapor?                 \n" )
f.write( "    n           (y or n)                    Interactive cloud?                       \n" )
f.write( " 513.75         Watts/m^2                   SCON = Solar constant                    \n" )
f.write( "  0.667         (0-1)                       COSZETA = Cosine of zenith angle to use if PERPETUAL=y \n" )
f.write( "   %s           ppm                         CO2 content                              \n" 			% co2 )
f.write( "    3           (1-12)                      Starting month                           \n" )
f.write( "   21           (1-31)                      Starting day                             \n" )
f.write( "   0.0          (0.0-24.0)                  Starting hour                            \n" )
f.write( "    n           (y or n)                    Time-dependent radiation?                \n" )
f.write( "    n           (y or n)                    Date-dependent radiation?                \n" )
f.write( "    n           (y or n)                    Diurnally averaged radiation?            \n" )
f.write( "    n           (y or n)                    Annual-average radiation?                \n" )
f.write( "    y           (y or n)                    PERPETUAL = do perpetual sun? uses SCON and COSZETA above; overrides other radiation options \n" )
f.write( "    n           (y or n)                    Calculate surface albedo over water?     \n" )
f.write( "  0.15                                      Ocean surface albedo (if n above)        \n" )
f.write( "  0.15                                      Land surface albedo                      \n" )
f.write( " -------------------------------                                                     \n" )
f.write( " Surface fluxes                                                                      \n" )
f.write( " -------------------------------                                                     \n" )
f.write( "    n           (y or n)                    Wind-dependent surface fluxes?           \n" )
f.write( "   0.0           m/s                        Assumed background surface wind across section \n" )
f.write( "   0.0           m/s                        Assumed background surface wind along section  \n" )
f.write( "   5.0           m/s                        Gust factor for surface fluxes           \n" )
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
f.write( "  24.0          hours                       Frequency of graphics output to time series   \n" )
f.write( " 500.0          days                        Composite averaging time at end of integration\n" )
f.write( "   0.0          m/s                         Composite translation speed              \n" )
f.write( "    y          (y or n)                     Write out average cloud and water vapor at end of integration? \n" )


f.close()

# End writing of input file params.in #####################################################################################



print "Content-type: text/html\n\n"
#print "<html>"+os.getcwd()+"</html>\n"
fsnd = open('sounding.in', 'r')
initsnd = fsnd.readline()
ssti = initsnd[initsnd.find("SST="):initsnd.find("\n")]
fsnd.close()

print "<html><p> number of days: " +days+ "</p><html>"

print "<html><p>Initial "+ssti+"</p></html>"
runstuff = subprocess.call('./rcmod_test.exe')

fsnd = open('output/sounding.out', 'r')
finalsnd = fsnd.readline()
sstf = finalsnd[finalsnd.find("SST="):finalsnd.find("\n")]
fsnd.close()
print "<html><p>Final "+sstf+"</p></html>"

time = []
for line in open('output/t.out','rt'):
    time.append(float(line))

Ts = []
for line in open('output/sst.out','rt'):
    tmp1,temp,tmp2 = [float (x) for x in line.split()]
    Ts.append(temp)

hp = plot(time,Ts)
hp = xlabel('time (days)')
hp = ylabel('SST (deg. C)')

hf = savefig('output/sst_t.png')

print "<img src=\"output/sst_t.png\" alt=\"SST as a function of time\" width=\"400\" height=\"300\" />"
