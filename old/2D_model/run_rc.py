#!/usr/bin/python

# Modules ############################################################

import cgi
import os
import subprocess 

os.environ['MPLCONFIGDIR'] = '/tmp/'
import matplotlib
matplotlib.use('agg')
from pylab import *

# Modules specific to RC model
from write_input      import write_params_in,write_sounding_in
from postprocess   import get_timeseries

# Initialize the output page #########################################
print "Content-type: text/html\n\n"
#print "<html>"+os.getcwd()+"</html>\n"


# Inputs #############################################################

# Read the values from the html form
form = cgi.FieldStorage()

# write the 'params.in' file
write_params_in(form)

# write the 'sounding.in' file
write_sounding_in(form)


# Run model ###########################################################
runstuff = subprocess.call('./rc_model.exe')




# Post processing #####################################################

# Get axes
time = []
for line in open('output/t.out','rt'):
    time.append(float(line))



# Get timeseries
Ts     = get_output('output/sst.out')
precip = get_output('output/precip.out')
olr    = get_output('output/olr.out')
#LH = get_output('output/lc.out')
#SH = get_output('output/hc.out')

# Get profiles


### Write some output to screen #######################################

fsnd = open('sounding.in', 'r')
initsnd = fsnd.readline()
ssti = initsnd[initsnd.find("SST="):initsnd.find("\n")]
fsnd.close()


print "<html><p>Initial "+ssti+"</p></html>"

fsnd = open('output/sounding.out', 'r')
finalsnd = fsnd.readline()
sstf = finalsnd[finalsnd.find("SST="):finalsnd.find("\n")]
fsnd.close()
print "<html><p>Final "+sstf+"</p></html>"


### Plotting ##############################################

hp = plot(time,Ts)
hp = xlabel('time (days)')
hp = ylabel('SST (deg. C)')

hf = savefig('output/sst_t.png')

print "<img src=\"output/sst_t.png\" alt=\"SST as a function of time\" width=\"400\" height=\"300\" />"


