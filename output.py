#!/usr/bin/python

# Modules ############################################################

import cgi
import os;   os.environ['MPLCONFIGDIR'] = '/tmp/'

# Modules specific to RC model
from postprocess import output_control


# Inputs #############################################################

# Read the values from the html form
form = cgi.FieldStorage()
dirname = form["dirname"].value

# Plot output ########################################################
fig_file = output_control(form,dirname)
print "Content-type: text/html\n\n"
#print "whoo"

