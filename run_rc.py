#!/usr/bin/python

# Modules #######################################################################

import cgi
import os;         os.environ['MPLCONFIGDIR'] = './temp/'
import subprocess 
import tempfile
import json

import sys

# Modules specific to RC model
from write_input   import write_params_in,write_sounding_in
from postprocess   import output_control

# Read Inputs ####################################################################

# Read the values from the html form
form = cgi.FieldStorage()

print "hello world"




# Make directory for input/output ###############################################

# if dirname exists try to use this directory
dirname = form["dirname"].value

# Check to see if directory still exists
if not os.path.isdir(dirname):

	if "restart" in form:
		# Trying to restart but the directory has vanished
		print "<html><h2>Restart error</h2>"
		print "<h4>Cannot find previous output: Your session has probably expired. </h4>"
		print "<p>If you leave the model idle for longer than 10 minutes its output may be cleaned up, preventing you from restarting the model from the end of the simulation."
	else:
		# Can't find directory, so create a new unique directory 
		dirname = tempfile.mkdtemp(dir='temp')


# Write inputs for model #######################################################

# write the 'params.in' file
write_params_in(form,dirname)

# write the 'sounding.in' file
write_sounding_in(form,dirname)


# Check input files exist
if ( not os.path.isfile(dirname+'/params.in') ) or ( not os.path.isfile(dirname+'/sounding.in') ): 
	print '<html><h2>Input error</h2>'
	print '<p>There was a problem writing the required input files to the RC model. Please go back and try running again. <\html>'
	exit()
	
# Run model #####################################################################
runstuff = subprocess.Popen(['./rc_web',dirname], \
	stderr=subprocess.STDOUT, stdout=subprocess.PIPE)

runstuff.wait()

# Check that there is output
if ( not os.path.isfile(dirname+'/sounding.out') ): 
	print '<html><h2>Output error</h2>'
	print '<p>The model was unable to complete the simulation. This may mean there was an error in the server, or that the model crashed.'
	print '<p> Below is the model output log if I can find it: <br><br>'
	print '    ----------------------------------------------- <br><br> <code>'
	for line in runstuff.stdout.readlines():
		print line+'<br>'
	print '</code></html>'
	exit()
	

# Plot output ####################################################################
fig_file = output_control(form,dirname)


# Print output and output form for new plotss ####################################

# Print the figure
if os.path.isfile(fig_file):
	image_code =  "<img id=plot src="+fig_file+" alt=\"RC model output\" width=\"600\" height=\"450\" />"
else:
	image_code = '<html><h2>Plotting error</h2>'+'<p>There was an error in creating the plot you asked for. <br>'+'You can try plotting something else, or rerun the simulation.'

# Read the replotting form html
with open('output_opts.html', 'r') as fin:
    html_file = fin.read()


# Print the plot
html_file = html_file.replace('<!-- image goes here -->',image_code)

# Make sure the new form knows where the output lives so we can restart
html_file = html_file.replace('dirname" value="','dirname" value="'+dirname)

# print the html to file
print html_file

###################################################################################
