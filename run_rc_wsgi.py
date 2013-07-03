#!/usr/bin/python

# Modules #######################################################################

import time
import cgi
import os
import subprocess 
import tempfile
import json

import sys
from mime import *

os.environ['MPLCONFIGDIR'] = './temp/'

# Modules specific to RC model
from write_input   import write_params_in,write_sounding_in
from postprocess   import output_control

TEMPDIR = 'temp'

# Logging #######################################################################

PIDFILE = "run_rc_wsgi.pid"
LOGFILE = "run_rc_wsgi.log"

open(PIDFILE,'w').write(str(os.getpid()))

def LOG(x):
    fp = open(LOGFILE,'a')
    if type(x)==dict:
        for k in x:
            if not k:
                continue
            s = '  %s : %s' % (k,x[k])
            fp.write(s)
            print s
    #if type(x)==type('str'):
    else:
        fp.write(x)
        fp.write('\n')
        print x

    sys.stdout.flush()
    fp.flush()
    fp.close()


##################################################################################
# Handle a POST call

def do_POST(form, path):

        # Read Inputs ####################################################################
        
        # Read the values from the html form
        # form = cgi.FieldStorage()
        
        LOG("hello world")
        LOG("form=%s" % dict((k,form[k].value) for k in form))
        LOG("path=%s" % path)
        
        # Make directory for input/output ###############################################
        
        # if dirname exists try to use this directory
        dirname = form["dirname"].value
        LOG('dirname=%s' % dirname)
        
	ret = ""

        # Check to see if directory still exists
        if not os.path.isdir(dirname):
        
        	if "restart" in form:
        		# Trying to restart but the directory has vanished
			ret += "<html><h2>Restart error</h2>"
        		ret += "<h4>Cannot find previous output: Your session has probably expired. </h4>"
        		ret += "<p>If you leave the model idle for longer than 10 minutes its output may be cleaned up, preventing you from restarting the model from the end of the simulation."
        	else:
        		# Can't find directory, so create a new unique directory 
        		dirname = tempfile.mkdtemp(dir=TEMPDIR)
        
        
        # Write inputs for model #######################################################
        
        # write the 'params.in' file
        write_params_in(form,dirname)
        
        # write the 'sounding.in' file
        write_sounding_in(form,dirname)
        
        
        # Check input files exist
        if ( not os.path.isfile(dirname+'/params.in') ) or ( not os.path.isfile(dirname+'/sounding.in') ): 
        	ret += '<html><h2>Input error</h2>'
        	ret += '<p>There was a problem writing the required input files to the RC model. Please go back and try running again. <\html>'
		return
        	
        # Run model #####################################################################
        runstuff = subprocess.Popen(['./rc_web',dirname], \
        	stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
        
        runstuff.wait()
        
        # Check that there is output
        if ( not os.path.isfile(dirname+'/sounding.out') ): 
        	ret += '<html><h2>Output error</h2>'
        	ret += '<p>The model was unable to complete the simulation. This may mean there was an error in the server, or that the model crashed.'
        	ret += '<p> Below is the model output log if I can find it: <br><br>'
        	ret += '    ----------------------------------------------- <br><br> <code>'
        	for line in runstuff.stdout.readlines():
        		ret += line+'<br>'
        	ret += '</code></html>'
		return
        	
        
        # Plot output ####################################################################
        fig_file = output_control(form,dirname)
        
        
        # Generate output and output form for new plotss #################################
        
        # Print the figure
        if os.path.isfile(fig_file):
        	image_code =  "<img id=plot src=/rc/"+fig_file+" alt=\"RC model output\" width=\"600\" height=\"450\" />"
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
	ret += html_file

	return HTML(ret)

###################################################################################
# GET response

def do_GET(data, path):
    '''
    This function returns files, like images
    '''
    if not path.startswith(TEMPDIR):
        return 'nothing at %s' % path
    
    if os.path.exists(path):
        if path.endswith('png'):
            return PNG(open(path).read())
        else:
            return HTML(open(path).read())

    return 'nothing at %s' % path
    

###################################################################################
# wsgi interface

DEFAULT_USER = "unknown"

# Entry point
def application(env, start_response):
	
	method = env['REQUEST_METHOD']
	path = env.get('PATH_INFO', '').lstrip('/')
	data = None
	
	sslcert = env.get('HTTP_SSL_CLIENT_S_DN','')	# username from SSL cert
	user = DEFAULT_USER
	if sslcert:
		user = sslcert.rsplit('emailAddress=',1)[-1]
	
	LOG('-' * 60)
	LOG(method)
	LOG('user = %s' % user)

	if method=='POST':
		data = cgi.FieldStorage(fp=env['wsgi.input'], environ=env.copy(), keep_blank_values=True)
                #ret = "Hey, the time is %s" % time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())
                ret = do_POST(data, path)
	else:
                LOG('get: path=%s' % path)
                ret = do_GET(data, path)
		
        if isinstance(ret, webobj):	# see mime.py
                mime = ret.mime
        else:
                mime = "text/plain"
        start_response('200 OK', [('Content-Type', mime)])

        LOG("done at %s" % time.ctime(time.time()))
        return [str(ret)]

        #start_response('200 OK', [('Content-Type', 'text/html')])
	#return ret
	
#--------------------

host = 'localhost'
PORT = 9004

def runserver():
	LOG('========> started at %s' % time.ctime(time.time()))
	from wsgiref.simple_server import make_server
	srv = make_server(host, PORT, application)
	srv.serve_forever()
	
if __name__ == '__main__':
	runserver()
