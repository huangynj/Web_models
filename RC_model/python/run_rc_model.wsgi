#!/usr/bin/python

# Modules #########################################################################################

import time
import cgi
import os
import signal
import shutil
import subprocess 
import tempfile
import json
import multiprocessing
import threading
import socket

import sys

# Modules specific to RC model
from mime           import *
from write_input    import write_params_in,write_sounding_in
from postprocess    import output_control, get_output, get_avgtime
from plot_model_log import plot_model_log

# Initial setup  ###################################################################################

### Parameters for the type of model setup ###

### Contact method for problems ###

# For athena model - email Martin Singh (or current maintainer)
contact = 'contact your instructor'

# For EdX platform - use discussion forums
#contact = 'post in the discussion thread titled ''Radiative-convective model issues'''

# Server information
host = 'localhost'
PORT = 8080


### Using datadog to send metircs? ###

datadog = 0

if datadog:
   from statsd import statsd

### Do a set of tasks once per day, or only on startup? ###
daily_tasks = 0


### Caching ###

caching = 2;    # 0: No caching - just run model for each new simulation
		# 1: Use Amazon S3 bucket for caching
		#    S3 bucket details (name, keys etc.) given in file "s3_functions.py" 
		# 2: Local caching

if caching == 1:
   from s3_functions          import send_to_bucket,get_from_bucket,cache_name

elif caching == 2:
   from local_cache_functions import send_to_local_cache,get_from_local_cache,cache_name

### Use Geo IP to get country names of users (must have GeoIP.py and GeoIP.dat)
GEOIP_TOOL = 1


### Parameters for server instance ###

# Number of CPUs available
num_CPUs = multiprocessing.cpu_count()

# Number of simultaneous model instances allowed
num_threads = 1

# max time for any given simulation (seconds)
max_simulation_time = 5*60

# max queue length before we start to respond to health checks with 502 errors
QMAX = 20

# Time between scheduled queue checks (seconds)
T_Qcheck = 5*60

# Verbosity of log
verbose = 0  	# 0 = standard statistical and error output
		# 1 = additional warnings
		# 2 = logging of all requests
		# 3 = include request debugging information 
		# 4 = include queue status infromation

### Error codes: ######################################################################################

# EXCESSIVE LOAD ERRORS
#   'TIMEOUT'		Occurs when max_simulation_time is exceeded.
#   'NLOGTIME'		Occurs when max_simulation_time is exceeded and there is not log file left over.
#   'QLONG'		Occurs when the length of the queue exceeds QMAX, model is not able to be submitted.

# MODEL CRASH ERRORS
#   'EXIT##'		Occurs when model exits with a code > 0, ## is replaced by the error code.
#   'CRASHEX'		Occurs when model crashes with exit code, should happen along with above code.
#   'NLOGU'		Occurs when simulation crashes, but no exit code is given.
#   'UNKNOWN_K'		Occurs when model crashes because unusual kill signal is sent.
#   'UNKNOWN_E'		Occurs when unknown fatal error occurs; no other error handling caught the error.

# INPUT ERRORS
#   'INVPARAM'		Occurs when invald parameters are specified. Should not happen with standard form input.
#   'NINPUT'		Occurs when the input files are not created or corrupt.

# OUTPUT ERRORS
#   'NPLOT'		Occurs when there is an error in creating or retreiving the plot.
#   'REQDIR'		Occurs when path to figure file is a directory.
#   'NPATH'		Occurs when figure file does not exist.

# IDLING ERRORS
#   'NRESTRT'		Occurs when user asks for a restart, but there is no restart file.




### Some directories we need ##############################################################


# Directory where PID files and queue status are placed
VARDIR     = "../../../var/"
PIDDIR     = VARDIR+"PIDs/"
PIDFILE    = VARDIR+"rc_model.pid"
Q_STATFILE = VARDIR+"queue_status.txt"

# Directory containing local cache
CACHEDIR = "../../../cache/"

# Directory where each model instance will be stored
TEMPDIR = '../../../temp/'

# Directory for log files
LOGDIR  = "../../../logs/"
REPORTDIR  = LOGDIR+"reports/"
LOGFILE = LOGDIR+"rc_model_log.txt"

# Location of the tmpwatch command
TMPWATCH   = "../../tools/tmpwatch/tmpwatch"

# Name of output html template
output_opts = '../output_opts.html'

# Relative path to model executable
model_exec = '../model/rc_web'

# relative path to O3 input file
O3in = '../model/O3.in'

# text to use for default IP
DEFAULT_USER = 'unknown'

### Setup server instance ###

os.environ['MPLCONFIGDIR'] = TEMPDIR+'/'


# Make a queue to handle the model simulations
queue = multiprocessing.JoinableQueue(QMAX)

# Define list/dictionary of the queued/running jobs
manager = multiprocessing.Manager()
queued_jobs = manager.dict()
queue_order = manager.list([])
running_jobs = manager.dict()

# Initialize some counters
count_submit = 0
count_complete = 0
count_cache = 0
count_error = 0


# Make sure the directories exist
if not os.path.exists(VARDIR):    os.makedirs(VARDIR)
if not os.path.exists(PIDDIR):    os.makedirs(PIDDIR)
if not os.path.exists(TEMPDIR):   os.makedirs(TEMPDIR)
if not os.path.exists(LOGDIR):    os.makedirs(LOGDIR)
if not os.path.exists(REPORTDIR): os.makedirs(REPORTDIR)
if not os.path.exists(CACHEDIR): os.makedirs(CACHEDIR)


# Send PID to logfile
open(PIDFILE,'w').write(str(os.getpid()))
open(PIDDIR+str(time.strftime("%Y%m%d_%H%M",time.localtime(time.time()))),'w').write(str(os.getpid()))

# Get IP address for this machine - this is a hack to get the internet facing socket
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.connect(("gmail.com",80))
IPid = s.getsockname()[0]
s.close()




def LOG(x): #######################################################################################
### Function to send information to log file ###

    # Open log file in append mode 
    fp = open(LOGFILE,'a')

    
    if type(x)==dict:          # format ouput for dictionary
        for k in x:
            if not k:
                continue
            s = '  %s : %s' % (k,x[k])
            t = '%s: ' % time.ctime(time.time())
            s = t+s
            fp.write(s)
            print s
    #if type(x)==type('str'):
    else:                      # format output for string
        t = '%s: ' % time.ctime(time.time())
        x = t+x
        fp.write(x)
        fp.write('\n')
        print x

    sys.stdout.flush()
    fp.flush()
    fp.close()


def background_tasks():
   """ Perform some tasks every so often """
   global count_submit
   global count_error
   global count_cache
   global count_complete
 
   if verbose > 3:
      LOG('=== Running background tasks to check queue health and remove stale output')  

   # Check what the queue is up to
   check_queue_health()

   # Clean the temp directory
   ret = subprocess.call([TMPWATCH,'1',TEMPDIR])
 


   # Send some data to the dog
   if datadog:
      statsd.gauge('RC_model.Gsubmit',count_submit,tags=[IPid])
      count_submit = 0
      statsd.gauge('RC_model.Gcomplete',count_complete,tags=[IPid])
      count_complete = 0
      statsd.gauge('RC_model.Gcached',count_cache,tags=[IPid])
      count_cache = 0
      statsd.gauge('RC_model.Gerror',count_error,tags=[IPid])
      count_error = 0
 

   # schedule the next task in T_Qcheck seconds
   t = threading.Timer(T_Qcheck,background_tasks)
   t.daemon = True
   t.start()
 
  

def startup_tasks():
   """ Perform the daily scheduled tasks"""

   LOG('=== Running startup/daily tasks')

   current_time = time.localtime(time.time())
   the_month = time.strftime("%b",current_time)
   the_day = int(time.strftime("%d",current_time))

   # Do the background tasks
   background_tasks()

   # Check if we need to move logfile output
   with open(LOGFILE, 'r') as f:
      line = f.readline()

   if the_month not in line:

      # Get the month that the log file had its first entry
      # This is dependent on the date format of the log file
      # (More bad coding practices from MSS!)
      log_month = line[4:7]
      log_year = line[20:24]

      # move the log file
      report_file = REPORTDIR+log_year+'_'+log_month+'_log.txt'
      subprocess.call(['cp',LOGFILE,report_file])

      # Remove all the data from the old log file 
      # Do this so that the log file retains all its previous permissions (hopefully) 
      lfile=open(LOGFILE,"w")
      lfile.close()

      # Start a new log file
      LOG('=== Welcome to the month of '+the_month)
      #os.chmod(LOGFILE, 777)

      # Make sure we don't lose any of this months logs
      rfile=open(report_file)
      lfile=open(LOGFILE)
         
      for line in rfile:
          if the_month in line:
             lfile.write(line)


      lfile.close()
      rfile.close()
      

   # Update the bar chart
   plot_model_log(LOGFILE,REPORTDIR,GEOIP_TOOL)

   if daily_tasks:
      schedule_daily_tasks


def schedule_daily_tasks():
   """Schedule tasks for 01:00UTC"""

   current_time = time.localtime(time.time())
   the_date = time.strftime("%Y %m %d",current_time)
   the_day = int(time.strftime("%d",current_time))

   schedule_time = time.mktime(time.strptime(the_date,"%Y %m %d"))+86400+3600
   t = threading.Timer(schedule_time-time.time(), startup_tasks, [the_day,schedule_time])
   t.daemon = True
   t.start()


class model_daemon(multiprocessing.Process): #############################################################
##### CLASS: Run queued jobs ###

    """Threaded RC model runner"""



    #----------------------------------------------------------------------
    def __init__(self, queue,queued_jobs,queue_order,running_jobs):
        multiprocessing.Process.__init__(self)
        self.queue = queue
        self.queued_jobs = queued_jobs
        self.queue_order = queue_order
        self.running_jobs = running_jobs

    #----------------------------------------------------------------------
    def run(self):
        while True:
            # gets the simulation parameters from the queue
            [dirname,submit_time,days,cache_file] = self.queue.get()
            write_queue_status()
 
            # Calculate queue wait time
            wait_time = time.time()-submit_time


            # Log the wait time
            LOG('run: wait time: '+dirname+': '+str(wait_time))
            if datadog: statsd.histogram('RC_model.queue_time',wait_time,tags=[IPid])
 
            # Initialize the running time
            run_time = time.time()

            # Run the simulation
            return_code = self.run_model(dirname)


            # Calculate the running time per day of simulation
            run_time = (time.time() - run_time)/float(days)

            if return_code == 0:
               LOG('run: run time: '+dirname+': '+str(run_time))
               if datadog: statsd.histogram('RC_model.run_time',run_time,tags=[IPid])

               # Note that result is about to be cached
               with open(dirname+'/log.out', 'w') as f:
      	  	  f.write('cached')

               # Send output to the cache
               if cache_file != '':   # If no caching wanted then cache file should be set to null
                  if caching == 1:
                     send_to_bucket(cache_file,dirname)
                  else:
                     ret = send_to_local_cache(cache_file,dirname)
                     if verbose > 1: LOG('cache send: '+ret)

            # send a signal to the queue that the job is done
            self.queue.task_done()


    #----------------------------------------------------------------------
    def run_model(self,dirname):
        """"""
        global count_submit
        global count_error
        global count_cache
        global count_complete
       
        json_output = {}

        # Extract directory name from form
        #dirname = form["dirname"].value
        json_output['dirname'] = dirname

        if self.queued_jobs[dirname] == 'run':

           # Remove from queued jobs
           del self.queued_jobs[dirname]
           self.queue_order.pop(0)
           
           # Run the model binary in a subprocess - use timeout to prevent hanging
           run_obj = subprocess.Popen(["timeout",str(max_simulation_time),model_exec,dirname], \
                    stderr=None, stdout=None, preexec_fn=os.setsid	)

           self.running_jobs[dirname] = run_obj.pid


           # wait for process to exit
           run_obj.wait()
           # Python 3.3 has native timeout - for now use shell function "timeout"
           #run_obj.wait(timeout=max_simulation_time)

           return_code = run_obj.returncode
           del self.running_jobs[dirname]

           
           # If process timed out note this in logfile
           if run_obj.returncode == 124:
               try:
                  with open(dirname+'/log.out', 'w') as f:
                     f.write('timeout')
               except:
                  LOG('>> Error: Timeout - '+dirname+': NLOGTIME')
                  if datadog: statsd.increment('RC_model.error',tags=[IPid,'NLOGTIME']); count_error +=1
 
               return return_code

           elif run_obj.returncode > 0:

               LOG('run: Model exit with code '+str(run_obj.returncode)+' - '+dirname)
               if datadog: statsd.increment('RC_model.error',tags=[IPid,'EXIT'+str(run_obj.returncode)]); count_error +=1
               LOG('>> Error: Exitcode - '+dirname+': EXIT'+str(run_obj.returncode))
               try:
                  with open(dirname+'/log.out', 'w') as f:
                     f.write('exit '+str(run_obj.returncode))
               except:
                  pass
 
               return return_code

 
        else:
           del self.queued_jobs[dirname]
           self.queue_order.pop(0)
           return_code = -1;

	   return return_code


	# Check that there is output
        if ( not os.path.isfile(dirname+'/profile.out') ):

                if verbose > 0: LOG('>> Warning: Clean: model terminated by user or crashed')
                try:
                   shutil.rmtree(dirname)
                   if verbose >1: LOG('Clean: Directory removed')
                except:
                   if verbose > 0: LOG('>> Warning: Clean: Directory cannot be removed')

                return return_code
        	
        
        # Plot output 
        form = {}
        form["plot_opt"] = "time_sst"
        json_output = output_control(form,dirname,json_output)


        
        # tell client we are done
        with open(dirname+'/log.out', 'w') as f:
           f.write('finished')
        

        return return_code


def replot_sim(form, path, queue,user): ################################################################
### Function to call when client asks to replot data from simulation ###

    '''
    This function replots data from a simulation
    '''

    # Initialize JSON output structure ----------------------------
    json_output = {}
    json_output['html'] = ''           # html to display
    json_output['status'] = 'queued'   # model status
    json_output['alert'] = ''          # error message to print

    if 'dirname' in form:
        dirname = form['dirname'].value
    else:
        dirname = ''

    if verbose > 1: LOG('Replot: '+user+': '+dirname)
    if verbose > 2: LOG("Inputs: %s" % dict((k,form[k].value) for k in form))

    json_output = output_control(form,dirname,json_output)
    return json.dumps(json_output,indent=1)



def submit_sim(form, path, queue,user): ################################################################
### Function to submit simulation ###

# This is what we do when the browser posts some data to the server

        global count_submit
        global count_error
        global count_cache
        global count_complete

        '''
        This function submits a simulation to be run.
        '''


        print "form=%s" % dict((k,form[k].value) for k in form)

        # Round the form inputs to help with caching
        form = round_form(form)

        # Put some info in the LOG file -------------------------------
        days = form['days'].value
        LOG('Submit simulation: user IP - %s: %s, length: %s days' % (user,form['dirname'].value,days)) 
        if datadog: statsd.increment('RC_model.submit',tags=[IPid]); count_submit +=1

        if verbose > 1: LOG("form: %s" % dict((k,form[k].value) for k in form))

        # Initialize JSON output structure ----------------------------
        json_output = {}
        json_output['html'] = ''           # html to display
        json_output['status'] = 'queued'   # model status
        json_output['alert'] = ''          # error message to print

 
        # Make directory for input/output -----------------------------
        
        # if dirname exists try to use this directory
        dirname = form["dirname"].value
        
        # Add dirname to JSON output
        json_output['dirname'] = dirname



        # Check to see if directory still exists
       	if not os.path.isdir(dirname):
        
       		if "restart" in form:
       			# Trying to restart but the directory has vanished
                        json_output['html']  ="<html><h2>Restart error</h2>"
                        json_output['html'] +="<h4>Cannot find previous output: Your session has probably expired. </h4>"
                        json_output['html'] +="<p>If you leave the model idle for longer than 20 minutes its output may be cleaned up,"
                        json_output['html'] += "preventing you from restarting the model from the end of the simulation."

                        json_output['alert']  ="Restart error: Cannot find previous output.   (Error code: NRESTRT)"
			json_output['alert'] +="\nYour session has probably expired.\n\n"
                        json_output['alert'] +="If you leave the model idle for longer than 20 minutes its output may be cleaned up,"
                        json_output['alert'] +=" preventing you from restarting the model from the end of the simulation"

		        LOG('>> Error: Restart error - '+dirname+': NRESTRT') 
		        if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'NRESTRT'])

			return json.dumps(json_output,indent=1)

       		else:
       			# Can't find directory, so create a new unique directory 
      			dirname = tempfile.mkdtemp(dir=TEMPDIR,prefix='RCmod_')
                        os.chmod(dirname, 0755)
                        form["dirname"].value = dirname
        

        # Add dirname to JSON output
        json_output['dirname'] = dirname

        # If restart move pforile.out to profile.old
        if ( os.path.isfile(dirname+'/profile.out') ): 
                os.rename(dirname+'/profile.out',dirname+'/profile.old')

        # Set name for cached storage of cached results
        if caching > 0:
           cache_file = cache_name(form)
           if verbose >1: LOG('cache file: '+cache_file)
           if caching == 2:
               cache_file = CACHEDIR+cache_file
        else:
           cache_file = ''



        # Attempt to obtain output from cache
        if cache_file != '':

           if caching == 1: # S3 bucket
              retval = get_from_bucket(cache_file,dirname)
           else:
              retval = get_from_local_cache(cache_file,dirname)
              if verbose > 1: LOG('cache get: '+str(retval))

           if retval == 0:
              os.system("touch "+dirname+"/*")
              json_output['html'] = '<br><br><br><center><h3>Retrieving cached results</h3><br><br><h2>Please Wait<h2></center><br>'
              form = {}
              form["plot_opt"] = "time_sst"
              json_output = output_control(form,dirname,json_output)
              LOG('run: Returning cached result')
              if datadog: statsd.increment('RC_model.cached',tags=[IPid]); count_cache +=1
              return json.dumps(json_output,indent=1)
        
	# Write inputs for model ------------------------------
        # write the 'params.in' file
        err = write_params_in(form,dirname)

        # If server-side form validation failed write error
        if err['err'] == 'fatal':
                json_output['html']  = '<html><h2>Input error</h2>'
                json_output['html'] += 'The input value for: <b>"'+err['var']+'"</b> produced an error. <br>'
                json_output['html'] += 'This input should be of type: <b>'+err['type']
                json_output['html'] += ' </b> in the range <b> ['+err['min']+','+err['max']+'] </b><br>'
                json_output['html'] += 'The value given was: <b>"'+err['value']+'"</b> <br> </html>'
			
		json_output['alert']  = "Input error:    (Error code: INVPARAM) \n\n"
                json_output['alert'] += "Invalid input for "+err['var']
                json_output['alert'] += "\n\nInput should be of type: "
                json_output['alert'] += err['type']+" in the range "+err['min']+" - "+err['max']
                json_output['alert'] += "\nValue given was: "+err['value']

                LOG('>> Error: Input error - '+dirname+': INVPARAM')
		if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'INVPARAM']); count_error +=1
                if verbose > 1:
                   LOG('>> Var:  '+err['var'])
                   LOG('>> Type: '+err['type'])
                   LOG('>> min:  '+err['min'])
                   LOG('>> max:  '+err['max'])
                   LOG('>> val:  '+err['val'])

                return json.dumps(json_output,indent=1)
	
       	# write the 'sounding in' file
        err_sounding = write_sounding_in(form,dirname,O3in)

        # If server-side form validation failed write error
        if err['err'] == 'fatal':
                json_output['html']  = '<html><h2>Input error</h2>'
                json_output['html'] += 'The input value for: <b>"'+err['var']+'"</b> produced an error. <br>'
                json_output['html'] += 'This input should be of type: <b>'+err['type']
                json_output['html'] += ' </b> in the range <b> ['+err['min']+','+err['max']+'] </b><br>'
                json_output['html'] += 'The value given was: <b>"'+err['value']+'"</b> <br> </html>'
			
		json_output['alert']  = "Input error:    (Error code: INVSOUND) \n\n"
		json_output['alert'] += "Invalid input for "+err['var']
                json_output['alert'] += "\n\nInput should be of type: "
                json_output['alert'] += err['type']+" in the range "+err['min']+" - "+err['max']
                json_output['alert'] += "\nValue given was: "+err['value']

                LOG('>> Error: Input error - '+dirname+': INVPARAM')
		if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'INVPARAM']); count_error +=1
                return json.dumps(json_output,indent=1)




       	# Check input files exist
        if ( not os.path.isfile(dirname+'/params.in') ) or ( not os.path.isfile(dirname+'/sounding.in') ): 
       		json_output['html'] = '<html><h2>Input error</h2>'
       		json_output['html'] +='<p>There was a problem writing the required input files to the RC model. '
                json_output['html'] +='Please go back and try running again. <\html>' 

       		json_output['alert'] = 'Input error:     (Error code: NINPUT) \n\n'
                json_output['alert'] += 'Cannot find input files.'
       		json_output['alert'] +='\n\nThere was a problem writing the required input files to the RC model.'
                json_output['alert'] +='Please go back and try running again. <\html>' 

                LOG('>> Error: Input error - '+dirname+': NINPUT')
                if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'NINPUT']); count_error +=1
		return json.dumps(json_output,indent=1)
        


	
       	# Put run in queue ------------------------------------
                 
        # Write status
      	with open(dirname+'/log.out', 'w') as f:
      		f.write('enqueued')


        # Put in queue 
        queued_jobs[dirname] = 'run' 
        queue_order.append(dirname)
        if datadog: statsd.increment('submission',tags=[IPid,user]); count_submit += 1
        submit_time = time.time()

        try:
          queue.put([dirname,submit_time,days,cache_file])             
          json_output['html'] = '<br><br><br><center><h3>Your job is in the queue</h3><br><br><h2>Please Wait<h2></center><br>'

        except:
          json_output['html'] = '<br><br><br><center><h3>Model capacity has been reached</h3><br><br><h2>Please try again in a few minutes<h2></center><br>'
          json_output['alert'] = 'Model capacity reached:     (Error code: QLONG)  \n\nThe model has reached its capacity and cannot handle more requests. Please wait a few minutes while extra capacity is added and try again.'
          LOG('>> Error: Queue length - '+dirname+': QLONG')
          if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'QLONG']); count_error +=1
          


        # Write the model and queue status
        check_queue_health()

        return json.dumps(json_output,indent=1)
              


def enquire_sim(form,path,queue,user): #################################################################
### Function called when client asks server on progress of simulation ###

    
    global count_submit
    global count_error
    global count_cache
    global count_complete
    '''
    This function returns the status of a simulation
    '''
    # Initialize JSON output ------------------------------------------
    json_output = {}
    json_output['html'] = ''    # html to display
    json_output['status'] = ''  # model status
    json_output['alert'] = ''   # Error output



    # Get the name of the temporary directory
    dirname = form["dirname"].value

    # Read the model status from the log file ------------------------
    model_log = dirname+'/log.out' 
   
    if verbose > 1: LOG('Enquire: '+user+': '+dirname)
    
    if os.path.exists(model_log):   # Look for model log and read
	with open(model_log, 'r') as fin:
            model_status = fin.read()

    else:		# Log does not exist model must have crashed
        model_status = 'undefined'
        json_output['status'] = 'undefined';
        json_output['alert']  = 'Simulation crashed!    (Error code: NLOGU) \n\n' 
        json_output['alert'] += 'The simulation you requested could not be completed. Possible reasons may be:\n'
        json_output['alert'] += ' - The initial condition is too far from equilibrium. Try changing the initial SST.\n'
        json_output['alert'] += ' - The parameters are too unrealistic. Try changing them to be closer to the defaults.\n\n'
        json_output['alert'] += 'If the problem occurs with the default parameter set, wait a few minutes, reload the page, and try again. ' 
        json_output['alert'] += 'If the problem still persists please '+contact+' and include as much detail of the circumstances leading to the problem as possible.'

        json_output['html'] = '<br><br><br><center><h3>Simulation could not be completed</h3><br><br>'
        json_output['html'] += '<h2>click "Run model" to start again<h2></center><br>'

        LOG('>> Error: Fatal - '+dirname+': NLOGU')
        if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'NLOGU']); count_error +=1

        return json.dumps(json_output,indent=1)

    # Return the status to the client --------------------------------

    try: 	# Try to convert to percentage
        model_status = float(model_status)*100
        json_output['status'] = 'running'
        json_output['progress'] = model_status
        json_output['html'] = '<br><br><br><center><h3>Model is running</h3><br><br><h2>Please Wait<h2></center><br>'

    except:	# If not number find out if queued or finished

        # Model is queued ~~~~~~~~~~~~~~~~~~~~
        if model_status == 'enqueued':

           json_output['status']   = 'queued' 
           json_output['progress'] = 'false' 

           json_output['html']  = '<br><br><br><center><h3>Your job is in the queue</h3>'
           try:
              queue_num = queue_order.index(dirname)+1
              json_output['html'] += '<br><br><h2>Your job is number '+str(queue_num)+' in the queue</h2></center><br>'
           except:
              json_output['html'] += '<br><br><h2>Please Wait</h2></center><br>'

        # Model is finished ~~~~~~~~~~~~~~~~~~
        elif  model_status == 'finished' or model_status == 'cached':

           json_output['status'] = 'done'
           fig_file = dirname+'/plot1.png'

	   if os.path.isfile(fig_file):
        	image_code =  "<img id=plot src=../../temp/"+fig_file.replace(TEMPDIR,'')+" alt=\"RC model output\" width=\"600\" height=\"450\" />"
	   else:
        	image_code =  '<html><h2>Plotting error</h2>'
                image_code += '<p>There was an error (CODE: NPLOT) in creating the plot you asked for. <br>'
                image_code += 'You can try plotting something else, or rerun the simulation.'
        
                LOG('>> Error: Plotting - '+dirname+': NPLOT')
                if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'NPLOT']); count_error +=1
       

           # Read some key numbers and from the output files
           table_vals      = get_output(dirname+'/topbar.out')
           table_vals.extend(get_output(dirname+'/botbar.out'))
           table_vals.extend(get_output(dirname+'/lwbar.out'))
           table_vals.extend(get_output(dirname+'/swbar.out'))
           table_vals.extend(get_output(dirname+'/shfbar.out'))
           table_vals.extend(get_output(dirname+'/sstbar.out'))

           avgtime = get_avgtime(dirname+'/params.in')


           
           # Read the replotting form html
           with open(output_opts, 'r') as fin:
        	html_file = fin.read()
        
        
	   # Print the plot
	   html_file = html_file.replace('<!-- image goes here -->',image_code)

           # See if we can make comparison plots
	   if ( os.path.isfile(dirname+'/profile.old') ): 
	      html_file = html_file.replace('<!--DIFF','')
	      html_file = html_file.replace('DIFF-->','')
           

	   # Add the average values to the table (Upward fluxes are positive)
	   html_file = html_file.replace('#FNTOA#',"%7.1f" % -float(table_vals[0][0]))
	   html_file = html_file.replace('#LWTOA#',"%7.1f" % -float(table_vals[2][0]))
	   html_file = html_file.replace('#LWSFC#',"%7.1f" % -float(table_vals[3][0]))
	   html_file = html_file.replace('#SWTOA#',"%7.1f" % -float(table_vals[4][0]))
	   html_file = html_file.replace('#SWSFC#',"%7.1f" % -float(table_vals[5][0]))
	   html_file = html_file.replace('#SHSFC#',"%7.1f" % float(table_vals[6][0]))
	   html_file = html_file.replace('#LHSFC#',"%7.1f" % float(table_vals[7][0]))
	   html_file = html_file.replace('##SST##',"%7.1f" % float(table_vals[8][0]))

	   html_file = html_file.replace('##AVGTIME##',"%s" % avgtime)
        
           # Make sure the new form knows where the output lives so we can restart
	   html_file = html_file.replace('dirname" value="','dirname" value="'+dirname)
       

           # Log that model completed succesfully
           LOG('run: success: '+dirname)
           if datadog: statsd.increment('RC_model.complete',tags=[IPid,user]); count_complete +=1
 
	   # print the html to file
	   json_output['html'] =  html_file

   

        # Model timed out ~~~~~~~~~~~~~~~~~~
        elif model_status =='timeout':
          json_output['alert']  = 'Simulation timed out    (Error code: TIMEOUT) \n\n ' 
          json_output['alert'] += 'The simulation you requested is taking too long.\n'
          json_output['alert'] += 'This may be because you have requested a job that is too intensive, or there is a problem with the server.\n'
          json_output['alert'] += 'Try shortening the length of the simulation and try again.'
          json_output['html'] = '<br><br><br><center><h3>Simulation timed out</h3><br><br>'
          json_output['html'] += '<h2>click "Run model" to start again<h2></center><br>'

          LOG('>> Error: Timeout - '+dirname+': TIMEOUT')
          if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'TIMEOUT']); count_error +=1

        # Model crashed  ~~~~~~~~~~~~~~~~~~
        elif model_status.startswith('exit'):
          json_output['alert']  = 'Simulation crashed!    (Error code: CRASHEX) \n\n ' 
          json_output['alert'] += 'The simulation you requested could not be completed. Possible reasons may be:\n'
          json_output['alert'] += ' - The initial condition is too far from equilibrium. Try changing the initial SST.\n'
          json_output['alert'] += ' - The parameters are too unrealistic. Try changing them to be closer to the defaults.\n\n'
          json_output['alert'] += 'If the problem occurs with the default parameter set, wait a few minutes, reload the page, and try again. ' 
          json_output['alert'] += 'If the problem still persists please '+contact+' and include as much detail of the circumstances leading to the problem as possible.'

          json_output['html'] = '<br><br><br><center><h3>Simulation could not be completed</h3><br><br>'
          json_output['html'] += '<h2>click "Run model" to start again<h2></center><br>'

          LOG('>> Error: Crash - '+dirname+': CRASHEX')
          if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'CRASHEX']); count_error +=1
        # Something else happened ~~~~~~~~~~~
        elif model_status =='kill':
          json_output['html'] = '<br><br><br><center><h3>Unknown fatal error occured</h3><br><br>'
          LOG('>> Error: Fatal - '+dirname+': UNKNOWN_K')
          if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'UNKNOWN_K']); count_error +=1
 
        else: 
          json_output['html'] = '<br><br><br><center><h3>Unknown fatal error occured</h3><br><br>'
          LOG('>> Error: Fatal - '+dirname+': UNKNOWN_E')
          if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'UNKNOWN_E']); count_error +=1
 
    
    return json.dumps(json_output,indent=1)


def clean_sim(form,path,queue,user): ###################################################################
### Function called when client wants to delete simulation and cleanup ###

    '''
    This function stops the active simulation and cleans up its output
    '''
    
    # Initialize JSON output structure ----------------------------
    json_output = {}
    json_output['html'] = ''           # html to display
    json_output['alert'] = ''          # error message to print




    if 'dirname' in form:
       dirname = form['dirname'].value
    else:
       dirname = 'nn'

    if verbose > 1: LOG("Clean: "+user+': '+dirname)

    if verbose > 3:
       LOG(running_jobs)
       LOG(queued_jobs)

    if dirname == '' or dirname == 'n': 	 # No cleanup required

       if verbose > 1: LOG("Clean: no cleaning up to be done: "+dirname)
       json_output['status'] = 'undefined';
       return json.dumps(json_output,indent=1)

    
    if dirname in running_jobs:			 # Model is running - need to stop

         jobid = running_jobs[dirname]
         os.killpg(jobid, signal.SIGTERM)
         LOG('run: killing simulation: '+dirname)
         json_output['status'] = 'kill';

         # Note that simulation killed in log
         try:
            with open(dirname+'/log.out', 'w') as f:
                f.write('kill')
         except:
                 if verbose > 0: LOG('>> Warning: clean - Missing log file')


    if dirname in queued_jobs:			 # simulation is queued - remove from queue
            queued_jobs[dirname] = 'kill'
            json_output['status'] = 'kill';

            LOG('run: removing from queue: '+dirname)
            # note that simulation killed in log
            with open(dirname+'/log.out', 'w') as f:
            	f.write('kill')

    
    if "leaving" in form:			 # Client is leaving delete their temp directory 

            try:
              os.system("rm -rf "+dirname)
            except:
              if verbose > 0: LOG(">> Warning: Can't find "+dirname+": probably already cleaned")


    return json.dumps(json_output,indent=1)


def round_form(form): ##############################################################

   form["time_step"].value  = str("%.0f" % float(form["time_step"].value))
   form["avg_time"].value   = str("%.0f" % float(form["avg_time"].value))
   form["graph_time"].value = str("%.0f" % float(form["graph_time"].value))
   form["hour"].value       = str("%.1f" % float(form["hour"].value))
   form["S0"].value         = str("%.0f" % float(form["S0"].value))
   form["theta"].value      = str("%.0f" % float(form["theta"].value))
   form["co2"].value        = str("%.0f" % float(form["co2"].value))
   form["ch4"].value        = str("%.1f" % float(form["ch4"].value))
   form["n2o"].value        = str("%.0f" % float(form["n2o"].value))
   form["cfc11"].value      = str("%.1f" % float(form["cfc11"].value))
   form["cfc12"].value      = str("%.1f" % float(form["cfc12"].value))
   form["ugust"].value      = str("%.0f" % float(form["ugust"].value))
   form["ml_depth"].value   = str("%.1f" % float(form["ml_depth"].value))
   form["water_frac"].value = str("%.1f" % float(form["water_frac"].value))
   form["w_max"].value      = str("%.1f" % float(form["w_max"].value))
   form["w_T"].value        = str("%.0f" % float(form["w_T"].value))
   form["w_top"].value      = str("%.0f" % float(form["w_top"].value))
   form["w_bot"].value      = str("%.0f" % float(form["w_bot"].value))
   form["w_p"].value        = str("%.1f" % float(form["w_p"].value))
   form["p_pbl"].value      = str("%.0f" % float(form["p_pbl"].value))
   form["SSTi"].value       = str("%.0f" % float(form["SSTi"].value))

   return form

def get_health(path, queue): ##############################################################
### Function called when client asks for health of queue ###

    '''
    This function returns the queue status files.
    '''
    write_queue_status()
  
    if queue.qsize() > QMAX:
       return 'ERROR: 502: Queue too large','502 Bad Gateway'
 
    if os.path.exists(Q_STATFILE):

       return  open(Q_STATFILE).read(),'200 OK'

    else:
       return 'ERROR: No queue status file','200 OK'


def get_textfile(form, path, queue,user): ##############################################################
### Function called when client asks for text output ###

    '''
    This function returns text files.
    '''

    textfile = form["textfile"].value
   
    if verbose > 1: LOG('Textfile: '+user+': '+textfile)
    if not TEMPDIR in textfile:
        return 'You attempted to open an illegal directory'
 
    if os.path.exists(textfile):

       return  open(textfile).read()

    else:
       return 'could not find text output'

def get_figurefile(form,path,queue,user): ##############################################################
### Function called when client asks for the figure ###

    '''
    This function returns figure files.
    '''
    if verbose > 1: LOG('Figure: '+user+': '+path)

    

    path = '{0}/{1}'.format(TEMPDIR, path)

    if os.path.exists(path):
       if path.endswith('png'):
          return PNG(open(path).read())
       elif os.path.isdir(path):
          return 'Error in request'
          LOG('>> Error: No output - '+path+': REQDIR')
          if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'REQDIR']); count_error +=1
       else:
          return HTML(open(path).read())

    LOG('>> Error: No output - '+path+': NPATH')
    if datadog: statsd.increment('RC_model.error',tags=[IPid,user,'NPATH']); count_error +=1
    return 'Cannot find requested output'


###################################################################################
# wsgi interface


# Entry point
def application(env, start_response):


        tic = time.time()
	
        visitor_IP = DEFAULT_USER

	method = env['REQUEST_METHOD']
	path = env.get('PATH_INFO', '').lstrip('/')
	try:
           visitor_IP = env['REMOTE_ADDR']
        except:
           pass

	data = None
	
        # Set the response to OK
        response = '200 OK'
        
        # Log the request
        if verbose > 2:	
	   LOG('-' * 60)
	   LOG(method)
	   LOG('IP = %s' % visitor_IP)
	   LOG('path = %s' % path)


        # Get the data from the request
	data = cgi.FieldStorage(fp=env['wsgi.input'], environ=env.copy(), keep_blank_values=True)


        if verbose > 0:
           LOG("form=%s" % dict((k,data[k].value) for k in data))

        # Decide what client is asking for ########################################

        if not data:
	   path  = '\n\n\n'
           path +='**************** Welcome to the RC model ****************\n\n'
           path +='This message should appear if you run "run_rc_model.wsgi" from the terminal.\n'
           path +="If it appears in any other circumstance it is a result of an http request error.\n"
           path +="Please "+contact+" if this happens repeatedly.\n\n"
           path +="**********************************************************\n\n\n"
           ret = path

	elif method=='POST': 			# POST method

             if "replot" in data: 	# Client is replotting some data

                 ret = replot_sim(data,path,queue,visitor_IP)

             else: 			# Client is submitting a simulation to be run     

                 ret = submit_sim(data,path,queue,visitor_IP)

	else:					# GET method

             if "clean" in data: 	# Client wants to stop simulation and clean it up
                 ret = clean_sim(data,path,queue,visitor_IP)

             elif "dirname" in data: 	# Client is asking for simulation progress  
                 ret = enquire_sim(data,path,queue,visitor_IP)
 
             elif "textfile" in data: 	# Client wants text data
                 ret = get_textfile(data,path,queue,visitor_IP)

             elif "health" in data: 	# asking about health of queue
                 ret,response = get_health(path,queue)
             elif "daily_tasks" in data: # ping with this to run daily tasks
                 startup_tasks()
                 ret = "running startup tasks"
             elif "wakeup" in data: # ping with this to ensure model is awake
                 ret = "wake up call"
             else:                 	# Client is asking for figure file
                 ret = get_figurefile(data,path,queue,visitor_IP)
                    
		
        if isinstance(ret, webobj):	# see mime.py
                mime = ret.mime
        else:
                mime = "text/plain"

        start_response(response, [('Content-Type', mime),('Access-Control-Allow-Origin','*')])
        
        # Write the model and queue status
        write_queue_status()

        if verbose > 2: LOG("done at %s" % time.ctime(time.time()))

        if verbose > 2: LOG("=== time taken: "+str((time.time()-tic)*1000)+" ms")
 
        # Send the IP address if sending a json object
        try:
           ret['IPaddress'] = IPid
        except:
           pass


        return [str(ret)]



def write_queue_status(): #########################################################################
### Function to write the wueue status to the log ###


       alivethread = len(multiprocessing.active_children())-1
       with open(Q_STATFILE, 'w') as f:
            f.write('-----------------------------------\n')
            f.write('Time: '+time.ctime(time.time())+'\n')
            f.write('Number of model threads:   '+str(num_threads)+'\n')
            f.write('Number of working threads: '+str(alivethread)+'\n\n')
            f.write('Running jobs: '+str(len(running_jobs))+'\n')

            for key in running_jobs.keys(): f.write('    '+str(key)+' : '+str(running_jobs.get(key))+'\n')
            f.write('\n')
            f.write('Queued jobs: '+str(len(queued_jobs))+'\n')

            for key in queued_jobs.keys(): f.write('    '+str(key)+' : '+str(queued_jobs.get(key))+'\n')

            f.write('\n')

 
       # Send some data to the dog...
       if datadog:
          statsd.gauge('RC_model.threads',num_threads,tags=[IPid])
          statsd.gauge('RC_model.working_threads',alivethread,tags=[IPid])
          statsd.gauge('RC_model.running_jobs',len(running_jobs),tags=[IPid])
          statsd.gauge('RC_model.queued_jobs',len(queued_jobs),tags=[IPid])

def check_queue_health():

       # Make sure we haven't lost any threads

       running_procs = multiprocessing.active_children()
       alivethread = len(running_procs)-1

       if  alivethread < num_threads:

            LOG("Found only "+str(alivethread)+" model thread(s). Restarting broken thread(s).")
            if datadog: statsd.increment('RC_model.restartthread',tags=[IPid])

            for i in range(alivethread,num_threads):
               t = model_daemon(queue,queued_jobs,queue_order,running_jobs)
               t.daemon = True
               t.start()

       write_queue_status()

#--------------------


def runserver(): ##################################################################################


	LOG('=== Server starting from IP: %s =======  ' %  str(IPid))
	LOG('=== %s threads on %s CPUs                ' % (str(num_threads),str(num_CPUs)) )
	LOG('=== Max sim. time: %s sec                ' %  str(max_simulation_time))



	# create a thread pool and give them a queue
	for i in range(num_threads):
	    t = model_daemon(queue,queued_jobs,queue_order,running_jobs)
            t.daemon = True
	    t.start()


        # Run some startup tasks in background (delay by 10 seconds).
        t = threading.Timer(10,startup_tasks)
        t.daemon = True
        t.start()    

        # Check the queue
        check_queue_health()

   
        # Use FastCGI to be compatible with Athena
        #from flup.server.fcgi import WSGIServer
        #WSGIServer(application).run()
   
        # start server
        from wsgiref.simple_server import make_server
        srv = make_server(host,PORT,application)
        try:
           srv.serve_forever()
        except (KeyboardInterrupt, SystemExit):
           LOG('=== Shutting down server ======  ')
           sys.exit()
       

	
if __name__ == '__main__':
	runserver()
