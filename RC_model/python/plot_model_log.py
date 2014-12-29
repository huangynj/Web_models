#!/usr/bin/python

import time
import matplotlib
import pylab as mp
import numpy as np
from collections import Counter


# Error code groups
timeout = ('TIMEOUT','NLOGTIME')
qlong   = ('QLONG')
crash   = ('EXIT','CRASHEX','NLOGU','UNKNOWN_K','UNKNOWN_E')
io      = ('INVPARAM','NINPUT','NPLOT','REQDIR','NPATH','NRESTRT')




def plot_model_log(LOGFILE,REPORTDIR,GeoIP_tool):

   if GeoIP_tool:
      import GeoIP

   # Width of the time windows to examine in seconds
   bin_width = 21600

   # Find current time - subtract 3 hours since we will be running this just after midnight
   current_time = time.localtime(time.time())

   # Get the beginning and end times of the month (This is very ugly, but can't be bothered fixing now)
   the_year = time.strftime("%Y",current_time)
   the_month = time.strftime("%m",current_time)
   day_num = int(time.strftime("%d",current_time))
   time_beg_month = time.mktime(time.strptime(the_month+' '+the_year,"%m %Y"))
   
   next_month = time.strftime("%m",time.localtime(time_beg_month+33*86400)) # Gets next month
   next_year = time.strftime("%Y",time.localtime(time_beg_month+33*86400)) # Gets next year
   time_end_month = time.mktime(time.strptime(str(int(next_month))+' '+next_year,"%m %Y"))

   # Name of the month we are in
   the_month = time.strftime("%b",time.localtime(time.time()))

   # Initialize the vectors
   time_vec = []      # vector of times of each call
   call_vec = []      # vector of type of call
   user_vec = []      # Vector of user names
   country_vec = []   # Vector of countries if using GeoIP_tool
   submit_vec = []    # Vector of times of simulation submisssion
   wait_vec = []      # Vector of times when wait time was returned
   wait_times = []    # Vector of wait times
   run_times = []     # Vector of run times
   run_vec = []       # Vector of times when run time was returned
   stop_vec = []      # Vector of user stopped sims
   error_vec = []     # Vector of times of error occurances
   timeout_vec = []   # Vector of timeouts
   crash_vec = []     # Vector of crashes
   io_vec = []        # Vector of io errors
   qlong_vec = []     # Vector of queue length violations
   success_vec = []   # Vector of succesful sims
   cache_vec = []     # Vector of cache returns

   # Get the simulation data -----------------------------------------
   for row in open(LOGFILE,'rt'):

     # Get the time string
     the_time = row[0:24]

     # Change time to a number in seconds
     try:
       time_vec.append(time.mktime(time.strptime(the_time, "%a %b %d %H:%M:%S  %Y")))

       # Get the rest of the info from the call
       the_rest = row[25:].split(': ')
     except:
       the_rest = 'READERROR'     


     # Call type
     call_type = the_rest[0].strip()
     if call_type == '>> Error':
        error_vec.append(time_vec[-1])
        call_vec.append(-1)
        err_code = the_rest[-1]
        if err_code.startswith(timeout) :
           timeout_vec.append(time_vec[-1])
        elif err_code.startswith(qlong) :
           qlong_vec.append(time_vec[-1])
        elif err_code.startswith(crash) :
           crash_vec.append(time_vec[-1])
        elif err_code.startswith(io) :
           io_vec.append(time_vec[-1])
     elif call_type.startswith('Enquire'):
        call_vec.append(0)
     elif call_type.startswith('Submit'):
        call_vec.append(1)
        submit_vec.append(time_vec[-1])
     elif call_type.startswith('Figure'):
        call_vec.append(2)
     elif call_type.startswith('Replot'):
        call_vec.append(3)
     elif call_type.startswith('Clean'):
        call_vec.append(4)
     elif call_type.startswith('run'):
        if 'wait time' in the_rest[1]:
           call_vec.append(5)
           try:
              wait_times.append(float(the_rest[3]))
              wait_vec.append(time_vec[-1])
           except:
              pass
        elif 'run time' in the_rest[1]:
           call_vec.append(6)
           try:
              run_times.append(float(the_rest[3]))
              run_vec.append(time_vec[-1])
           except:
              pass
        elif 'kill' in the_rest[1]:
           stop_vec.append(time_vec[-1])
           call_vec.append(7)
        elif 'queue' in the_rest[1]:
           stop_vec.append(time_vec[-1])
           call_vec.append(7)
        elif 'success' in the_rest[1]:
           call_vec.append(8)
           success_vec.append(time_vec[-1])
        elif 'cache' in the_rest[1]:
           call_vec.append(10)
           cache_vec.append(time_vec[-1])
        else:
           call_vec.append(9)
        
     # If the user is named put it in the user vector
     if len(the_rest) > 1:
        this_user = the_rest[1].strip()
        if 'user' in this_user:
           this_user = this_user.split()
           this_user = this_user[3]
           user_vec.append(this_user)


   # Get the unique users
   user_list = list(set(user_vec))

   if GeoIP_tool:
      for user in user_list:
         country_vec.append(GeoIP.country(user,'GeoIP.dat'))
           
      # Get the unique countries
      countries = Counter(country_vec)

 

   # Bin the data -----------------------------------------------------

   # The bins are each day of the month
   bin_ends = np.linspace(time_beg_month,time_end_month,round((time_end_month-time_beg_month)/bin_width+1))
   bins = ( bin_ends[1:] + bin_ends[0:-1] )*0.5

   # Bin the simulation submissions and errors
   simulations, bins2 = np.histogram(submit_vec,bins = bin_ends)
   errors, bins2 = np.histogram(error_vec,bins = bin_ends)

   max_wait_time = []
   median_wait_time = []
   mean_wait_time = []

   max_run_time = []
   min_run_time = []
   mean_run_time = []
   # Find the median and maximum wait times
   for i in range(0,len(bin_ends)-1):
      
      day_wait_times = []
      for j in range(0,len(wait_vec)):
         if wait_vec[j] >= bin_ends[i] and wait_vec[j] <  bin_ends[i+1]:
            day_wait_times.append(wait_times[j])


      try:
         max_wait_time.append(max(day_wait_times))
         median_wait_time.append(np.median(day_wait_times))
         mean_wait_time.append(np.mean(day_wait_times))
      except:
         max_wait_time.append(0)
         median_wait_time.append(0)
         mean_wait_time.append(0)


      day_run_times = []
      for j in range(0,len(run_vec)):
         if run_vec[j] >= bin_ends[i] and run_vec[j] <  bin_ends[i+1]:
            day_run_times.append(run_times[j]*1000)

      try:
         max_run_time.append(max(day_run_times))
         min_run_time.append(min(day_run_times))
         mean_run_time.append(np.mean(day_run_times))
      except:
         max_run_time.append(0)
         min_run_time.append(0)
         mean_run_time.append(0)


   # PLot the data ----------------------------------------------------

   
   leg_loc = 1 if day_num < 25 else 2
   tit = 'RC model simulation history: '+the_month+' '+the_year


   xtickmarks = []
   xticks = []
   for t in bin_ends:
      if ( t -time_beg_month ) % 86400 == 0:
         xtickmarks.append(time.strftime("%d",time.localtime(t)))
         xticks.append(t)


   # Make the figure ----------------------------------------
   fig = mp.figure(figsize=(15, 9))
   ax = fig.add_subplot(311)

   ax.set_position([0.07,0.7,0.7,0.25])

   sims = ax.bar(bin_ends[0:-1]+0.05*bin_width,simulations,width=0.9*bin_width,edgecolor='none')
   errs = ax.bar(bin_ends[0:-1]+0.25*bin_width,errors,width=0.5*bin_width,color='red',edgecolor='none')


   ax.set_xlim(bin_ends[0],bin_ends[-1])
   ax.set_xticks(xticks)
   ax.set_title(tit,fontsize=18)
   ax.set_ylabel('No. of submissions',fontsize=12)
   xticknames = ax.set_xticklabels([])
   ax.legend( (sims[0], errs[0]), ('Submissions', 'errors'), loc=leg_loc)
   mp.setp(ax.get_legend().get_texts(), fontsize='10')

   # ---------------------

   ax2 = fig.add_subplot(312)
   ax2.set_position([0.07,0.4,0.7,0.25])
   maxwait = ax2.bar(bin_ends[0:-1]+0.05*bin_width,max_wait_time,width=0.9*bin_width,color='blue',edgecolor='none')
   meanwait = ax2.bar(bin_ends[0:-1]+0.05*bin_width,mean_wait_time,width=0.9*bin_width,color='red',edgecolor='none')
   medwait = ax2.bar(bin_ends[0:-1]+0.05*bin_width,median_wait_time,width=0.9*bin_width,color='green',edgecolor='none')

   ax2.set_xlim(bin_ends[0],bin_ends[-1])

   ax2max = max([max([float(i) for i in max_wait_time])*1.2, 1])
   ax2.set_ylim(0,ax2max)

   ax2.set_xticks(xticks)
   xticknames = ax2.set_xticklabels([],fontsize=10)
   ax2.legend( (maxwait[0], meanwait[0],medwait[0]), ('Maximum', 'Mean','Median'), loc=leg_loc)
   mp.setp(ax2.get_legend().get_texts(), fontsize='10')
   ax2.set_ylabel('Wait time (sec)',fontsize=12)

   ax2.set_title('Queue wait times',fontsize=18)

   # -------------------------------

   ax3 = fig.add_subplot(313)
   ax3.set_position([0.07,0.1,0.7,0.25])

   maxrun = ax3.bar(bin_ends[0:-1]+0.05*bin_width,max_run_time,width=0.9*bin_width,color='blue',edgecolor='none')
   meanrun = ax3.bar(bin_ends[0:-1]+0.05*bin_width,mean_run_time,width=0.9*bin_width,color='red',edgecolor='none')
   minrun = ax3.bar(bin_ends[0:-1]+0.05*bin_width,min_run_time,width=0.9*bin_width,color='green',edgecolor='none')

   ax3.set_xlim(bin_ends[0],bin_ends[-1])

   ax3max = max([max([float(i) for i in max_run_time])*1.2, 0.01])
   ax3.set_ylim(0,ax3max)

   ax3.set_xlabel('Day of month',fontsize=16)
   ax3.set_xticks(xticks)
   xticknames = ax3.set_xticklabels(xtickmarks,fontsize=10)
   ax3.legend( (maxrun[0], meanrun[0],minrun[0]), ('Maximum', 'Mean','Minimum'), loc=leg_loc )
   mp.setp(ax3.get_legend().get_texts(), fontsize='10')
   ax3.set_ylabel('Run time (sec/1000 days)',fontsize=12)

   ax3.set_title('Running times',fontsize=18)


   # -----------------------------

   #mp.setp(xticknames, rotation=90, fontsize=10)
   string = 'Total submissions: '
   ax.text(1.025, 0.75, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(submit_vec)))
   ax.text(1.2, 0.75, string, fontsize=15,transform=ax.transAxes)

   string = 'Successful: '
   ax.text(1.045, 0.62, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(success_vec)))
   ax.text(1.2, 0.62, string, fontsize=15,transform=ax.transAxes)

   string = 'Cached: '
   ax.text(1.045, 0.51, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(cache_vec)))
   ax.text(1.2, 0.51, string, fontsize=15,transform=ax.transAxes)

   string = 'Stopped by user: '
   ax.text(1.045, 0.40, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(stop_vec)))
   ax.text(1.2, 0.40, string, fontsize=15,transform=ax.transAxes)

   string = 'Total Errors: '
   ax.text(1.025, 0.22, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(error_vec)))
   ax.text(1.2, 0.22, string, fontsize=15,transform=ax.transAxes)

   string = 'Crashes: '
   ax.text(1.045, 0.09, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(crash_vec)))
   ax.text(1.2, 0.09, string, fontsize=15,transform=ax.transAxes)

   string = 'Timeouts: '
   ax.text(1.045, -0.02, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(timeout_vec)))
   ax.text(1.2, -0.02, string, fontsize=15,transform=ax.transAxes)

   string = 'Queue too long: '
   ax.text(1.045, -0.13, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(qlong_vec)))
   ax.text(1.2, -0.13, string, fontsize=15,transform=ax.transAxes)

   string = 'I/O errors: '
   ax.text(1.045, -0.24, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(io_vec)))
   ax.text(1.2, -0.24, string, fontsize=15,transform=ax.transAxes)


   string = 'Total Users: '
   ax.text(1.025, -0.42, string, fontsize=15,transform=ax.transAxes)
   string = '{:>8}'.format(str(len(user_list)))
   ax.text(1.2, -0.42, string, fontsize=15,transform=ax.transAxes)

   string = 'All times local to Boston, MA, USA.'
   ax.text(1.025, -0.65, string, fontsize=12,transform=ax.transAxes)

   string = 'Updated: '+time.ctime(time.time())
   ax.text(1.025, -0.73, string, fontsize=12,transform=ax.transAxes)

   if GeoIP_tool and len(countries) > 0:

      # Now make the pie chart if using GeoIP
      axP = mp.axes([0.83, 0.1, 0.12, 0.21])
   

      # The slices will be ordered and plotted counter-clockwise.
      labels = countries.keys()
      fracs = [int(i) for i in countries.values()]

      mp.rcParams['font.size'] = 9
      patches, texts, autotexts =  mp.pie(fracs, labels=labels,
                                    autopct='%d %%', shadow=False, startangle=0)

      mp.title('Users by Country',fontsize=10)

   hf = mp.savefig(REPORTDIR+the_year+'_'+the_month+'.png')
   hf = mp.savefig(REPORTDIR+'latest.png')

   mp.close(fig)

   return
