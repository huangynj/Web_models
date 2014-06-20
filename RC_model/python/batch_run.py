#!/usr/bin/python

# This script runs the RC model.
# Can be used to prime the cache
#
import time

import urllib2
import urllib 
import json 


#form_url = 'http://192.168.33.43/rc'

form_url = 'http://12.340x.scripts.mit.edu/eaps-rc-model-12340x/RC_model/python/run_rc_model.fcgi'



# These are the default values - change them below
jdata = {
                      "days"      :  "500"  ,
                      "co2"       :  "360"  ,
                      'dry_conv'  :  'on'   , 
                      'n2o'       :  '310'  ,
                      'w_max'     :  '0.0'  ,
                      'S0'        :  '1360' ,
                      'month'     :  '3'    ,
                      'graph_time':  '6'    , 
                      'time_step' :  '10'   ,
                      'dirname'   :  'temp/test', 
                      'w_p'       :  '500.0', 
                      'avg_time'  :  '25'   ,
                      'cfc11'     :  '280.0',
                      'cfc12'     :  '484.0', 
                      'turb_flux' :  'on'   ,
                      'water_frac':  '1.0'  ,
                      'moist_conv':  'on'   ,
                      'day'       :  '1'    , 
                      'rad_cld'   :  '0'    ,
                      'theta'     :  '15'   , 
                      'w_bot'     :  '1000' ,
                      'SSTi'      :  '12'   , 
                      'w_top'     :  '100'  , 
                      'w_T'       :  '1000' , 
                      'alpha'     :  '0.2' , 
                      'rad_type'  :  '0'    , 
                      'ugust'     :  '5'    ,
                      'rad_freq'  :  '1'    ,
                      'hour'      :  '0.0'  , 
                      'ch4'       :  '1.7'  , 
                      'p_pbl'     :  '850'  , 
                      'ml_depth'  :  '1.0'
                   }

# Start any looping here

days = [500, 1000]
co2 =  [50, 100, 200, 360, 400, 2000, 4000] 
S0 =  [1000, 1100, 1200,1300,1400, 1500] 
albedo =  [0.2] 

sst = [15]

for ico2 in range(0,7):
  for ialb in range(0,1):
    for iS0 in range(0,6):
      for idays in range(0,2):
        for isst in range(0,1):


         # Change the parameters like this
         jdata["days"] = str(days[idays])
         jdata["co2"] = str(co2[ico2])
         jdata["alpha"] = str(albedo[ialb])
         jdata["S0"] = str(S0[iS0])
         jdata["SSTi"] = str(sst[isst])

         print jdata

         data = urllib.urlencode(jdata)
         req = urllib2.Request(form_url,data)
         req.add_header("Content-type", "application/x-www-form-urlencoded")
    
         try:
           response = urllib2.urlopen(req)
           print response.read()
         except:
           print 'received error! skipping this simulation and waiting 3 seconds'
           time.sleep(60)
    
         time.sleep(5)



