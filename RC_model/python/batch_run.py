#!/usr/bin/python

# This script runs the RC model.
# Can be used to prime the cache
#
import time

import urllib2
import urllib 
import json 


#form_url = 'http://192.168.33.43/rc'

form_url = 'http://eaps-prod.mitx.mit.edu/rc'



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

days = [1000, 600, 700, 800, 900, 1000]
co2 =  [50, 100, 200, 500] 
S0 =  [1356, 1358, 1360, 1362, 1364, 1366, 1368, 1370] 
albedo =  [0.1, 0.2, 0.3, 0.4, 0.5, 0.6] 

sst = 21
idays = 2

for ico2 in range(0,3):
  for ialb in range(0,5):
    for iS0 in range(0,7):
      for idays in range(0,5):


        # Change the parameters like this
        jdata["days"] = str(days[idays])
        jdata["co2"] = str(co2[ico2])
        jdata["alpha"] = str(albedo[ialb])
        jdata["S0"] = str(S0[iS0])
        jdata["SSTi"] = str(sst)

        print jdata

        data = urllib.urlencode(jdata)
        req = urllib2.Request(form_url,data)
        req.add_header("Content-type", "application/x-www-form-urlencoded")

        response = urllib2.urlopen(req)
        print response.read()
    
        #exit()
        time.sleep(1)



