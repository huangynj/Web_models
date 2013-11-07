#!/usr/bin/python

# This script runs the RC model.
# Can be used to prime the cache
#



import urllib2
import urllib 
import json 

form_url = 'http://eaps-prod.mitx.mit.edu/rc'

# These are the default values - change them below
jdata = {
                      "days"      :  "50"  ,
                      "co2"       :  "360"  ,
                      'dry_conv'  :  'on'   , 
                      'n2o'       :  '310'  ,
                      'w_max'     :  '0.0'  ,
                      'S0'        :  '1360' ,
                      'month'     :  '3'    ,
                      'graph_time':  '6'    , 
                      'time_step' :  '10'   ,
                      'dirname'   :  './batch_run/', 
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
                      'SSTi'      :  '21'   , 
                      'w_top'     :  '100'  , 
                      'w_T'       :  '1000' , 
                      'alpha'     :  '0.25' , 
                      'rad_type'  :  '0'    , 
                      'ugust'     :  '5'    ,
                      'rad_freq'  :  '1'    ,
                      'hour'      :  '0.0'  , 
                      'ch4'       :  '1.7'  , 
                      'p_pbl'     :  '850'  , 
                      'ml_depth'  :  '1.0'
                   }

# Start any looping here

# Change the parameters like this
# jdata["days"] = str(500)



data = urllib.urlencode(jdata)
req = urllib2.Request(form_url,data)
req.add_header("Content-type", "application/x-www-form-urlencoded")

response = urllib2.urlopen(req)
print response.read()







