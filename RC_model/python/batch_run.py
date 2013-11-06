#!/usr/bin/python


import urllib2
import urllib 
import json 

form_url = 'https://eaps.mitx.mit.edu/rc'


jdata = {
                      "days"      :  "100"  ,
                      "co2"       :  "720"  ,
                      'dry_conv'  :  'on'   , 
                      'n2o'       :  '310'  ,
                      'w_max'     :  '0.0'  ,
                      'S0'        :  '1360' ,
                      'month'     :  '3'    ,
                      'graph_time':  '6'    , 
                      'time_step' :  '10'   ,
                      'dirname'   :  'temp/tmphuKmMJ', 
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
                      'alpha'     :  '0.15' , 
                      'rad_type'  :  '0'    , 
                      'ugust'     :  '5'    ,
                      'rad_freq'  :  '1'    ,
                      'hour'      :  '0.0'  , 
                      'ch4'       :  '1.7'  , 
                      'p_pbl'     :  '850'  , 
                      'ml_depth'  :  '1.0'
                   }


data = urllib.urlencode(jdata)
print data
req = urllib2.Request(form_url, data)

response = urllib2.urlopen(req)
print response







