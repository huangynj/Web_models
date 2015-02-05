import shutil
import os
import traceback


def send_to_local_cache(cache_dir,dirname):

   
  try: 
   ret = os.makedirs(cache_dir)

   shutil.copy (dirname+'/botbar.out',cache_dir+'/botbar.out')
   shutil.copy (dirname+'/lwbar.out',cache_dir+'/lwbar.out')
   shutil.copy (dirname+'/shfbar.out',cache_dir+'/shfbar.out')
   shutil.copy (dirname+'/sstbar.out',cache_dir+'/sstbar.out')
   shutil.copy (dirname+'/swbar.out',cache_dir+'/swbar.out')
   shutil.copy (dirname+'/table.out',cache_dir+'/table.out')
   shutil.copy (dirname+'/time.out',cache_dir+'/time.out')
   shutil.copy (dirname+'/topbar.out',cache_dir+'/topbar.out')
   shutil.copy (dirname+'/params.in',cache_dir+'/params.in')
   shutil.copy (dirname+'/profile.out',cache_dir+'/profile.out')
   shutil.copy (dirname+'/sounding.out',cache_dir+'/sounding.out')
   shutil.copy (dirname+'/log.out',cache_dir+'/log.out')
   return ret
  except:
   return str(traceback.format_exc())

def get_from_local_cache(cache_dir,dirname):


  
   if os.path.exists(cache_dir):
     try:
       shutil.copy (cache_dir+'/botbar.out',dirname+'/botbar.out')
       shutil.copy (cache_dir+'/lwbar.out',dirname+'/lwbar.out')
       shutil.copy (cache_dir+'/shfbar.out',dirname+'/shfbar.out')
       shutil.copy (cache_dir+'/sstbar.out',dirname+'/sstbar.out')
       shutil.copy (cache_dir+'/swbar.out',dirname+'/swbar.out')
       shutil.copy (cache_dir+'/table.out',dirname+'/table.out')
       shutil.copy (cache_dir+'/time.out',dirname+'/time.out')
       shutil.copy (cache_dir+'/topbar.out',dirname+'/topbar.out')
       shutil.copy (cache_dir+'/params.in',dirname+'/params.in')
       shutil.copy (cache_dir+'/profile.out',dirname+'/profile.out')
       shutil.copy (cache_dir+'/sounding.out',dirname+'/sounding.out')
       shutil.copy (cache_dir+'/log.out',dirname+'/log.out')
       return 0
     except:
       return str(traceback.format_exc())
       
   else:
     return 1


def cache_name(form):
# Construct a file name for sending to the S3 bucket

   keys_list = ['days','time_step','avg_time','graph_time','rad_type','month','day','hour','rad_freq','rad_wv','rad_cld','S0','theta','co2','ch4','n2o','cfc11','cfc12','dry_conv','moist_conv','turb_flux','water_frac','ugust','ml_depth','alpha','w_cubic','w_max','w_T','w_top','w_bot','w_p','p_pbl','SSTi']

   # Don't cache if:
   dont_cache = 0

   if 'restart'     in form: 			dont_cache = 1	# This is a restart
   if 'surf_int'    in form: 			dont_cache = 1	# The surface temperature is fixed
   if 'calc_albedo' in form:			dont_cache = 1 	# aledo is calculated
   if 'forcing_opt' in form:			dont_cache = 1	# We are running with forcing
   if form["time_step"].value   != '10':	dont_cache = 1  # time step is different from 10 minutes
   if form["cfc11"].value       != '280.0':	dont_cache = 1  # cfc11 concentration is different from default
   if form["cfc12"].value       != '484.0':	dont_cache = 1  # cfc12 concentration is different from default
   if form["avg_time"].value    != '25':	dont_cache = 1  # avg time is different from 25 days
   if form["graph_time"].value  != '6':		dont_cache = 1  # graph time is different from 6 hours

   if dont_cache == 1:
       name = ''
       return str(name)

   # Eventually will need to use /data partition as well
   # currently does not work due to permissions problem
   #days = form['days'].value
   #if float(days) > 750:
   #   name = '../../../data/RC2_'
   #else:
   #   name = 'RC_'
    
   name = 'RC_'
 
   for key in keys_list:

      if key == 'rad_wv':
         keyval  = 'n' if "rad_wv" in form else 'y'
      elif key == 'dry_conv':
        keyval   = 'y' if "dry_conv"   in form else 'n'
      elif key == 'moist_conv':
        keyval   = 'y' if "moist_conv" in form else 'n'
      elif key == 'turb_flux':
        keyval   = 'y' if "turb_flux"  in form else 'n'
      elif key == 'surf_int':
        keyval   = 'n' if "surf_int"    in form else 'y'
      elif key == 'calc_albedo':
        keyval   = 'y' if "calc_albedo" in form else 'n'
      elif key == 'w_cubic':
        keyval   = 'y' if "w_cubic" in form else 'n'
      else:
         keyval = form[key].value
      
 
         
      name += '/'+key+'-' 
      name += keyval 



   return str(name)










