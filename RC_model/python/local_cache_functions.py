import shutil
import os



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
   return 9

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
      
       return 9
   else:
     return 1


def cache_name(form):
# Construct a file name for sending to the S3 bucket

   if 'restart' in form or 'surf_int' in form:
       name = ''
       return str(name)

   keys_list = ['days','time_step','avg_time','graph_time','rad_type','month','day','hour','rad_freq','rad_wv','rad_cld','S0','theta','co2','ch4','n2o','cfc11','cfc12','dry_conv','moist_conv','turb_flux','water_frac','ugust','ml_depth','alpha','w_cubic','w_max','w_T','w_top','w_bot','w_p','p_pbl','SSTi']

   days = form['days'].value
   if float(days) > 750:
     name = 'RC2_'
   else:
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
      elif key == 'wtg':
         keyval  = 'y' if "wtg" in form else 'n'
      else:
         keyval = form[key].value

  
      name += '/'+key+'-' 
      name += keyval 



   return str(name)










