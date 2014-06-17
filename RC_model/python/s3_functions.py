import boto
import boto.s3.connection
import os

bucket_name = 'eaps-rc-model-12340x'
access_key  = 'AKIAIYH3XSY6GZOXJ3GA'
secret_key  = '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'




def send_to_bucket(s3_file,dirname):


   # Connect to the S3 bucket
   conn = boto.connect_s3(
        aws_access_key_id = access_key,
        aws_secret_access_key = secret_key,
        calling_format = boto.s3.connection.OrdinaryCallingFormat(),
        )


   bucket = conn.create_bucket(bucket_name)

   key = bucket.new_key(s3_file+'/botbar.out')
   key.set_contents_from_filename(dirname+'/botbar.out')

   key = bucket.new_key(s3_file+'/lwbar.out')
   key.set_contents_from_filename(dirname+'/lwbar.out')

   key = bucket.new_key(s3_file+'/shfbar.out')
   key.set_contents_from_filename(dirname+'/shfbar.out')

   key = bucket.new_key(s3_file+'/sstbar.out')
   key.set_contents_from_filename(dirname+'/sstbar.out')

   key = bucket.new_key(s3_file+'/swbar.out')
   key.set_contents_from_filename(dirname+'/swbar.out')

   key = bucket.new_key(s3_file+'/table.out')
   key.set_contents_from_filename(dirname+'/table.out')

   key = bucket.new_key(s3_file+'/time.out')
   key.set_contents_from_filename(dirname+'/time.out')

   key = bucket.new_key(s3_file+'/topbar.out')
   key.set_contents_from_filename(dirname+'/topbar.out')

   key = bucket.new_key(s3_file+'/params.in')
   key.set_contents_from_filename(dirname+'/params.in')

   key = bucket.new_key(s3_file+'/profile.out')
   key.set_contents_from_filename(dirname+'/profile.out')

   key = bucket.new_key(s3_file+'/sounding.in')
   key.set_contents_from_filename(dirname+'/sounding.in')

   key = bucket.new_key(s3_file+'/sounding.out')
   key.set_contents_from_filename(dirname+'/sounding.out')

   key = bucket.new_key(s3_file+'/log.out')
   key.set_contents_from_filename(dirname+'/log.out')



def get_from_bucket(s3_file,dirname):


   # Connect to the S3 bucket
   conn = boto.connect_s3(
        aws_access_key_id = access_key,
        aws_secret_access_key = secret_key,
        calling_format = boto.s3.connection.OrdinaryCallingFormat(),
        )

   bucket = conn.create_bucket(bucket_name)
   file_list = bucket.list(s3_file)
   if file_list:
     try:

      key = bucket.new_key(s3_file+'/botbar.out')
      key.get_contents_to_filename(dirname+'/botbar.out')

      key = bucket.new_key(s3_file+'/lwbar.out')
      key.get_contents_to_filename(dirname+'/lwbar.out')

      key = bucket.new_key(s3_file+'/shfbar.out')
      key.get_contents_to_filename(dirname+'/shfbar.out')

      key = bucket.new_key(s3_file+'/sstbar.out')
      key.get_contents_to_filename(dirname+'/sstbar.out')

      key = bucket.new_key(s3_file+'/swbar.out')
      key.get_contents_to_filename(dirname+'/swbar.out')

      key = bucket.new_key(s3_file+'/table.out')
      key.get_contents_to_filename(dirname+'/table.out')

      key = bucket.new_key(s3_file+'/time.out')
      key.get_contents_to_filename(dirname+'/time.out')

      key = bucket.new_key(s3_file+'/topbar.out')
      key.get_contents_to_filename(dirname+'/topbar.out')

      key = bucket.new_key(s3_file+'/params.in')
      key.get_contents_to_filename(dirname+'/params.in')

      key = bucket.new_key(s3_file+'/profile.out')
      key.get_contents_to_filename(dirname+'/profile.out')

      key = bucket.new_key(s3_file+'/sounding.in')
      key.get_contents_to_filename(dirname+'/sounding.in')

      key = bucket.new_key(s3_file+'/sounding.out')
      key.get_contents_to_filename(dirname+'/sounding.out')

      key = bucket.new_key(s3_file+'/log.out')
      key.get_contents_to_filename(dirname+'/log.out')


      return 0
     except:
      
      return 9
   else:
     return 1


def cache_name(form):
# Construct a file name for sending to the S3 bucket

   if 'restart' in form:
       name = ''
       return

   keys_list = ['days','time_step','avg_time','graph_time','rad_type','month','day','hour','rad_freq','rad_wv','rad_cld','S0','theta','co2','ch4','n2o','cfc11','cfc12','dry_conv','moist_conv','turb_flux','water_frac','ugust','ml_depth','alpha','w_cubic','w_max','w_T','w_top','w_bot','w_p','p_pbl','SSTi']

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

  
      name += '_'+key+'-' 
      name += keyval 



   return name










