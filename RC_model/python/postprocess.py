#!/usr/bin/python

# some post processing scripts

import os;          os.environ['MPLCONFIGDIR'] = '/tmp/'
import numpy as np
import matplotlib;  matplotlib.use('agg')
import pylab as mp

def output_control(form,dirname,json_output):



	# Parse plotting options ############################################

        diff_plot = "false"
        if type(form) is dict:
           plot_opt = form["plot_opt"]

        else:
           
   	   plot_opt = form["plot_type"].value

	   if plot_opt == "time_opts":
	   	plot_opt = form["timeplot"].value
	   elif plot_opt == "prof_opts":
		plot_opt = form["profplot"].value
 	   elif plot_opt == "hov_opts":
		plot_opt = form["hovplot"].value
	   elif plot_opt == "diff_opts":
		plot_opt = form["diffplot"].value
                diff_plot = "true"
	   else:	
		json_output['html'] = '<h2>Plotting error</h2><p> Unknown plotting option </html>'
                json_output['alert'] = 'Plotting error: Unknown plotting option'
                return json_output
      
	# get output data ####################################################
	outdir = dirname+'/'
        
        try:
	   time = np.array(get_output(outdir+'time.out'))
   	   profile = np.array(get_output(outdir+'profile.out'))

           # Not plotting the time-height sections anymore
	   #cldhov = np.array(get_output(outdir+'cldhov.out'))
	   #thov = np.array(get_output(outdir+'thov.out'))
	   #rhhov = np.array(get_output(outdir+'rhhov.out'))
	   #mhov = np.array(get_output(outdir+'mhov.out'))
	   #mphov = np.array(get_output(outdir+'mphov.out'))
	   #buoy = np.array(get_output(outdir+'buoy.out'))
	   #mhov = np.array(get_output(outdir+'omhov.out'))


           if (os.path.isfile(dirname+'/profile.old')):
   	      profileold = np.array(get_output(outdir+'profile.old'))

        except:
	   json_output['html'] = '<h2>Plotting error</h2><p> Cannot find output </html>'
           json_output['alert'] = 'Plotting error: Cannot find output.\nYour session may have expired.\n\nIf you leave the model idle for longer than 20 minutes the output may be cleaned up '
           return json_output

	# Convert to variables we require
	p=profile[:,0];
	time1=time[:,0];
	timem=time[:,11];
	tout=np.mean(time[:,12]);
	tk=profile[:,1]+273.15;
	qm=0.001*profile[:,2];
	tv=tk*(1+0.608*qm);
	a=len(p);

	z=np.array([0] * a)

	for i in range(1,a):
		z[i] = z[i-1] + (287/9.81)*0.5*(tv[i]+tv[i-1])*np.log(p[i-1]/p[i]);

	zkm=0.001*z;

        if (os.path.isfile(dirname+'/profile.old')):

	   p=profileold[:,0];
	   tk=profileold[:,1]+273.15;
	   qm=0.001*profileold[:,2];
	   tv=tk*(1+0.608*qm);
	   a=len(p);
   	   zold=np.array([0] * a)

	   for i in range(1,a):
		zold[i] = zold[i-1] + (287/9.81)*0.5*(tv[i]+tv[i-1])*np.log(p[i-1]/p[i]);

	   zoldkm=0.001*zold;

        #LOG(str(profileold[:,1]))
	if (diff_plot == "true"):
           profile = profile - profileold
    
	# Plot the figure that they want #############################################


	mp.rcParams.update({'font.size': 16})
	mp.rcParams.update({'lines.linewidth': 2})
	mp.rcParams.update({'axes.linewidth': 2})

        fig = mp.figure()
	if plot_opt == "time_sst":
		filename = dirname+"/plot1.png"
        	hp1=plot_timeseries(time1,time[:,3:5],'SST (deg. C)','Surface temperature') # col3: air temp, col4: sst
		mp.legend(["Air Temp","SST"],loc='best' )
                l = mp.gca().get_legend(); mp.setp(l.get_texts(), fontsize=12)
		hf = mp.savefig(filename)

	elif plot_opt == "time_precip":
		filename = dirname+"/plot1.png"
                plot_timeseries(timem,time[:,1:3],'(mm/day)','Precipitation & Evaporation') # col1: precip, col2: evap
		mp.legend(["precip","evap"],loc='best' )
                l = mp.gca().get_legend(); mp.setp(l.get_texts(), fontsize=12)
		hf = mp.savefig(filename)

	elif plot_opt == "time_T500":
		filename = dirname+"/plot1.png"
                plot_two_timeseries(time1,time[:,5],time[:,6],'T (C)','q (g/kg)','500mb Temperature and humidity') # col5: T500, col6: q500
		hf = mp.savefig(filename)

	elif plot_opt == "time_radflx":
		filename = dirname+"/plot1.png"
                plot_timeseries(time1,time[:,7:9],'W/m^2','TOA SW and LW fluxes') # col7: sw, col8: lw
		mp.legend(["SW","LW"],loc='best' )
                l = mp.gca().get_legend(); mp.setp(l.get_texts(), fontsize=12)
		hf = mp.savefig(filename)

	elif plot_opt == "profile_cld":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,15],p,'CF','Cloud fraction') # col15: cld fraction
		hf = mp.savefig(filename)

	elif plot_opt == "profile_clw":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,16],p,r'$q_w$ g/kg','Cloud condensed water') # col17: cld water
		hf = mp.savefig(filename)

	elif plot_opt == "profile_t":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,1],p,'Temp (deg. C)','Temperature profile') # col1: temp
		hf = mp.savefig(filename)

	elif plot_opt == "profile_q":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,2],p,r'$q$ (g/kg)','Specific humidity profile') # col2: q
		hf = mp.savefig(filename)

	elif plot_opt == "profile_b":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,3],p,r'$b$ (C)','Buoyancy') # col3: buoy
		hf = mp.savefig(filename)

	elif plot_opt == "profile_rh":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,4]*100,p,r'$RH$ (%)','Relative humidity') # col4: RH
		hf = mp.savefig(filename)

	elif plot_opt == "profile_heating":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,5:9],p,'Heating (C/day)','Heating rate') # col5: conv, col6: rad, col7: turb, col8: lscale
                mp.legend(['Convective','Radiative','Turbulent', 'Large-scale adiabatic'],loc='best')
                l = mp.gca().get_legend(); mp.setp(l.get_texts(), fontsize=12)
		hf = mp.savefig(filename)

	elif plot_opt == "profile_massflux":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,9:12],p,r'Mass flux (10$^{-3}$ kg/m$^2$)','Convective mass flux') # col9: up, col10: pen, col11: down
                mp.legend(['Net upward','Penetrative','Unsaturated'],loc='best')
                l = mp.gca().get_legend(); mp.setp(l.get_texts(), fontsize=12)
		hf = mp.savefig(filename)

	elif plot_opt == "profile_entrainment":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,12:14],p,r'(10$^{-3}$ kgm$^{-2}$s$^{-1}$)','Entrainment and detrainment') # col12, entrain, col13: detrain
                mp.legend(['Entrainment','Detrainment'],loc='best')
                l = mp.gca().get_legend(); mp.setp(l.get_texts(), fontsize=12)
		hf = mp.savefig(filename)

	elif plot_opt == "profile_MSE":
		filename = dirname+"/plot1.png"
		plot_profile(profile[:,14]*1.0057,p,'MSE (kJ/kg)','Moist static energy') # col14, MSE
		mp.xlim((min(profile[:,14])-5)*1.0057,(profile[0,14]+20)*1.0057)
		hf = mp.savefig(filename)

	elif plot_opt == "hov_t":
		filename = dirname+"/plot1.png"
                tht=thov.transpose();
		plot_hov(time1,p,tht,'','Temperature (K)')
		hf = mp.savefig(filename)

	elif plot_opt == "hov_q":
		filename = dirname+"/plot1.png"
                rht=rhhov.transpose();
		plot_hov(time1,p,rht,'','Relative humidity (%)')
		hf = mp.savefig(filename)

	elif plot_opt == "hov_cld":
		filename = dirname+"/plot1.png"
                cld=cldhov.transpose();
		plot_hov(time1,zkm,cld,'','Cloud fraction')
		hf = mp.savefig(filename)

	elif plot_opt == "hov_buoy":
		filename = dirname+"/plot1.png"
                buoy=buoy.transpose();
		buoy[buoy < -0.1] = -0.1
		plot_hov(timem,zkm,buoy,r'$b$ (K)','Parcel buoyancy')
		mp.ylim((0,20))
		hf = mp.savefig(filename)

	else:
		json_output['html'] = '<h2>Plotting error</h2><p> Unknown plotting option </html>'
                json_output['alert'] = 'Plotting error: Unknown plotting option'
                return json_output

 
        mp.close(fig)
        json_output['fig_file'] = filename
	return json_output

######################################################################

def get_output(filename):

	outvar = []
        for row in open(filename,'rt'):
		#print row
		row_data = [float (x) for x in row.split()]
		outvar.append(row_data)

	return outvar


def plot_timeseries(x,y,ylab,tit):


	hp = mp.plot(x,y)
	mp.xlabel('time (days)')
	mp.ylabel(ylab)
	mp.title(tit)

	return hp

def plot_two_timeseries(x,y1,y2,ylab1,ylab2,tit):

        rect = [0.15,0.1,0.75,0.8]
        a1=mp.axes(rect)
        a1.yaxis.tick_left()
	hp = mp.plot(x,y1)
	mp.xlabel('time (days)')
	mp.ylabel(ylab1)
	mp.title(tit)

   
        a2=mp.axes(rect,frameon=False)
        a2.yaxis.tick_right()
        hp2=mp.plot(x,y2,'g')
        a2.yaxis.set_label_position('right')
        mp.ylabel(ylab2)
        a2.set_xticks([])

	mp.legend([hp[0],hp2[0]],[r'$T$',r'$q$'],loc='best' )
        l = mp.gca().get_legend(); mp.setp(l.get_texts(), fontsize=12)



	return hp


def plot_profile(x,y,xlab,tit):


	hp = mp.plot(x,y)
	mp.xlabel(xlab)
	mp.ylabel('Pressure (hPa)')
        mp.title(tit)
	mp.gca().invert_yaxis()

	return hp


def plot_hov(x,y,z,ylab,tit):

	CS = mp.contourf(x,y,z,20, cmap=mp.cm.jet)	
	mp.xlabel('time (days)')
	mp.title(tit)

	if y[-1] > y[0]:
		mp.ylabel('z (km)')
	else:
		mp.gca().invert_yaxis()
		mp.ylabel('Pressure (hPa)')


        cbar = mp.colorbar(CS)
        cbar.ax.set_ylabel(ylab)


	return CS
