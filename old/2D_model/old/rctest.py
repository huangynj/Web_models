#!/usr/bin/python
import cgi
import os
import subprocess 

os.environ['MPLCONFIGDIR'] = '/tmp/'
import matplotlib
matplotlib.use('agg')
from pylab import *

print "Content-type: text/html\n\n"
#print "<html>"+os.getcwd()+"</html>\n"
fsnd = open('sounding.in', 'r')
initsnd = fsnd.readline()
ssti = initsnd[initsnd.find("SST="):initsnd.find("\n")]
fsnd.close()

print "<html><p>Initial "+ssti+"</p></html>"
runstuff = subprocess.call('./rcmod_test.exe')

fsnd = open('output/sounding.out', 'r')
finalsnd = fsnd.readline()
sstf = finalsnd[finalsnd.find("SST="):finalsnd.find("\n")]
fsnd.close()
print "<html><p>Final "+sstf+"</p></html>"

time = []
for line in open('output/t.out','rt'):
    time.append(float(line))

Ts = []
for line in open('output/sst.out','rt'):
    tmp1,temp,tmp2 = [float (x) for x in line.split()]
    Ts.append(temp)

hp = plot(time,Ts)
hp = xlabel('time (days)')
hp = ylabel('SST (deg. C)')

hf = savefig('output/sst_t.png')

print "<img src=\"output/sst_t.png\" alt=\"SST as a function of time\" width=\"400\" height=\"300\" />"
