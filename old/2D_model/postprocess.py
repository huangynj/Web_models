#!/usr/bin/python

# some post processing scripts

def get_output(filename):

	outvar = []
	for line in open(filename,'rt'):
		tmp1,tmp,tmp2 = [float (x) for x in line.split()]
		outvar.append(tmp)
		
	return outvar





