#-----------------------------------------------------------------------------

PIDFILE = "webfoil_server.pid"
LOGFILE = "webfoil_server.log"

open(PIDFILE,'w').write(str(os.getpid()))

def LOG(x):
    fp = open(LOGFILE,'a')
    if type(x)==dict:
        for k in x:
            if not k:
                continue
            s = '  %s : %s' % (k,x[k])
            fp.write(s)
            print s
    #if type(x)==type('str'):
    else:
        fp.write(x)
        fp.write('\n')
        print x

    sys.stdout.flush()
    fp.flush()
    fp.close()
