#!/usr/bin/python

from time import localtime, strftime 

import re, os
import hashlib
import logging
import time
import sys

class DOFILE(object):
    def __init__(self,fn):
        self.fn = fn
    def fp(self):
        return open(self.fn)

#-----------------------------------------------------------------------------

def do_update_vgl():
    p = os.popen('./GET_NEW_VGL-JAR')
    msg = p.read()
    p.close()
    return msg

#-----------------------------------------------------------------------------


def do_GET(data, path):


    #return '/home/mssingh/'+path    
    # path should be jnlp/app/submission-url (without problem_check suffix)
    # eg jnlp/VGL-A/http://sandbox-bio-001.m.edx.org/courses/MITx/7.00x/2013_Spring/modx/i4x://MITx/7.00x/problem/example_vgl1
    with open('/home/mssingh/'+path, 'r') as fin:
       html_file = fin.read()

    return html_file
    #try:
    #    (root, app, suburl) = path.split('/',2)
    #except:
    #    msg = "Hey, the time is %s" % strftime("%a, %d %b %Y %H:%M:%S", localtime())
    #    msg += '\npath=%s' % path
    #    return msg

    sdir = hashlib.sha1(suburl).hexdigest()

    fn = '%s.jnlp' % app.replace('..','.')

    if app=='updatevgl':
        return do_update_vgl()

    if app=='GET':
        # return the file, if exists
        fn = 'VGL/%s' % suburl.replace('..','.')
        print "GET of file %s" % fn
        sys.stdout.flush()
        if suburl.endswith('.jar'):
            if '/' in suburl:
                (jdir, jarfile) = suburl.rsplit('/',1)
            else:
                jarfile = suburl
            jfn = 'VGL/%s' % jarfile.replace('..','.')
            if os.path.exists(jfn):
                #return 'jar file %s found' % jfn
                return DOFILE(jfn)
        if os.path.exists(fn):
            return open(fn).read()
        return "not found: %s" % suburl

    sfn = 'VGL/%s/%s' % (sdir,fn)
    if os.path.exists(sfn):
        #jnlp = open(sfn).read()
        return DOFILE(sfn)

    m = re.search('(http[s]*://[^/]+)/(.*)(i4x://.*)', suburl)
    if not m:
        msg = "Hey, the time is %s" % strftime("%a, %d %b %Y %H:%M:%S", localtime())
        msg += 'path=%s\n' % path
        msg += 'root=%s\n' % root
        msg += 'app=%s\n' % app
        msg += 'suburl=%s\n' % suburl
        return msg
    
    print m.groups()
    (url, course, location) = m.groups()
    
    if location.startswith('i4x://'):
        location = 'i4x://' + location[6:].replace(':','_')

    appfn = 'APPS/' + fn

    if os.path.exists(appfn):
        jnlp = open(appfn).read()
        
        # lines to change:
        #<argument>-edXCookieURL=http://localhost:8000</argument>
        #<argument>-edXLoginURL=http://localhost:8000/login</argument>
        #<argument>-edXSubmissionURL=http://localhost:8000/courses/MITx/7.00x/2013_Spring/modx/i4x://MITx/7.00x/problem/example_vgl1/problem_check</argument>
        #<argument>-edXLocation=input_i4x://MITx/7.00x/problem/example_vgl1_2_1</argument>

        jnlp = re.sub('<argument>-edXCookieURL=([^<]+)</argument>','<argument>-edXCookieURL=%s</argument>' % url, jnlp)
        jnlp = re.sub('<argument>-edXLoginURL=([^<]+)</argument>','<argument>-edXLoginURL=%s/login</argument>' % url, jnlp)
        jnlp = re.sub('<argument>-edXSubmissionURL=([^<]+)</argument>','<argument>-edXSubmissionURL=%s/problem_check</argument>' % suburl, jnlp)
        jnlp = re.sub('<argument>-edXLocation=([^<]+)</argument>','<argument>-edXLocation=input_%s_2_1</argument>' % location, jnlp)

        #jnlp = jnlp.replace('http://www.securebio.umb.edu/edX/','https://biox.mitx.mit.edu/jnlp/GET/%s/' % sdir)
        jnlp = jnlp.replace('http://www.securebio.umb.edu/edX/" href="','https://biox.mitx.mit.edu/jnlp/GET/" href="%s/' % sdir)

        if not os.path.exists('VGL/' + sdir):
            os.mkdir('VGL/' + sdir)
        open(sfn,'w').write(jnlp)

        return DOFILE(sfn)
        
    msg = "no such app %s" % app
    if 1:
        msg += 'path=%s\n' % path
        msg += 'root=%s\n' % root
        msg += 'app=%s\n' % app
        msg += 'suburl=%s\n' % suburl
    return msg

def do_POST(data, path):
    return "Hey, the time is %s" % strftime("%a, %d %b %Y %H:%M:%S", localtime())


# Entry point
def application(env, start_response):
    logging.basicConfig()

    path = env.get('PATH_INFO', '').lstrip('/')

    # Handle request
    method = env['REQUEST_METHOD']
    data = env['wsgi.input'].read()

    rip = env.get('REMOTE_ADDR','')
    
    print '-' * 60
    print '%s from %s at %s' % (method, rip, time.ctime(time.time()))
    print env
    sys.stdout.flush()

    handlers = {'GET': do_GET,
                 'POST': do_POST,
                 }
    if method in handlers.keys():
        reply = handlers[method](data, path)
        sys.stdout.flush()
        # print ' [*] reply:\n%s\n' % reply
        
        if isinstance(reply, DOFILE):

            if reply.fn.endswith('.jnlp'):
                start_response('200 OK', [('Content-Type', 'application/x-java-jnlp-file')])

            elif reply.fn.endswith('.jar'):
                start_response('200 OK', [('Content-Type', 'application/java-archive')])

            if 'wsgi.file_wrapper' in env:
                return env['wsgi.file_wrapper'](reply.fp(), 1024)
            else:
                return iter(lambda: reply.fp().read(1024), '')

        start_response('200 OK', [('Content-Type', 'text/html')])
        return reply
    else:
        start_response('404 Not Found', [('Content-Type', 'text/plain')])
        return ''
