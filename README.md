eaps-12340x web-interface models
================================

    Web interface models used in the EdX course 12.340x. 


    Models included
    ---------------

    ### RC_model ###

    Radiative-convective model written in FORTRAN. 
    Developed by Kerry Emanuel and collaborators.
    Web interface developed by Martin Singh & Tim Cronin with help from Isaac Chuang.

        * To start the server type ./start_server
        * To kill the server type ./kill_server

    ### Nlayer ###
 
    N-layer radiative equilibrium model written in javascript (i.e. runs client side)
    Written by Martin Singh.


    ### Nlayer_RC ###
  
    2-layer radiative-convective model written in javascript (client side). Gray radiation, and convective adjustment.
    Written by Martin Singh based on MATLAB version developed y Kerry Emanuel


    ### EBM ###

    Plan to write a javascript version of the Bitz Energy balance model. Currently work in progress


    Directory Structure
    -------------------

        README.md        - this file


        ### tools/ ###

        Tools and packages used by the web interfaces

           ### jquery-1.9.1/ ###

           Jquery library

           ### jquery-ui.1.10.3/ ###

           Jquery user interface library. Configured to have progressbar included

           ### jquery-validation-1.11.1 ###

           Jquery form validation library.


        ### RC_model/ ###

        The Radiative convective model and its web interface implementation

            rc_model.html    - Model web interface html
            rc_model.js      - javascript functions for web interface
            rc_model.css     - style sheet for model web interface
            output_opts.html - html snippet to display options for displaying output when simulation is finished
            output_opts.js   - javascript functions for output options page
            start_server     - script to start python server
            kill_server      - script to kill python server

            ### python/ ###

   	    Contains Python scripts to run server, receieve requests from client and run model

                * run_rc_wsgi.py    - main script for dealing with client requests
                * write_input.py    - script that writes input files for rc model
                * postprocess.py    - script that reads output for rc model and plots data
                * plot_model_log.py - script that reads output for rc log file and plots it
                * mime.py           - function to work with mime types

            ### temp/ ###

	    Directory for temporary files made for each user session

            ### model/ ###

            Contains fortran executable and input files

               * rc_web             - fortran executable
               * O3.in              - Ozone input profile
               * profile.reference  - Reference profile used for fixed cloud simulations
               * src/               - Source code ofor RC model

            ### log/ ###

            Contains log files of server instance
        
                * rc_model_log.txt - main log file for server
                * rc_model.pid     - process id of server instance
                * queue_status.txt - status of job queue
                * reports/         - directory containing old log files and plots of the model log
        
            ### build/ ###
 
            Contains builds of python utilities used in server code

            ### docs/ ###
 
            Contains some documentation on the model and web interface (always a work in progress)

                * Flowchart.pdf   - Flowachart of subroutines used in the web interface to run RC model

        ### Nlayer ###

        Contains the code for the N-layer radiative model

            * N_layer.html        - file with the html and javascript to run the n-layer model
            * sun.png             - images used in the schematic for model
            * layer.png
            * surface.png


        ### Twolayer_RC ###

        Contains the code for the two-layer radiative-convective model

            * 2_Layer_simple.html - file with the html and javascript to run the 2-layer model
            * 3_Layer.html        - file with the html and javascript to run  3-layer model
            * rancon.m            - original MATLAB 3 layer model written by K. Emanuel
            * sun.png             - images used in the schematic for model
            * layer.png
            * surface.png




