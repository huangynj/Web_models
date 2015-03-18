eaps-12340x web-interface models
================================

    Web interface models used in the EdX course 12.340x. 


    Models included
    ---------------

    ### RC_model ###

    Radiative-convective model written in FORTRAN. 
    Developed by Kerry Emanuel and collaborators.
    Web interface developed by Martin Singh & Tim Cronin with help from Isaac Chuang.

    This version of the RC model is constructed to run locally through a browser. An install script describes the python packages you need to run the model.
    Works on Ubuntu 14.04
    No support for other ditros 


    ### Nlayer ###
 
    N-layer radiative equilibrium model written in javascript (i.e. runs client side)
    Written by Martin Singh.


    ### Twolayer_RC ###
  
    2-layer radiative-convective model written in javascript (client side). Gray radiation, and convective adjustment.
    Written by Martin Singh based on MATLAB version developed by Kerry Emanuel

    ### Onelayer_icealbedo ###
  
    1-layer radiative model with crude formulation of ice-albedo feedback
    Written in javascript (client side).
    Written by Martin Singh



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

            ### python/ ###

   	    Contains Python scripts to run server, receieve requests from client and run model

                * run_rc_model.wsgi  - main script for dealing with client requests
                * write_input.py    - script that writes input files for rc model
                * postprocess.py    - script that reads output for rc model and plots data
                * plot_model_log.py - script that reads output for rc log file and plots it
                * mime.py           - function to work with mime types

            ### model/ ###

            Contains fortran executable and input files

               * rc_web             - fortran executable
               * O3.in              - Ozone input profile
               * profile.reference  - Reference profile used for fixed cloud simulations
               * src/               - Source code ofor RC model

            ### log/ ###

            Contains html log interface
        
                * model_log.html
        
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




