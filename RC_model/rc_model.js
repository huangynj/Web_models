//////////////////////////////////////////////////////////////////////////////////////////
//                           Javascript to run the RC model                    		//
//                  									//
//                              Written by Martin Singh					//
//                                   Oct 3 2013						//
//////////////////////////////////////////////////////////////////////////////////////////


// Initialize variables
var model_terminated = 0; // has the model been terminated?
var validator = 0;        // null value of form validation






// Function to read the variables passed into the URL ////////////////////////////////////
function getUrlVars() {
   var vars = {};
   var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                  vars[key] = value;
               });
      
   return vars;
}


// Ensure all these functions wait for the document to be ready ///////////////////////////
$(document).ready(function(){


 
   // Set up validation of form using jquary validaiton plugin
   validator = $("#model_options").validate({
                          errorPlacement: function(error, element) 
                             {
                             error.appendTo( element.parent("label").next("span") );
                             }
                    });


   // Hide components of form not needed initially
   $("#adv_opts").hide()                // Hide advanced options
   $("#theta").hide()                   // Hide latitude in radiation selector
   $("#date_label").hide()             // Hide months   in radiation selector
   $("#hour").hide()                    // Hide hour     in radiation selector
   $("#force").hide()                   // Hide forcing options
   $(".force_opts").hide()              // Hide forcing options (all)
   $("#restart").hide()                 // Hide option to restart
   $("#stop_model").hide()              // Hide button to stop model

   // Decide whether to show various options depending on the URL given
   if (getUrlVars()["sim"]==1) {$("#sim").val(1)} else { $("#adv_simulation").hide() }
   if (getUrlVars()["rad"]==1) {$("#rad").val(1)} else { $("#adv_radiation").hide() }
   if (getUrlVars()["gas"]==1) {$("#gas").val(1)} else { $("#adv_composition").hide() }
   if (getUrlVars()["srf"]==1) {$("#srf").val(1)} else { $("#adv_surface").hide() }
   if (getUrlVars()["par"]==1) {$("#par").val(1)} else { $("#adv_parameterizations").hide() }
   if (getUrlVars()["frc"]==1) {$("#frc").val(1)} else { $("#adv_forcing").hide() }


   // Show advanced options ------------------------------------------------------------
    $("#show_adv").click(function(){
        $("#adv_opts").toggle();
    });


   // Function to ensure the month has the right number of days -------------------------
   $("#month").change(function () {
      var mon = parseInt($("#month option:selected").val())
      var months = [4,6,9,11];

      if (months.indexOf(mon)>-1) {
         $("#d29").show();
         $("#d30").show();
         $("#d31").hide();
      } else if (mon=="2") {
         $("#d29").hide();
         $("#d30").hide();
         $("#d31").hide();
      } else {
         $("#d29").show();
         $("#d30").show();
         $("#d31").show();
      }
   })


   // Show the date options only when appropriate -------------------------------------
   $("#rad_type").change(function () {
      var rad = $("#rad_type option:selected").val()
      if (rad=="0") {
         $("#theta").hide();
         $("#date_label").hide();
         $("#hour").hide();}
      else if (rad=="1") {
         $("#theta").show();
         $("#date_label").hide();
         $("#hour").hide();}
      else if (rad=="2" ) {
         $("#theta").show();
         $("#date_label").show();
         $("#hour").hide();}
      else if (rad=="3" || rad=="4") {
         $("#theta").show();
         $("#date_label").show();
         $("#hour").show();}
    })

    // Show the forcing options only when appropriate -----------------------------------
    $("#forcing").click(function(){
       $("#force").toggle();
       $('.force_opts').hide();
    })

    // Switch for type of forcing (WTG or w profile) -----------------------------------
    $("input:radio[name='rad_cld']").change(function() {     
       var rad = $("input:radio[name='rad_cld']:checked").val() 
       if (rad=='y'){
          $('#rad_wv').attr('checked', false);
          $('#rad_wv').attr('disabled', true);}
       else {
          $('#rad_wv').attr('disabled', false);}
    });


    // Switch to ensure water vapor interactive when clouds are -------------------------
    $("input:radio[name='forcing_opt']").click(function() {      
       $('.force_opts').hide();
       $('#' + $("input:radio[name='forcing_opt']:checked").val()).show();
    });





   // function to submit model run to server -------------------------------------------
   $("#run_model").click(function() {

      var url = "/rc"; // the script where you handle the form input.

      $.ajax({
           type: "POST",
           url: url,
           datatype: "json", // use json objects to pass data around.
           data: $("#model_options").serialize(), // serializes the form's elements.
           beforeSend: function(){ 

                          // If form is valid (check using jquery validate plugin)
                          if ($("#model_options").valid()){

                             // Show the server response
                             $('#loading').html('<br><br><br><center><h3>Looking for cached results</h3><br><br><h2>Please Wait<h2></center><br>')
                             $('#loading').show()
  
                             // Set the progress bar to inderterminate
                             $( "#progress" ).progressbar({value: false})
                             $("#progress").show() 

                             // Clear previous output
                             $('#put').html("")

                             // disable form input
                             jQuery('input[type=input], input').attr('disabled', true);
                             jQuery('input[type=submit]').attr('disabled', false);

                             // Enable stop model button
                             $('#run_model').hide()
                             $('#stop_model').show()
                          }

                          // If form is invalid
                          else {

                              validator.focusInvalid();

                              // Don't submit form
                              return false;
                          }
            },


           // In case of error (This should never happen)
           error: function() {

                      // Print an error message - this error means all other error handling/validation has failed
                      alert('A fatal error occured. If this problem persists please email mssingh@mit.edu, including as much detail of the circumstances as possible.')

                      // allow for form resubmsion 
                      $("#stop_model").hide();
                      $("#run_model").show();
                      jQuery('input[type=input], input').attr('disabled', false);

           },

           // Sucessfully posted simulation to server
           success: function(data) {   
               
 
                       // Parse the response from the server
                       var data = JSON.parse(data);

                       // If no alert message returned
                       if (data.alert == ''){

                          // Show the server response
                          $('#loading').html(data.html)
                          $('#loading').show()
 
                          // Set the progress bar to inderterminate
                          $( "#progress" ).progressbar({value: false})
                          $("#progress").show() 

                          // Clear the old output
                          $('#put').html("");

                          // Get the directory name of the output from the server
                          $("#dirname").val(data.dirname);

                          // Start the polling script which will poll the server for model progress
                          dopoll()
                       }

                       // Server responds with alert
                       else {

                          // Display the servers alert message
                          alert(data.alert) 
 
                          // Model terminated message
                          $('#loading').html('<br><br><br><center><h3>Simulation terminated</h3><br><br><h2>click "Run model" to start again<h2></center><br>');

                          // Hide progress bar and previous output
                          $('#progress').hide()
                          $('#put').html("");

                          // Allow form resubmission
                          $("#stop_model").hide();
                          $("#run_model").show();
                          jQuery('input[type=input], input').attr('disabled', false);}
                       }

       }); // End ajax call
 
       // avoid to execute the actual submit of the form.
       return false;


   }); // end run_model function





   // Function to stop model simulation -------------------------------------------------
   $("#stop_model").click(function() {

       // Display model terminated message
       // This must be consistent with run_mdoel and dopoll functions
       $('#loading').html('<br><br><br><center><h3>Simulation terminated</h3><br><br><h2>click "Run model" to start again<h2></center><br>')
       model_terminated =1

       // Hide progress bar and old output
       $('#progress').hide()
       $('#put').html("");
 
       // Allow form resubmission
       $("#stop_model").hide();
       $("#run_model").show();

       var url = "/rc"; // the script where you handle the form input.

       // Send request to server to clean up the mess we made
       $.ajax({
           type: "GET",
           url: url,
           data: {dirname:  $('#dirname').val(), clean: 'yes' },
           beforeSend: function() {
                          jQuery('input[type=input], input').attr('disabled', false);
                       }

       });

       return false; // avoid to execute the actual submit of the form.
   });



   // Function to reset form //////////////////////////////////////////////////////////////
   $("#reset_form").click(function() {
         
          // Hide advanced options
          $("#adv_opts").hide()   

          // reset validation
          validator.resetForm(); 

          // reset form
          return true;
   });


   // Function to create spinners /////////////////////////////////////////////////////////
   $(function() {
      $( "#days" ).spinner({
        step: 50
      });
      $( "#co2" ).spinner({
        step: 20
      });
      $( "#S0" ).spinner({
        step: 2
      });
      $( "#SSTi" ).spinner({
        step: 1
      });
      $( "#ch4" ).spinner({
        step: 0.1
      });
      $( "#n2o" ).spinner({
        step: 10
      });
      $( "#alpha" ).spinner({
        step: 0.05
      });
      $( "#ugust" ).spinner({
        step: 0.5
      });
   }); 


}); // End of document ready function 






// Polling funciton to ask server for model simulation progress /////////////////////
function dopoll(){
       var url = "/rc"; // the script where you handle the form input.

       // If simulation has been terminated stop poll
       if (model_terminated == 1) {model_terminated=0}

       // Else send request to server via ajax call
       else {
           $.ajax({
             type: "GET",
             url: url,
             data: {dirname:  $('#dirname').val() }, // serializes the form's elements.
             error: {},
             success: function(data)
                 {
                 // If simulation has been terminated stop poll    
                 if (model_terminated ==1){model_terminated=0}

                 else{
                    var data = JSON.parse(data);
                 
                    // If no alert to be shown                     
                    if (data.alert == '') {

                       // If model status is "running"
                       if(data.status=='running') {

                           // Reset progress bar if required
                           if($("#progress").progressbar("value")==false) {
                              $( "#progress" ).progressbar({
                                value: 0.1
                              });
                           }
 
                           // Set progress bar to new value
                           $("#progress .ui-progressbar-value").animate({ 
                              width: data.progress*3 // This number depends on the width of the progress bar
                           }, {queue: false});
                  
                           // show response from server
                           $('#loading').html(data.html); 
 
                           // Poll again in 1 second
                           setTimeout(dopoll,1000);} 
                    

                        // If model status is "done"
                        else if(data.status =='done')  {
                 
 			   // Hide the loading info 
                           $('#loading').hide()
	                   $('#progress').hide()
 
                           // Show the response from the server 
          	           $('#put').html(data.html);

                           // Hide extra plotting options
                           $("#prof_opts").hide()
                           $("#hov_opts").hide()
                           $("#diff_opts").hide()

                           // Allow restart of previous simulation
                           $("#restart").show();
 
                           // Show the run model button
                           $("#stop_model").hide();
                           $("#run_model").show();

			   // Reload the figure by attaching a time stamp
                           var d = new Date();
                           var figfile = $("#plot").attr("src")
                           $("#plot").attr("src", figfile+"?"+d.getTime());
                  
                           // allow the form to be edited again
                           jQuery('input[type=input], input').attr('disabled', false);
                        }

			// If model status is "queued"
                        else if(data.status =='queued') {

                            // Set indeterminate progess bar
                            $( "#progress" ).progressbar({
                               value: false 
                            })

                            // Show response from server
                            $('#loading').html(data.html); // show response from the python script.
              
                            // Poll again in 1 second                  
                            setTimeout(dopoll,1000); 
                        }

                        // If model status is "undefined" (This shouldn't really happen)
                        else if(data.status =='undefined') {

                            // Hide the progess bar
                            $('#progress').hide()

                            // Show response from server
                            $('#loading').html(data.html);
 
                            // Allow model to be submitted again                    
                            $("#stop_model").hide();
                            $("#run_model").show();
                            jQuery('input[type=input], input').attr('disabled', false);
                         }

                     }
       
                     // If there is alert text
                     else { 
 
                         // Show an alert with the alert text 
                         alert(data.alert)

                         // Hide the  progress bar
                         $('#progress').hide()

                         // Show response from server
                         $('#loading').html(data.html);
 
                         // Allow form to be resubmitted
                         $("#stop_model").hide();
                         $("#run_model").show();
                         jQuery('input[type=input], input').attr('disabled', false);
                     }


                } // End if simulation not terminated

              
             } // End success of ajax request

         }); // End ajax request

      } // End if simulation not terminated

}; // End function dopoll




// Function to clean up directories when user navigates away ////////////////////////////
window.onbeforeunload = function (e) {

    var url = "/rc"; // the script where you handle the form input.

    // Message to pass to user (currently unused)
    var msg = 'If you leave this page any running simulation will be stopped, and any output data lost.';

    // Send request to server to clean directory
    $.ajax({
           type: "GET",
           url: url,
           async: false,
           data: {dirname:  $('#dirname').val(), clean: 'yes', leaving: 'yes' }, 
           success: function(data){ }
    });

    return null;

};










