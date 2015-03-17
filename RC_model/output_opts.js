//////////////////////////////////////////////////////////////////////////////////////////
//                           Javascript to run the RC model                    		//
//                    		   output options					//
//                              Written by Martin Singh					//
//                                   Oct 3 2013						//
//////////////////////////////////////////////////////////////////////////////////////////






// Function to show correct plotting options ////////////////////////////////////////////////
$("input:radio[name='plot_type']").click(function() {      
      $('.out_opts').hide();
      $('#' + $("input:radio[name='plot_type']:checked").val()).show();
});



// Function to replot data /////////////////////////////////////////////////////////////////
$("#replot").click(function() {

    var url = 'http://localhost:8080'

    // Send ajax request to server
    $.ajax({
           type: "POST",
           url: url,
           data: $("#output_options").serialize(), // serializes the form's elements.
           success: function(data)
           {

               // Get the server response
               var data = JSON.parse(data);

               // If no alert needed
               if (data.alert == ''){

                   // Update figure using time stamp
                   var d = new Date();
                   var figfile = $("#plot").attr("src")
                   var figfile = figfile.substring(0, figfile.indexOf('?'));
                   $("#plot").attr("src", figfile+"?"+d.getTime());}

               // If server responds with an alert
               else{alert(data.alert); 
                   }
                 
           }
         });

    return false; // avoid to execute the actual submit of the form.
});






