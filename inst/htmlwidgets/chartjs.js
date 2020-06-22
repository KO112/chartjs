// HTMLWidgets definition for chartjs
HTMLWidgets.widget({
  
  name: 'chartjs',
  
  type: 'output',
  
  factory: function(el, width, height) {
    
    // Create the chart object, the new chart/canvas element, then place the chart in the widget element
    var chartObj;
    var canv = document.createElement("canvas");
    el.appendChild(canv);
    
    // Return the rendering/resizing functions
    return {
      
      // Return the chart object itself
      chart: chartObj,
      
      // Runs to render the chart
      renderValue: function(x) {
        
        // Print out some debugging info
        console.log(x);
        console.log(el);
        console.log(width);
        console.log(height);
        
        // 
        chartObj = new Chart(canv.getContext('2d'), x);
        /*chartObj = new Chart(canv.getContext('2d'), {
          type: x.type,
          data: x.data,
          options: x.options
        });*/
        
      },
      
      // Runs when the object is resized
      resize: function(width, height) {
        console.log("RESIZING");
        // chartObj.resize();
      }
      
    };
    
  }
  
});
