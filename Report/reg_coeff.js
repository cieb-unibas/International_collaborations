// Interactive plot showing regression coefficients
// CR: 22_3_2021

function round(value, decimals) {
  return Number(Math.round(value+'e'+decimals)+'e-'+decimals);
}






Plotly.d3.csv("https://raw.githubusercontent.com/cieb-unibas/International_collaborations/main/Report/dat_coeff.csv", function(err, rows){
function unpack(rows, key) {
        return rows.map(function(row) { return row[key]; });
    }

var     var1 = unpack(rows, 'term'),
        var2 = unpack(rows, 'model'),
        ctry_name = unpack(rows, 'ctry_name'),
        est = unpack(rows, 'estimate'),
        conf_l = unpack(rows, 'conf_int'),
        conf_h = unpack(rows, 'conf.high'),
        conf_low = unpack(rows, 'conf.low'),
        p_value = unpack(rows, 'p.value'),
        listVar1 = [],
        listVar2 = [],
        currentest = [],
        currentctry = [],
        currentmodel = [],
        currentconf_l = [],
        currentconf_h = [],
        current_ctry_model = [],
        confl = [],
        text_var = [],
        color_var = [],
        current_color = [];
        
        
    for (var i = 0; i < var2.length; i++ ){
        if (listVar2.indexOf(var2[i]) === -1 ){
            listVar2.push(var2[i]);
        }
    }
    
    
      for (var i = 0; i < var1.length; i++ ){
        if (listVar1.indexOf(var1[i]) === -1 ){
            listVar1.push(var1[i]);
        }
    }
      

    function getCountryData(chosenCountry, chosenCountry2) {
        currentest = [];
        currentconf_l = [];
        currentconf_h = [];
        currentctry = [];
        currentmodel = [];
        current_ctry_model = [];
        text_var =[];
        confl = [];
        color_var = [];
        current_color = [];
        
        for (var i = 0 ; i < var1.length ; i++){
           if(var2[i] === "Overall"){
           color_var[i] = d3.interpolateViridis(1);
           } else if (var2[i] === "Chemistry"){
           color_var[i] = d3.interpolateViridis(0.2);
           } else if (var2[i] === "Electrical engineering"){
            color_var[i] = d3.interpolateViridis(0.4);
           } else if (var2[i] === "Instruments"){
           color_var[i] = d3.interpolateViridis(0.6);
           } else if (var2[i] === "Other fields"){
           color_var[i] = d3.interpolateViridis(0.8);
           } else if (var2[i] === "Mechanical engineering"){
           color_var[i] = d3.interpolateViridis(0);
           } 

          for(var j = 0; j < var2.length; j ++){
          for(var k = 0; k < var1.length; k++){
            if (var1[i] === chosenCountry[j] && var2[i] === chosenCountry2[k]) {
                currentest.push(est[i]);
                currentctry.push(var1[i]);
                currentmodel.push(var2[i]);
                currentconf_l.push(conf_l[i]);
                currentconf_h.push(conf_h[i]);
                current_ctry_model.push(var1[i] + "-" + var2[i]);
                current_color.push(color_var[i]);

                text_var.push('Country: ' + ctry_name[i] + '<br>' +  'Technology: ' + var2[i] + '<br>' + 'Estimated coefficient: ' + round(est[i], 4) +
                    '<br>' +  'P-value: ' + p_value[i] +'<br>' + '95% Confidence Intervall: [' + round(conf_low[i], 4) + ', ' + round(conf_h[i], 4) + ']');
                
          }
    }
    }
}
}
// Default Country Data
setBubblePlot(["CH", "GB", "US", "CN", "DE"], ["Overall"]);

function setBubblePlot(chosenCountry, chosenCountry2) {
        getCountryData(chosenCountry, chosenCountry2);
 

if( /Android|webOS|iPhone|iPad|Mac|Macintosh|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent) ) {
  var margin_left = 100,
      tick_y_axis = 14,
      text_right = "<b>Better</b><br>(compared to<br>only domestic)",
      text_left = "<b>Worse</b><br>(compared to<br>only domestic)";

} else{ 
  var margin_left = 280,
      tick_y_axis = 18,
      text_right = "<b>Better</b><br>(compared to only domestic)",
      text_left = "<b>Worse</b><br>(compared to only domestic)";
  }
 
var trace1 = {
            x: currentest,
            y: [currentctry, currentmodel],
            customdata: text_var,
            type: 'scatter',
            mode: "markers+text",
            color: current_color,
            text:  currentctry,
            marker: {size: 20, color: current_color},
            error_x: {
            type: 'data', color: current_color,
            array: currentconf_l},
            orientation: 'h',
            hovertemplate:  '%{customdata}' + '<extra></extra>',
            textposition: 'top',
        };
        
        
var data = [trace1];
 
var layout = {
          hovermode: "closest",
          hoverlabel: { bgcolor: "#FFF" },
        //  bargap: 0.4,
        //  bargroupgap:  0.1, 
          showlegend: false,
          scrollZoom: false,
          height: 80 + 50*currentest.length,
          margin: {l: margin_left,
                   r: 0,
                   b: 80,
                   t: 0},
           shapes: [{
    type: 'line',
    x0: 0,
    y0: -1,
    x1: 0,
    y1: current_ctry_model.length + 1,
    line: {
      color: 'grey',
      width: 1.5,
      dash: 'dash'
    }}],   
    annotations: [{
    xref: 'x',
    yref: 'y',
    x: 0.01,
    xanchor: 'left',
    y: current_ctry_model.length + 0.5,
    yanchor: 'bottom',
    text:text_right,
    font: {size: 14},
    showarrow: false
  },
  {
    xref: 'x',
    yref: 'y',
    x: -0.01,
    xanchor: 'right',
    y: current_ctry_model.length + 0.5,
    yanchor: 'bottom',
    text: text_left,
    font: {size: 14},
    showarrow: false
  }],
    xaxis: {autotick: true,
            fixedrange: true,
            zeroline: false,
            autorange: true,
                       // tickvals: [1, 2, 3, 4, 5], 
            tickfont: {size: 18},
            title: {text: '<b>Coefficient estimate</b>', font: {size: 14}}
           },
    yaxis: {autotick: false,
            showgrid: false,
            fixedrange: true,
            zeroline: false,
            showticklabels: true,
             categoryorder: "array",
            categoryarry: currentmodel, 
            tickfont: {size: tick_y_axis, width: 2},
            title: {text: '<b></b>'}}

  
};
  

Plotly.newPlot('myDiv', data, layout, {displayModeBar: false});
//Plotly.update('myDiv', data, layout);
}


var countrySelector = document.getElementById("select_1");
var countrySelector2 = document.getElementById("select_2");




//function assignOptions(textArray, selector) {
//        for (var i = 0; i < textArray.length-1;  i++) {
//            var currentOption = document.createElement("option");
//            currentOption.text = textArray[i];
//            selector.appendChild(currentOption);
//        }
//    }


//assignOptions(listVar1, countrySelector);
//assignOptions(listVar2, countrySelector2);

function updateCountry(){
  var var_2l = [];
  var var_1l = [];


  $.each($("#select_1 option:selected" ), function() {
  var_2l.push($(this).val());  
  })
  
    $.each($("#select_2 option:selected" ), function() {
  var_1l.push($(this).val());  
  })
  
setBubblePlot(var_2l, var_1l);
    }


countrySelector.addEventListener('change', updateCountry, false);
countrySelector2.addEventListener('change', updateCountry, false);



  
});    
