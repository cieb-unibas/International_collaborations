// Interactive plot showing regression coefficients
// CR: 18_3_2021


Plotly.d3.csv("https://raw.githubusercontent.com/cieb-unibas/female_inventors/main/Data/dat_coeff.csv", function(err, rows){
function unpack(rows, key) {
        return rows.map(function(row) { return row[key]; });
    }

var     var1 = unpack(rows, 'term'),
        var2 = unpack(rows, 'model'),
        est = unpack(rows, 'estimate'),
        conf_l = unpack(rows, 'conf.low'),
        conf_h = unpack(rows, 'conf.high'),
        listVar1 = [],
        listVar2 = [],
        currentest = [],
        currentctry = [],
        currentmodel = [],
        currentconf_l = [],
        currentconf_h = [];

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
        for (var i = 0 ; i < var1.length ; i++){
          for(var j = 0; j < var2.length; j ++){
            if (var1[i] === chosenCountry[j] && var2[i] === chosenCountry2) {
                currentest.push(est[i]);
                currentctry.push(var1[i]);
                currentmodel.push(var2[i]);
                currentconf_l.push(conf_l[i]);
                currentconf_h.push(conf_h[i]);
                }
    }
    }
}

// Default Country Data
setBubblePlot("CN", "Overall");

function setBubblePlot(chosenCountry, chosenCountry2) {
        getCountryData(chosenCountry, chosenCountry2);

        var trace1 = {
            x: currentest,
            y: currentctry,
            error_x: {
            type: 'data',
            array: currentconf_h,
            visible: true},
         //    customdata: currentmedian,
         //    text: currentVarName,
            type: 'scatter',
            mode: "markers",
            textposition: "auto",
            orientation: 'h',
         //    transforms: [{
         //    type: 'sort',
         //    target: 'y',
         //    order: 'descending',
         //    }], 
       //      hovertemplate:  '<b>%{text}</b>' + 
      //                        '<br><b>Arithmetisches Mittel: %{x}</b>' + '<br><b>Median: %{customdata}</b>' + '<extra></extra>',
            marker: { color:  'rgba(53,91,118,0.8)', size: 20},
            textposition: 'center',
            hoverinfo: 'none'
        };
        
      

var data = [trace1];
 
        var layout = {
          hovermode: "closest",
          hoverlabel: { bgcolor: "#FFF" },
          bargap: 0.4,
          bargroupgap:  0.1, 
          showlegend: false,
          scrollZoom: false,
          margin: {l: 480},
    xaxis: {fixedrange: true,
            zeroline: false,
           // tickvals: [1, 2, 3, 4, 5], 
            tickfont: {size: 18}
           // title: {text: '<b>1: nicht wichtig bzw. sehr gering                 5: sehr wichtig bzw. sehr hoch</b>'}
           },
    yaxis: {fixedrange: true,
            zeroline: false,
            tickfont: {size: 18, width: 2},
            title: {text: '<b></b>'}},
    shapes: [{
      type: 'line',
      x0: 0, 
      y0: currentctry.length,
      x1: 0,
      y1: currentctry.length,
      line: {dash: 'dot'}
    }]};
  

Plotly.newPlot('myDiv', data, layout, {displayModeBar: false});
//Plotly.update('myDiv', data, layout);
};    


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
  $.each($("#select_1 option:selected" ), function() {
  var_2l.push($(this).val());  
  })
  document.getElementById("test1").innerHTML = var_2l; 
        setBubblePlot(var_2l, countrySelector2.value);
    }


countrySelector.addEventListener('change', updateCountry, false);
countrySelector2.addEventListener('change', updateCountry, false);



  
});    
