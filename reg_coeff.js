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
        currentconf_h = [],
        current_ctry_model = [],
        current_model_1 = [],
        current_model_2 = [],
        current_model_3 = [],
        current_model_4 = [],
        current_model_5 = [],
        current_model_6 = [];

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
        current_model_1 = [];
        current_model_2 = [];
        current_model_3 = [];
        current_model_4 = [];
        current_model_5 = [];
        current_model_6 = [];
        
        for (var i = 0 ; i < var1.length ; i++){
          for(var j = 0; j < var2.length; j ++){
          for(var k = 0; k < var1.length; k++){
            if (var1[i] === chosenCountry[j] && var2[i] === chosenCountry2[k]) {
                currentest.push(est[i]);
                currentctry.push(var1[i]);
                currentmodel.push(var2[i]);
                currentconf_l.push(conf_l[i]);
                currentconf_h.push(conf_h[i]);
                current_ctry_model.push(var1[i] + " " + var2[i]);
                }
    }
    }
}
}
// Default Country Data
setBubblePlot(["CH", "GB", "US", "CN", "DE"], ["Overall"]);

function setBubblePlot(chosenCountry, chosenCountry2) {
        getCountryData(chosenCountry, chosenCountry2);

        var trace1 = {
            x: currentest,
            y: current_ctry_model,
            offset: 0.5,
            error_x: {
            type: 'data',
            array: currentconf_l,
            visible: true},
            customdata: currentmodel,
         //    text: currentVarName,
            type: 'scatter',
            mode: "markers",
            marker: {width: 10},
      //      textposition: "auto",
            orientation: 'h',
           transforms: [
          {
           type: 'groupby',
           target: 'y',
           groups: currentctry,
           styles:[
             {target: "DE", value: {marker: {color: '#FDE725FF'}}},
             {target: "AT", value: {marker: {color: '#FDE725FF'}}},
             {target: "BE", value: {marker: {color: '#440154FF"'}}},
             {target: "CA", value: {marker: {color: '#470E61FF'}}},
             {target: "CH", value: {marker: {color: '#481B6DFF'}}},
             {target: "CN", value: {marker: {color: '#FDE725FF'}}},
             {target: "DE", value: {marker: {color: '#FDE725FF'}}},
             {target: "DK", value: {marker: {color: '#FDE725FF'}}},
             {target: "ES", value: {marker: {color: '#FDE725FF'}}},
             {target: "FI", value: {marker: {color: '#FDE725FF'}}},
             {target: "FR", value: {marker: {color: '#FDE725FF'}}},
             {target: "GB", value: {marker: {color: '#FDE725FF'}}},
             {target: "IE", value: {marker: {color: '#FDE725FF'}}},
             {target: "IL", value: {marker: {color: '#FDE725FF'}}},
             {target: "IT", value: {marker: {color: '#FDE725FF'}}},
             {target: "JP", value: {marker: {color: '#FDE725FF'}}},
             {target: "KR", value: {marker: {color: '#FDE725FF'}}},
             {target: "NL", value: {marker: {color: '#FDE725FF'}}},
             {target: "NO", value: {marker: {color: '#FDE725FF'}}},
             {target: "SE", value: {marker: {color: '#FDE725FF'}}},
             {target: "SG", value: {marker: {color: '#FDE725FF'}}},
             {target: "US", value: {marker: {color: '#FDE725FF'}}},
             {target: "Rest", value: {marker: {color: '#FDE725FF'}}}]
           }],
            hovertemplate:  '<b>Country: </b> <br>' + '<b>Technology: %{customdata}</b>' + '<extra></extra>',
            marker: { color:  'rgba(53,91,118,0.8)', size: 20},
            textposition: 'center',
            hoverinfo: 'none'
        };
        
            var trace2 = {
            x: currentest,
            y: currentctry,
            error_x: {
            type: 'data',
            array: currentconf_l,
            visible: true},
            customdata: currentctry,
         //    text: currentVarName,
            type: 'scatter',
            mode: "markers",
            textposition: "auto",
            orientation: 'h',
           transforms: [{
           type: 'sort',
           target: 'customdata',
           order: 'ascending',
           }],  
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
          height: 50 + 40*currentest.length,
          margin: {l: 250,
                   r: 0,
                   b: 50,
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
                   
    xaxis: {fixedrange: true,
            zeroline: false,
           // tickvals: [1, 2, 3, 4, 5], 
            tickfont: {size: 18},
            title: {text: '<b>Coefficient estimate</b>', font: {size: 18}}
           },
    yaxis: {fixedrange: true,
            zeroline: true,
            categoryorder: currentctry,
   //         tickvals: Array(current_ctry_model.length).fill().map((element, index) => index + 0), 
    //        ticktext: currentctry, 
            tickfont: {size: 18, width: 2},
            title: {text: '<b></b>'}}
 //   shapes: [{
//      type: 'line',
//      x0: 0, 
//      y0: currentctry.length,
//      x1: 0,
//      y1: currentctry.length,
//      line: {dash: 'dot'}
//    }]
  
};
  

Plotly.newPlot('myDiv', data, layout, {displayModeBar: false});
Plotly.update('myDiv', data, layout);
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
