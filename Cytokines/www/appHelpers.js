
function launchTutorial(id,tutorialName){
    //console.log('tutorial event triggered for ' + nsid +' '+ tutorialName);
    Shiny.setInputValue(id +"-TutorialName", tutorialName);
  }

function ShowBoxplotGroupOptions(gd,tutorialName) {

  id = gd.id.split('-',1).toString();  
  console.log(id,tutorialName);
  comparisonElementName = id + "-" + "GroupAnalysisOptions";
  document.getElementById(comparisonElementName).scrollIntoView();
    
  }


function clearBoxplotGroups(gd){

  id = gd.id.split('-',1).toString();
  traces = gd._fullData;
  
  for (var i = 0; i < traces.length; i++) {
      traceName = traces[i].name;
      if(traceName.startsWith("Group")) {
        fullTraceName = id +"-"+traceName.replace(' ','');
        Shiny.setInputValue(fullTraceName, "");
      }
    }
}

function overrideModebarDivId(el) {
  //id = el.id.split('-',1).toString();
  parentId = el.id;
  modebarDivId = el.getElementsByClassName("modebar")[0].id
 
  el.getElementsByClassName("modebar")[0].id = parentId + "-modebar";
  //console.log(el,id,modebarDivId);

}


function annotatePointByKey(plotName,keyName,annotationsToKeep) {
  
  var el = document.getElementById(plotName);
  var data = el.data;
  var trace = -1;
  var keyIndex = -1;
  
  if (data !== undefined) {

    selectedIndex = Array.from({ length: data.length }, (v, i) => []);

    for (var i = 0; i < data.length; i++) {
      Plotly.restyle(plotName, { selected : {marker: { size:14, opacity:1.0, color:"#ff0000"} } }, [i]);
      Plotly.restyle(plotName, { unselected : {marker: { size:8, opacity:0.6} } }, [i]);
      keys = data[i].key;
      for(var j = 0; j < keys.length; j++) {
        if (keys[j] == keyName) {
          trace = i;
          keyIndex = j;
          break;
        }
      }
    }

    annotations = el.layout.annotations.slice(0,annotationsToKeep) || [];

    if(trace > -1 & keyIndex > -1) {

      selectedIndex[trace][0] = keyIndex;
    
      Plotly.update(plotName, {selectedpoints: selectedIndex});
          
      annotation = {
        text: data[trace].key[keyIndex],
        x: data[trace].x[keyIndex],
        y: parseFloat(data[trace].y[keyIndex].toPrecision(4)), 
        showarrow : true, 
        arrowhead : 1, 
        startarrowhead : 1, 
        arrowside : 'end', 
        arrowcolor : '#e74c3c',
        ax : 20, 
        ay : -40,
        font : {color : 'Black', family : 'Arial', size : 16}, 
        bgcolor : '#abb2b9', 
        standoff : 4
      }

      annotations.push(annotation);
              
    }
          
    Plotly.relayout(plotName,{annotations: annotations});

  }

}
