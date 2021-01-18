
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
  //modebarDivId = el.getElementsByClassName("modebar")[0].id
  el.getElementsByClassName("modebar")[0].id = parentId + "-modebar";
  //console.log(el,id,modebarDivId);

}

function updateVolcanoSelected(plotName,keyName,delimiter) {

  var trace0keys = document.getElementById(plotName).data[0].key;
  var trace1keys = document.getElementById(plotName).data[1].key;
  var trace2keys = document.getElementById(plotName).data[2].key;
  var vals = keyName.split(delimiter);

  var index0 = [], i = -1;
  for (j=0;j<vals.length;j++) {
      val = vals[j];
      while ((i = trace0keys.indexOf(val, i+1)) != -1){
          index0.push(i);
      }
  }
        
  var index1 = [], i = -1;
  for (j=0;j<vals.length;j++) {
      val = vals[j];
      while ((i = trace1keys.indexOf(val, i+1)) != -1){
          index1.push(i);
      }
  }
  
  var index2 = [], i = -1;
  for (j=0;j<vals.length;j++) {
      val = vals[j];
      while ((i = trace2keys.indexOf(val, i+1)) != -1){
          index2.push(i);
      }
  }
       
  var indicies = [index0,index1,index2];
  //console.log(indicies);

  Plotly.update(plotName, {selectedpoints: indicies});
  Plotly.restyle(plotName, { selected : { marker: { size:10, opacity:1.0, color:"#ff0000" } } }, [0] );
  Plotly.restyle(plotName, { unselected : { marker: { size:8, opacity:0.6 } } }, [0] );
  Plotly.restyle(plotName, { selected : {marker: { size:10, opacity:1.0, color:"#ff0000"} } }, [1]);
  Plotly.restyle(plotName, { unselected : {marker: { size:8, opacity:0.6} } }, [1]);
  Plotly.restyle(plotName, { selected : {marker: { size:10, opacity:1.0, color:"#ff0000"} } }, [2]);
  Plotly.restyle(plotName, { unselected : {marker: { size:8, opacity:0.6} } }, [2]);
  
}