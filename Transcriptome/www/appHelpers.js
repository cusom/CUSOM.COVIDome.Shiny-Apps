
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



/*Shiny.addCustomMessageHandler("scrollToElement",
  function(message) {
    nameSpace = message[1];
    elementName = message[2];
    scollToName = message[3];
    console.log(message);
  }
);
*/