function setStrength(assertionId, strength, doc) {
  var callback = {
    success: handleSetStrength,
    failure: handleFailedSetStrength,
    argument: {doc: doc} 
  };
  var host = window.location.protocol + "//" + window.location.host;
  var requestURL = host + "/cgi-bin/ws/xml-set-lexical-mapping-strength?assert=" + assertionId + "&strength=" + strength;
  var request = YAHOO.util.Connect.asyncRequest("get", requestURL, callback);
}

function handleFailedSetStrength (o) {
  alert("Error while setting lexical mapping strength.");
  }

function handleSetStrength(o) {
  var termId = o.argument.termId;
  var inputParent = o.argument.doc.location.reload();
  }



function setGenString(assertionId, action, doc) {
  var callback = {
    success: handleSetGenString,
    failure: handleFailedSetGenString,
    argument: {doc: doc} 
  };
  var host = window.location.protocol + "//" + window.location.host;
  var requestURL = host + "/cgi-bin/ws/xml-set-genstring-assertion?assert=" + assertionId + "&action=" + action;
  var request = YAHOO.util.Connect.asyncRequest("get", requestURL, callback);
}

function handleFailedSetGenString (o) {
  alert("Error while adding/removing genStringAssertion assertion.");
  }

function handleSetGenString(o) {
  var inputParent = o.argument.doc.location.reload();
  }

