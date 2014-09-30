var isaId = "Mx4rvViBBJwpEbGdrcN5Y29ycA";
var genlsId = "Mx4rvViBDpwpEbGdrcN5Y29ycA";

function startCreate(termId, eltId, createType) {
  var elt = document.getElementById(eltId);
  //see fi there's already one of these.  If so, re-use it.
  var inputParentId = getCreateParentId(createType, eltId);
  var inputSpanId = getCreateInputSpanId(createType, eltId);
  var inputParent = document.getElementById(inputParentId);
  var inputId = getCreateInputId(createType, eltId);
  var inputElt = document.getElementById(inputId);
  if (!inputParent) {
    inputParent = document.createElement("div");
    inputParent.setAttribute("id", inputParentId);
    elt.parentNode.insertBefore(inputParent, elt.nextSibling);
  }
  var keyPress = "if (isEnter(event)) {document.getElementById('" + inputId + "').blur();}"
    if (!document.getElementById(inputSpanId)) {
      inputSpan = document.createElement("span");
      inputSpan.setAttribute("id", inputSpanId);
      inputParent.insertBefore(inputSpan, inputParent.firstChild);
      inputSpan.innerHTML = "Name: <input onkeypress=\"" + keyPress + "\" action='javascript:null;' type='text' style='width:125' id='" + inputId + "' onchange='createNamed(\"" + termId + "\", \"" + eltId + "\", \"" + createType + "\");'></input>";
      inputSpan.innerHTML += literalQueryHTML(termId, createType);
    }
  document.getElementById(inputId).focus();
}

function literalQueryHTML (termId, createType) {
  var predId;
  var argpos=2;
  switch (createType) {
  case "spec": 
    predId = genlsId;
    break;
  case "instance":
    predId = isaId;
    break;
  default:
    return "";
  }
  return "<a target='content' href='cg?cb-lq&" + termId + "&" + argpos + "&" + predId + "'><img border='0' src='/cycdoc/img/cb/plus-green.gif'/></a>";
}

function getCreateParentId (createType, eltId) {
  return "create" + createType + eltId; 
}

function getCreateInputSpanId (createType, eltId) {
  return "create" + createType + "span" + eltId;
}

function getCreateInputId(createType, eltId) {
  return "create" + createType + "Input" + eltId;
}

function getCreateNewTermsId(createType, eltId) {
  return "create" + createType + "Completed" + eltId;
}

function createNamed(termId, originatingEltId, createType) {
  var inputElt = document.getElementById(getCreateInputId (createType, originatingEltId));
  var name = inputElt.value;
  var callback = {
    success: handleCreate,
    failure: handleFailedCreate,
    argument: {originatingEltId: originatingEltId,
               inputString: name,
               createType: createType}
  };
  var host = window.location.protocol + "//" + window.location.host;
  var requestURL = host + "/cgi-bin/ws/xml-term-create?createType=" + createType + "&name=" + encodeURIComponent(encodeURIComponent(name)) + "&baseType=" + termId;
  var request = YAHOO.util.Connect.asyncRequest("get", requestURL, callback);
  inputElt.disabled=true;
}

function handleFailedCreate (o) {
  var createSpan = document.getElementById(getCreateInputSpanId(o.argument.createType, o.argument.originatingEltId));
  createSpan.innerHTML="Error. Unable to create instance.";   
  }

function handleCreate(o) {
  var newId = o.responseXML.documentElement.getAttribute("externalId");
  var newAnchor = o.responseXML.documentElement.getAttribute("url");
  var originatingId = o.argument.originatingEltId;
  var createType = o.argument.createType;
  var inputParent = document.getElementById(getCreateParentId(createType, originatingId));
  var createdEltId = getCreateNewTermsId(createType, originatingId);
  var createdElt = document.getElementById(createdEltId);
  if (!createdElt) {
    createdElt = document.createElement("div");
    createdElt.setAttribute("id", createdEltId);
    inputParent.appendChild(createdElt, inputParent.firstChild);
  }
  createdElt.innerHTML =
    "New Constant: " + decodeURIComponent(newAnchor) + "<br>" +
    createdElt.innerHTML;
  var inputSpanId = getCreateInputSpanId(createType, originatingId);
  var inputSpan = document.getElementById(inputSpanId);
  if (inputSpan) inputSpan.parentNode.removeChild(inputSpan);
  }


function isEnter(e) {
  var keynum;
  var keychar;
  if (window.event) {//IE
    keynum = e.keyCode;
  } else if (e.which) { //Netscape/firefox/opera
    keynum = e.which;
  }
  keychar = String.fromCharCode(keynum);
  return (keynum == 13);
    }
    
  
