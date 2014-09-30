var adornmentsHTML = "<br>"+ 

"<span id='configbox' style='color:gray;border:solid green 1px;visibility:hidden;position:absolute;left:-9999px;background-color:white;padding:5px;z-index:4'>"+
  "<table width='100%'><tr><td align='right'><font id='hideconfigline' onClick='dohideconfigline();' style='visibility:hidden;font-size:14pt;color:#aaaaaa' class=\"handhover\" onmouseover='this.style.color=\"#000000\";' onmouseout='this.style.color=\"#aaaaaa\";'>&#215;</font></td></tr></table>"+
  "<span id='configboxline' style='visibility:hidden;font-size:11pt;white-space:nowrap'>"+
    "<input type='checkbox' id='vresinlinetogglespan' onClick='resinlinetoggle()' title='Control whether to show results over the text or on the side' style='font-size:11pt'><label for='vresinlinetogglespan'>Show Results in Text Area</label>"+ 
    "<br>"+
    "<input type='checkbox' id='vsmblinktogglespan' onClick='smblinktoggle()' title='Control whether matching parentheses blink for 300ms' style='font-size:11pt'><label for='vsmblinktogglespan'>Show Match With Blink</label>"+ 
    "<br>"+
    "<input type='checkbox' id='vaitogglespan' onClick='aitoggle()' title='Automatically indent new lines' style='font-size:11pt'><label for='vaitogglespan'>AutoIndent</label>"+ 
    "<br>"+
    "<input type='checkbox' id='vsmtogglespan' onClick='smtoggle()' title='Automatically show matching parenthesis' style='font-size:11pt'><label for='vsmtogglespan'>AutoMatch</label>"+
    "<br>"+
    "<input type='checkbox' id='vsctogglespan' onClick='sctoggle()' title='Automatically show comment for last open predicate' style='font-size:11pt'><label for='vsctogglespan'>AutoComment</label>"+ 
    "<br>"+
    "<input type='checkbox' id='vactogglespan' onClick='actoggle()' title='Automatically search for term completions' style='font-size:11pt'><label for='vactogglespan'>AutoComplete</label>"+ 
    "<br>"+
    "<input type='checkbox' id='vwctogglespan' onClick='wctoggle()' title='Automatically perform wff-checking' style='font-size:11pt'><label for='vwctogglespan'>AutoCheck</label>"+ "<br>"+
  "</span>"+
"</span>"+

"<span id='buttonspan' style='white-space:nowrap'>"+
  "<input id='showopts-3' type='button' value='Options' title='Show and select options' onClick='popoptions();' style='font-size:8pt'>"+
  "<input id='fmtLbutton-3' type='button' value='FormatLine' title='Format while respecting current line breaks (do not insert newlines)' onClick='doformatL();' style='font-size:8pt'>"+
  "<input id='fmtAbutton-3' type='button' value='FormatAll' title='Format with permission to insert newlines' onClick='doformatA();' style='font-size:8pt'>"+
  "<input id='corrbutton-3' type='button' value='Insert Lines' title='Insert newlines into long lines' onClick='docorr();' style='font-size:8pt'>"+
  //"<input id='undobutton-3' type='button' value='Undo' onClick='doundo();' style='font-size:8pt'>"+
  //"<input id='redobutton-3' type='button' value='Redo' onClick='doredo();' style='font-size:8pt'>"+
  "<input id='cpbutton-3' type='button' value='Append )s' title='Append missing end parentheses' onClick='closeparens();' style='font-size:8pt'>"+
  "<input id='smbutton-3' type='button' value='Match This )' title='Find matching parentheses' onClick='forcesm();' style='font-size:8pt'>"+
  "<input id='gcbutton-3' type='button' value='Get Comment' title='Show comment for last open predicate' onClick='forcecomment();' style='font-size:8pt'>"+
  "<input id='acbutton-3' type='button' value='Complete' title='Complete term ending at endSelection point (and possibly replace whole term with ctrl-space)' onClick='forceac();' style='font-size:8pt'>"+
  "<input id='searchbutton-3' type='button' value='Search' title='Search term ending at endSelection point (and possibly replace whole term)' onClick='dosearch();' style='font-size:8pt;visibility:visible'>"+
  "<input id='wcbutton-3' type='button' value='WFF/OK?' title='Check for well-formedness of entire text' onClick='forcewc();' style='font-size:8pt'>"+
  "&nbsp;&nbsp;<span id='wffok'>&nbsp;</span>"+
  "&nbsp;&nbsp;&nbsp;<font id='msgs'>&nbsp;</font>"+
"</span>"+

// "<span id='mmfont' style='color:#eeeeee'>*</span>"+
// "<span id='dosfont' style='color:#eeeeee'>*</span>"+
// "<span id='debug' style='color:#eeeeee'>*</span>"+
"<br>"+
"<span id='commentspan' style='visibility:hidden;height:0'><input id='comment' name='comment' size=0 style='visibility:hidden;height:0'></span>"+

"<div id='wffexplain' style='padding:3px;overflow-x:hidden;overflow-y:auto;white-space:pre;position:absolute;top:-999px;left:-999px;max-height:335px;background-color:white;z-index:+1'>&nbsp;</div>"+

// "<input id='searchbutton-3' type='button' value='Search' onClick='dosearch();' style='position:absolute;top:0px;font-size:8pt;visibility:visible'>"+
"<span id='se-search-results-3' class='yui-ac-container' style='left:450px;width:350px;padding:3px;top:30px;font-size:8pt;font-family:arial;z-index:2'></span>"+
"<form method='GET' action=''>"+
"<font id='se-autocomplete-3' class='yui-ac' style='position:absolute;visibility:hidden;top:500;left:600;width:650;font-size:8pt;z-index:2'>"+
"<input id='se-input-3' size='10' style='visibility:hidden;color:cccccc;width:350px;top:0px;height:0px;font-size:0pt;z-index:-99' class='yui-ac-input'><br>"+
"<span id='se-autocomplete-results-3' class='yui-ac-container' style='left:0px;top:0px;width:350px;font-size:8pt;z-index:2'></span>"+
"</font>"+
"</form>"+
"<style>"+
"  font.handhover:hover { cursor: pointer }"+
"</style>"+

"";


//"<textarea id='sentenceID' wrap='physical' style='resize:none' rows='20' onClick='setTimeout(\"clicktokc();dowffck();\", 100);' onKeyUp='setTimeout(\"keytokc();dowffck();\", 100);'></textarea>"+

function dobj(x) { return document.getElementById(x); }
function bepix(n) { return n + 'px'; }

var numrows = new Array;
var numcols = new Array;
numrows['sentence'] = 50;
numcols['sentence'] = 80; // 80 is default but will be overridden by cols in the setup call
var orgnumcols = new Array; // stores cols from the original setup

var focalid; // this is a convenient global, set on change of obj.focus

var mainformobj;
var masterdowffchecking;

function setupSentenceEditor(tempid, cols, useqno, dowffchecking) {
  focalid = tempid;
  orgnumcols[focalid] = cols; // does not vary
  numcols[focalid] = cols; // allowed to vary
  if (dowffchecking != null) masterdowffchecking = dowffchecking;
  else masterdowffchecking = true;

  var obj = dobj(focalid);
  if (focalid == 'sentence') {
    var preDiv = document.createElement("div");
    preDiv.innerHTML = adornmentsHTML;
    var parent = obj.parentNode;
    parent.insertBefore(preDiv, obj);
    var postDiv = document.createElement("span");
    postDiv.innerHTML = "<font class='handhover' color='gray' id='opencommentbutton' onclick='opencomment();' title='click to manually open comment/matching parenthesis box (will otherwise open automatically when needed)' style='vertical-align:top'>&nbsp;&#8595;</font>";
    parent.insertBefore(postDiv, obj.nextSibling);
  }
  obj.setAttribute("wrap", "soft");
  obj.setAttribute("style", "resize:both");

  obj.setAttribute("onClick", "clicktokc(); return true;");
  obj.setAttribute("onKeyUp", "keytokc(); return true;");
  obj.setAttribute("onfocus", "setfocalid('" + focalid + "'); return true;");

  obj.style.fontSize = '10pt';
  obj.style.fontFamily = 'monospace';
  obj.rows = Math.floor(.5 + 1.5*obj.rows);
  numrows[focalid] = obj.rows;

  window.onunload = savevals;
  document.onkeydown = kcheck; // have to capture keys before someone else grabs them, esp for arrowing around menus // onkeydown is right; onKeydown will not work
  // obj.onmousewheel = wcheck; // this was old stuff to suppress showmatch on wheel motions
  // if (obj.addEventListener) obj.addEventListener('DOMMouseScroll', wcheck, false);

  if (focalid == 'sentence') {
    // figure out who is the main form -- it differs based on the page
    var allf = document.forms;
    for (var allfi=0; allfi<allf.length; allfi++) { 
      var allt = allf[allfi].getElementsByTagName('textarea');
      for (var allti=0; allti<allt.length; allti++) {
        if (allt[allti].id == undefined) continue;
        if (allt[allti].id == 'sentence') mainformobj = allf[allfi];
      }
    }

    mainformobj.setAttribute('onsubmit',''); // kill the old java attachment
    restorepreferences(); // this fills in the options menu choices
    if (smmode || scmode) opencomment();

    initializeConstantComplete3(); // get the YUI2 set up on the hidden input box
    locatedtop = obj.offsetTop; // messages will use this as an anchor // could use commentspan top, but then where would wff messages go?
    // constantComplete3.autoHighLight = false; // why does this get unset after being assigned above?
    if (mainformobj.getAttribute('onsubmit')) mainformobj.setAttribute('onsubmit', 'savevals();' + mainformobj.getAttribute('onsubmit'));
    else mainformobj.setAttribute('onsubmit', 'savevals();');
    fillmtnames();
    // alert(localStorage.useqno+','+useqno);
    if (localStorage.useqno >= useqno) restorevals();
    var allinputs = mainformobj.getElementsByTagName('input');
    for (ia=0; ia<allinputs.length; ia++) if (allinputs[ia].value == 'Clear Sentence') {
      var ocpast = allinputs[ia].getAttribute('onclick');
      allinputs[ia].setAttribute('onclick', ocpast + ';' );
    }
  }

  var dv = obj.value;
  saveundopoint(focalid, dv, 0);

  // moderinze the look of the surrounding area and button sizes, which should be fixed in the calling lisp
  var allareas = document.getElementsByTagName('textarea');
  for (var iarea in allareas) {
    if (allareas[iarea].type == 'textarea' && allareas[iarea].id.match(/^input-[0-9]/)) allareas[iarea].rows = '1';
  }
  var allbuttons = document.getElementsByTagName('input');
  for (var ibutton in allbuttons) {
    if (allbuttons[ibutton].type == 'button') allbuttons[ibutton].style.fontSize = '8pt';
    if (allbuttons[ibutton].type == 'reset') allbuttons[ibutton].style.fontSize = '8pt';
    if (allbuttons[ibutton].type == 'submit') allbuttons[ibutton].style.fontSize = '8pt';

    if (allbuttons[ibutton].value == 'Assert Sentence') allbuttons[ibutton].style.fontSize = '12pt';
    if (allbuttons[ibutton].value == 'Start Inference') allbuttons[ibutton].style.fontSize = '12pt';
    if (allbuttons[ibutton].value == 'Start as New') allbuttons[ibutton].style.fontSize = '12pt';
    if (allbuttons[ibutton].value == 'Perform Edit') allbuttons[ibutton].style.fontSize = '12pt';

    if (allbuttons[ibutton].value == 'Assert Sentence') allbuttons[ibutton].style.backgroundColor = '#ccccee';
    if (allbuttons[ibutton].value == 'Start Inference') allbuttons[ibutton].style.backgroundColor = '#ccccee';
    if (allbuttons[ibutton].value == 'Start as New') allbuttons[ibutton].style.backgroundColor = '#ccccee';
    if (allbuttons[ibutton].value == 'Perform Edit') allbuttons[ibutton].style.backgroundColor = '#ccccee';
  }

  // dobj('comment').innerHTML = useqno;
  // dobj('msgs').innerHTML = useqno;
  localStorage.setItem('focalid', focalid);
  localStorage.setItem('cols' + focalid, cols);
  localStorage.setItem('useqno', useqno);

  setTimeout("savevals();", 3000); // delay just to be nice and reduce setup drag

  setTimeout("dobj(focalid).focus();", 500);

}

function setfocalid(fidname) { focalid = fidname; }

var textareawho = new Array; // converts name to id for surrounding textareas

function fillmtnames() {
  var allt = mainformobj.getElementsByTagName('textarea');
  for (var allti=0; allti<allt.length; allti++) if (allt[allti].name != undefined) { if (allt[allti].name.match(/mt-/)) if (allt[allti].id != undefined) textareawho[allt[allti].name] = allt[allti].id; }
}

var storednumcols; 

function restorepreferences() {
// booleans were coerced to strings in storage
  if (localStorage.wcmode == 'true') wcmode = true;
  else wcmode = false;
  if (localStorage.smmode == 'true') smmode = true;
  else smmode = false;
  if (localStorage.scmode == 'true') scmode = true;
  else scmode = false;
  if (localStorage.smblink == 'true') smblink = true;
  else smblink = false;
  if (localStorage.numcolssentence != null) storednumcols = parseInt(localStorage.numcolssentence);
  else storednumcols = orgnumcols['sentence'];
  if (storednumcols > orgnumcols['sentence']) numcols['sentence'] = orgnumcols['sentence'] + Math.floor(orgnumcols['sentence']/2);
  if (localStorage.aimode == 'true') aimode = true;
  else aimode = false;
  if (localStorage.acmastermode == 'true') acmastermode = true;
  else acmastermode = false;
  if (localStorage.resinline == 'true') resinline = true;
  else resinline = false;
}

function restorevals() {
  var showv = '';
  var titlename = document.getElementsByTagName('title')[0].innerHTML;
  dobj('sentence').value = localStorage.getItem(titlename + 'sentence');
  // dobj('comment').innerHTML = localStorage.getItem(titlename + 'comment');
  dobj(textareawho['mt-monad']).value = localStorage.getItem(titlename + 'input0');
  dobj(textareawho['mt-time-interval']).value = localStorage.getItem(titlename + 'input1');
  dobj(textareawho['mt-time-parameter']).value = localStorage.getItem(titlename + 'input2');
  // dobj('sentence').selectionStart = localStorage.sentenceSelectionStart;
  // dobj('sentence').selectionEnd = localStorage.sentenceSelectionEnd;
  if (dobj('non_exp_sentence') != undefined) dobj('non_exp_sentence').value = localStorage.getItem(titlename + 'non_exp_sentence');
  // mainformobj.setAttribute('onsubmit', 'savevals();' + mainformobj.getAttribute('onsubmit'));
  var eall = mainformobj.getElementsByTagName('input'); 
  for (var i=0; i<eall.length; i++) { 
    if (eall[i].type == 'radio') {
      if (localStorage.getItem(titlename + eall[i].name + ',' + eall[i].value + 'Checked') == 'true') eall[i].checked = true; showv += ';'+eall[i].name+','+eall[i].value+'='+eall[i].checked;
    } else if (eall[i].type == 'checkbox') {
      if (localStorage.getItem(titlename + eall[i].name + 'Checked') == 'true') { if (eall[i].checked != undefined) eall[i].checked = true; showv += ';'+eall[i].name+'='+eall[i].checked }
    } else if (eall[i].type == 'input' || eall[i].type == 'text') {
      if (eall[i].value != undefined) { eall[i].value = localStorage.getItem(titlename + eall[i].name + 'Value'); showv += ';'+eall[i].name+'='+eall[i].value }
    }
  }
}

function savevals() {
  // looks like you lose the undo/redo stack and browser undo info
  // options and mode values are stored separately
  var showv = '';
  var titlename = document.getElementsByTagName('title')[0].innerHTML;
  localStorage.setItem(titlename + 'sentence', dobj('sentence').value);
  // localStorage.setItem(titlename + 'comment', dobj('comment').innerHTML);
  localStorage.setItem(titlename + 'input0', dobj(textareawho['mt-monad']).value);
  localStorage.setItem(titlename + 'input1', dobj(textareawho['mt-time-interval']).value);
  localStorage.setItem(titlename + 'input2', dobj(textareawho['mt-time-parameter']).value);
  localStorage.setItem(titlename + 'sentenceSelectionStart', dobj('sentence').selectionStart);
  localStorage.setItem(titlename + 'sentenceSelectionEnd', dobj('sentence').selectionEnd);
  if (dobj('non_exp_sentence') != undefined) localStorage.setItem(titlename + 'non_exp_sentence', dobj('non_exp_sentence').value);
  var eall = mainformobj.getElementsByTagName('input'); 
  for (var i=0; i<eall.length; i++) { 
    if (eall[i].type == 'radio') {
      localStorage.setItem(titlename + eall[i].name + ',' + eall[i].value + 'Checked', eall[i].checked); showv += ';'+eall[i].name+','+eall[i].value+'='+eall[i].checked
    } else if (eall[i].type == 'checkbox') {
      if (eall[i].checked != undefined) { localStorage.setItem(titlename + eall[i].name + 'Checked', eall[i].checked); showv += ';'+eall[i].name+'='+eall[i].checked }
    } else if (eall[i].type == 'input' || eall[i].type == 'text') {
      if (eall[i].value != undefined) { localStorage.setItem(titlename + eall[i].name + 'Value', eall[i].value); showv += ';'+eall[i].name+'='+eall[i].value }
    }
  }
}

function striptp(x) { // could actually parse it, but this will suffice for wff cycl without strings
  x = x.replace(/^\#\$/, ''); 
  x = x.replace(/ \#\$/, ' '); 
  x = x.replace(/\(\#\$/, '('); 
  return(x); 
}

function findcommonprefix(x,y) {
  var tempc = '';
  for (var itemp=0; itemp < y.length && itemp <= x.length; itemp++) {
    if (y.slice(itemp,itemp+1) == x.slice(itemp,itemp+1)) tempc = tempc + y.slice(itemp,itemp+1);
    else break;
  }
  return tempc;
}

var acresultnls = new Array;
var acresultterms = new Array;
var acresultn = 0;

function setcc3options() {
  constantComplete3.allowBrowserAutocomplete = false;
  // constantComplete3.queryMatchSubset = false; // do not let it use cache -- keep it communicating!
  constantComplete3.autoHighlight = false;
  constantComplete3.minQueryLength = 3;
  constantComplete3.animSpeed = 0.1;
  constantComplete3.maxResultsDisplayed = 20;
  constantComplete3.queryDelay = 0.3;
  constantComplete3.embeddedInToolbar = false;
  constantComplete3.highlightClassName = "";
  // constantComplete3.maxCacheEntries = 0;
}

var constantCompleteDataSource3;
var constantComplete3;
var lastConst = '';
function initializeConstantComplete3() {
// constantCompleteDataSource3 = new YAHOO.widget.DS_XHR('../../cgi-bin/cg', ['Term', 'cycl', 'nl']);
constantCompleteDataSource3 = new YAHOO.widget.DS_XHR('../../cgi-bin/cg', ['Term', 'cycl']);
// constantCompleteDataSource3.scriptQueryParam = 'xml-complete&filter=c297&junk=' + Math.random() + '&prefix';
constantCompleteDataSource3.scriptQueryParam = 'xml-complete&filter=c297&prefix';
// add constraining-stentence=...
constantCompleteDataSource3.responseType = YAHOO.widget.DS_XHR.TYPE_XML;
constantComplete3 = new YAHOO.widget.AutoComplete('se-input-3', 'se-autocomplete-results-3', constantCompleteDataSource3);
setTimeout('setcc3options();',1000);
constantComplete3.setHeader("<table width='100%'><tr><td align='right' valign='top'><font class='handhover' style='font-size:14pt' onclick='hideac();' color='gray'>&nbsp;&#215;</font></td></tr></table>");
constantComplete3.formatResult = function(oResultItem, sQuery) {
  var thisConst = oResultItem[0];
  //var string = oResultItem[1];
  //acresultnls[acresultn] = string;
  acresultterms[acresultn] = thisConst;
  //var sMarkup = '<span id="acresultline' + acresultn + '" onMouseOver="acconsider(' + acresultn + ');" onMouseOut="acdark(' + acresultn + ');">' + string + ' <span class=\"autoCompCycL\" style=\"color:#008030\">' + thisConst + '</span></span>';
  var sMarkup = '<span id="acresultline' + acresultn + '" onMouseOver="acconsider(' + acresultn + ');" onMouseOut="acdark(' + acresultn + ');">' + ' <span class=\"autoCompCycL\" style=\"color:#008030;font-size:9pt;line-height:11pt\">' + thisConst + '</span></span>';
  ++acresultn;
  dobj('se-autocomplete-3').style.visibility = 'visible';
  sendmsg('');
  if (commonacprefix == '') commonacprefix = thisConst;
  else if (thisConst.length > 2) {
    var tempc = findcommonprefix(thisConst,commonacprefix);
    var tempd = findcommonprefix(thisConst,lastConst);
    if (tempd.length > 2 && !seenasac[tempd]) commonacprefixes[commonacprefixes.length] = tempd;
    seenasac[tempd] = true;
    // console.log(tempd + ',' + seenasac[tempd] + ',' + commonacprefixes.length);
    commonacprefix = tempc;
  }
  lastConst = thisConst;
  return (sMarkup);
};
var returnHandler3 = function(sType, aArgs) {
  if (!acresultn) sendmsg('no completions found');
  //for (var i=0; i<acresultn; i++) delete acresultnls[i];
  for (var i=0; i<acresultn; i++) delete acresultterms[i];
  acresultn = 0;
  lastnrotated = -1;
  hidesearch();
  if (resinline) inlinelocate(focalid);
  else sidelocate(focalid);
  acvisible = true;
  clearwffmsgs();
  // dobj('se-autocomplete-3').style.visibility = 'visible';
  setTimeout("dobj('acbutton-3').value = 'Complete';", 100); // pops out to soon otherwise // purely cosmetic
}
constantComplete3.dataRequestEvent.subscribe(returnHandler3);
var selectHandler3 = function(sType, aArgs) {
  // dmsg('entering handler' + aArgs[0] + '/' + aArgs[1] + '/' + aArgs[2]);
  // aArgs2 looks like:  #$BiologicalAndToxinWeaponsConvention,BiologicalAndToxinWeaponsConvention
  var chosenresult = aArgs[2];
  var chosenvalue = chosenresult[0];
  if (chosenvalue != "") doselect(focalid, chosenvalue, true);
};
constantComplete3.itemSelectEvent.subscribe(selectHandler3);
}
var searchresguids = new Array;
var searchresnls = new Array;
var searchresterms = new Array;
var searchresn = 0;
var handleSearchSuccess = function(o) {
  if (o.responseText !== undefined) {
    var searchRespChildren = o.responseXML.getElementsByTagName('Term');
    searchresn = searchRespChildren.length;
    lastnrotated = -1;
    var searchlist = "<table width='100%'><tr><td align='right' valign='top'><font class='handhover' style='font-size:14pt' onclick='hidesearch();' color='gray'>&nbsp;&#215;</font></td></tr></table>";
    if (searchresn == 0) {
      sendmsg('no results found');
      return;
    }
    for (var i=0; i<searchresn; i++) {; // no clearing of prior search data?
      searchresguids[i] = searchRespChildren[i].getAttribute('hlId');
      searchresnls[i] = searchRespChildren[i].getAttribute('nl');
      searchresterms[i] = searchRespChildren[i].getAttribute('cycl');
      searchlist = searchlist + '<font class="searchresultline_class" id="searchresultline' + i + '" onClick="dochoose(' + i + ')" onMouseOver="srlite(' + i + ');" onMouseOut="srdark(' + i + ');">&nbsp;&nbsp;&nbsp;' + searchresnls[i] + '<br>';
      searchlist = searchlist + '<span class="autoCompCycL" style=\"color:#003080\">' + searchresterms[i] + '</span></font><br>';
    }
    if (searchresn == 1) { // go ahead and commit the single returned value
      sendmsg('unique result found.');
      dochoose(0); // numbering starts at 0
      return;
    }
    dobj('se-search-results-3').innerHTML = searchlist;
    dobj('se-search-results-3').style.border = 'solid black 1px';
    dobj('se-search-results-3').style.backgroundColor = 'white';
    hideac();
    if (resinline) inlinelocate(o.argument.foo);
    else sidelocate(o.argument.foo);
    searchvisible = true;
  }
  dobj('searchbutton-3').value = 'Search';
}
var handleSearchFailure = function(o) {
  dobj('searchbutton-3').value = 'Search';
}
var searchcallback = {
  success:handleSearchSuccess,
  failure:handleSearchFailure,
  argument: { foo:"food", bar:"barn" }
}

function matchoptionvalues() {
  if (resinline) maketrue('vresinlinetogglespan');
  else makegray('vresinlinetogglespan');
  if (smblink) maketrue('vsmblinktogglespan');
  else makegray('vsmblinktogglespan');
  if (smmode) maketrue('vsmtogglespan');
  else makegray('vsmtogglespan');
  if (scmode) maketrue('vsctogglespan');
  else makegray('vsctogglespan');
  if (acmastermode) maketrue('vactogglespan');
  else makegray('vactogglespan');
  if (aimode) maketrue('vaitogglespan');
  else makegray('vaitogglespan');
  if (wcmode) maketrue('vwctogglespan');
  else makegray('vwctogglespan');
}

function maketrue(x) { dobj(x).checked = 'true'; }
function makegray(x) { dobj(x).style.color = 'gray'; }

function popoptions() {
  matchoptionvalues();
  doshowconfigline();
  dobj('configbox').style.left = '100px';
  dobj('configbox').style.visibility = 'visible';
}

function doshowconfigline() {
  dobj('configboxline').style.visibility = 'visible';
  dobj('hideconfigline').style.visibility = 'visible';
}

function dohideconfigline() {
  dobj('configboxline').style.visibility = 'hidden';
  dobj('hideconfigline').style.visibility = 'hidden';
  dobj('configbox').style.left = '-9999px';
  dobj('configbox').style.visibility = 'hidden';
}

var wcmode = false;

function wctoggle() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  if (wcmode == true) { 
    wcmode = false;
    if (wtimeout != undefined) clearTimeout(wtimeout);
    clearwffmsgs();
  } else {
    wcmode = true;
    ok2ac = true;
    ok2sm = true;
    dowffck(focalid);
  }
  matchoptionvalues();
  localStorage.setItem('wcmode', wcmode);
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

var smmode = false;

function smtoggle() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  if (smmode) { 
    smmode = false;
    clearcomment();
  } else {
    smmode = true;
    ok2ac = true;
    ok2sm = true;
  }
  if (smmode || scmode) opencomment();
  if (!smmode && !scmode) clearcomment();
  matchoptionvalues();
  localStorage.setItem('smmode', smmode);
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

var scmode = false;

function sctoggle() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  if (scmode) { 
    scmode = false;
    clearcomment();
  } else {
    scmode = true;
    forcecomment();
  }
  if (smmode || scmode) opencomment();
  if (!smmode && !scmode) clearcomment();
  matchoptionvalues();
  localStorage.setItem('scmode', scmode);
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

var smblink = false;

function smblinktoggle() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  if (smblink) { 
    smblink = false;
  } else { 
    smblink = true;
  }
  matchoptionvalues();
  localStorage.setItem('smblink', smblink);
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

var aimode = false;

function aitoggle() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  if (aimode) { 
    aimode = false;
  } else { 
    aimode = true;
  }
  matchoptionvalues();
  localStorage.setItem('aimode', aimode);
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

var acmastermode = false;

function actoggle() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  if (acmastermode) {
    acmastermode = false;
    hideac();
  } else {
    acmastermode = true;
  }
  matchoptionvalues();
  localStorage.setItem('acmastermode', acmastermode);
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

var resinline = false;

function resinlinetoggle() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  if (resinline) {
    resinline = false;
    sidelocate(focalid);
  } else {
    resinline = true;
    inlinelocate(focalid);
  }
  matchoptionvalues();
  localStorage.setItem('resinline', resinline);
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

function inlinelocate(focalid) {
  dobj('se-autocomplete-3').style.top = bepix(locatedtop + 80);
  dobj('se-autocomplete-3').style.left = bepix(locatedleft + 20);
  dobj('se-search-results-3').style.top = bepix(locatedtop + 40);
  dobj('se-search-results-3').style.left = bepix(locatedleft + 20);
  dobj('wffexplain').style.top = bepix(locatedtop + 140);
  dobj('wffexplain').style.left = bepix(locatedleft + 20);
}

function sidelocate(focalid) {
  dobj('se-autocomplete-3').style.top = bepix(dobj(focalid).offsetTop);
  dobj('se-autocomplete-3').style.left = bepix(dobj(focalid).offsetLeft + dobj(focalid).offsetWidth + 10);
  dobj('se-search-results-3').style.top = bepix(dobj(focalid).offsetTop + 2);
  dobj('se-search-results-3').style.left = bepix(dobj(focalid).offsetLeft + dobj(focalid).offsetWidth + 5);
  dobj('wffexplain').style.top = bepix(dobj('buttonspan').offsetTop + 30);
  dobj('wffexplain').style.left = bepix(dobj(focalid).offsetLeft + dobj(focalid).offsetWidth + 40);
}

function autoindent() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  var atext = dobj(focalid).value;
  var latext = atext.slice(0, cwhere);
  var ratext = atext.slice(cwhere);
  nwha = latext.match(/\(/g);
  if (nwha != null) nopeneds = nwha.length;
  else nopeneds = 0;
  nwha = latext.match(/\)/g);
  if (nwha != null) ncloseds = nwha.length;
  else ncloseds = 0;
  ndiff = nopeneds - ncloseds;
  var tinsert = makespaces(ndiff, '');
  latext += tinsert;
  dobj(focalid).value = latext + ratext;
  dobj(focalid).selectionEnd = cwhere + tinsert.length;
  dobj(focalid).selectionStart = dobj(focalid).selectionEnd;
}

function hidesearch() {
  searchvisible = false;
  dobj('se-search-results-3').style.border = 'solid black 0px';
  dobj('se-search-results-3').innerHTML = '';
  dobj('se-search-results-3').style.backgroundColor = 'transparent';
  dobj('searchbutton-3').value = 'Search';
}

var acvisible = false;
var searchvisible = false;

function hideac() {
  acvisible = false;
  dobj('se-autocomplete-3').style.visibility = 'hidden';
  dobj('se-autocomplete-3').style.left = '-999px';
  dobj('acbutton-3').value = 'Complete';
}

function keytokc() { // slow keyclick pathway to acsm
  if (ok2ac == false && ok2sm == false) return;
  hidesearch();
  if (searchrequest) YAHOO.util.Connect.abort(searchrequest, searchcallback, 'search abort');
  kc(focalid);
}

function clicktokc() { // slow mousecklick pathway to acsm
  // entered thru a click, like a paste or a re-point;
  hidesearch(); // was commented out for persistent results 
  hideac(); // was commented out for persistent results
  if (!('selectionEnd' in dobj(focalid))) return;
  if (dobj(focalid).selectionStart != dobj(focalid).selectionEnd) return; // they are selecting text -- leave them alone
  ok2ac = false;
  ok2sm = true;
  if (searchrequest) YAHOO.util.Connect.abort(searchrequest, searchcallback, 'search abort');
  kc(focalid);
}

var maxtermlen = 50;
var maxaclen = 50;
var kctimeout;

function kc(focalid) { // the primary check to do stuff @cursor routine
  var dv = dobj(focalid).value;
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;

  var lastchar = dv.slice(cwhere - 1, cwhere);
  // var lastchar = dv.substring(cwhere - 1, 1); // see if this is faster // seems slower
  // if (!lastchar.match(/[a-z]|[A-Z]|[0-9]|\_|\-|\:/)) { hideac(); ok2ac = false; } // disabling this might be faster

  var uq = "";

  if (wcmode && !smlockout && ok2wff) {
    var uq = underquotes(dv, cwhere);
    if (!uq) dowffck2(focalid); 
  }

  if (ok2sm && smmode) {
    if (lastchar == ')' && lastkeypressed == 48) { ok2sm = false; showmatch(focalid); return; }
    if (lastchar == '(' && lastkeypressed == 57) { ok2sm = false; showmatch2(focalid); return; }
    if (lastchar == ')' && lastkeypressed != 48 && ok2sm == true) { ok2sm = false; showmatch3(focalid); return; }
    if (lastchar == '(' && lastkeypressed != 57 && ok2sm == true) { ok2sm = false; showmatch2(focalid); return; }
  }
  if (lastchar == '\n' && lastkeypressed == 13) { if (aimode) autoindent(focalid); return; }

  if (uq) return;

  if (scmode) {
    findpword(dv, cwhere);
    // if (!findpword(dv, cwhere)) clearcomment();
    // else if (pword != showingcommentword) dogetcomment(pword);
    if (pword != showingcommentword) dogetcomment(pword);
    else dobj('comment').style.width = bepix(dobj(focalid).offsetWidth - dobj(focalid).offsetLeft - 20 + 4); // let's be manic about this
  }

  if (!acmastermode) return;

  if (kctimeout != undefined) clearTimeout(kctimeout);
  if (ok2ac) kctimeout = setTimeout("doac(focalid);",300);
}

function correctlonglines(focalid, cols) {
  var dv = dobj(focalid).value;
  if ('selectionEnd' in dobj(focalid)) savepoint = dobj(focalid).selectionEnd;
  else savepoint = 0;
  var dvnew = '';
  var dvlines = dv.replace(/%0A/ig, '\n').split('\n', 1000);
  var ndvlines = dvlines.length;
  var countiters = 0;
  var mycount = 0;
  var poffset = 0;
  for (var il=0; il<ndvlines; il++) {
    if (dvlines[il].length <= cols) {
      dvnew += '\n' + dvlines[il]; 
      mycount += dvlines[il].length;
    } else {
      var remain = dvlines[il];
      while (remain) {
	countiters++;
	if (countiters > 100) { alert('long line correction loop error count iter = ' + countiters); return }
        var remove = remain.slice(0, cols);
        var removel = remove.length;
	// possibly stick the last word on the next line
	if (removel == cols && remove.slice(removel - 1, removel) != ' ') {
	  var finalword = remove.match(/[^ ]*$/);
	  var finalwordl = finalword[0].length;
	  if (finalwordl < cols) {  // we won't go all the way back -- or else this loop won't terminate!
	    remove = remove.slice(0, removel - finalwordl - 1);
	    remain = finalword + remain;
	  }
	}

        // see if you can track the selection point
	mycount += remove.length + 1;
	if (mycount < savepoint && (mycount < savepoint-finalwordl)) poffset++; // still not quite right
	dvnew += '\n' + remove;
	remain = remain.slice(cols);
      }
    }
  }
  dobj(focalid).value = dvnew.slice(1); // remove leading \n
  dobj(focalid).selectionStart = savepoint + poffset;
  dobj(focalid).selectionEnd = savepoint + poffset;
  dobj(focalid).focus();
}

function clearwffmsgs() {
  clearwffexplain();
  clearwffok();
}

function clearwffexplain() {
  dobj('wffexplain').innerHTML = '';
  dobj('wffexplain').style.border = 'none';
}

function clearwffok() {
  dobj('wffok').innerHTML = '';
}

function clearmsg() {
  dobj('msgs').innerHTML = '';
}

function clearcomment() {
  dobj('commentspan').style.visibility = 'hidden';
  dobj('commentspan').style.height = '';
  dobj('commentspan').innerHTML = "<input id='comment' name='comment' size=0 style='visibility:hidden;height:0'>";
  dobj('comment').innerHTML = '';
  dobj('opencommentbutton').style.visibility = 'visible';
  dobj('opencommentbutton').style.height = '';
  showingcommentword = '';
}

function sendwffok(x) {
  dobj('wffok').innerHTML = x;
}

function sendwffexplain(x) {
  dobj('wffexplain').style.border = 'solid white 0px';
  dobj('wffexplain').innerHTML = x;
}

function sendmsg(x) {
  dobj('msgs').innerHTML = x;
return;
  setTimeout('clearmsg()', 5000);
}

function forcewc() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  dobj('wcbutton-3').value = 'Doing Wff-Check...';
  dowffck2(focalid);
  dobj(focalid).focus();
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

function forcecomment() {
  var dv = dobj(focalid).value;
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;

  var thisword = dv.slice(0, cwhere).replace(/.*[ \n\(\)]/mg,'');
  var remainword = dv.slice(cwhere).replace(/[ \n\(\)].*/mg,'');
  dogetcomment(thisword + remainword);
  
  //if (!findpword(dv, cwhere)) clearcomment();
  //else dogetcomment(pword);

  dobj(focalid).focus();
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

function forcesm() {
  if (smlockout) return;
  opencomment(); // gotta show it somewhere
  var dv = dobj(focalid).value;
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  var pointchar = dv.slice(cwhere - 1, cwhere);
  if (pointchar == ")" || pointchar == "}") { showmatch3(focalid); }
  else if (pointchar == "(" || pointchar == "{") { showmatch2(focalid); }
  else {
    var prefmatch = dv.slice(0, cwhere).match(/\([^\(]*$/); // back up to prior (
    if (prefmatch) {
      var prefix = prefmatch[0];
      cwhere -= (prefix.length - 1);
      dobj(focalid).selectionEnd = cwhere;
      showmatch2(focalid);
    }
  }
  dobj(focalid).focus();
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

function forceac() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) var savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  var lastchar = dobj(focalid).value.slice(cwhere - 1, cwhere);
  if (!lastchar.match(/[a-z]|[A-Z]|[0-9]|\_|\-|\:/)) {
    sendmsg('no autocomplete at this position');
    return;
  }
  var dv = dobj(focalid).value;
  doac(focalid);
  dobj(focalid).focus();
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

var commonacprefix;
var icommonacprefix = 0;
var commonacprefixes = new Array;
var seenasac = new Array;

function doac(focalid) {
  var dv = dobj(focalid).value;
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if (!findlword(dv, cwhere)) return; // false is returned from findlword if lword is a ?
  if (lword.length < 3) return;
  if (lword.length <= maxaclen) {
    hideac();
    hidesearch();
    dobj('acbutton-3').value = 'Completing: ' + lword + '...';
    ok2ac = false;
    constantComplete3.clearList;
    // console.log(lword);
    // constantComplete3.sendQuery("' + lword + '");
    commonacprefix = '';
    commonacprefixes = [];
    seenasac = [];
    constantComplete3.sendQuery(lword);
    if (resinline) locate(focalid, dv.slice(0, cwhere - lword.length + 1)); // will use the locatedtop and locatedleft in results handler
  }
}

function underquotes(dv, cwhere) {
  var nwha = dv.slice(0, cwhere).match(/"/g);
  if (nwha == null) return false;
  if (nwha.length % 2) return true; // dont't fret over escaped quotes -- not worth the drag
  return false;
}

var pword;

function findpword(dv, cwhere) {
  var remain = dv.slice(0, cwhere);
  var parendepth = 0;
  // to do: count open and closed parens
  var nwha; var nopeneds; var ncloseds; var ndiff; var cumdiff = 0;

  var rwords = remain.split(' ', 1000);
  for (var iw=rwords.length - 1; iw>=0; iw--) {

    // update paren count (foo (bar)...<select> should have cumdiff=1 at (foo
    var thisword = rwords[iw];
    nwha = thisword.match(/\(/g);
    if (nwha != null) nopeneds = nwha.length;
    else nopeneds = 0;
    nwha = thisword.match(/\)/g);
    if (nwha != null) ncloseds = nwha.length;
    else ncloseds = 0;
    ndiff = nopeneds - ncloseds;
    cumdiff += ndiff;

    thisword = rwords[iw].replace(/^\(/, ''); // it should have a leading ( but could be ((
    if (ispredicate(thisword) && cumdiff > 0) { pword = thisword; return true; }

  }
  return false;
}

function ispredicate(x) {
  if (x.match(/^[a-z]/)) return true; // fires on user input before search and match!
  if (x.match(/^\#\$[a-z]/)) return true;
  return false;
}

var ctimeout; 

var showingcommentword = '';

function opencomment() {
  dobj('opencommentbutton').style.visibility = 'hidden';
  dobj('opencommentbutton').style.height = '0';
  dobj('commentspan').style.visibility = 'visible';
  dobj('commentspan').style.height = '';
  dobj('commentspan').innerHTML = "<table><tr><td><div id='comment' height='100' style='height:80px;overflow:auto;padding:10px;border:dotted black 1px;font-size:10pt;font-style:italic;background-color:#eeeeee'>&nbsp;</div></td><td valign='top'><font class='handhover' onclick='clearcomment();' color='gray'>&nbsp;&#215;</font></td></tr></table>";
  dobj('comment').style.width = bepix(dobj(focalid).offsetWidth - dobj(focalid).offsetLeft - 20 + 4);
  dobj('comment').style.height = '100';
}

function dogetcomment(x) {
  if (smlockout) return;
  opencomment();
  showingcommentword = x;
  if (x != undefined) x = x.replace(/\n/mg, '').replace(/\#/g, '%23').replace(/\$/g, '%24'); // needs to be there or it will be an erroneous request
  if (ctimeout != undefined) clearTimeout(ctimeout);
  var sUrl = '../../cgi-bin/cg?xml-query-variable&query=%28comment%20' + x + '%20?X%29&mt-monad=InferencePSC';
  commentcallback.argument.foo = x;
  ctimeout = setTimeout('gencommentreq("' + sUrl + '");', 300);

  // wget -q -O - 'http://astro:3602/cgi-bin/cg?xml-qu&query=%28argIsa%20coreConceptOfDomain%203%20?X%29&mt-monad=InferencePSC'
}

var commentrequest;

function gencommentreq(sUrl) {
  commentrequest = YAHOO.util.Connect.asyncRequest('GET', sUrl, commentcallback);
}

var handleCommentSuccess = function(o) {
  if (o.responseXML != undefined)
    if (o.responseXML.getElementsByTagName('Term').length > 0) {
      if (o.argument.foo != undefined) dobj('comment').innerHTML = o.argument.foo.replace(/^%23%24/, '') + ':\n'; // sometimes nice to see what term's comment is being shown
      dobj('comment').innerHTML += o.responseXML.getElementsByTagName('Term')[0].getAttribute('cycl').replace(/(\(\#\$[^\)]*\))/m,"<font color='darkgreen'>$1</font>"); // embed any parenthesized expression in darkgreen // sorry about the extra parens, but i needed the backreference
      dobj('comment').style.width = bepix(dobj(focalid).offsetWidth - dobj(focalid).offsetLeft - 20 + 4);
    }
}
var handleCommentFailure = function(o) {
  if (o.responseText !== undefined) {
    var div = dobj('comment' + lastpoppedn);
    if (div == undefined) return;
    div.innerHTML = "<ul><li>Comment Call FAILURE Transaction id: " + o.tId + "</li>";
    div.innerHTML += "<li>HTTP status: " + o.status + "</li>";
    div.innerHTML += "<li>Status code message: " + o.statusText + "</li></ul>";
  }
}
var commentcallback = {
  success:handleCommentSuccess,
  failure:handleCommentFailure,
  argument: { foo:"food", bar:"barn" }
};

var wtimeout; 

function dowffck(focalid) {
  if (smlockout) return;
  if (wcmode == false) return;
  if (!ok2wff) return
  dowffck2(focalid);
}

function dowffck2(focalid) {
  if (!masterdowffchecking) {
    dobj('wcbutton-3').value = 'Wff-Check Disabled';
    return;
  }
  clearwffmsgs();
  dobj('wcbutton-3').value = 'Doing Wff-Check...';
  if (wtimeout != undefined) clearTimeout(wtimeout);
  var dv = dobj(focalid).value;
  // encodeURIComponent was too strong, encodeURI too weak
  dv = dv.replace(/\#/g, '%23').replace(/\$/g, '%24');
  dv = dv.replace(/\&/g, '%26').replace(/\=/g, '%3D').replace(/\?/, '%3F');

  var mmval = dobj(textareawho['mt-monad']).value;
  mmval = mmval.replace(/\#/g, '%23').replace(/\$/g, '%24');
  var mtival = dobj(textareawho['mt-time-interval']).value;
  mtival = mtival.replace(/\#/g, '%23').replace(/\$/g, '%24');
  var mtpval = dobj(textareawho['mt-time-parameter']).value;
  mtpval = mtpval.replace(/\#/g, '%23').replace(/\$/g, '%24');

  var sUrl = "../../cgi-bin/cg?cb-handle-assert=&assertion-queue=%3Alocal&id=916588&mt-monad=" + mmval + "&mt-time-dimension-specified=na&mt-time-interval=" + mtival + "&mt-time-parameter=" + mtpval + "&sentence=" + encodeURI(dv) + "&strength=%3Amonotonic&uniquifier-code=34&wff-check=WFF-Check%20Sentence&workaround-assertion-bug-number=";
  wffcallback.argument.foo = focalid;
  wtimeout = setTimeout('genwffreq("' + sUrl + '");', 2000);
}

var wffrequest;

function genwffreq(sUrl) {
  wffrequest = YAHOO.util.Connect.asyncRequest('GET', sUrl, wffcallback);
}

var handleWffSuccess = function(o) {
  if (o.responseText != undefined) {
    console.log(o.responseText);
    var rtitle = o.responseText.replace(/<.title>(.|\n)*/i, '').replace(/(.|\n)*<title>/i, '');
    var rsuffix = o.responseText.replace(/(.|\n)*<.h2>/m,'').replace(/<form(.|\n)*/m,''); // why is /m not working?
    if (rtitle == 'Sentence is Well Formed') {
      sendwffok('<font style="padding-top:1px;padding-bottom:1px;padding-right:4px;padding-left:4px;background-color:lightgreen">OK</font>');
      clearwffexplain();
    } else {
      sendwffok('<font style="padding-top:1px;padding-bottom:1px;padding-right:4px;padding-left:4px;background-color:lightgray">NOT OK</font>');
      var errtitle = "<table width='100%'><tr><td align='right' valign='top'><font class='handhover' style='font-size:14pt' onclick='clearwffexplain();' color='gray'>&nbsp;&#215;</font></td></tr></table>";
      errtitle += '<font color="red" size=-1>' + rtitle + '<hr>' + rsuffix +'</font>';
      if (resinline) inlinelocate(o.argument.foo);
      else sidelocate(o.argument.foo);
      if (o.responseText.match(/<div +class=.wff-explain.>/i)) {
        sendwffexplain(errtitle + '<br><font color="black" size=-1>' + RegExp.rightContext.replace(/<\/div>(.|\n)*/i, '') + '</font>');
      } else {
        sendwffexplain(errtitle);
      }
      dobj('wffexplain').style.border = 'solid gray 1px';
      ok2ac = false;
    }
  }
  dobj('wcbutton-3').value = 'WFF/OK?';
}
var handleWffFailure = function(o) {
  if (o.responseText !== undefined) {
    var div = dobj('comment' + lastpoppedn);
    if (div == undefined) return;
    div.innerHTML = "<ul><li>Wff Call FAILURE Transaction id: " + o.tId + "</li>";
    div.innerHTML += "<li>HTTP status: " + o.status + "</li>";
    div.innerHTML += "<li>Status code message: " + o.statusText + "</li></ul>";
  }
  dobj('wcbutton-3').value = 'WFF/OK?';
}
var wffcallback = {
  success:handleWffSuccess,
  failure:handleWffFailure,
  argument: { foo:"food", bar:"barn" }
};

var lword = '';
var lastwordregexp = /([a-z]|[A-Z]|[0-9]|\:|\_|-|\?|\#|\$)*$/; // double quotes, too? more trouble than it's worth -- let them search under quotes

function findlword(dv, cwhere) { // the last word ending at the cursor
  // sets the global lword
  var cleft = cwhere - maxtermlen; if (cleft < 0) cleft = 0;
  var tpiece = dv.slice(cleft, cwhere);
  lword = lastwordregexp.exec(tpiece)[0];
  lword = lword.replace(/.*\#\$/g, ''); // truncate anything before a #$, including #$
  if (lword.slice(0, 1) == '?') return false;
  else if (lword.slice(0, 1) == ':') return false;
  else return true;
}

var blinkmetext;

var wscale = 1.0003;
var wshift = -3;
var vscale = 0.9995;
var vshift = 2;

if (navigator.userAgent.match(/MSIE/)) vscale = 1.00018;
if (navigator.userAgent.match(/MSIE/)) wscale = 1.00002;
if (navigator.userAgent.match(/MSIE/)) vshiftoffdir = -0.15;
else vshiftoffdir = 0.0;
if (navigator.userAgent.match(/MSIE/)) wshiftoffdir = 0.9;
else wshiftoffdir = 0.8;


var poffset;
var voffset;
var locatedleft = 0;
var locatedtop;

var mozillabug = true;

function locate(focalid, ptext, ptextfollow) { // used to be crucial when coloring parens by pixel offset -- now could be simplified and just guess
  // at the end of ptext is where you want to locate, BUT! ptext could be followed by enough text that it causes premature line wrap
  // we have a problem because text wraps kinda funny in textareas, at least in mozilla... try "test foo   bar" and space the bar until it wraps "foo *bar"!
  if (ptextfollow == undefined) ptextfollow = '';
  var arrnwha = ptext.replace(/%0A/ig, '\n').split('\n', 1000); // you actually have to worry about long lines, too, and figure out where they are broken
  nwha = arrnwha.length;
  var wrappedlines = 0;
  var thisline;
  // check if ptextfollow causes shift back or forward
  if (ptextfollow.match(/^[ 	]*\n/)) var nextstringstartsnewline = true;
  else nextstringstartsnewline = false;
  var nextstring = ptextfollow.replace(/[ 	\n].*/g, '');
  var effcols = numcols[focalid] - 2;
  if (browserchrome) effcols = numcols[focalid] + 1;
  for (var i=0; i<nwha; i++) {
    thisline = arrnwha[i];
    if ((i == nwha - 1) && !nextstringstartsnewline) thisline = thisline + nextstring; // see where the damn thing ends up, with wrapping
    var ntimes = 0;
    while (thisline.length > effcols) {
      ++ntimes;
      var idealplus = thisline.slice(0, effcols);
      var partoverflow = idealplus.replace(/.*[ 	\-\n]/g, '');
      var actualremain;
      if (partoverflow == idealplus) actualremain = thisline.slice(numcols[focalid]);
      else actualremain = thisline.slice(idealplus.length - partoverflow.length).replace(/^  */, ''); // unless there is \n, leading spaces trunc'ed
      wrappedlines++;
      var lastline = thisline;
      thisline = actualremain;
      if (thisline == lastline) break;
      if (ntimes > 100) { alert('loop bug'); break; }
    }
  }
  var remp = thisline;
  // remp is the leftover on this line, followed by nextstring
  if (!nextstringstartsnewline) {
    // remove nextstring, which you added on in order to see if it caused your word to be wrapped
    remp = remp.slice(0, remp.length - nextstring.length)
  }
  var remplen = remp.length;
  var dimsw = (dobj(focalid).offsetWidth - 4)/(0.0001 + numcols[focalid]);
  var dimsh = (dobj(focalid).offsetHeight - 2)/(0.0001 + numrows[focalid]);
  if (remplen < numcols[focalid]/2) {
    poffset = wshift + wscale * (remplen * dimsw) - dobj(focalid).scrollLeft;
    locatedleft = dobj(focalid).offsetLeft + poffset;
  } else {
    poffset = wscale * ((wshiftoffdir + numcols[focalid] - remplen) * dimsw) + dobj(focalid).scrollLeft;
    locatedleft = dobj(focalid).offsetLeft + dobj(focalid).offsetWidth - poffset;
  }
  var linesdown = nwha - 1 + wrappedlines;
  if (linesdown < numrows[focalid]/2) {
    voffset = vshift + vscale * (linesdown * dimsh) - dobj(focalid).scrollTop;
    locatedtop = dobj(focalid).offsetTop + voffset;
  } else {
    voffset = vscale * ((vshiftoffdir + numrows[focalid] - linesdown) * dimsh) + dobj(focalid).scrollTop;
    locatedtop = dobj(focalid).offsetTop + dobj(focalid).offsetHeight - voffset;
  }
}

var smlockout = false;
var savestart;

function showmatch(focalid) { // check these for behavior with no match // this works at end of line, but not in the interior; try 3?
  // keyboard an end-paren
  if (smlockout) return;
  var dv = dobj(focalid).value;
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  if (cwhere < dv.length) var internalchange = true;
  else internalchange = false;
  var lsent;
  if (internalchange) lsent = '';
  else lsent = dv.slice(cwhere, cwhere + 1);
  var findopen = 0
  var i = cwhere;
  while (--i >= 0) {
    var thischar = dv.slice(i, i + 1);
    if (thischar == ')') findopen++;
    if (thischar == '(') findopen--;
    lsent = thischar + lsent;
    if (findopen <= 0) break;
  }
  if (i < 0) {
    // no match
    dobj(focalid).selectionStart = savestart;
    dobj(focalid).selectionEnd = cwhere;
    dobj(focalid).focus();
    return;
  }
  blinkmetext = dv.slice(cwhere - lsent.length + 1, cwhere);
  dobject('comment').innerHTML = 'Matched Paren:\n(' + blinkmetext;
  dobj('comment').style.width = bepix(dobj(focalid).offsetWidth - dobj(focalid).offsetLeft - 20 + 4);
  var ptextlen = cwhere - lsent.length;
  var ptext = dv.slice(0, ptextlen);
  var sufftext;
  if (internalchange) sufftext = dv.slice(cwhere);
  else sufftext = dv.slice(cwhere + 1);
  if (smblink) {
    // dobj(focalid).value = ptext + '[' + blinkmetext.replace(/[a-zA-Z]/g, '.').replace(/\)$/, ']').toUpperCase() + sufftext;
    dobj(focalid).value = ptext + ' ' + blinkmetext.replace(/\)$/, ' ') + sufftext;
    // dobj(focalid).value = ptext + '[' + blinkmetext.replace(/\)$/, ']') + sufftext;
    var cpos = cwhere - lsent.length;
    smlockout = true; setTimeout('clearmatch("' + focalid + '", ' + cpos + ', ' + cwhere + ', ' + internalchange + ');', 300);
  }
  dobj(focalid).selectionStart = savestart;
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).focus();
}

function clearmatch(focalid, cpos, cwhere, internalchange) {
  var dv = dobj(focalid).value;
  var stpiece = dv.slice(cpos, cpos + 1);
  var ptext = dv.slice(0, cpos);
  var sufftext;
  if (internalchange) sufftext = dv.slice(cwhere);
  else sufftext = dv.slice(cwhere + 1);
  if (stpiece == ' ') dobj(focalid).value = ptext + '(' + blinkmetext.replace(/\ $/, ')') + sufftext;
  dobj(focalid).selectionStart = savestart;
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).focus();
  smlockout = false;
}

function showmatch2(focalid) {
  // key or click to behind begin paren
  if (smlockout) return;
  var dv = dobj(focalid).value;
  var last = dv.length;
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  var lsent = dv.slice(cwhere - 1, cwhere);
  var findclosed = 1
  var i = cwhere - 1;
  while (++i <= last) {
    var thischar = dv.slice(i, i + 1);
    if (thischar == ')') findclosed--;
    if (thischar == '(') findclosed++;
    lsent += thischar;
    if (findclosed <= 0) break;
  }
  if (i > last) {
    // no match
    dobj(focalid).selectionStart = savestart;
    dobj(focalid).selectionEnd = cwhere;
    dobj(focalid).focus();
    return;
  }
  blinkmetext = dv.slice(cwhere - 1, cwhere + lsent.length - 2);
  dobject('comment').innerHTML = 'Matched Paren:\n' + blinkmetext + ')';
  dobj('comment').style.width = bepix(dobj(focalid).offsetWidth - dobj(focalid).offsetLeft - 20 + 4);
  var ptextlen = cwhere - 1
  var ptext = dv.slice(0, ptextlen);
  var sufftext = dv.slice(cwhere + lsent.length - 1);
  if (smblink) {
    // dobj(focalid).value = ptext + blinkmetext.replace(/[a-zA-Z]/g, '.').replace(/^\(/, '[').toUpperCase() + ']' + sufftext;
    dobj(focalid).value = ptext + blinkmetext.replace(/^\(/, ' ') + ' ' + sufftext;
    // dobj(focalid).value = ptext + blinkmetext.replace(/^\(/, '[') + ']' + sufftext;
    var cpos = cwhere + lsent.length;
    smlockout = true; setTimeout('clearmatch2("' + focalid + '", ' + cpos + ', ' + cwhere + ');', 300);
  }
  dobj(focalid).selectionStart = savestart;
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).focus();
}

function clearmatch2(focalid, cpos, cwhere) {
  var dv = dobj(focalid).value;
  var stpiece = dv.slice(cpos - 2, cpos - 1);
  var ptext = dv.slice(0, cwhere - 1);
  var sufftext = dv.slice(cpos - 1);
  if (stpiece == ' ') dobj(focalid).value = ptext + blinkmetext.replace(/^\ /, '(') + ')' + sufftext;
  dobj(focalid).selectionStart = savestart;
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).focus();
  smlockout = false;
}

function showmatch3(focalid) {
  // key or click to behind end paren
  if (smlockout) return;
  var dv = dobj(focalid).value;
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  if ('selectionStart' in dobj(focalid)) savestart = dobj(focalid).selectionStart;
  else savestart = 0;
  var lsent = dv.slice(cwhere, cwhere + 1);
  if (cwhere == dv.length) lsent = lsent + ' ';
  var findopen = 0
  var i = cwhere;
  while (--i >= 0) {
    var thischar = dv.slice(i, i + 1);
    if (thischar == ')') findopen++;
    if (thischar == '(') findopen--;
    lsent = thischar + lsent;
    if (findopen <= 0) break;
  }
  if (i < 0) {
    // no match
    dobj(focalid).selectionStart = savestart;
    dobj(focalid).selectionEnd = cwhere;
    dobj(focalid).focus();
    return;
  }
  blinkmetext = dv.slice(cwhere - lsent.length + 2, cwhere);
  dobject('comment').innerHTML = 'Matched Paren:\n(' + blinkmetext;
  dobj('comment').style.width = bepix(dobj(focalid).offsetWidth - dobj(focalid).offsetLeft - 20 + 4);
  var ptextlen = cwhere - lsent.length + 1;
  var ptext = dv.slice(0, ptextlen);
  var sufftext = dv.slice(cwhere);
  if (smblink) {
    // dobj(focalid).value = ptext + '[' + blinkmetext.replace(/[a-zA-Z]/g, '.').replace(/\)$/, ']').toUpperCase() + sufftext;
    dobj(focalid).value = ptext + ' ' + blinkmetext.replace(/\)$/, ' ') + sufftext;
    // dobj(focalid).value = ptext + '[' + blinkmetext.replace(/\)$/, ']') + sufftext;
    var cpos = cwhere - lsent.length;
    smlockout = true; setTimeout('clearmatch3("' + focalid + '", ' + cpos + ', ' + cwhere + ');', 300);
  }
  dobj(focalid).selectionStart = savestart;
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).focus();
}

function clearmatch3(focalid, cpos, cwhere) {
  var dv = dobj(focalid).value;
  var stpiece = dv.slice(cpos + 1, cpos + 2);
  var ptext = dv.slice(0, cpos + 1);
  var sufftext = dv.slice(cwhere);
  if (stpiece == ' ') dobj(focalid).value = ptext + '(' + blinkmetext.replace(/\ $/, ')') + sufftext;
  dobj(focalid).selectionStart = savestart;
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).focus();
  smlockout = false;
}

var undostack = new Array;
var undowhere = new Array;
var nundo = 0;
var highestnundo = 0;

function doredo(focalid) {
  ok2ac = false;
  ok2sm = false;
  hideac();
  hidesearch();
  if (nundo >= highestnundo) {
    sendmsg('cannot redo past the last edit');
    return;
  }
  nundo++;
  dobj(focalid).value = undostack[nundo];
  dobj(focalid).selectionEnd = undowhere[nundo];
  dobj(focalid).selectionStart = dobj(focalid).selectionEnd;
  dobj(focalid).focus();
  dowffck(focalid);
}

function doundo(focalid) {
  if (nundo == highestnundo) saveundopoint(focalid, dobj(focalid).value, dobj(focalid).selectionEnd);
  ok2ac = false;
  ok2sm = false;
  hideac();
  hidesearch();
  if (nundo < 1) {
    sendmsg('cannot undo the first edit');
    return;
  }
  nundo--;
  dobj(focalid).value = undostack[nundo];
  dobj(focalid).selectionEnd = undowhere[nundo];
  dobj(focalid).selectionStart = dobj(focalid).selectionEnd;
  dobj(focalid).focus();
  dowffck(focalid);
}

function saveundopoint(focalid, dv, cwhere) {
  if (nundo == 0 || dv != undostack[nundo]) {
    nundo++;
    undostack[nundo] = dv;
    undowhere[nundo] = cwhere;
    highestnundo = nundo;
  }
}

var savelastleft = '';
var savelastright = '';

function doselect(focalid, x, hideaccommand) {
  var dv = dobj(focalid).value;
  dobj(focalid).focus(); // restore focus so we can pick up selectionpoints in IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  saveundopoint(focalid, dv, cwhere);
  var cleft = cwhere - maxtermlen; if (cleft < 0) cleft = 0;
  var tpiece = dv.slice(cleft, cwhere);
  var lword = lastwordregexp.exec(tpiece)[0]; // BUG sometimes dies and leaves lhs for no reason!
  var pieces = lword.split('#$'); // if you have #$foo#$goo, replace the #$goo but not the #$foo
  var consider = pieces[pieces.length - 1]; // you might have football, #$foot#$ball, foot#$ball, football#$ // consider is the last segment
  if (consider != '') {
    if (pieces.length > 1) lword = '#$' + consider; // the last segment is not the first segment, so there was a #$
    else lword = consider; // the last segment is the first segment, so there was not a #$
  } else lword = pieces[0]; // is this right?  if the last segment is empty, choose the first (why not the 2nd to last?) // but this seems to work
  var upiece = dv.slice(cwhere);
  var nextwordregexp = /^([a-z]|[A-Z]|[0-9]|\_|-|\?|\#|\$)*/;
  var nword = nextwordregexp.exec(upiece)[0]; // this is the stuff after cwhere, which you might want to include
  nword = nword.replace(/\#\$.*/, ''); // but not after #$
  savelastleft = dv.slice(0, cwhere-lword.length);
  savelastright = dv.slice(cwhere + nword.length);
  // var newstring = savelastleft + x + savelastright;
  var newstring = savelastleft + x + ' ' + nword + savelastright;
  dobj(focalid).value = newstring;
  dobj(focalid).selectionEnd = cwhere - lword.length + x.length;
  dobj(focalid).selectionStart = dobj(focalid).selectionEnd;
  dobj(focalid).focus();
  saveundopoint(focalid, dobj(focalid).value, dobj(focalid).selectionEnd);
  if (hideaccommand) hideac();
  if (scmode) {
    if (ispredicate(x)) {
      dogetcomment(x);
    }// else clearcomment();
  }
  dowffck(focalid);
  ok2ac = false;
  ok2sm = true;
}

function srlite(n) {
  if (dobj('searchresultline' + n) != undefined) dobj('searchresultline' + n).style.backgroundColor = 'skyblue';
}

function srdark(n) {
  if (dobj('searchresultline' + n) != undefined) dobj('searchresultline' + n).style.backgroundColor = 'white';
}

function acconsider(n) {
  acdark(lastnrotated);
  aclite(n);
  lastnrotated = n;
}

function aclite(n) {
  if (dobj('acresultline' + n) != undefined) dobj('acresultline' + n).style.backgroundColor = 'skyblue';
}

function acdark(n) {
  if (dobj('acresultline' + n) != undefined) dobj('acresultline' + n).style.backgroundColor = 'white';
}

var lastchoice = 1; // use this global rather than loop through all to clear

function dochoose(i) {
  if (lastchoice != undefined) srdark(lastchoice);
  lastchoice = i;
  var chosenvalue = searchresterms[i];
  srlite(i);
  doselect(focalid, chosenvalue, true);
  if (true) { hidesearch(); hideac(); }  // you could keep the windows up for changing selections
}

var searchrequest;

function dosearch() {
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  var savestart = dobj(focalid).selectionStart;
  var sUrl;
  var dv = dobj(focalid).value;
  clearwffmsgs();
  findlword(dv, cwhere);
  var mystring = lword;
  // sUrl = "cg?xml-term-search&searchString=" + mystring + "&constrainingSentence=" + cyclcontext + "&caseSensitive=nil";
  // sUrl = "/cgi-bin/cg?xml-term-search&searchString=" + mystring + "&caseSensitive=nil&return-attrs=(:cycl)";
  sUrl = "/cgi-bin/cg?xml-term-search&searchString=" + mystring + "&caseSensitive=nil";
  searchcallback.argument.foo = focalid;
  if (searchrequest) YAHOO.util.Connect.abort(searchrequest, searchcallback, 'search abort');
  searchrequest = YAHOO.util.Connect.asyncRequest('GET', sUrl, searchcallback);
  dobj('searchbutton-3').value = 'Searching: ' + mystring + '...';
  if (resinline) locate(focalid, dv.slice(0, cwhere - lword.length + 1)); // will use the locatedtop and locatedleft in results handler
  hideac();
  dobj(focalid).focus();
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
}

function closeparens() {
  dobj(focalid).focus(); // for IE
  var cwhere = dobj(focalid).selectionEnd;
  var savestart = dobj(focalid).selectionStart;
  var dv = dobj(focalid).value;
  saveundopoint(focalid, dv, dobj(focalid).selectionEnd);
  var nwha; var nopeneds; var ncloseds; var ndiff;
  nwha = dv.match(/\(/g);
  if (nwha != null) nopeneds = nwha.length;
  else nopeneds = 0;
  nwha = dv.match(/\)/g);
  if (nwha != null) ncloseds = nwha.length;
  else ncloseds = 0;
  ndiff = nopeneds - ncloseds;
  if (ndiff > 0) dobj(focalid).value += makeparens(ndiff);
  dobj(focalid).focus();
  dobj(focalid).selectionEnd = cwhere;
  dobj(focalid).selectionStart = savestart;
  // dobj(focalid).selectionEnd = dobj(focalid).value.length;
  // dobj(focalid).selectionStart = dobj(focalid).value.length;
  saveundopoint(focalid, dobj(focalid).value, dobj(focalid).selectionEnd);
  lastkeypressed = dobj(focalid).value.slice(dobj(focalid).value.length - 1); // simulate so it will showmatch as needed
  ok2sm = true;
  kc(focalid);
  dowffck(focalid);
}

var savetext = '';

function docorr() {
  correctlonglines(focalid, numcols[focalid]);
}

function doformatL() {
  saveundopoint(focalid, dobj(focalid).value, dobj(focalid).selectionEnd);
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  var savestart = dobj(focalid).selectionStart;
  var atext = dobj(focalid).value.replace(/  +/g, ' ').replace(/ \)/, ')').replace(/\( /g, '(');
  var apieces = new Array;
  apieces = atext.split('\n', 1000);
  var res = '';
  var cumindent = 0;
  var nwha; var nopeneds; var ncloseds; var ndiff;
  for (var i=0; i<apieces.length; i++) {
    nwha = apieces[i].match(/\(/g);
    if (nwha != null) nopeneds = nwha.length;
    else nopeneds = 0;
    nwha = apieces[i].match(/\)/g);
    if (nwha != null) ncloseds = nwha.length;
    else ncloseds = 0;
    ndiff = nopeneds - ncloseds;
    var bareapp = apieces[i].replace(/^ */, '').replace(/ *$/, '');
    if (cumindent >= 0 && cumindent + ndiff < 0) {
      // figure out which ) is an overflow on this line
      res += '\n' + makespaces(cumindent, '');
      var tempcounter = cumindent;
      remain = bareapp;
      var imatchtuple;
      while (imatchtuple = remain.match(/[\(\)]/)) {
        var remain = RegExp.rightContext;
        var skippedtext = RegExp.leftContext;
        var matching = imatchtuple[0].slice(0, 1);
	if (matching == '(') tempcounter ++;
	if (matching == ')') tempcounter --;
	res += skippedtext;
	if (tempcounter < 0) res += '*';
	res += matching;
      }
      res += remain;
    } else res += '\n' + makespaces(cumindent, bareapp) + bareapp;
    cumindent += ndiff;
  }
  dobj(focalid).value = res.slice(1); // if you want to truncate leading newline, use 1
  dobj(focalid).focus();
  dobj(focalid).selectionEnd = cwhere; // it won't be quite right, but maybe won't scroll annoyingly
  dobj(focalid).selectionStart = savestart;
  saveundopoint(focalid, dobj(focalid).value, dobj(focalid).selectionEnd);
  // lastkeypressed = dobj(focalid).value.slice(dobj(focalid).value.length - 1); // simulate so it will showmatch as needed
  ok2sm = true;
  kc(focalid);
}

function doformatA() {
  saveundopoint(focalid, dobj(focalid).value, dobj(focalid).selectionEnd);
  dobj(focalid).focus(); // for IE
  if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
  else cwhere = 0;
  var savestart = dobj(focalid).selectionStart;
  // var atext = dobj(focalid).value.replace(/\n/g, ' ').replace(/  +/g, ' ').replace(/ \)/, ')').replace(/\( /g, '(');
  var atext = dobj(focalid).value.replace(/  +/g, ' ').replace(/ \)/, ')').replace(/\( /g, '(');
  var apieces = new Array;
  atext = atext.replace(/\(/g, '\n(');
  if (true) while (atext.match(/\)\)/)) atext = atext.replace(/\)\)/g, '\)\n\)'); // break up all the runs of closed parens
  atext = atext.replace(/\) /g, ')\n');
  // atext = atext.replace(/\n(\n)* *\n(\n)*/g, '\n');
  atext = atext.replace(/\n( *\n)*/g, '\n');
  // atext = atext.replace(/\n(\n)*/g, '\n');
  apieces = atext.split('\n', 5000);
  var res = '';
  var cumindent = 0;
  var nwha; var nopeneds; var ncloseds; var ndiff;
  for (var i=0; i<apieces.length; i++) {

    // get an idea how this changes subsequent indentation
    nwha = apieces[i].match(/\(/g);
    if (nwha != null) nopeneds = nwha.length;
    else nopeneds = 0;
    nwha = apieces[i].match(/\)/g);
    if (nwha != null) ncloseds = nwha.length;
    else ncloseds = 0;
    ndiff = nopeneds - ncloseds;

    // special case:  if prior line ends with (, i.e., it is just a possibly indented (, and next line begins with (, don't generate the \n
    var bareapp = apieces[i].replace(/^ */, '').replace(/ *$/, '');
    if (res.slice(res.length - 1) == '(' && bareapp.slice(0, 1) == '(') res += bareapp;
    else res += '\n' + makespaces(cumindent, bareapp) + bareapp;

    cumindent += ndiff;
  }
  dobj(focalid).value = res.replace(/^(\n)*/, ''); // truncate leading newlines
  dobj(focalid).focus();
  dobj(focalid).selectionEnd = cwhere; // it won't be quite right, but maybe won't scroll annoyingly
  dobj(focalid).selectionStart = savestart;
  saveundopoint(focalid, dobj(focalid).value, dobj(focalid).selectionEnd);
  // lastkeypressed = dobj(focalid).value.slice(dobj(focalid).value.length - 1); // simulate so it will showmatch as needed
  ok2sm = true;
  kc(focalid);
}

function makespaces(n, nextchars) {
  if (n < 0) return '*';
  if (n == 0 && nextchars.slice(0, 1) == ')') return '*'; // could still have an extra cp on this line, but that' the author's business
  var res = '';
  for (var i=1; i<=n; i++) res += '  ';
  return res;
}

function makeparens(n) {
  var res = '';
  for (var i=1; i<=n; i++) res += ')';
  return res;
}

function wcheck(e) { // scrolling event
  if (dobj(focalid).scrollLeft || dobj(focalid).scrollTop)  { ok2sm = false; }
}

var lastkeypressed = '';
var lastnrotated = 0; 
var ok2ac = false;
var ok2sm = true;
var ok2wff = true;
var rotatingcommonac = false;

function kcheck(e) { // master capture of keydown prior to textarea commit of keystroke
  var bkey = e ? e : window.event; // this line (kk) came from the web and works with document.onkeydown = kcheck;
  var key = e ? e.which : window.event.keyCode;
  if (key != 16 && key != 17 && key != 18) lastkeypressed = key; // don't catch sticky Shift or Ctrl or Alt

  if (key != 32 || !e.ctrlKey) rotatingcommonac = false;  // clear this if not ctrl-space

  if (key == 86 && e.ctrlKey) { // this is ctrl-V
    ok2sm = true;
    dowffck(focalid);
    return true;
  } else if (key == 88 && e.ctrlKey) { // this is a ctrl-X
    ok2sm = true;
    dowffck(focalid);
    return true;
  } else if (key == 32 && e.ctrlKey) { // this is a ctrl-space
    ok2sm = true;
    ok2ac = false;
    if (!rotatingcommonac) {
      if (!acmastermode) forceac();
      if (commonacprefix.length > 2) {
        doselect(focalid, commonacprefix, false); // #$ will always be common
        if (commonacprefixes.length == 0 || (commonacprefixes.length == 1 && commonacprefixes[0] == commonacprefix)) hideac();
      }
      icommonacprefix = 0;
      rotatingcommonac = true;
      dowffck(focalid);
      return false; // changed per Witbrock
    } else {
      if (icommonacprefix < commonacprefixes.length) {
        if (commonacprefixes[icommonacprefix]) doselect(focalid, commonacprefixes[icommonacprefix], false); 
      } else {
        icommonacprefix = 0;
        if (commonacprefixes[0]) doselect(focalid, commonacprefixes[0], false);
      }
      ++icommonacprefix;
      dowffck(focalid);
      return false; // changed per Witbrock
    }
  } else if (key == 90 && e.ctrlKey) { // this is a ctrl-Z
    ok2sm = true;
    dowffck(focalid);
    return true;
  } else if (acvisible && key == 32 && bkey.ctrlKey) {
    if (lastnrotated != undefined) acdark(lastnrotated);
    if (bkey.shiftKey) { if (++lastnrotated > acresultn) lastnrotated = 1; }
    else { if (--lastnrotated <= 0) lastnrotated = acresultn; }
    if (acresultterms[lastnrotated] != undefined) aclite(lastnrotated);
    ok2ac = false;
    ok2sm = false;
    return false;
  } else if (acvisible && key == 13) { // enter as select
    if (lastnrotated >= 0) {
      doselect(focalid, acresultterms[lastnrotated], true);
      hideac();
      return false; // ignore the CR, since it was a select-gesture, not an char-input
    } else return true; // accept the CR in the textarea
  } else if (e.altKey || e.ctrlKey) {
    ok2ac = false;
    ok2sm = false;
    return true;
  }

  // dobj('comment').innerHTML = key + ',' + lastkeypressed + ',' + e.shiftKey + ',' + e.ctrlKey + ',' + e.altKey; 

  if (key == 9) { // be smart about accepting a tab in the input field at an ok place, otherwise tab forward thru elements
    if ('selectionEnd' in dobj(focalid)) cwhere = dobj(focalid).selectionEnd;
    else cwhere = 0
    dv = dobj(focalid).value;
    if (cwhere && dv.match(/\n/) && dv.slice(cwhere-1,cwhere).match(/[ \t\n]/)) {
      if (cwhere) dobj(focalid).value = dv.slice(0,cwhere) + '    ' + dv.slice(cwhere);
      dobj(focalid).selectionStart = cwhere+4
      dobj(focalid).selectionEnd = cwhere+4;
      return false;
    }
  }

  if (key == 27) { // esc or shift-esc, but not ctrl-esc or alt-esc
    dohideconfigline();
    hidesearch();
    hideac();
    if (resinline) clearwffexplain();
    dobj(focalid).focus();
    ok2ac = false;
    ok2sm = false;
    ok2wff = false;
  } else { 
    ok2ac = true;
    ok2sm = true;
    ok2wff = true;
  }

  if (acvisible) {
    if (lastkeypressed == 40) { // down arrow
      if (lastnrotated != undefined) acdark(lastnrotated);
      if (--lastnrotated < 0) lastnrotated = acresultn - 1;
      if (acresultterms[lastnrotated] != undefined) aclite(lastnrotated);
      ok2ac = false;
      ok2sm = false;
      return false;
    } else if (lastkeypressed == 39) { // right arrow
      if (lastnrotated != undefined) acdark(lastnrotated);
      if (--lastnrotated < 0) lastnrotated = acresultn - 1;
      if (acresultterms[lastnrotated] != undefined) aclite(lastnrotated);
      ok2ac = false;
      ok2sm = false;
      return false;
    } else if (lastkeypressed == 38) { // up arrow
      if (lastnrotated != undefined) acdark(lastnrotated);
      if (++lastnrotated >= acresultn) lastnrotated = 0;
      if (acresultterms[lastnrotated] != undefined) aclite(lastnrotated);
      ok2ac = false;
      ok2sm = false;
      return false;
    } else if (lastkeypressed == 37) { // left arrow
      hideac();
      ok2ac = false;
      ok2sm = true;
      return false;
    } else if (lastkeypressed == 13) { // enter
      if (lastnrotated == -1) {
	// ignore the ac since it was never entered
        ok2ac = false;
        ok2sm = true;
        hideac();
        return true; 
      } else doselect(focalid, acresultterms[lastnrotated], true);
      hideac();
      return false;
    }
  } else if (searchvisible) { // mirror ac with new vars
    if (lastkeypressed == 40) { // down arrow
      if (lastnrotated != undefined) srdark(lastnrotated);
      if (++lastnrotated >= searchresn) lastnrotated = 0; // yes, opposite direction as ac
      if (searchresterms[lastnrotated] != undefined) srlite(lastnrotated);
      ok2ac = false;
      ok2sm = false;
      return false;
    } else if (lastkeypressed == 39) { // right arrow
      if (lastnrotated != undefined) srdark(lastnrotated);
      if (++lastnrotated >= searchresn) lastnrotated = 0; // yes, opposite direction as ac
      if (searchresterms[lastnrotated] != undefined) srlite(lastnrotated);
      ok2ac = false;
      ok2sm = false;
      return false;
    } else if (lastkeypressed == 38) { // up arrow
      if (lastnrotated != undefined) srdark(lastnrotated);
      if (--lastnrotated < 0) lastnrotated = searchresn - 1; // yes, opposite direction as ac
      if (searchresterms[lastnrotated] != undefined) srlite(lastnrotated);
      ok2ac = false;
      ok2sm = false;
      return false;
    } else if (lastkeypressed == 37) { // left arrow
      hideac();
      ok2ac = false;
      ok2sm = true;
      return false;
    } else if (lastkeypressed == 13) { // enter
      if (lastnrotated == -1) {
	// ignore the ac since it was never entered
        ok2ac = false;
        ok2sm = true;
        hideac();
        return true; 
      } else doselect(focalid, searchresterms[lastnrotated], true);
      hideac();
      return false;
    }
  }

  // this block says do not ac after an arrow if ac not visible
  if (key == 37) { ok2ac = false; }
  if (key == 38) { ok2ac = false; }
  if (key == 39) { ok2ac = false; }
  if (key == 40) { ok2ac = false; }

  if (key >= 65 || key == 32 || key == 48 || key == 57) clearwffmsgs(); // clear on alpha or higher, space, or open/close paren

  return true;

}

var browserchrome = navigator.userAgent.match(/chrome/i);

// BUG?:  search didn't fix left and right pos for complex fn commit

// we are not saving/restoring selection pts in 2ndtextarea, or who was last in focus
