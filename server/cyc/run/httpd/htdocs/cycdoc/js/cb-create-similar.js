/*

	XML to HTML for create-similar v2.0 CommercialOpenCyc
	Ronald Loui for Cycorp (c) 2011
	see also Dave Schneider, Chris Deaton

	April - May 2011

	This js rewrites xml into js, css, and dhtml
	Since it can be hard to see the js that is written by the (meta-)js, we provide an example of the input and output of this program
	in doc.rons-file.js

	to-do:  where is the cursor on popme boxes? on input-0? (depends on browser)
	to-do:  suppress clicks till js fully loads
	to-do:  suppress same-search when executed twice in a row -- but when?
	to-do:  warn that navigating away will destroy all textchanges (except box checks) -- really?

	NOTE that if the user chooses from auto/c, we don't get a new guid for the linktable
	NOTE that ie and chrome put spaces in the textarea where the ENTER was hit; we can add a commit button
	NOTE that undo does not undo input-0 focal term changes, nor explicit checkbox changes

	to-do:  free edit after search needs to clear search results first... DONE Mon Mar 26 17:47:29 CDT 2012
	to-do:  cloning a line needs to set up autocomplete -- gotta do this... DONE Mon Mar 26 18:44:43 CDT 2012
	to-do:  cloned line, when cloned, doesn't renumber? DONE Tue Mar 27 12:27:15 CDT 2012

*/

var obuffer = ''; // for buffered output -- we need lookahead on the complex mt expressions to check for equivalence to prior mt

function dwln(x) { document.writeln(x); }
function dw(x) { document.write(x); }

dwln('<meta http-equiv="Content-type" content="text/html; charset=unicode">');


function accbdw(x) { 
  if (workingonmtflag) newmt += x; // accumulate the entire markedup active text in order to have internal function
  obuffer = obuffer + x; 
}
function flushobuffer() { document.write(obuffer); obuffer = ''; }
// function msg(x) { dobject('messages').innerHTML = '<font color=red>' + x + '</font>'; }
function msg(x) { if (x != '') alert(x); }
function dmsg(x) { dobject('debug').innerHTML += x; }
function cleardmsg(x) { dobject('debug').innerHTML = x; }

function stripHTML(x) { // note that this will also reassign &quot; to "
   var tmp = document.createElement("DIV");
   tmp.innerHTML = x;
   return tmp.textContent || tmp.innerText;
}


// THESE GLOBAL VARS/ARRAYS/OBJECTS ARE USED FOR PASSING PARSER INFO INTO THE REWRITTEN PAGE'S JS

  var extraspacefornumbersflag = true;	// pads numbers with an extra space for clickability
  var nextspaceflag = true;	// controls prepending of spaces before terms -- preds begin sentences, but fn names are not distinguishable from terms

// <rownum><mtnum>Mt <textme>foo:
//   <rownum>checkbox: <textme>term <textme>term <textme>term
//   <rownum>checkbox: <textme>term <textme>term <textme>term

  var row2mt = new Array();  // associates mtnum with a rownum
  var textme2row = new Array();  // associates rownum with a textmenum
  var textisamt = new Array();  // booleans for which textme's are mt's
  var mtnum = 0;	// counts mts, starting at 1
  var mt2cycl = new Array();	// converts mtnum to its cyclnum (rownum)
  var rownum = 0;	// counts cyls, most of which are boxes (some are mts), starting at 1 // careful, NOT ALL rows have boxes (Mt rows don't)
  var ntextme = 0;	// counts popme/linkme textnum, starting at 1
  var linktable = new Array();	// stores links for popme/linkme text, from text to href

  var remain = document.body.innerHTML;
  var originaltext = document.body.innerHTML;
  document.body.innerHTML = '';

//////////////////////////////////////////////////////////////////////////////////////////////////


function striptp(x) { // could actually parse it, but this will suffice for wff cycl without strings
  x = x.replace(/^\#\$/g, ''); 
  x = x.replace(/ \#\$/g, ' '); 
  x = x.replace(/\(\#\$/g, '('); 
  return(x); 
}

function dobject(x) { return(document.getElementById(x)); }

function tc() { 
  // if (popmeopenflag) { textrestore(lastpoppedn); dismisspopme(lastpoppedn); clearpopmeflags(); }
  var all = document.getElementsByTagName('font'); // would be a lot faster to go thru className
  for (var i=0; i<all.length; ++i) {
     if (all[i].className == 'dyntext_class') {
       if (all[i].innerHTML == dobject('input-0').value) return; // if this is same, all are same
       all[i].innerHTML = striptp(dobject('input-0').value);
     }
  }
}

var nextcheckallflag = true;
function togglecheckall() { // handles the ALL/RESET button
  if (nextcheckallflag) {
    checkall(); nextcheckallflag = false;
  } else {
    uncheckall(); nextcheckallflag = true;
  }
}

function checkrowerrstate(n) {
  var target = dobject('checkbox' + n);
  if (target == undefined) return;
  if (target.checked == true && currenterrs[n]) dobject('spanrow' + n).className = 'ackerr_class';
  if (target.checked == false && currenterrs[n]) dobject('spanrow' + n).className = 'clearederr_class';
} 

function togglebox(n) { // toggles box n
  var target = dobject('checkbox' + n);
  if (target == undefined) return;
  if (target.checked == false) target.checked = true;
  else target.checked = false;
  checkrowerrstate(n);
}

function currentselectstate(n) { // returns current select state of box n
  if (dobject('checkbox' + n) == undefined) return(undefined);
  return(dobject('checkbox' + n).checked);
}

function selectbox(n) { // selects box n
  if (dobject('checkbox' + n) != undefined) dobject('checkbox' + n).checked = true;
  checkrowerrstate(n);
}

function uncheckall() { // checks all boxes
  for (var i=1; i<=rownum; i++) if (dobject('checkbox' + i) != undefined) {
    dobject('checkbox' + i).checked = false;
    checkrowerrstate(i);
  }
  for (i in mt2turnon) { mt2turnon[i] = true; }
}

function checkall() { // checks all boxes
  for (var i=1; i<=rownum; i++) if (dobject('checkbox' + i) != undefined) {
    dobject('checkbox' + i).checked = true;
    checkrowerrstate(i);
  }
  for (i in mt2turnon) { mt2turnon[i] = false; }
}

var row2mt = new Array();
var mt2turnon = new Array(); // handles the ALL button at the Mt level

function togglemt(mtn) {
  if (mt2turnon[mtn]) {
    for (var i=1; i<=rownum; i++) if (dobject('checkbox' + i) != undefined) {
      if (row2mt[i] == mtn) {
        dobject('checkbox' + i).checked = true;
        checkrowerrstate(i);
      }
    }
    mt2turnon[mtn] = false;
  } else {
    for (var i=1; i<=rownum; i++) if (dobject('checkbox' + i) != undefined) {
      if (row2mt[i] == mtn) {
        dobject('checkbox' + i).checked = false;
        checkrowerrstate(i);
      }
    }
    mt2turnon[mtn] = true;
  }
}

function removeoldhiddenresults(sn, shtml) { // removes old ac or search results prior to extracting cycl from html
// better to try using clearList API on ac -- but what for search res? clear those manually...  
// or, copy the object, use dom syntax to null the results boxes in the copy, and then get writecycl -- so much better...
// for now, just copy the innerHTML as a string, then null the results using a regexp replace
  for (var m=1; m<=ntextme; m++) {
    if (textme2row[m] == sn) {
      var thisreg = new RegExp('csi-ac-results-' + m + '[\"\'](.|\n)*end-search-results-' + m + '[\"\']'); // remove old autocomplete results if any // do this for each m in n 
      shtml = shtml.replace(thisreg, 'end-search-results');
    }
  }
  return shtml;
}

function writecycl(sn) { // returns the cycl of the sentence sn
  var shtml = dobject('cycl' + sn).innerHTML;
  shtml = removeoldhiddenresults(sn, shtml);
  shtml = shtml.replace(/<[^>]*>/g, ''); // dehtml
  shtml = encodeURI(shtml);
  shtml = shtml.replace(/&nbsp;/g, '%20');
  return(shtml);
}

function writecyclcontext(n) { // returns the cycl context of the sentence containing textme n
  var shtml;
  var sn = textme2row[n];
  if (n in textisamt) {
    shtml = dobject('cycl' + sn).innerHTML;
    shtml = '(isa ' + shtml + ' Microtheory)';
  } else if (n in textme2row) {
    shtml = dobject('cycl' + sn).innerHTML;
  } else return('');
  shtml = removeoldhiddenresults(sn, shtml);
  shtml = encodeURI(shtml.replace(/<[^>]*>/g, '')); // dehtml
  shtml = shtml.replace(/&nbsp;/g, '%20'); // amazingly, encodeURI does not touch the &nbsp;
  shtml = shtml.replace(/%0[Aa]/g, ''); // nuisance LF's
  shtml = shtml.replace(/:/g, '%3A'); // amazingly, encodeURI does not touch the : // probably optional
  shtml = shtml.replace(/-/g, '%2D'); // amazingly, encodeURI does not touch the - // probably optional
  return(shtml);
}

function openinputset(n, oldtext) { // opens a box set of term input, contextual autocomplete, and contextual search
//  dmsg('openinputset called on ' + n);
  dobject('csi-ac-' + n).style.visibility= 'visible';
  dobject('csi-ac-' + n).style.visibility= 'visible';
  dobject('csi-ac-' + n).style.width = '400px';
  dobject('csi-ac-' + n).style.height = dobject('input-0').style.height;
  dobject('csi-ac-' + n).style.left = '0px';
  dobject('csi-input-' + n).style.visibility= 'visible';
  dobject('csi-input-' + n).style.top = '0px';
  dobject('csi-input-' + n).style.width = '340px';
  dobject('csi-input-' + n).style.height = dobject('input-0').style.height;
  dobject('csi-input-' + n).style.padding = '1px';
  dobject('csi-input-' + n).style.borderWidth = '1px';
  dobject('csi-input-' + n).style.fontSize = '12pt';
  dobject('searchbutton-' + n).style.visibility= 'visible';
  dobject('searchbutton-' + n).style.width = '60px';
  dobject('searchbutton-' + n).style.height = '25px';
  dobject('searchbutton-' + n).style.padding = '1px';
  dobject('searchbutton-' + n).style.borderWidth = '1px';
  dobject('searchbutton-' + n).style.left = '0px';
  dobject('searchbutton-' + n).style.fontSize = '10pt';
  dobject('csi-ac-results-' + n).style.visibility= 'visible';
  dobject('csi-ac-results-' + n).style.width = '400px';
  dobject('csi-ac-results-' + n).style.fontSize = '10pt';
  dobject('csi-input-' + n).size = maxmin2size(oldtext.length);
  dobject('csi-input-' + n).value = oldtext;
  //showprops('csi-input-' + n); // works in IE only
  dobject('csi-input-' + n).focus();
}

function showprops(idwho) { // works in IE only
  dmsg('');
  var myobj = dobject(idwho);
  for (var k in myobj.style) dmsg('......' + k + '=' + eval("myobj.style." + k));
}

var acceptableresults = new Array(); // this is populated when ac or search results return

// this is your global state of popped inputboxset
var popmeopenflag = false;
var popme2openflag = false; // implies popmeopen flag
var popme2stringflag = false; // implies popme2openflag
var lastpoppedn = 0; // sorry, but this is a very important global variable! take good care of it!
var lastoldtext = ""; // sorry, but this is a very important global variable! take good care of it!

// this is your undo stack stuff
var savedtext = new Array();
var savedwhere = new Array();
var savedselectstate = new Array();
var savedpoint = 0;
var highestsavedpoint = -1;
var lastdir = 'neither';

var cyclcontext;
var npops=0;

//POP CODE

function popme(n) { // handles a click on a term, for openinput candidates; also 'falsely' entered when a/c result is clicked, on search button, or search result
  npops++;
  // dmsg('popme called on ' + n + ', ' + lastpoppedn + ' npops=' + npops + ' popmeopenflag=' + popmeopenflag + ' chosenvalue=' + chosenvalue);
  if (popmeopenflag && n == lastpoppedn) return;
  if (popmeopenflag && popme2openflag) {
    var newvalue = dobject('popme2input').value;
    dismisspopme(lastpoppedn);
    textcommit(lastpoppedn, newvalue, 'popme-already-open');
    clearpopmeflags();
  } else if (popmeopenflag && !popme2openflag) {
    var newvalue = dobject('csi-input-' + lastpoppedn).value;
    dismisspopme(lastpoppedn);
    if (acceptableresults[newvalue]) textcommit(lastpoppedn, newvalue, 'popme-already-open');
    else textrestore(lastpoppedn);
    clearpopmeflags();
  }
  var oldtext = dobject('textme' + n).innerHTML;
  if (lookupmode == 'browse') {
    if (linktable[oldtext] != undefined) if (linktable[oldtext] != '') window.open(linktable[oldtext], '_new');
    // dmsg(oldtext + ', ' + linktable[oldtext]);
    return;
  }
  openinputset(n, oldtext);
  lastpoppedn = n;
  lastoldtext = oldtext;
  popmeopenflag = true;
  popme2openflag = false;
  popme2stringflag = false;
  // do the a/c stuff last since it is partly async
  for (var i in acceptableresults) delete acceptableresults[i]; // clear acceptable list every time we pop anew
  acceptableresults[oldtext] = 1;
  dobject('textme' + n).innerHTML = ':term-to-replace'; // easier to change the dom here than change it to blank
  cyclcontext = writecyclcontext(n); // see sUrl specs in makeSearchRequest call  -- this is how we define the context
  dobject('textme' + n).innerHTML = '';
  // dmsg('popme context = ' + cyclcontext);
  cyclcontext = '';
  constantCompleteDataSources[n].scriptQueryParam = 'xml-complete&filter=c297&constrainingSentence=' + cyclcontext + '&caseSensitive=nil&prefix';

}

function popme2string(n) { // handles a click on a term, for text strings with CR commit and quotes
  if (popmeopenflag && n == lastpoppedn) return;
  if (popmeopenflag && popme2openflag) {
    var newvalue = dobject('popme2input').value;
    dismisspopme(lastpoppedn);
    textcommit(lastpoppedn, newvalue, 'popme-already-open');
    clearpopmeflags();
  } else if (popmeopenflag && !popme2openflag) {
    var newvalue = dobject('csi-input-' + lastpoppedn).value;
    dismisspopme(lastpoppedn);
    if (acceptableresults[newvalue]) textcommit(lastpoppedn, newvalue, 'popme-already-open');
    else textrestore(lastpoppedn);
    clearpopmeflags();
  }
  var oldtext = dobject('textme' + n).innerHTML.replace(/^"/, '').replace(/"$/, ''); // strip quotes at front and back
  oldtext = oldtext.replace(/\\"/g, '"'); // turn slash-quote back into quote
  var inputboxsize = oldtext.length;
  if (inputboxsize > maxwidth) inputboxsize = maxwidth;
  if (inputboxsize < minwidth) inputboxsize = minwidth;
  var numrows = 2+Math.floor((oldtext.length+10)/maxwidth);
  dobject('textme' + n).innerHTML = '&quot;<span style="white-space:nowrap"><textarea id="popme2input" onKeyUp="this.cols=maxmin2size(this.value.length);" onClick="this.cols=maxmin2size(this.value.length);" rows="' + numrows + '" cols="' + inputboxsize + '" style="font-size:10pt"></textarea>&quot;<input type="button" style="font-size:8pt" value="done" onClick="commitopenstring();"></span>'; // decorate input box with surrounding quotes
  dobject('popme2input').focus();
  dobject('popme2input').innerHTML = oldtext;
  lastpoppedn = n;
  lastoldtext = '"' + oldtext + '"';
  // dmsg(lastoldtext);
  popmeopenflag = true;
  popme2openflag = true;
  popme2stringflag = true;
}

function popme2number(n) { // handles a click on a term, for numbers with CR commit and no quotes
  if (popmeopenflag && n == lastpoppedn) return;
  if (popmeopenflag && popme2openflag) {
    var newvalue = dobject('popme2input').value;
    dismisspopme(lastpoppedn);
    textcommit(lastpoppedn, newvalue, 'popme-already-open');
    clearpopmeflags();
  } else if (popmeopenflag && !popme2openflag) {
    var newvalue = dobject('csi-input-' + lastpoppedn).value;
    dismisspopme(lastpoppedn);
    if (acceptableresults[newvalue]) textcommit(lastpoppedn, newvalue, 'popme-already-open');
    else textrestore(lastpoppedn);
    clearpopmeflags();
  }
  var oldtext = dobject('textme' + n).innerHTML;
  var inputboxsize = oldtext.length;
  if (inputboxsize > maxwidth) inputboxsize = maxwidth;
  dobject('textme' + n).innerHTML = '<input id="popme2input" onKeyUp="this.size=maxmin2size(this.value.length);" onClick="this.size=maxmin2size(this.value.length);" size="' + inputboxsize + '" style="font-size:10pt">';
  dobject('popme2input').focus();
  dobject('popme2input').value = oldtext;
  lastpoppedn = n;
  lastoldtext = oldtext;
  // dmsg(lastoldtext);
  popmeopenflag = true;
  popme2openflag = true;
  popme2stringflag = false;
}

var lookupmode = 'edit';
var linktable = new Array();

function togglelookupmode(n) { // handles the EDIT/BROWSE button
  //if (popmeopenflag) { 
    //msg('please close input box');
    //return; 
  //}
  if (lookupmode == 'browse') {
    lookupmode = 'edit';
    dobject('lookupbutton').value = 'Switch To Browse Mode';
    dobject('lookupbutton2').value = 'Switch To Browse Mode';
    var all = document.getElementsByTagName('font');
    for (var i=0; i<all.length; ++i) {
       if (all[i].className == 'textme_class') {
         all[i].color = 'black';
         all[i].style.textDecoration = 'none';
       } else if (all[i].className == 'dyntext_class') {
         all[i].color = '#600030';
         all[i].style.textDecoration = 'none';
       }
    }
  } else {
    lookupmode = 'browse';
    dobject('lookupbutton').value = 'Switch To Edit Mode';
    dobject('lookupbutton2').value = 'Switch To Edit Mode';
    var all = document.getElementsByTagName('font');
    var linktext;
    for (var i=0; i<all.length; ++i) {
       if (all[i].className == 'textme_class') {
         linktext = all[i].innerHTML.replace(/<[^>]*>/g, '');
         if (linktext in linktable) {
           all[i].color = 'blue';
           all[i].style.textDecoration = 'underline';
         }
       }
    }
  }
}

var maxwidth = 80;
var minwidth = 40;

function maxmin2size(x) { // handles input box resizing
  if (x < minwidth) x = minwidth;
  if (x > maxwidth) x = maxwidth;
  return(x);
}

function doescape() {
  if (popmeopenflag) {
    textrestore(lastpoppedn);
    dismisspopme(lastpoppedn);
  }
  clearpopmeflags();
}

function clearpopmeflags() { // permits a new inputset to be opened
  popmeopenflag = false;
  popme2openflag = false;
  popme2stringflag = false;
}

function checkbodykey(e) { // handles a keypress in the body (can be called redundantly!)
  var evt = e || window.event;
  // dmsg(evt.keyCode);
  if (evt.keyCode == 27) {
    if (popmeopenflag) {
      textrestore(lastpoppedn);
      dismisspopme(lastpoppedn);
    }
    clearpopmeflags();
    for (irow=1; irow<=rownum; irow++) if (fopened[irow]) { restorefree(irow); }
  } else if (evt.keyCode == 13) {
    if (popme2openflag && !popme2stringflag) { // commit number // nop on string
      commitopenstring();
    } else if (popmeopenflag && !popme2stringflag) { // search on normal popme
      var newvalue = dobject('csi-input-' + lastpoppedn).value;
      if (acceptableresults[newvalue] && newvalue != lastoldtext) {
        textcommit(lastpoppedn, newvalue, 'checkbodykey-13');
	dismisspopme(lastpoppedn);
	clearpopmeflags();
      } else checkanddosearch();
    }
  }
}

function commitopenstring() {
  textcommit(lastpoppedn, dobject('popme2input').value, 'commitopenstring');
  dismisspopme(lastpoppedn); // should be done first
  setTimeout('clearpopmeflags()',100);
}

function textrestore(n) { // restores text to earlier state, primarily when escaping out of input sets
  var restoretext;
  restoretext = lastoldtext;
  dobject('textme' + n).innerHTML = restoretext;
  msg(''); // clear any prior warning to close an open box
}

function protecttextarea(x) {
  x = x.replace(/"/g, '&quot;');
  x = x.replace(/\</g, '&lt;');
  x = x.replace(/\>/g, '&gt;');
  x = x.replace(/\n/g, '');
  return(x);
}

function showeachchar(x) {
  x = x.replace(/./g, '($&)') + '.' + x.length; // show me each character and final length
  msg(x);
}

function textcommit(n, newtext, fromwhere) { // commits new text from a popped open input box
  // msg('textcommit called with ' + n + ' and /' + newtext + '/' + ' from ' + fromwhere);
  if (popme2openflag && popme2stringflag) {
    // deal with textarea
    newtext = newtext.replace(/%0A%0D/, ' '); // remove newline // doesn't work if the browser is xlating textarea LF CR
    newtext = decodeURI(newtext); // my browser encodes URI when textarea is committed
    newtext = newtext.replace(/"/g, '\\"');
    if (dobject('textme' + n).textContent != undefined) dobject('textme' + n).textContent = '"' + newtext + '"'; // supply double quotes
    else if (dobject('textme' + n).innerText != undefined) dobject('textme' + n).innerText = '"' + newtext + '"'; // supply double quotes
  } else if (popme2openflag && !popme2stringflag) {
    // convert to numeric
    if (newtext.match(/^0+[^0]/)) {
      newtext = newtext.replace(/^0+/, '');
    } // leading 0's interpreted as octal!
    var newnumber = null;
    if (newtext.match(/^0+$/)) newnumber = parseInt('0');
    else if (newtext.match(/\./)) newnumber = parseFloat(newtext); // note w/o replace, this will convert 016 to 14 (octal)
    else newnumber = parseInt(newtext);
    if (isNaN(newnumber)) {
      // this is like an escape
      dobject('textme' + n).innerHTML = lastoldtext;
      return;
    } else if (newnumber != null) dobject('textme' + n).innerHTML = newnumber;
    if (newnumber != null) dobject('textme' + n).innerHTML = newnumber;
  } else {
    // a normal commit
    if (newtext!= '') dobject('textme' + n).innerHTML = newtext;
  }
  if (newnumber != null || newtext != '' || popme2stringflag) { // allow strings to be null 
    // showeachchar(dobject('textme' + n).innerHTML); // temporary
    // dobject('textme' + n).textContext = newtext;
    // dmsg(newtext.length); dmsg('=?='); dmsg(dobject('textme'+n).textContent.length);
    //dobject('textme' + n).textContext = dobject('textme' + n).textContext.replace(/\&amp\;quot\;/g, '&quot;');
    // dmsg(' >>' + newtext.length); dmsg('=?='); dmsg(dobject('textme'+n).textContent.length);
    if (newtext != lastoldtext) {
      // save the old in the odds and the new in the evens // trickier than usual because multiple edit points
      // pointer is sitting on the last new
      var thisrow = textme2row[n] // for auto-check
      ++savedpoint; // save the old
      savedtext[savedpoint] = lastoldtext;
      savedwhere[savedpoint] = n;
      savedselectstate[savedpoint] = currentselectstate(thisrow);
      ++savedpoint; // save the new
      savedtext[savedpoint] = newtext;
      savedwhere[savedpoint] = n;
      highestsavedpoint = savedpoint;
      lastdir = 'neither';
      selectbox(thisrow);
      savedselectstate[savedpoint] = currentselectstate(thisrow);
    }
  } else { dobject('textme' + n).innerHTML = lastoldtext; }
  checkrowerrstate(textme2row[n]);
}

function uncommit() { // handles a click on the UNDO button
var mysavedtext;
var mysavedwhere;
  if (lookupmode == 'browse') return;
  // pointer is sitting on the last new; to undo, drop one, print, then drop again
  savedpoint--;
  if (savedpoint >= 1) {
    mysavedwhere = savedwhere[savedpoint]; mysavedtext = savedtext[savedpoint];
    if (dobject('textme' + mysavedwhere).innerHTML != undefined) dobject('textme' + mysavedwhere).innerHTML = mysavedtext;
    var thisrow = textme2row[mysavedwhere]
    if (dobject('checkbox' + thisrow) != undefined) dobject('checkbox' + thisrow).checked = savedselectstate[savedpoint]
  }
  savedpoint--;
  if (savedpoint < 1) savedpoint = 0; // sit below first new
  // dmsg(savedpoint);
  // showundostack();
  if (popmeopenflag) dismisspopme(lastpoppedn);
  if (popmeopenflag) textrestore(lastpoppedn);
  clearpopmeflags();
  lastdir = 'undo';
  checkrowerrstate(textme2row[lastpoppedn]);
}

function recommit() { // handles a click on the REDO button
var mysavedtext;
var mysavedwhere;
  if (lookupmode == 'browse') return;
  // pointer is sitting on the last new; to redo, go up two, print
  ++savedpoint;
  ++savedpoint;
  if (savedpoint >= highestsavedpoint) savedpoint = highestsavedpoint; // highest is always a new
  mysavedwhere = savedwhere[savedpoint]; mysavedtext = savedtext[savedpoint];
  if (dobject('textme' + mysavedwhere).innerHTML != undefined) dobject('textme' + mysavedwhere).innerHTML = mysavedtext;
  var thisrow = textme2row[mysavedwhere]
  if (dobject('checkbox' + thisrow) != undefined) dobject('checkbox' + thisrow).checked = savedselectstate[savedpoint]
  // dmsg(savedpoint);
  // showundostack();
  if (popmeopenflag) dismisspopme(lastpoppedn);
  if (popmeopenflag) textrestore(lastpoppedn);
  clearpopmeflags();
  lastdir = 'redo';
  checkrowerrstate(textme2row[lastpoppedn]);
}

//function showundostack() { // this is a debugging procedure
//  for (var i=0;i<=highestsavedpoint+1;i++) {
//    if (i==savedpoint) dmsg('.....*' + i + '*=' + savedtext[i] + '@' + savedwhere[i]);
//    else if (i==highestsavedpoint) dmsg('.....[' + i + ']=' + savedtext[i] + '@' + savedwhere[i]);
//    else dmsg('.....' + i + '=' + savedtext[i] + '@' + savedwhere[i]);
//  }
//}

var currenterrs = new Array(); // holds rownums of currenterrs

var handleSubmitFailure = function(o){ }
var handleSubmitSuccess = function(o){ 
    var responsetext = '';
    // responsetext += 'Transaction id: ' + o.tId;
    // responsetext += 'HTTP status: ' + o.status;
    // responsetext += 'Status code message: ' + o.statusText;
    // responsetext += '<li>HTTP headers: <ul>' + o.getAllResponseHeaders + '</ul></li>';
    // responsetext += '<font color=red>' + o.responseText + '</font>';
    // msg(responsetext); // just in case there is some nasty warning
    qox = o.responseXML.documentElement.tagName;
    var errcount = 0;
    var rox = new Array();
    var roxt = new Array();
    var thiserr; var thistext; var thisrownum;

    // sample responseXML:
    // <response newTermId="Mx4rQ1fWF4u7EeCRKQAhm0kILA">
    //   <error>
    //     <invalidSentence id="4">"(ist CycSubjectClumpsMt ((cycSubjectClumps ToysForAgents HelpingAndHarming-Role-CSC))" </invalidSentence>
    //   </error>
    // </response>

// HERE is where we need code to detect other ERROR conditions

    // store the focal term's guid
    var roxy = o.responseXML.getElementsByTagName('response')[0];
    if (roxy != undefined) {
      newguid = roxy.getAttribute('newTermId');
      if (newguid != undefined) linktable[focaltermsubmitted] = "cg?cb-cf&" + newguid;
    }

    // check for invalid term
    roxt = o.responseXML.getElementsByTagName('invalidTerm');
    if (roxt[0] != undefined)
      if (roxt[0] != undefined) {
        var roxerr = roxt[0].textContent || roxt[0].innerText;
        msg('invalid term: ' + roxerr);
        return;
      }

    // pull out invalid sentences
    rox = o.responseXML.getElementsByTagName('invalidSentence');
    if (roxt.length != 0) errterm = true; else errterm = false;
    for (var i in currenterrs) delete currenterrs[i];
    if (!errterm) {
      for (var i=0; i<rox.length; i++) {
        thiserr = rox[i];
        if (thiserr == undefined) continue; // sometimes there are 3 errs, but length is 5, go figure
        thisrownum = thiserr.getAttribute('id');
        if (thisrownum != undefined) {
          // we have a sentence number
          if (dobject('spanrow' + thisrownum) != undefined) dobject('spanrow' + thisrownum).className = 'err_class';
          currenterrs[thisrownum] = true;
          errcount++;
          continue;
        }
        continue; // let's not even try to pick up the id from the stored/returned text
        thistext = thiserr.textContent || thiserr.innerText;
        thistext = thistext.replace(/[ \n]*$/, '').replace(/^"/, '').replace(/"$/, ''); // seems to append two extra spaces and begin/end quotes
        thistext = thistext.replace(/\\"/g, '"'); // quotes come back as \"
        if (thistext != undefined) {
          // responsetext += '&err' + thistext + '@' + thisrownum + ', ' + thistext.length;
          if (dobject('spanrow' + thisrownum) != undefined) dobject('spanrow' + thisrownum).className = 'err_class';
          currenterrs[thisrownum] = true;
          errcount++;
        }
      }

      // show non-errs
      // for (i in storedsubmitdatawho) if (!currenterrs[i]) {
        // dobject('spanrow' + i).className = 'asserted_class';
        // dobject('checkbox' + i).checked = false;
      // }

      // summarize errs and non-errs
      var nonerrcount = nsubmitted - errcount;
      var mymsg = '';
      mymsg += (0+errcount) + ' assertion error'; if (errcount != 1) mymsg += 's';
      mymsg += '; ' + (0+nonerrcount) + ' assertion'; if (nonerrcount != 1) mymsg += 's';
      mymsg += ' ok.';
      msg(mymsg);
      if (errcount == 0) {
         if (linktable[focaltermsubmitted] != '' && linktable[focaltermsubmitted] != undefined) window.open(linktable[focaltermsubmitted], '_self');
      }
    }

    // responsetext += 'PHP responseXML: ' + o.responseXML;
    // responsetext += 'Argument object: ' + o.argument;
    // dmsg(responsetext);
    // dmsg('<hr><hr>rewritten page HTML:<br>' + indent(document.body.innerHTML)); // show the rewritten HTML
} 

var submitcallback = { 
  success:handleSubmitSuccess, 
  failure: handleSubmitFailure, 
  argument: ['foo', 'bar'] 
};

function restorefromworking() {
  document.body.style.backgroundColor = '#ffffff';
  dobject('submitbutton').value = 'SEND';
  dobject('submitbutton2').value = 'SEND';
}

function notifyworking() {
  // msg('submit data?');
  document.body.style.backgroundColor = '#cccccc';
  dobject('submitbutton').value = '...SUBMITTING...';
  dobject('submitbutton2').value = '...SUBMITTING...';
}

// set up data to interpret error results
var focaltermsubmitted; // can't sense it from input box because it could change after submit!
var storedsubmitdata = new Array(); // indexed by 1, 2, ... , returns rownum 
var storedsubmitdatawho = new Array(); // indexed by rownum returns 1, 2, ...
var storedsubmitdatawhat = new Array(); // indexed by cycl returns rownum
var storedsubmitdatanwhat = new Array(); // indexed by 1, 2, ... returns cycl
var nsubmitted = 0;

function dosubmit() { // handles final assertion
  // dmsg('dosubmit called');
  //if (popmeopenflag) {
    //msg('please close input box');
    //return; // don't return if a inputset is open
  //}
  if (dobject('input-0').value == '') {
    msg('must supply a focal term');
    return ; // don't return on a null focal term
  }
  // define the handle
  var submitdata = 'cb-handle-create-similar';
  // give the focal term
  focaltermsubmitted = striptp(dobject('input-0').value);
  submitdata += '&focalterm=' + encodeURI(focaltermsubmitted);
  //each assertion gets something like &sentence3=(ist mt cycl) and the whole thing is URI encoded
  var ubound = document.form1.elements.length;
  if (nsubmitted > 0) { // clear prior submitted data
    for (i in storedsubmitdata) delete storedsubmitdata[i];
    for (i in storedsubmitdatawho) delete storedsubmitdatawho[i];
    for (i in storedsubmitdatawhat) delete storedsubmitdatawhat[i];
  }
  var thismt;
  var thismtcycl;
  var thisrowcycl;
  var thissentence;
  var thissentencedecoded;
  nsubmitted = 0;
  for (var i=1; i<=rownum; i++) {
    if (dobject('checkbox' + i) != undefined) {
      if (dobject('checkbox' + i).checked) {
        nsubmitted++;
        thismt = row2mt[i];
        thismtcycl = writecycl(mt2cycl[thismt]);
	thisrowcycl = writecycl(i);

	// somewhat a brain ufck, I am taking the &amp;quot; and making them &quot; for postdata; this is because we made &amp; so the user can see the escaped characters in the textarea of a string // technically I should only do this substitution under quotes, but writecycl is not that smart (yet)
	thisrowcycl = thisrowcycl.replace(/&/g, '%26').replace(/;/g, '%3B'); // have to change &lt; to %26lt%3B etc., and &quot; goes to %26quot%3B under quotes // BECAUSE URI encoding in writecycl didn't encode the &, at least not under "... can you believe it!?

        thissentence = '%28ist%20' + thismtcycl + '%20' + thisrowcycl + '%29';
        // if (nsubmitted%3 == 1) thissentence = '%28ist%20' + thismtcycl + '%20%28' + thisrowcycl + '%29'; // poison sentence for debugging:  added extra %28 to test errs 

        submitdata += '&sentence' + i + '=' + thissentence;
	thissentencedecoded = decodeURI(thissentence);
	storedsubmitdata[nsubmitted] = i;
	storedsubmitdatawho[i] = nsubmitted;
	storedsubmitdatawhat[thissentencedecoded] = i; // could be ambiguous if Mt is repeated and two unequal sentences are made the same
	storedsubmitdatanwhat[nsubmitted] = thissentencedecoded; // can be used to deal with ambiguity if you are willing to iterate
      }
    }
  }
  //if (nsubmitted == 1) {
    //confirmation = confirm(nsubmitted + ' assertion to make.');
  //} else confirmation = confirm(nsubmitted + ' assertions to make.');
  // if (confirmation && nsubmitted > 0) { // permit submit on zero sentences?
    notifyworking();
    submitcallback.argument = decodeURI(submitdata).replace(/&/g, '<hr>&');
    var submitXHR = YAHOO.util.Connect.asyncRequest('POST', 'cg', submitcallback, submitdata);
    restorefromworking();
    for (var i in currenterrs) dobject('spanrow' + i).className= '';
  // }
}

function protectcgi(x) {
  x = x.replace(/\&quot;/g, '"');
  x = x.replace(/\&lt;/g, '<');
  x = x.replace(/\&gt;/g, '>');
  return(x.replace(/\&/g, '%26').replace(/\=/g, '%3D'));
}

function checkanddosearch() { if (popmeopenflag) dosearch(); } // give the flags a chance to clear on a CR select from AC

function dosearch() {
  var n = lastpoppedn;
  var ntext = dobject('csi-input-' + n).value;
  ntext = striptp(ntext);
  // if (newtext == '') return;
  // dmsg('dosearch called on ' + n + ', ' + ntext);
  dobject('search-results-' + n).style.border = 'solid black 0px';
  dobject('search-results-' + n).innerHTML = '<i>&nbsp;&nbsp;searching ...</i>';
  dobject('search-results-' + n).style.width = '400px';
  dobject('search-results-' + n).style.top = '0px';
  dobject('search-results-' + n).style.left = '405px';
  dobject('search-results-' + n).style.padding = '5px';
  dobject('search-results-' + n).style.fontSize = '8pt';
  dobject('search-results-' + n).style.visibility = 'visible';
  makeSearchRequest(ntext);
}

function doselect(chosenvalue) { // handles select within the AC menu
  // dmsg('doselect called with chosenvalue=' + chosenvalue);
  if (chosenvalue in acceptableresults) {
    // dmsg('acceptable');
    textcommit(lastpoppedn, chosenvalue, 'doselect');
    chosenvalue = '';
    dismisspopme(lastpoppedn);
    clearpopmeflags();
  }
}

var chosenvalue = '';

function dochoose(m) { // handles onClick within a search results list
  chosenvalue = striptp(searchresterms[m]);
  if (searchresguids[m] != '') linktable[chosenvalue] = 'cg?cb-cf&' + searchresguids[m]; // else suppress the link?
  textcommit(lastpoppedn, chosenvalue, 'dochoose');
  chosenvalue = '';
  dismisspopme(lastpoppedn);
  clearpopmeflags();
}

function dismisspopme(n) { // hides a complete set of input box, AC results span, and search results span
  // btw, popme2input is 'dismissed' by reassinging a string to the innerHTML of its enclosing textme
  // msg('dismiss called on ' + n + 'with' + popme2openflag);
  if (dobject('csi-ac-' + n) == undefined) return;
  dobject('csi-input-' + n).style.visibility = 'hidden';
  dobject('csi-input-' + n).value = '';
  dobject('csi-input-' + n).style.width = '0px';
  dobject('csi-input-' + n).style.height = '0px';
  dobject('csi-input-' + n).style.padding = '0px';
  dobject('csi-input-' + n).style.borderWidth = '0px';
  dobject('csi-input-' + n).style.fontSize = '0pt';
  dobject('searchbutton-' + n).style.visibility = 'hidden';
  dobject('searchbutton-' + n).style.width = '0px';
  dobject('searchbutton-' + n).style.height = '0px';
  dobject('searchbutton-' + n).style.padding = '0px';
  dobject('searchbutton-' + n).style.borderWidth = '0px';
  dobject('searchbutton-' + n).style.fontSize = '0pt';
  dobject('csi-ac-results-' + n).style.visibility = 'hidden';
  dobject('csi-ac-results-' + n).style.width = '0px';
  dobject('csi-ac-results-' + n).style.height = '0px';
  dobject('csi-ac-results-' + n).style.fontSize = '0pt';
  dobject('search-results-' + n).style.visibility = 'hidden';
  dobject('search-results-' + n).style.width = '0px';
  dobject('search-results-' + n).style.padding = '0px';
  dobject('search-results-' + n).style.border = 'solid black 0px';
  dobject('search-results-' + n).style.fontSize = '0pt';
  dobject('csi-ac-' + n).style.visibility = 'hidden';
  dobject('csi-ac-' + n).style.width = '0px';
  dobject('csi-ac-' + n).style.height = '0px';
  dobject('csi-ac-' + n).style.fontSize = '0pt';
  // dmsg('dismiss finishing on ' + n);
}

function clearsearchresults(n) { // used to dismiss on blur of input, since Yahoo is hiding a/c
  dobject('search-results-' + n).style.visibility = 'hidden';
  dobject('search-results-' + n).style.width = '0px';
  dobject('search-results-' + n).style.padding = '0px';
  dobject('search-results-' + n).style.border = 'solid black 0px';
  dobject('search-results-' + n).style.fontSize = '0pt';
}

// this is taken from the developer.yahoo.com/yui/examples/connection/get_clean.html
// this is the search-results handler

var searchresguids = new Array;
var searchresnls = new Array;
var searchresterms = new Array;
var searchresn = 0;
var handleSearchSuccess = function(o) {
  if (o.responseText !== undefined) {
    var searchRespChildren = o.responseXML.getElementsByTagName('Term');
    searchresn = searchRespChildren.length;
    var searchlist = '';
    dobject('search-results-' + lastpoppedn).style.border = 'solid black 1px';
    if (searchresn == 0) {
      dobject('search-results-' + lastpoppedn).innerHTML = '<i> no results found.</i>';
      return;
    }
    for (var i=0; i<searchresn; i++) {;
      searchresguids[i] = searchRespChildren[i].getAttribute('hlId');
      searchresnls[i] = searchRespChildren[i].getAttribute('nl');
      searchresterms[i] = searchRespChildren[i].getAttribute('cycl');
      //searchlist = searchlist + '<font class="searchresultline_class" onClick="dochoose(' + i + ')">' + searchresnls[i];
      //searchlist = searchlist + '&nbsp;<span class="autoCompCycL">' + searchresterms[i] + '</span></font><br>';
      searchlist = searchlist + '<font class="searchresultline_class" onClick="dochoose(' + i + ')">';
      searchlist = searchlist + '<span class="autoCompCycL">' + searchresterms[i] + '</span></font><br>';
      acceptableresults[striptp(searchresterms[i])] = 1;
    }
    if (searchresn == 1) { // go ahead and commit the single returned value
      dobject('search-results-' + lastpoppedn).innerHTML = '&nbsp;&nbsp;unique result found.';
      dochoose(0); // numbering starts at 0
      // clearpopmeflags(); // cleared in dochoose
      return;
    }
    dobject('search-results-' + lastpoppedn).innerHTML = searchlist;
  }
}
var handleSearchFailure = function(o) {
  if (o.responseText !== undefined) {
    var div = dobject('textme' + lastpoppedn);
    if (div == undefined) return;
    div.innerHTML = "<ul><li>Search Call FAILURE Transaction id: " + o.tId + "</li>";
    div.innerHTML += "<li>HTTP status: " + o.status + "</li>";
    div.innerHTML += "<li>Status code message: " + o.statusText + "</li></ul>";
  }
}
var searchcallback = {
  success:handleSearchSuccess,
  failure:handleSearchFailure,
  argument: { foo:"food", bar:"barn" }
};

function makeSearchRequest(mystring) { // initiates a search request with context; keep examples around!
  var sUrl;
  mystring = striptp(mystring);
  cyclcontext = '';
  if (cyclcontext == '') sUrl = "cg?xml-term-search&searchString=" + mystring + "&caseSensitive=nil&return-attrs=(:cycl)";
  else sUrl= "cg?xml-term-search&searchString=" + mystring + "&constrainingSentence=" + cyclcontext + "&caseSensitive=nil&return-attrs=(:cycl)";
  // dmsg(cyclcontext);
  var request = YAHOO.util.Connect.asyncRequest('GET', sUrl, searchcallback);
}

var constantCompleteDataSources = new Array();
var constantCompletes = new Array();
var selectHandlers = new Array();

function declareautos(ntextme) {
// set up the focal term's input box and AC code
  var constantCompleteDataSource = new YAHOO.widget.DS_XHR('./cg', ['Term', 'cycl', 'nl']);
    constantCompleteDataSource.scriptQueryParam = 'xml-complete&filter=c297&prefix';
    // add constraining-stentence=...
    constantCompleteDataSource.responseType = YAHOO.widget.DS_XHR.TYPE_XML;
    var constantComplete = new YAHOO.widget.AutoComplete('input-0', 'autocomplete-results-0', constantCompleteDataSource);
    constantComplete.allowBrowserAutocomplete = false;
    constantComplete.autoHighlight = false;
    constantComplete.minQueryLength = 3;
    constantComplete.animSpeed = 0.1;
    constantComplete.maxResultsDisplayed = 20;
    constantComplete.queryDelay = 0.3;
    constantComplete.embeddedInToolbar = false;
    constantComplete.formatResult = function(oResultItem, sQuery) {
      var thisConst = oResultItem[0];
      var string = oResultItem[1];
      var sMarkup = string + ' <span class=\"autoCompCycL\">' + thisConst + '</span>';
      return (sMarkup);
    };

  for (var n=1; n<=ntextme; n++) {
    if (dobject('csi-input-' + n) == undefined) continue;
    constantCompleteDataSources[n] = new YAHOO.widget.DS_XHR('./cg', ['Term', 'cycl', 'nl']);
    constantCompleteDataSources[n].scriptQueryParam = 'xml-complete&filter=c297&prefix';
    constantCompleteDataSources[n].responseType = YAHOO.widget.DS_XHR.TYPE_XML;
    constantCompletes[n] = new YAHOO.widget.AutoComplete('csi-input-' + n, 'csi-ac-results-' + n, constantCompleteDataSources[n]);
    constantCompletes[n].allowBrowserAutocomplete = false;
    constantCompletes[n].autoHighlight = false;
    constantCompletes[n].minQueryLength = 3;
    constantCompletes[n].animSpeed = 0.1;
    constantCompletes[n].maxResultsDisplayed = 20;
    constantCompletes[n].queryDelay = 0.3;
    constantCompletes[n].embeddedInToolbar = false;
    constantCompletes[n].formatResult = function(oResultItem, sQuery) {
      var thisConst = oResultItem[0];
      var string = oResultItem[1];
      var sMarkup = string + ' <span class=\"autoCompCycL\">' + thisConst + '</span>';
      acceptableresults[striptp(thisConst)] = 1; // note that we pick up a list of acceptable results every time we AC
      return (sMarkup);
    };
    constantCompletes[n].doBeforeLoadData = function(sQuery, oResponse, oPayload) {
      if (dobject('csi-ac-results-' + n)) dobject('csi-ac-results-' + n).style.height = '400px';
      return true;
    };
    selectHandlers[n] = function(sType, aArgs) {
      // dmsg('entering handler' + aArgs[0] + '/' + aArgs[1] + '/' + aArgs[2]);
      // aArgs2 looks like:  #$BiologicalAndToxinWeaponsConvention,BiologicalAndToxinWeaponsConvention
      var chosenresult = aArgs[2];
      var chosenvalue = striptp(chosenresult[0]);
      if (chosenvalue != "") doselect(chosenvalue);
    };
    constantCompletes[n].itemSelectEvent.subscribe(selectHandlers[n]);
  }
}

function declareoneauto(ntextme) {
  for (var n=ntextme; n<=ntextme; n++) { // yeah, just do this one
    if (dobject('csi-input-' + n) == undefined) continue;
    constantCompleteDataSources[n] = new YAHOO.widget.DS_XHR('./cg', ['Term', 'cycl', 'nl']);
    constantCompleteDataSources[n].scriptQueryParam = 'xml-complete&filter=c297&prefix';
    constantCompleteDataSources[n].responseType = YAHOO.widget.DS_XHR.TYPE_XML;
    constantCompletes[n] = new YAHOO.widget.AutoComplete('csi-input-' + n, 'csi-ac-results-' + n, constantCompleteDataSources[n]);
    constantCompletes[n].allowBrowserAutocomplete = false;
    constantCompletes[n].autoHighlight = false;
    constantCompletes[n].minQueryLength = 3;
    constantCompletes[n].animSpeed = 0.1;
    constantCompletes[n].maxResultsDisplayed = 20;
    constantCompletes[n].queryDelay = 0.3;
    constantCompletes[n].embeddedInToolbar = false;
    constantCompletes[n].formatResult = function(oResultItem, sQuery) {
      var thisConst = oResultItem[0];
      var string = oResultItem[1];
      var sMarkup = string + ' <span class=\"autoCompCycL\">' + thisConst + '</span>';
      acceptableresults[striptp(thisConst)] = 1; // note that we pick up a list of acceptable results every time we AC
      return (sMarkup);
    };
    constantCompletes[n].doBeforeLoadData = function(sQuery, oResponse, oPayload) {
      if (dobject('csi-ac-results-' + n)) dobject('csi-ac-results-' + n).style.height = '400px';
      return true;
    };
    selectHandlers[n] = function(sType, aArgs) {
      // dmsg('entering handler' + aArgs[0] + '/' + aArgs[1] + '/' + aArgs[2]);
      // aArgs2 looks like:  #$BiologicalAndToxinWeaponsConvention,BiologicalAndToxinWeaponsConvention
      var chosenresult = aArgs[2];
      var chosenvalue = striptp(chosenresult[0]);
      if (chosenvalue != "") doselect(chosenvalue);
    };
    constantCompletes[n].itemSelectEvent.subscribe(selectHandlers[n]);
  }
}

function indent(x) { // used to show a bit of html and js, indented somewhat, but not css aware
  var noindent = new Array();
  noindent['input'] = true;
  noindent['br'] = true;
  noindent['hr'] = true;
  noindent['li'] = true;
  noindent['/li'] = true;
  noindent['img'] = true;
  var result = '';
  var imatchtuple = x.match("<[^>]*>"); // the match of each tag
  var indentstring = ':BR:..';
  var scripton = false;
  if (imatchtuple != '') while (imatchtuple[0] != '') {
    remain = RegExp.rightContext;
    skippedtext = RegExp.leftContext;
    tagcontents = imatchtuple[0].slice(1, imatchtuple[0].length-1);
    tagname = tagcontents.replace(/ .*/, '').toLowerCase(); // strip the <>
    if (!scripton) if (tagcontents.slice(0, 6) == 'script') scripton = true
    if (scripton) skippedtext = skippedtext.replace(/ /g, '.');
    if (scripton) skippedtext = skippedtext.replace(/\;/g, ';:BR:');
    if (scripton) skippedtext = skippedtext.replace(/{/g, '{:BR:');
    if (scripton) skippedtext = skippedtext.replace(/}/g, ':BR:}:BR:');
    if (skippedtext != '') result += indentstring + skippedtext;
    if (scripton) if (tagcontents.slice(0, 7) == '/script') scripton = false;
    if (!scripton) if (!(tagname in noindent)) if (tagcontents[0].slice(0, 1) == '/') indentstring = indentstring.slice(0, indentstring.length-2);
    if (indentstring.length < 4) indentstring = ':BR:';
    result += indentstring + imatchtuple[0]; // append the tag
    if (!scripton) if (!(tagname in noindent)) if (tagcontents[0].slice(0, 1) != '/') indentstring += '..';
    imatchtuple = remain.match("<[^>]*>");
    if (imatchtuple == null) break;
  }
  // now make html pretty
  result = result.replace(/\</g, '&lt;');
  result = result.replace(/\&lt;script/g, '<hr>&lt;script');
  result = result.replace(/\&lt;\/script>/g, '&lt;/script><hr>');
  result = result.replace(/:BR:(\n):BR:/g, ':BR:');
  result = result.replace(/:BR:/g, '<BR>');
  return(result);
}

function writetopstuff(x, xg) { // note that we cannot call this until we pick up the focal guid from the parser
dwln("<body class=\"yui-skin-sam\" onMouseOver=\"tc();\" onKeyUp=\"checkbodykey(event);\">");
dwln("");
dwln("<h3>Copied From <a href=\"cg?cb-cf&" + xg + "\">" + x + "</a>");
dwln("<a href=\"/cycdoc/help/cb-create-similar.html\">");
dwln("&nbsp;<img src=\"/cycdoc/img/cb/help_btn_s.gif\" alt=\"[Help]\" align=\"top\" border=\"0\"></img>");
dwln("</a>");
dwln("</h3>");
dwln("<form name=\"form1\" action=\"\" method=\"get\" onSubmit=\"return(false);\">");
dwln("");
dwln("<input id=\"submitdata\" type=\"hidden\" name=\"cb-handle-create-similar\">");
dwln("<font id=\"defaulttext\"><i style=\"line-height:150%\">Enter name for new term:</i></font><br />");

// set up the focal term's input box and AC span
dw("<span id=\"autocomplete-0\" class=\"yui-ac\">");
dw("<input type=\"text\" style=\"position:static;width:auto;color:#600030\" id=\"input-0\" value=\"COPY_OF_" + x + "\" size=\"40\" class=\"yui-ac-input\"");
dw(" onKeyUp=\"tc();\" onMouseDown=\"tc();\" onMouseUp=\"tc();\" onMouseMove=\"tc();\" onMouseOver=\"tc();\" onMouseOut=\"tc();\" onClick=\"tc();\" onLoad=\"tc();\" ");
dw(">")
dw("<span id=\"autocomplete-results-0\" class=\"yui-ac-container\" style=\"left:0px\" onClick=\"tc();\"></span>");
dwln("</span>");
dwln("<span id=\"messages\" style=\"visibility:visible\">&nbsp;</span>");
dwln("<span id=\"debug\" style=\"visibility:visible\">&nbsp;</span>");

dwln("");
dwln("<br />");
dwln("<br />");
dwln("&nbsp;&nbsp;&nbsp;&nbsp; <i>-- For detailed help and warnings about creating, please read the <a href=\"/cycdoc/ref/cycl-syntax.html#naming conventions\">Cyc Naming Conventions</a> documentation.</i><hr>");

// dwln("Check or alter the assertions below:<br />");
dwln("<input type=\"button\" value=\"All Mts\" title=\"Check All / Uncheck All\" onClick=\"togglecheckall();\" style=\"font-size:10pt\">");
dwln("<input type=\"button\" value=\"Undo\" onClick=\"uncommit()\" style=\"font-size:10pt\">");
dwln("<input type=\"button\" value=\"Redo\" onClick=\"recommit()\" style=\"font-size:10pt\">");
dwln("<input id=\"lookupbutton\" type=\"button\" value=\"Switch To Browse Mode\" title=\"Switch between click-to-edit and click-to-view\" onClick=\"togglelookupmode();\" style=\"font-size:10pt\">");
//dwln("<input id=\"escbutton\" type=\"button\" value=\"Escape\" title=\"Discard changes to the currently-open input box\" onClick=\"doescape();\" style=\"font-size:10pt\">");
dwln("<input id=\"submitbutton\" type=\"button\" value=\"SEND\" title=\"Assert selected sentences\" onClick=\"dosubmit();\" style=\"font-size:10pt;background-color:#ccccee\">");
dwln("<br>");

}

function writebuttonrow() { 
  dwln("<input type=\"button\" value=\"All Mts\" title=\"Check All / Uncheck All\" onClick=\"togglecheckall();\" style=\"font-size:10pt\">");
  dwln("<input type=\"button\" value=\"Undo\" onClick=\"uncommit()\" style=\"font-size:10pt\">");
  dwln("<input type=\"button\" value=\"Redo\" onClick=\"recommit()\" style=\"font-size:10pt\">");
  dwln("<input id=\"lookupbutton2\" type=\"button\" value=\"Switch To Browse Mode\" title=\"Switch between click-to-edit and click-to-view\" onClick=\"togglelookupmode();\" style=\"font-size:10pt\">");
dwln("<input id=\"escbutton2\" type=\"button\" value=\"Escape\" title=\"Discard changes to the currently-open input box\" onClick=\"doescape();\" style=\"font-size:10pt\">");
  dwln("<input id=\"submitbutton2\" type=\"button\" value=\"SEND\" title=\"Assert selected sentences\" onClick=\"dosubmit();\" style=\"font-size:10pt\";background-color:#ccccee>");
}

function writebottomstuff() { // called at the end of the parse
  dwln("<br><br>");
  writebuttonrow();
  dwln("</form>")
  dwln("</body>")
}

// THIS IS THE PARSER OF THE PAGE'S ORIGINAL XML ////////////////////////////////////////////////////////

// this could be improved by manipulating the XML as an object with tags rather than a string delimited by <>...
// sorry I didn't do it that way at first

if (true) {
  writecss();

  var ntags = 0;	// used to quit on ultra long pages
  var startflag = false;        // set true upon seeing <assertions>
  var valstack = new Array();	// stores non tag data associated with each taglevel
  var tagstack = new Array();	// stores a stack of tags (lower case, just the tagname)
  var taglevel = -1;
  var stored = new Array(); // additional stored values
  var workingonmtflag = false;	// used to signal append cycl fragments to newmt
  var newmt = ''; // used to accumulate cycl fragements in an mt section
  var newmttextmeset = new Array(); // array of integers that are the textme's seen in this newmt

  var matchtuple = remain.match("<[^>]*>"); // the match of each tag
  // this is the basic parser which does not know about < or > under quotes

  if (matchtuple != null) while (matchtuple[0] != "") {

    // note that the match has to be the most recent use of regexp, including any string replaces
    var remain = RegExp.rightContext;
    var skippedtext = RegExp.leftContext;
    if (valstack[taglevel] == "") { valstack[taglevel] = skippedtext; }
    else { valstack[taglevel] = valstack[taglevel] + " " + skippedtext; }
    // accbdw("<br>" + ntags + " ");

    var tagcontents = matchtuple[0].slice(1, matchtuple[0].length-1);
    // accbdw("<li>" + tagcontents);

    // manage the stack
    if (tagcontents.slice(0, 10).toLowerCase() == "assertions") startflag = true;
    if (startflag) {
      if (tagcontents.slice(0, 1) == "/") {
        if (tagstack[taglevel] == tagcontents.slice(1).toLowerCase()) {
        // accbdw(" match");
        } else {
          accbdw(" ERR!  UNMATCHED XML TAG IN PAGE! <BR>");
          accbdw(tagstack[taglevel]);
        }
        // if (valstack[taglevel] != "") accbdw("....." + valstack[taglevel]);
        managecontentprepop();
        // accbdw(" pop");
        if (tagcontents.toLowerCase() == "/assertions") break;
        taglevel--;
        valstack[taglevel] = "";
      } else {
        // accbdw(" push");
        // accbdw(" "+stored['lastmt']);
        var shortlowertagname = tagcontents.replace(/ .*/, "").toLowerCase(); // remove attributes
        tagstack[++taglevel] = shortlowertagname;
        valstack[taglevel] = "";
        managecontentpostpush(shortlowertagname, tagcontents);
      }
    }

    matchtuple = remain.match("<[^>]*>");
    if (matchtuple == null) break;
    ++ntags;
    if (ntags > 100000) break; // a crazy number of tags, I quit
  }
  flushobuffer();

  declarerow2mts();
  declareautos(ntextme);
  writebottomstuff();
}

function managecontentprepop() {
  // called during a pop before tag is popped
  if (tagstack[taglevel] == "guid") {
    stored['lastguid'] = valstack[taglevel];
    //accbdw("storing lastguid of " + valstack[taglevel]);
  }
  var thispath0 = tagstack[taglevel] + ">" + tagstack[taglevel-1];
  var thispath1 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2];
  var thispath2 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2] + ">" + tagstack[taglevel-3] ;
  var newname = valstack[taglevel];
  if (thispath1 == "microtheory>assertion>assertions") {
    // write or extend an mt block
    var comparemt = newmt.replace(/<[^>]*>/g, ''); // strip html for comparison because it contains unequal rownums
    //dw("<hr><li>" + stored['lastmt'] + " vs " + comparemt + "<hr>");
    if (stored['lastmt'] != comparemt) {
      obuffer = '';
      // set up a new mt header on the stored cycl fragment
      ++mtnum;
      dw("<img src=\"/cycdoc/js/blank.gif\" border=\"0px\" height=\"30px\" width=\"1\" style=\"visibility:hidden\"><input type=\"button\" value=\"All\" style=\"font-size:9pt\" onClick=\"togglemt(" + mtnum + ");\"> "); // img is there to resize line in IE and chrome
      dw("<font color=\"#8888ff\" style=\"line-height:30px;text-decoration:underline\">Mt :"); // line height is there for Firefox
      ++rownum; // used to recover cycl fragments, but most are boxes, except these mt's which are just cycl terms // think of mt's as having boxes that are hidden in favor of the ALL button
      for (i in newmttextmeset) textme2row[i] = rownum; // fix textme's so they point to this new box // they were set earlier, but the mt now claims its pieces
      for (i in newmttextmeset) textisamt[i] = true; // all textme's in this set are considered mt textme's
      for (i in newmttextmeset) delete newmttextmeset[i];
      mt2cycl[mtnum] = rownum; // used to recover mt text from mtnum
      dw("<span id=\"cycl" + rownum + "\">");
      dw(newmt);
      dw("</span>"); // end cycl
      dw("</font>"); // end color
      // accbdw("<br>");
      stored['lastmt'] = comparemt;
      //dw("<hr>storing lastmt as " + newmt + "<hr>");
      workingonmtflag = false;
    } else {
      // if they match, just flush, don't set up a new Mt header
      obuffer = '';
      workingonmtflag = false;
    }
  }


  if (thispath0 == "name>symbol") {
    // can we check that we just popped off <package>KEWYORD</package>?
    newname = ':' + newname;
  }

  if (tagstack[taglevel] == "name") {
    if (newname == focalname) {
      newterm();
      accbdw("<font class=\"dyntext_class\" id=\"popme" + ntextme + "\">" + 'COPY_OF_' + newname + "</font>");
    } else {
      newterm();
      accbdw("<font class=\"popmelinkme_class\" id=\"popme" + ntextme + "\"><font class=\"textme_class\" id=\"textme" + ntextme + "\" style=\"left:0\" onClick=\"popme(" + ntextme + ");\">" + newname + "</font>");
      createnewinputset(ntextme);
      accbdw("</font>");
      linktable[newname] = "cg?cb-cf&" + stored['lastguid'];
    }
    nextspaceflag = true;
  }

  if (tagstack[taglevel] == "string") {
    newterm();
    // text comes to us with unprotected quotes
    newname = newname.replace(/"/g, '\\"');
    accbdw("<font class=\"popme2linkme_class\" id=\"popme2_" + ntextme + "\"><font class=\"textme_class\" id=\"textme" + ntextme + "\" style=\"left:0\" onClick=\"popme2string(" + ntextme + ");\">" + "\"" + protecttextarea(newname) + "\"" + "</font></font>");
    nextspaceflag = true;
  }

  if (tagstack[taglevel] == "number") {
    newterm();
    accbdw("<font class=\"popme2linkme_class\" id=\"popme2_" + ntextme + "\">");
    if (extraspacefornumbersflag == true) accbdw("&nbsp;");
    accbdw("<font class=\"textme_class\" id=\"textme" + ntextme + "\" style=\"left:0\" onClick=\"popme2number(" + ntextme + ");\">" + newname + "</font>");
    if (extraspacefornumbersflag == true) accbdw("&nbsp;");
    accbdw("</font>");
    nextspaceflag = true;
  }

  if (tagstack[taglevel] == "variable") {
    if (nextspaceflag) accbdw("&nbsp;")
    accbdw(newname);
    nextspaceflag = true;
  }

  if (tagstack[taglevel] == "function") {
    // end a function // ignore nextspaceflag
    accbdw(")");
    nextspaceflag = true;
  }

  if (thispath0 == "sentence>assertions") {
    // this ends a top level sentence // ignore nextspaceflag
    accbdw(")");
    nextspaceflag = true;
    accbdw("</span></span></div>");
  } else if (thispath0 == "sentence>function") {
    // this ends a sentence in a function
    // yes, really do nothing!
  } else if (tagstack[taglevel] == "sentence") {
    // this ends an embedded sentence // ignore nextspaceflag
    accbdw(")");
    nextspaceflag = true;
  }

}

function newterm() {
  ++ntextme;
  if (workingonmtflag) newmttextmeset[ntextme] = 1;
  if (nextspaceflag) accbdw("&nbsp;");
  nextspaceflag = true;
  textme2row[ntextme] = rownum;
}

var logicaloperators = new Array();
logicaloperators["not"] = true;
logicaloperators["and"] = true;
logicaloperators["or"] = true;
logicaloperators["implies"] = true;

function managecontentpostpush(shortlowertagname, tagcontents) {
  // called during a pop before tag is popped
  var thispath0 = tagstack[taglevel] + ">" + tagstack[taglevel-1];
  var thispath1 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2];
  var thispath2 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2] + ">" + tagstack[taglevel-3] ;
  var thispath2gap = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + ">" + tagstack[taglevel-3] ;
  var thispath3 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2] + ">" + tagstack[taglevel-3] + ">" + tagstack[taglevel-4];
  if (shortlowertagname == "assertions") {
    // tag looked like <assertions focalTerm="IntelligenceCommunityFn" hlid="Mx4rv4nDAZwpEbGdrcN5Y29ycA">
    focalname = tagcontents.replace(/.*focalterm=./i, "").replace(/\".*/, "");
    focalname = focalname.replace(/^\:/, "");
    focalname = focalname.replace(/^\#\$/, "");

    focalguid = tagcontents.replace(/.*hlid="/i, "").replace(/\"/, "");
    writetopstuff(focalname, focalguid);
  }
  //if (thispath3 == "name>constant>predicate>sentence>assertions") {
  if (thispath0 == "sentence>assertions") {
    // generate new predicate or embedded sentence at the assertion level, hence it gets a checkbox
    ++rownum;
    row2mt[rownum] = mtnum;
    accbdw("<div id =\"divrow" + rownum + "\" style=\"width:90%\"><img src=\"/cycdoc/js/blank.gif\" height=\"25px\" width=\"1px\"><span class='dynamicline' id=\"spanrow" + rownum + "\" style=\"line-height:150%;width:50\">");

    //accbdw("<font onClick=\"togglebox(" + rownum + ");\">&nbsp;&nbsp;&nbsp;</font><input id=\"checkbox" + rownum + "\" type=\"checkbox\" onClick=\"setTimeout('checkrowerrstate(" + rownum + ")', 100);\"><font class=\"hoverclass\" id=\"checkadd" + rownum + "\" onClick=\"addrow(" + rownum + ");\" style=\"font-size:10pt\">&#9802;&nbsp;</font><font class=\"hoverclass\" onClick=\"freeedit(" + rownum + ");\">&#9872&nbsp;&nbsp;&nbsp;</font>");
    accbdw("<font class=\"hoverclass\" id=\"checkadd" + rownum + "\" onClick=\"addrow(" + rownum + ");\" style=\"font-size:10pt\">&nbsp;<img class='hidebutton' id='addsign" + rownum + "' src='/cycdoc/js/addsign.png' height='12' title='clone this row'>&nbsp;</font><font class=\"hoverclass\" onClick=\"freeedit(" + rownum + ");\"><img class='hidebutton' id='textsign" + rownum + "' src='/cycdoc/js/text.png' height='15' title='edit this row as free text'>&nbsp;&nbsp;&nbsp;</font><input id=\"checkbox" + rownum + "\" type=\"checkbox\" onClick=\"setTimeout('checkrowerrstate(" + rownum + ")', 100);\">");
    accbdw("<span id=\"cycl" + rownum + "\">");  
    nextspaceflag = false;
  } 

  if (thispath2gap == "name>constant>>sentence" && thispath2 != "name>constant>function>sentence" && tagstack[taglevel-2] != "sentence") {
    // this starts an embedded sentence; the gap is a logical operator such as implies, and, or
    // it's not entirely clear that these cases suffice, but i don't have a full spec!
    if (nextspaceflag) accbdw("&nbsp;")
    accbdw("(");
    nextspaceflag = false;
  }

  if (shortlowertagname == "microtheory") {
    flushobuffer();
    workingonmtflag = true;
    newmt = '';
  }

  if (shortlowertagname == "function") {
    // generate new function
    accbdw("&nbsp;(");
    nextspaceflag = false;
  }
}

function createnewinputset(n) {
  accbdw("<font id=\"csi-ac-" + n + "\" class=\"yui-ac\" style=\"visibility:visible;width:0;height:0;font-size:0pt;padding:0;background-color:white;white-space:nowrap\">");
  accbdw("<input id=\"csi-input-" + n + "\" type=\"text\" style=\"visibility:visible;width:0;height:0;border-width:0;font-size:0pt;padding:0;background-color:white;position:relative\" value=\"\" size=\"0\" class=\"yui-ac-input\" onKeyUp=\"this.size=maxmin2size(this.value.length)\" onClick=\"this.size=maxmin2size(this.value.length);\" onBlur=\"clearsearchresults(" + n + ");\">");
  accbdw("<input id=\"searchbutton-" + n + "\" type=\"button\" value=\"Search\" onClick=\"dosearch();\" style=\"visibility:visible;width:0;height:0;border-width:0;font-size:0pt;padding:0;position:relative\">");
  accbdw("<span id=\"csi-ac-results-" + n + "\" class=\"yui-ac-container\" style=\"top:4px;visibility:visible;width:0;height:0;font-size:0pt;padding:0;background-color:white;left:0px\"></span>");
  accbdw("<span id=\"search-results-" + n + "\" class=\"yui-ac-container\" style=\"visibility:visible;width:0;font-size:0pt;padding:0;background-color:white;border-width:0\"></span>");
  accbdw("<span id=\"end-search-results-" + n + "\" style=\"visibility:visible;width:0;height:0;font-size:0pt;padding:0\"><span>");  // note that we use the csi-ac-results-n to end-search-results-n strings to ID the old AC results when clearing
  accbdw("</font>");
}

function declarerow2mts() {
  // declare these at each mt so partial load has partial function
  // also do this at the very end since there is no final mt
  for (var i=1; i<=mtnum; i++) mt2turnon[i] = true;
  return;
}

function writecss() {
  dwln("<style type=\"text/css\">");
  dwln("  font.dyntext_class { color:#600030; }");
  dwln("  font.popmelinkme_class:hover { cursor:pointer; }");
  dwln("  font.hoverclass:hover { cursor:pointer; }");

  dwln("  span.dynamicline img.hidebutton { visibility:hidden }");
  dwln("  span.dynamicline:hover img.hidebutton { visibility:visible }");
  dwln("  span.dynamicline:hover { background-color:#ffffe0; }");

  dwln("  font.searchresultline_class { color:red; line-height:180%; font-size:10pt; }");
  dwln("  font.searchresultline_class:hover { background-color:#ddddff; }");

  dwln("  span.err_class { border:solid red 1px; }");
  dwln("  span.err_class img.hidebutton { visibility:hidden }");
  dwln("  span.err_class:hover img.hidebutton { visibility:visible }");
  dwln("  span.err_class:hover { background-color:#ffffe0; }");

  dwln("  span.ackerr_class { border:dotted #ff8080 1px; }");
  dwln("  span.ackerr_class img.hidebutton { visibility:hidden }");
  dwln("  span.ackerr_class:hover img.hidebutton { visibility:visible }");
  dwln("  span.ackerr_class:hover { background-color:#ffffe0; }");

  dwln("  span.asserted_class { border:dotted #80ff80 1px; }");
  dwln("  span.asserted_class img.hidebutton { visibility:hidden }");
  dwln("  span.asserted_class:hover img.hidebutton { visibility:visible }");
  dwln("  span.asserted_class:hover { background-color:#ffffe0; }");

  dwln("  span.clearederr_class img.hidebutton { visibility:hidden }");
  dwln("  span.clearederr_class:hover img.hidebutton { visibility:visible }");
  dwln("  span.clearederr_class:hover { background-color:#ffffe0; }");

  dwln("</style>");
}

var holdtxt = new Array;
var fholdtxt = new Array;
var fopened = new Array;

function restorefree(arownum) {
  dobject("cycl" + arownum).innerHTML = holdtxt[arownum];
  fopened[arownum] = false;
}

function savefree(arownum) {
  dobject("cycl" + arownum).innerHTML = dobject("openfree" + arownum).value;
  dobject("checkbox" + arownum).checked = true;
  fopened[arownum] = false;
}

function freeedit(arownum) {
  holdtxt[arownum] = dobject("cycl" + arownum).innerHTML;
  var whoreg = new RegExp('csi-ac-results-[0-9]*','m');
  var whomatch = holdtxt[arownum].match(whoreg)[0].replace(/csi-ac-results-/m,'');
  var thisreg = new RegExp('csi-ac-results-' + whomatch + '[\"\'](.|\n)*end-search-results-' + whomatch + '[\"\']','mg'); // remove old autocomplete results if any 
  fholdtxt[arownum] = holdtxt[arownum].replace(thisreg, 'end-search-results');
  fholdtxt[arownum] = fholdtxt[arownum].replace(/<[^>]*>/mg,"");
  fopened[arownum] = true;
  dobject("cycl" + arownum).innerHTML = "<textarea id='openfree" + arownum + "' style='width:70%'>" + fholdtxt[arownum] + "</textarea><input type='button' value='save' style='font-size:8pt' onclick='savefree(" + arownum + ");'><input type='button' value='cancel' style='font-size:8pt' onclick='restorefree(" + arownum + ");'>";
}

function renumber(s,arownum) {
// but the target might itself have been renumbered...
  for (var m=1; m<=ntextme; m++) {
    if (textme2row[m] == arownum) {
      ntextme++;
      textme2row[ntextme] = rownum; // rownum is the global counter here
      // alert(m + " becomes " + ntextme);
      // assuming we have unique enough numbers we don't need to match end quotes on the regexp target
      s = s.replace("popme" + m, "popme" + ntextme);
      s = s.replace("textme" + m, "textme" + ntextme);
      s = s.replace("popme\(" + m + "\)", "popme(" + ntextme + ")");
      s = s.replace("clearsearchresults\(" + m + "\)", "clearsearchresults(" + ntextme + ")");
      s = s.replace("csi-ac-" + m, "csi-ac-" + ntextme);
      s = s.replace("csi-input-" + m, "csi-input-" + ntextme);
      s = s.replace("csi-ac-results-" + m, "csi-ac-results-" + ntextme);
      s = s.replace("search-results-" + m, "search-results-" + ntextme);
      s = s.replace("searchbutton-" + m, "searchbutton-" + ntextme);
      s = s.replace("end-search-results-" + m, "end-search-results-" + ntextme);
      s = s.replace("-" + m, "csi-input-" + ntextme);
      nautos2declare++;
      autos2declare[nautos2declare] = ntextme;
    }
  }

  // also have to do ac box, ugh
  
  return s;
}

var autos2declare = new Array;
var nautos2declare = 0;
function addrow(arownum) {
  rownum++;
  var cyclinnards = dobject("cycl" + arownum).innerHTML;
  row2mt[rownum] = row2mt[arownum];
  var s = "<img src=\"/cycdoc/js/blank.gif\" height=\"25px\" width=\"1px\"><span class='dynamicline' id=\"spanrow" + rownum + "\" style=\"line-height:150%;width:50\">";
  s += "<font class=\"hoverclass\" id=\"checkadd" + rownum + "\" onClick=\"addrow(" + rownum + ");\" style=\"font-size:10pt\">&nbsp;<img class='hidebutton' id='addsign" + rownum + "' src='/cycdoc/js/addsign.png' height='12' title='clone this row'>&nbsp;</font><font class=\"hoverclass\" onClick=\"freeedit(" + rownum + ");\"><img class='hidebutton' id='textsign" + rownum + "' src='/cycdoc/js/text.png' height='15' title='edit this row as free text'>&nbsp;&nbsp;&nbsp;</font><input id=\"checkbox" + rownum + "\" type=\"checkbox\" onClick=\"setTimeout('checkrowerrstate(" + rownum + ")', 100);\">";
  s += "<span id=\"cycl" + rownum + "\">";  
  nautos2declare = 0;
  s += renumber(cyclinnards,arownum);
  s += "</span></span>";
  var os = document.createElement('div');
  os.innerHTML = s;
  os.id = 'divrow' + rownum;
  os.style.width = '90%';
  dobject('divrow' + arownum).parentNode.insertBefore(os,dobject('divrow' + arownum));
  //now declare autocompletes
  for (var nai=1; nai<=nautos2declare; nai++) {
    declareoneauto(autos2declare[nai]);
  }
}
