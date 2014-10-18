/*

	INLINE EDIT ASSERTION CODE (POPUP WHEAT) JS FUNCTIONS for CommercialOpenCyc
	Ronald Loui for Cycorp (c) 2011
	see also Dave Schneider
	Modified work @ 2014 Andrew Smart

	June 2011

	This js function takes an assertion id, gets xml, then permits editing in a popup

to do:  finish changing global var names and fn names to coexist in larger NS
to do:  figureout how to cancel prior ac requests at the server ; we did this in textarea with explicit sentQuery and timeout; it seems there may be a way to return false out of GenerateRequest if something else intervenes...

NOTE:  every time the pop successfully handles getting assertion xml from an assertion id, it rewrites the body's onKeyUp event, so if
someone else has defined js for this event on the body, i'll overwrite:

to do:  add freeedits to undo/redo stack DONE Tue Mar 27 16:14:06 CDT 2012

  document.body.setAttribute('onKeyUp','checkbodykey(event);'); // could be attached to something smaller, like the innerchange


*/

/*
document.writeln('<p>mhello<p>');
document.writeln('<a href=http://achernar:3662/cgi-bin/cyccgi/cg?xml-assertion-info&assertionId=6189511>CLICK ON ME FOR XML</a>');
document.writeln("<span id=\"target1\" onClick=\"genchange('target1', '6199511', false);\">ball</span>");
document.writeln('<p>adieu<p>');
*/

/*
document.writeln('<script>window.open("/cycdoc/js/test.html","_self")</script>');
*/

/*

	90% OF THIS CODE INHERITED FROM: cb-create-similar.js
	EDIT ASSERTION JS FUNCTIONS for EnterpriseCyc
	Ronald Loui for Cycorp (c) 2011
	see also Dave Schneider

	June 2011

	This js function takes an assertion id, gets xml, then permits editing in a popup

to do:

	to-do:  where is the cursor on popme boxes? 
	to-do:  suppress same-search when executed twice in a row -- but when?

	to-do:  add code to append &oldAssertionId=903282 to cb-handle-edit-assertion
	to-do:  make global variables distinct for shared NS

*/

//Function loads script dynamically, and calls callback when done:
function loadScript(url, callback) {
	var script = document.createElement('script');
	script.type = 'text/javascript';
	script.src = url;
	// Then bind the event to the callback function.
	// There are several events for cross browser compatibility.
	script.onreadystatechange = callback;
	script.onload = callback;

	// Fire the loading
	document.getElementsByTagName('head')[0].appendChild(script);
}

//Load YUI3, has better autocomplete than OpenCyc's included YUI2.
var yui3LoadScriptFinishedHandler = function() {
	//Adds autocomplete feature to assert sentence box.
	YUI().use('node', 'event-base', 'autocomplete', 'autocomplete-highlighters', 'datasource-get', 'datasource-io', 'datasource-xmlschema', function (Y) {
		// Extends Y.Plugin.Autocomplete plugin class to override protected method _updateValue.
		// Logic of extending plugin inferred from Y.Plugin.AutoComplete inheriting from ACListPlugin: https://github.com/yui/yui3/blob/dev-master/src/autocomplete/js/autocomplete-plugin.js
		function CycAutoCompletePlugin(config) {
			CycAutoCompletePlugin.superclass.constructor.apply(this, arguments);
		}
		// Extend Y.Plugin.AutoComplete, and override the default method _updateValue
		Y.extend(CycAutoCompletePlugin, Y.Plugin.AutoComplete, {
			_updateValue : function(newVal) {
                var VALUE = 'value',
	                QUERY = 'query';
				var query = this.get(QUERY),
					value = this.get(VALUE);
				var re = new RegExp('([^$]|^)(' + query + ')');
				this.set(VALUE, value.replace(re, '$1' + newVal));
			}
		}, {
			NAME : 'cycAutoCompletePlugin',
			NS : 'ac',
			CSS_PREFIX : Y.ClassNameManager.getClassName('aclist')
		});

		var handleSentenceAfterDOMReady = function() {
			var assertBox = Y.one('#sentence');
			if(assertBox) { //Note cb-inline-edit.js is included from more than just the assert tool.
				//Need to apply YUI3 skinning in order for autocomplete popup to be skinned,
				// Will be skinned via a div so that nothing else (e.g. YUI2 stuff) will be skinned with YUI3 default skin.
				var assertBoxParent = assertBox.ancestor();
				if(assertBoxParent) {
					assertBoxParent.removeChild(assertBox);
					var div = Y.Node.create('<div/>').addClass('yui3-skin-sam').appendTo(assertBoxParent);
					div.appendChild(assertBox);
				}

				// Create a DataSource instance.
				var ds = new Y.DataSource.IO({
					source: './cg?xml-complete&filter=c292&prefix='
				});

				// Parse xml using XML schema
				ds.plug(Y.Plugin.DataSourceXMLSchema, {
					schema: {
						resultListLocator: "Term",
						resultFields: [
							{key:"cycl", locator:"@*[local-name()='cycl']"},
							{key:"nl", locator:"@*[local-name()='nl']"}
						]
					}
				});

				assertBox.plug(CycAutoCompletePlugin, { //Y.Plugin.AutoComplete, {
					allowBrowserAutocomplete: true,
					maxResults: 20,
					queryDelay: 50,
					queryDelimiter: /[()\s]/,
					resultHighlighter: 'phraseMatch',
					source: ds, // Use the DataSource instance as the result source.
					resultTextLocator: 'cycl',
					activateFirstItem: true,
					align: {
						node: '#sentence',
						points: ['tl', 'tr']
					},
					tabSelect: true
				});
			}
		};
		//Wait till DOM is loaded:
        Y.on('domready', handleSentenceAfterDOMReady);
	});
};

loadScript("http://yui.yahooapis.com/3.17.2/build/yui/yui-min.js", yui3LoadScriptFinishedHandler);

var Dom = YAHOO.util.Dom;

var obuffer = ''; // for buffered output -- we need lookahead on the complex mt expressions to check for equivalence to prior mt

function idw(x) { document.head.innerHTML += x;  }
function bdw(x) { // anything unbalanced assigned to innerHTML will be balanced, so buffer the partials
  obuffer = obuffer + x; 
}
function accbdw(x) { // anything unbalanced assigned to innerHTML will be balanced, so buffer the partials
  if (workingonmtflag) newmt += x; // accumulate the entire markedup active text in order to have internal function
  obuffer = obuffer + x; 
}
function flushobuffer(flushwhere) { dobject(flushwhere).innerHTML += obuffer; obuffer = ''; }

var extraspacefornumbersflag = true;	// pads numbers with an extra space for clickability
var nextspaceflag = true;	// controls prepending of spaces before terms -- preds begin sentences, but fn names are not distinguishable from terms

var textme2row = new Array();  // associates rownum with a textmenum
var textisamt = new Array();  // booleans for which textme's are mt's
var mtnum = 0;	// counts mts, starting at 1
var rownum = 0;	// counts cyls, most of which are boxes (some are mts), starting at 1 // careful, NOT ALL rows have boxes (Mt rows don't)
var ntextme = 0;	// counts popme/linkme textnum, starting at 1
var assertionXML = '';
var cyclwidth; // width of inline popped box(es)

var handleAssertionSuccess = function(o) {
  assertionXML = o.responseText;
  var targetElt = dobject(inlineEditChangePoint);
  var newNode = document.createElement("span");
  newNode.setAttribute("id", "deleteMe");
  newNode.setAttribute("class", "editContainer");
  inlineEditChangePoint = "deleteMe";
  targetElt.parentNode.insertBefore(newNode, targetElt.nextSibling);

  newNode.innerHTML = '<span id=\"innerchange\" onClick=\"clearierr();\" onKeyDown=\"clearierr();\" style=\"font-size:10pt\"></span>';
  dobject('innerchange').innerHTML += "<input value=\"Cancel\" type=\"button\" onClick=\"setTimeout('cancel(true)',100);\" title='close this window'>";
  dobject('innerchange').innerHTML += "<input value=\"Undo\" type=\"button\" onClick=\"uncommit();\" title='undo last normal change'>";
  dobject('innerchange').innerHTML += "<input value=\"Redo\" type=\"button\" onClick=\"recommit();\" title='redo last normal change'>";
  //  dobject('innerchange').innerHTML += "<input value=\"Escape\" title=\"Discard changes to currently open input box\" type=\"button\" onClick=\"doescape();\">";
  dobject('innerchange').innerHTML += "<input value=\"FreeText Mt\" type=\"button\" onClick=\"freeedit_mt();\" title='edit Mt in textarea with no constraints'>";
  dobject('innerchange').innerHTML += "<input value=\"FreeText CycL\" type=\"button\" onClick=\"freeedit();\" title='edit CycL in textarea with no constraints'>";
  dobject('innerchange').innerHTML += "<input id=\"innerchangesubmitbutton\" value=\"" + getInlineSubmitLabel() + "\" type=\"button\" title=\"" + getInlineSubmitLabel() + " selected sentences\" onClick=\"dosubmit();\" style=\"background-color:#ccccee\">";
  dobject('innerchange').innerHTML = "<div id=\"button-row\" class=\"button-row\">" + dobject('innerchange').innerHTML + "</div>";
  document.body.setAttribute('onKeyUp','checkbodykey(event);'); // could be attached to something smaller, like the innerchange

  parse('<assertions>' + assertionXML + '</assertions>', 'innerchange');

  inlineEditOpenMasterFlag = true;
  for (var i=1; i<=ntextme; i++) if (dobject('csi-ac-' + i) != undefined) { setTimeout("declareauton(" + i + ")",300); }
  dobject('innerchange').innerHTML += '<br>';
  dobject('innerchange').style.backgroundColor = 'wheat';
  dobject('innerchange').style.width = '700px';

}
var handleAssertionFailure = function(o) {
    responsetext = 'Transaction id: ' + o.tId;
    responsetext += 'HTTP status: ' + o.status;
    responsetext += 'Status code message: ' + o.statusText;
    responsetext += '<li>HTTP headers: <ul>' + o.getAllResponseHeaders + '</ul></li>';
    responsetext += '<font color=red>' + o.responseText + '</font>';
    // alert(responsetext);
}

function striptp(x) { // could actually parse it, but this will suffice for wff cycl without strings
  x = x.replace(/^\#\$/g, ''); 
  x = x.replace(/ \#\$/g, ' '); 
  x = x.replace(/\(\#\$/g, '('); 
  return(x); 
}

function dobject(x) { return(document.getElementById(x)); }

function removeoldhiddenresults(sn, shtml) { // removes old ac or search results prior to extracting cycl from html
// better to try using clearList API on ac -- but what for search res? clear those manually...  
// or, copy the object, use dom syntax to null the results boxes in the copy, and then get writecycl -- so much better...
// for now, just copy the innerHTML as a string, then null the results using a regexp replace
  for (var m=1; m<=ntextme; m++) {
    if (textme2row[m] == sn) {
      var thisreg = new RegExp('csi-ac-results-' + m + '[\"\'](.|\n)*end-search-results-' + m + '[\"\']'); // remove old autocomplete results if any // do this for each m in n  // quotes are there to prevent -1 from matching -11, -12, -13, etc.
      shtml = shtml.replace(thisreg, 'end-search-results');
    }
  }
  return shtml;
}

function removeallhiddenresults(shtml) { // removes old ac or search results prior to extracting cycl from html
// better to try using clearList API on ac -- but what for search res? clear those manually...  
// or, copy the object, use dom syntax to null the results boxes in the copy, and then get writecycl -- so much better...
// for now, just copy the innerHTML as a string, then null the results using a regexp replace
  for (var m=1; m<=ntextme; m++) {
    var thisreg = new RegExp('csi-ac-results-' + m + '[\"\'](.|\n)*end-search-results-' + m + '[\"\']'); // remove old autocomplete results if any // do this for each m in n  // quotes are there to prevent -1 from matching -11, -12, -13, etc.
    shtml = shtml.replace(thisreg, 'end-search-results');
  }
  return shtml;
}

function getCyclElt(sn) {
  return dobject("cycl" + sn);
}

function writecycl(sn) { // returns the cycl of the sentence 'cycl'+sn
  var shtml = dobject('cycl' + sn).innerHTML;
  //if (shtml.match(/<textarea.*freeedittextarea/)) { shtml = dobject('freeedittextarea').value; }  // must use value, not innerHTML to let FF capture keyboard inputs
  if (shtml.match(/<span.*postcommitspan_mt/)) { shtml = dobject('postcommitspan_mt').innerHTML; } 
  else if (shtml.match(/<span.*postcommitspan/)) { shtml = dobject('postcommitspan').innerHTML; }
  shtml = removeoldhiddenresults(sn, shtml);
  shtml = shtml.replace(/<[^>]*>/g, ''); // dehtml
  shtml = encodeURI(shtml);
  shtml = shtml.replace(/&nbsp;/g, '%20');
  return(shtml);
}

function writecycl2(sname) { // returns the cycl of the sentence sname
  var shtml = dobject(sname).innerHTML;
  //if (shtml.match(/<textarea.*freeedittextarea/)) { shtml = dobject('freeedittextarea').value; }  // must use value, not innerHTML to let FF capture keyboard inputs
  if (shtml.match(/<span.*postcommitspan_mt/)) { shtml = dobject('postcommitspan_mt').innerHTML; }
  else if (shtml.match(/<span.*postcommitspan/)) { shtml = dobject('postcommitspan').innerHTML; }
  shtml = removeoldhiddenresults(2, shtml); // cycl2 is the name of the editable sentence in this inline edit version
  shtml = shtml.replace(/<[^>]*>/g, ''); // dehtml
  shtml = shtml.replace(/^Mt : /,''); // remove superfluous Mt : at front if it is there
  shtml = shtml.replace(/^Mt :&nbsp;/,''); // remove superfluous Mt : at front if it is there
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
  Dom.removeClass(dobject('csi-input-' + n), "invisible");
  Dom.removeClass(dobject('csi-ac-' + n), "invisible");
  Dom.removeClass(dobject('searchbutton-' + n), "invisible");
  Dom.removeClass(dobject('csi-ac-results-' + n), "invisible");
  dobject('csi-input-' + n).size = maxmin2size(oldtext.length);
  dobject('csi-input-' + n).value = oldtext;
  //showprops('csi-input-' + n); // works in IE only
  dobject('csi-input-' + n).focus();
  // looks like this: <font class="popmelinkme_class" id="popme12"><font class="textme_class" id="textme12" style="left:0" onClick="popme(12);">Mandarin</font>
  // dobject('textme' + n).setAttribute('onClick',''); // disable the onClick until restored
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
var savedpoint = 0;
var highestsavedpoint = -1;
var lastdir = 'neither';

var cyclcontext;
var npops=0;

//POP CODE

function popme(n) { // handles a click on a term, for openinput candidates; also 'falsely' entered when a/c result is clicked, on search button, or search result
  npops++;
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
  popmeopenflag = true;
  popme2openflag = true;
  popme2stringflag = false;
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
    clearpopmeflags();
  }
}

function clearpopmeflags() { // permits a new inputset to be opened
  popmeopenflag = false;
  popme2openflag = false;
  popme2stringflag = false;
}

function checkbodykey(e) { // handles a keypress in the body (can be called redundantly!)
  var evt = e || window.event;
  if (evt.keyCode == 27) {
    if (popmeopenflag) {
      textrestore(lastpoppedn);
      dismisspopme(lastpoppedn);
      clearpopmeflags();
    } else if (freeeditflag) {
      freeeditcancel();
    } else if (freeeditflag_mt) {
      freeeditcancel_mt();
    } else cancel(false);
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
  setTimeout('clearpopmeflags()',100);
}

function textrestore(n) { // restores text to earlier state, primarily when escaping out of input sets
  var restoretext;
  restoretext = lastoldtext;
  if (dobject("textme" + n)) {
    dobject("textme" + n).innerHTML = restoretext;
  }
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
  alert(x);
}

function textcommit(n, newtext, fromwhere) { // commits new text from a popped open input box
  if (popme2openflag && popme2stringflag) {
    // deal with textarea
    newtext = newtext.replace(/%0A%0D/, ' '); // remove newline // doesn't work if the browser is xlating textarea LF CR
    newtext = decodeURI(newtext); // my browser encodes URI when textarea is committed
    newtext = newtext.replace(/"/g, '\\"');
    newtext = newtext.replace(/</g, '\&lt;');
    dobject('textme' + n).innerHTML = '"' + newtext + '"'; // supply double quotes
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
  } else {
    // a normal commit
    if (newtext!= '') dobject('textme' + n).innerHTML = newtext;
  }
  if (newnumber != null || newtext != '' || popme2stringflag) { // allow strings to be null
    // showeachchar(dobject('textme' + n).innerHTML); // temporary
    if (newtext != lastoldtext) {
      // save the old in the odds and the new in the evens
      // pointer is sitting on the last new
      var thisrow = textme2row[n] // for auto-check
      ++savedpoint; // save the old
      savedtext[savedpoint] = lastoldtext;
      savedwhere[savedpoint] = n;
      ++savedpoint; // save the new
      savedtext[savedpoint] = newtext;
      savedwhere[savedpoint] = n;
      highestsavedpoint = savedpoint;
      lastdir = 'neither';
    }
  } else { dobject('textme' + n).innerHTML = lastoldtext; }
}

var freeeditflag = false;
var orgcontents;
function freeedit() {
  if (popmeopenflag) {
    textrestore(lastpoppedn);
    dismisspopme(lastpoppedn);
    clearpopmeflags();
  }
  if (freeeditflag_mt) freeeditcancel_mt();
  var alldivs = document.getElementsByTagName('div');
  for (var idiv in alldivs) if (alldivs[idiv] != undefined) if (alldivs[idiv].id != undefined) if (alldivs[idiv].id.match(/^divrow/)) {
    var dcontents = alldivs[idiv].innerHTML
    orgcontents = dcontents;
    dcontents = removeallhiddenresults(dcontents);
//alert(dcontents.match(/<span id=.cycl[0-9]+.>/));
    dcontents = dcontents.replace(/<[^>]*>/gm,'');
    dcontents = dcontents.replace(/(&nbsp;(&nbsp;)+)/gm,'\n$1');
    dcontents = dcontents.replace(/&nbsp;/gm,' ');
    var divrowwidth = '90%';
    alldivs[idiv].innerHTML = '<span id="cycl' + rownum + '"><textarea id="freeedittextarea" cols="80" rows="12">' + dcontents + '</textarea></span>';
    break;
  }
  dobject('button-row').innerHTML = "<input value=\"Cancel\" type=\"button\" onClick=\"freeeditcancel();\" style=\"font-size:8pt\">";
  dobject('button-row').innerHTML += "<input id=\"innerchangesubmitbutton\" value=\"" + "Commit" + "\" type=\"button\" title=\"Commit edits\" onClick=\"docommit();\" style=\"font-size:8pt\"><br>";
  dobject('freeedittextarea').focus();
  freeeditflag = true;
} 

function docommit() {
  newcontents = dobject('freeedittextarea').value;
  var alldivs = document.getElementsByTagName('div');
  for (var idiv in alldivs) if (alldivs[idiv] != undefined) if (alldivs[idiv].id != undefined) if (alldivs[idiv].id.match(/^divrow/)) alldivs[idiv].innerHTML = '<span id="postcommitspan">' + newcontents + '</span>';
  freeeditflag = false;
  buttonrestore();
  if (true) {
    ++savedpoint; // save the old
    savedtext[savedpoint] = orgcontents;
    savedwhere[savedpoint] = 'whole cycl';
    ++savedpoint; // save the new
    savedtext[savedpoint] = newcontents;
    savedwhere[savedpoint] = 'whole cycl';
    highestsavedpoint = savedpoint;
    lastdir = 'neither';
  }
}

function freeeditcancel() {
  var alldivs = document.getElementsByTagName('div');
  for (var idiv in alldivs) if (alldivs[idiv] != undefined) if (alldivs[idiv].id != undefined) if (alldivs[idiv].id.match(/^divrow/)) alldivs[idiv].innerHTML = orgcontents;
  freeeditflag = false;
  buttonrestore();
  dobject('innerchange').focus();
}

function buttonrestore() {
  dobject('button-row').innerHTML = "<input value=\"Cancel\" type=\"button\" onClick=\"setTimeout('cancel(true)',100);\" title='close this window'>";
  dobject('button-row').innerHTML += "<input value=\"Undo\" type=\"button\" onClick=\"uncommit();\" title='undo last normal change'>";
  dobject('button-row').innerHTML += "<input value=\"Redo\" type=\"button\" onClick=\"recommit();\" title='redo last normal change'>";
  //  dobject('button-row').innerHTML += "<input value=\"Escape\" title=\"Discard changes to currently open input box\" type=\"button\" onClick=\"doescape();\">";
  dobject('button-row').innerHTML += "<input value=\"FreeText Mt\" type=\"button\" onClick=\"freeedit_mt();\" title='edit Mt in textarea with no constraints'>";
  dobject('button-row').innerHTML += "<input value=\"FreeText CycL\" type=\"button\" onClick=\"freeedit();\" title='edit CycL in textarea with no constraints'>";
  dobject('button-row').innerHTML += "<input id=\"innerchangesubmitbutton\" value=\"" + getInlineSubmitLabel() + "\" type=\"button\" title=\"" + getInlineSubmitLabel() + " selected sentences\" onClick=\"dosubmit();\" style=\"background-color:#ccccee\">";
}

var freeeditflag_mt = false;
var orgcontents_mt;
function freeedit_mt() {
  if (popmeopenflag) {
    textrestore(lastpoppedn);
    dismisspopme(lastpoppedn);
    clearpopmeflags();
  }
  if (freeeditflag) freeeditcancel();
  var allspans = document.getElementsByTagName('span');
  for (var ispan in allspans) if (allspans[ispan] != undefined) if (allspans[ispan].id != undefined) if (allspans[ispan].id.match(/^mtrow/)) {
    var scontents = allspans[ispan].innerHTML
    orgcontents_mt = scontents;
    scontents = removeallhiddenresults(scontents);
//alert(scontents.match(/<span id=.cycl[0-9]+.>/));
    scontents = scontents.replace(/<[^>]*>/gm,'');
    scontents = scontents.replace(/(&nbsp;(&nbsp;)+)/gm,'\n$1');
    scontents = scontents.replace(/&nbsp;/gm,' ');
    scontents = scontents.replace(/Mt : /,'');
    var spanrowwidth = allspans[ispan].style.width;
    allspans[ispan].innerHTML = '<span id="cycl' + rownum + '"><span id="prefree">Mt : </span><textarea id="freeedittextarea" cols="80">' + scontents + '</textarea></span>';
    break;
  }
  dobject('button-row').innerHTML = "<input value=\"Cancel\" type=\"button\" onClick=\"freeeditcancel_mt();\" style=\"font-size:8pt\">";
  dobject('button-row').innerHTML += "<input id=\"innerchangesubmitbutton\" value=\"" + "Commit" + "\" type=\"button\" title=\"Commit edits\" onClick=\"docommit_mt();\" style=\"font-size:8pt\"><br>";
  dobject('freeedittextarea').focus();
  freeeditflag_mt = true;
} 


function docommit_mt() {
  newcontents = dobject('freeedittextarea').value;
  if (!newcontents.match(/^ *Mt :/)) newcontents = 'Mt : ' + newcontents;
  dobject('prefree').innerHTML = '';
  var allspans = document.getElementsByTagName('span');
  for (var ispan in allspans) if (allspans[ispan] != undefined) if (allspans[ispan].id != undefined) if (allspans[ispan].id.match(/^mtrow/)) allspans[ispan].innerHTML = '<span id="postcommitspan_mt">' + newcontents + '</span>';
  freeeditflag_mt = false;
  buttonrestore();
  if (true) {
    ++savedpoint; // save the old
    savedtext[savedpoint] = orgcontents_mt;
    savedwhere[savedpoint] = 'whole mt';
    ++savedpoint; // save the new
    savedtext[savedpoint] = newcontents;
    savedwhere[savedpoint] = 'whole mt';
    highestsavedpoint = savedpoint;
    lastdir = 'neither';
  }
}

function freeeditcancel_mt() {
  var allspans = document.getElementsByTagName('span');
  for (var ispan in allspans) if (allspans[ispan] != undefined) if (allspans[ispan].id != undefined) if (allspans[ispan].id.match(/^mtrow/)) allspans[ispan].innerHTML = orgcontents_mt;
  freeeditflag_mt = false;
  buttonrestore();
  dobject('innerchange').focus();
}

function uncommit() { // handles a click on the UNDO button
var mysavedwhere;
  // pointer is sitting on the last new; to undo, drop one, print, then drop again
  savedpoint--;
  if (savedpoint >= 1) {
    mysavedwhere = savedwhere[savedpoint]; mysavedtext = savedtext[savedpoint];

    if (mysavedwhere == 'whole cycl') {
      var alldivs = document.getElementsByTagName('div');
      for (var idiv in alldivs) if (alldivs[idiv] != undefined) if (alldivs[idiv].id != undefined) if (alldivs[idiv].id.match(/^divrow/)) alldivs[idiv].innerHTML = '<span id="postcommitspan">' + mysavedtext + '</span>';

    } else if (mysavedwhere == 'whole mt') {
      var allspans = document.getElementsByTagName('span');
      for (var ispan in allspans) if (allspans[ispan] != undefined) if (allspans[ispan].id != undefined) if (allspans[ispan].id.match(/^mtrow/)) allspans[ispan].innerHTML = '<span id="postcommitspan_mt">' + mysavedtext + '</span>';

    } else if (dobject('textme' + mysavedwhere).innerHTML != undefined) dobject('textme' + mysavedwhere).innerHTML = mysavedtext;

  }
  savedpoint--;
  if (savedpoint < 1) savedpoint = 0; // sit below first new
  if (popmeopenflag) dismisspopme(lastpoppedn);
  if (popmeopenflag) textrestore(lastpoppedn);
  clearpopmeflags();
  lastdir = 'undo';
}

function recommit() { // handles a click on the REDO button
var mysavedtext;
var mysavedwhere;
  // pointer is sitting on the last new; to redo, go up two, print
  ++savedpoint;
  ++savedpoint;
  if (savedpoint >= highestsavedpoint) savedpoint = highestsavedpoint; // highest is always a new
  mysavedwhere = savedwhere[savedpoint]; mysavedtext = savedtext[savedpoint];

  if (mysavedwhere == 'whole cycl') {
    var alldivs = document.getElementsByTagName('div');
    for (var idiv in alldivs) if (alldivs[idiv] != undefined) if (alldivs[idiv].id != undefined) if (alldivs[idiv].id.match(/^divrow/)) alldivs[idiv].innerHTML = '<span id="postcommitspan_mt">' + mysavedtext + '</span>';

  } else if (mysavedwhere == 'whole mt') {
    var allspans = document.getElementsByTagName('span');
    for (var ispan in allspans) if (allspans[ispan] != undefined) if (allspans[ispan].id != undefined) if (allspans[ispan].id.match(/^mtrow/)) allspans[ispan].innerHTML = '<span id="postcommitspan_mt">' + mysavedtext + '</span>';

  } else if (dobject('textme' + mysavedwhere) && dobject('textme' + mysavedwhere).innerHTML != undefined) dobject('textme' + mysavedwhere).innerHTML = mysavedtext;

  if (popmeopenflag) dismisspopme(lastpoppedn);
  if (popmeopenflag) textrestore(lastpoppedn);
  clearpopmeflags();
  lastdir = 'redo';
}

var handleSubmitFailure = function(o) { }
var handleSubmitSuccess = function(o) {
    var responsetext = '';
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

    // check for invalid term
    roxt = o.responseXML.getElementsByTagName('invalidTerm');
    if (roxt[0] != undefined)
      if (roxt[0] != undefined) {
        var roxerr = roxt[0].textContent || roxt[0].innerText;
        alert('old invalid term err ' + roxerr);
      }

    // pull out invalid sentences
    rox = o.responseXML.getElementsByTagName('invalidSentence');
    if (roxt.length != 0) errterm = true; else errterm = false;
    if (!errterm) {
      for (var i=0; i<rox.length; i++) {
        thiserr = rox[i];
        if (thiserr == undefined) continue; // sometimes there are 3 errs, but length is 5, go figure
	thisrownum = thiserr.getAttribute('id');
	if (thisrownum != undefined) {
	  // we have a sentence number
	  if (dobject('spanrow' + thisrownum) != undefined) dobject('spanrow' + thisrownum).className = 'err_class';
	  errcount++;
	}
      }
    }
    // summarize errs and non-errs
errterm = false;
    if (errterm || errcount) {
      if (dobject('innerchangeerrmsg') != undefined) dobject('innerchangeerrmsg').innerHTML = 'There is an error; assertion not added';
      else dobject('innerchange').innerHTML += '<font id=\"innerchangeerrmsg\" color=\"red\">There is an error; assertion not added</font>';
    } else {
      //      o.argument.removeOriginal = true;
      cancel(true, o.argument);
      
    }

} 


function restorefromworking(rownum) {
  document.body.style.backgroundColor = '#ffffff';
  dobject('innerchangesubmitbutton').value = getInlineSubmitLabel();
}

function getInlineSubmitLabel () {
  return (inlineEditRemoveBoolean) ? "Edit" : "Assert";
}

function notifyworking(rownum) {
  document.body.style.backgroundColor = '#cccccc';
  dobject('innerchangesubmitbutton').value = '...SUBMITTING...';
}

function dosubmit() { // handles final assertion
  if (popmeopenflag) {
    if (popme2openflag) commitopenstring();
    else {
      var newvalue = dobject('csi-input-' + lastpoppedn).value;
      dismisspopme(lastpoppedn);
      if (acceptableresults[newvalue]) textcommit(lastpoppedn, newvalue, 'do-submit');
      else textrestore(lastpoppedn);
      // alert('please close open input box');
      // return; // don't return if a inputset is open
    }
  }
  var submitdata = 'cb-handle-edit-assertion';
  var thismtcycl = writecycl2('mtrow');
  var alldivs = document.getElementsByTagName('div');
  for (var idiv in alldivs) if (alldivs[idiv] != undefined) if (alldivs[idiv].id != undefined) if (alldivs[idiv].id.match(/^divrow/)) var divrow = alldivs[idiv].id;
  var thisrowcycl = writecycl2(divrow);
  var thissentence;
  var cyclElt = getCyclElt(rownum);
  // rownum is the last allocated row, which is who we want to send
  thisrowcycl = thisrowcycl.replace(/&/g, '%26').replace(/;/g, '%3B'); // have to change &lt; to %26lt%3B etc., and &quot; goes to %26quot%3B under quotes // BECAUSE URI encoding in writecycl didn't encode the &, at least not under "... can you believe it!?

  thissentence = '%28ist%20' + thismtcycl + '%20' + thisrowcycl + '%29';
  // thissentence = '%28ist%20' + thismtcycl + '%20%28' + thisrowcycl + '%29'; // poison sentence for debugging:  added extra %28 to test errs 

  submitdata += '&sentence' + i + '=' + thissentence;
  if (inlineEditRemoveBoolean) submitdata += '&oldAssertionId=' + inlineEditAssertionId;

  var containerElt = YAHOO.util.Dom.getAncestorByClassName(cyclElt, "editContainer");
  notifyworking(rownum);
  var submitcallback = { success:handleSubmitSuccess, failure: handleSubmitFailure };
  submitcallback.argument = 
    {sentence: decodeURI(thissentence).replace(/&/g, '<hr>&'),
     sentencenum: i,
     oldAssertionId:inlineEditAssertionId,
     containerElt: containerElt,     
     removeOriginal: inlineEditRemoveBoolean};
  //  submitcallback.argument = decodeURI(submitdata).replace(/&/g, '<hr>&');

  // check inlineEditRemoveBoolean possibly to remove old assertion
  var submitXHR = YAHOO.util.Connect.asyncRequest('POST', 'cg', submitcallback, submitdata);

  restorefromworking(rownum);
}

function checkanddosearch() { if (popmeopenflag) dosearch(); } // give the flags a chance to clear on a CR select from AC

function dosearch() {
  var n = lastpoppedn;
  var ntext = dobject('csi-input-' + n).value;
  ntext = striptp(ntext);
  var searchButton = dobject('searchbutton-' + n);
  var left = searchButton.offsetLeft;
  var searchResultsElt = dobject('search-results-' + n)
  Dom.removeClass(searchResultsElt, "invisible");
  searchResultsElt.innerHTML = '<i>&nbsp;&nbsp;searching ...</i>';
  searchResultsElt.style.left = searchButton.offsetLeft;
  makeSearchRequest(ntext, n);
}

function doselect(chosenvalue) { // handles select within the AC menu
  if (chosenvalue in acceptableresults) {
    dismisspopme(lastpoppedn);
    textcommit(lastpoppedn, chosenvalue, 'doselect');
    chosenvalue = '';
    clearpopmeflags();
  } else alert('trying to select unacceptable term');
}

var chosenvalue = '';

function dochoose(m) { // handles onClick within a search results list
  chosenvalue = striptp(searchresterms[m]);
  textcommit(lastpoppedn, chosenvalue, 'dochoose');
  chosenvalue = '';
  dismisspopme(lastpoppedn);
  clearpopmeflags();
}

function dismisspopme(n) { // hides a complete set of input box, AC results span, and search results span
  if (dobject('csi-ac-' + n) == undefined) return;
  dobject('csi-input-' + n).value = '';
  Dom.addClass(dobject('csi-input-' + n), "invisible");
  Dom.addClass(dobject('searchbutton-' + n), "invisible");
  Dom.addClass(dobject('csi-ac-results-' + n),"invisible");
  Dom.addClass(dobject('search-results-' + n), "invisible");
  Dom.addClass(dobject('csi-ac-' + n), "invisible");
  // if (dobject('textme' + n).getAttribute('onClick') == '') dobject('textme' + n).setAttribute('onClick','popme(' + n + ')'); // disable the onClick until restored
}

function clearsearchresults(n) { // used to dismiss on blur of input, since Yahoo is hiding a/c
  Dom.addClass(dobject('search-results-' + n), "invisible");
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
    Dom.removeClass(dobject('search-results-' + lastpoppedn), "invisible");
    if (searchresn == 0) {
      dobject('search-results-' + lastpoppedn).innerHTML = '<i> no results found.</i>';
      return;
    }
    for (var i=0; i<searchresn; i++) {;
      searchresguids[i] = searchRespChildren[i].getAttribute('hlId');
      searchresnls[i] = searchRespChildren[i].getAttribute('nl');
      searchresterms[i] = searchRespChildren[i].getAttribute('cycl');
      searchlist = searchlist + '<font class="searchresultline_class" onClick="dochoose(' + i + ')">' + searchresnls[i];
      searchlist = searchlist + '&nbsp;<span class="autoCompCycL">' + searchresterms[i] + '</span></font><br>';
      acceptableresults[striptp(searchresterms[i])] = 1;
    }
    if (searchresn == 1) { // go ahead and commit the single returned value
      dobject('search-results-' + lastpoppedn).innerHTML = '&nbsp;&nbsp;unique result found.';
      dochoose(0); // numbering starts at 0
      clearpopmeflags();
      return;
    }
    dobject('search-results-' + lastpoppedn).innerHTML = searchlist;
  }
}
var handleSearchFailure = function(o) { }
var searchcallback = { success:handleSearchSuccess, failure:handleSearchFailure };

function makeSearchRequest(mystring, n) { // initiates a search request with context
  var sUrl;
  var rownum = textme2row[n];
  var thismtnum = rownum - 1;
  var thismtcycl = writecycl(thismtnum);
  dobject('textme' + n).innerHTML = ':term-to-replace'; // easier to change the dom here than change it to blank
  var thisrowcycl = writecyclcontext(n);
  dobject('textme' + n).innerHTML = '';
  thisrowcycl = thisrowcycl.replace(/&/g, '%26').replace(/;/g, '%3B'); // have to change &lt; to %26lt%3B etc., and &quot; goes to %26quot%3B under quotes // BECAUSE URI encoding in writecycl didn't encode the &, at least not under "... can you believe it!?
  if (rownum == 1) cyclcontenxt = thisrowcycl;
  else cyclcontext = '%28ist%20' + thismtcycl + '%20' + thisrowcycl + '%29';

  mystring = striptp(mystring);
  if (cyclcontext == '') sUrl = "cg?xml-term-search&searchString=" + mystring + "&caseSensitive=nil";
  else sUrl= "cg?xml-term-search&searchString=" + mystring + "&constrainingSentence=" + cyclcontext + "&caseSensitive=nil";
  var request = YAHOO.util.Connect.asyncRequest('GET', sUrl, searchcallback);
}

var constantCompleteDataSources = new Array();
var constantCompletes = new Array();
var selectHandlers = new Array();

//function declareauto0() {
//// set up the focal term's input box and AC code
//  var constantCompleteDataSource = new YAHOO.widget.DS_XHR('./cg', ['Term', 'cycl', 'nl']); 
//  //  var constantCompleteDataSource = YAHOO.util.XHRDataSource('./cg');//, ['Term', 'cycl', 'nl']);
//    constantCompleteDataSource.scriptQueryParam = 'xml-complete&filter=c297&prefix';
//    //   constantCompleteDataSource.responseSchema= { fields:['Term', 'cycl', 'nl']};
//    // add constraining-stentence=...
//    constantCompleteDataSource.responseType = YAHOO.widget.DS_XHR.TYPE_XML;
//    var constantComplete = new YAHOO.widget.AutoComplete('input-0', 'autocomplete-results-0', constantCompleteDataSource);
//    constantComplete.allowBrowserAutocomplete = false;
//    constantComplete.autoHighlight = false;
//    constantComplete.minQueryLength = 3;
//    constantComplete.animSpeed = 0.1;
//    constantComplete.maxResultsDisplayed = 20;
//    constantComplete.queryDelay = 0.3;
//    constantComplete.embeddedInToolbar = false;
//    constantComplete.formatResult = function(oResultItem, sQuery) {
//      var thisConst = oResultItem[0];
//      var string = oResultItem[1];
//      var sMarkup = string + ' <span class=\"autoCompCycL\">' + thisConst + '</span>';
//      return (sMarkup);
//    };
//}

function declareauton(n) {
  if (dobject('csi-input-' + n) == undefined) return;
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
    var sMarkup = string + ' <span class="autoCompCycL">' + thisConst + '</span>';
    acceptableresults[striptp(thisConst)] = 1; // note that we pick up a list of acceptable results every time we AC
    return (sMarkup);
  };
  selectHandlers[n] = function(sType, aArgs) {
    var chosenresult = aArgs[2];
    var chosenvalue = striptp(chosenresult[0]);
    doselect(chosenvalue);
    return true;
  };
  constantCompletes[n].itemSelectEvent.subscribe(selectHandlers[n]);
}

var ntags = 0;	// used to quit on ultra long pages
var valstack = new Array();	// stores non tag data associated with each taglevel
var tagstack = new Array();	// stores a stack of tags (lower case, just the tagname)
var taglevel = -1;
var lastguid;
var lastmt;
var workingonmtflag = false;	// used to signal append fragments to newmt
var newmt = ''; // used to accumulate fragements in an mt section
var newmttextmeset = new Array(); // array of integers that are the textme's seen in this newmt
var linktable = new Array();
var thislinetermcharcount = 0;
var startwraptermcharcount = 30; // 30 for the first line, then shift to 60

function parse(remain, flushwhere) {
  ntags = 0;
  seenstart = false;
  taglevel = 1;
  thislinetermcharcount = 0;
  startwraptermcharcount = 30; // 30 for the first line, then shift to 60
  newmt = '';
  for (var i in tagstack) delete tagstack[i];
  for (var i in valstack) delete valstack[i];
  for (var i in newmttextmeset) delete newmttextmeset[i];

  var matchtuple = remain.match("<[^>]*>"); // the match of each tag
  // this is the basic parser which does not know about < or > under quotes // but xml traversal using .children is very slow! -- we tried it!

  if (matchtuple != null) while (matchtuple[0] != "") {

    // note that the match has to be the most recent use of regexp, including any string replaces
    var remain = RegExp.rightContext;
    var skippedtext = RegExp.leftContext;
    if (valstack[taglevel] == "") { valstack[taglevel] = skippedtext; }
    else { valstack[taglevel] = valstack[taglevel] + " " + skippedtext; }
    var tagcontents = matchtuple[0].slice(1, matchtuple[0].length-1);

    // manage the stack
    if (tagcontents.slice(0,10).toLowerCase() == "assertions") seenstart = true;
    if (seenstart) {
      if (tagcontents.toLowerCase() == "/assertions") break;
      if (tagcontents.slice(0, 1) == "/") {
        if (tagstack[taglevel] == tagcontents.slice(1).toLowerCase()) {
        } else {
          bdw(" ERR!  UNMATCHED XML TAG IN PAGE! ");
          bdw(tagstack[taglevel] + '@' + taglevel + ' vs. ' + tagcontents);
        }
        // if (valstack[taglevel] != "") bdw("....." + valstack[taglevel]);
        managecontentprepop(flushwhere);
        taglevel--;
        valstack[taglevel] = "";
      } else {
        var shortlowertagname = tagcontents.replace(/ .*/, "").toLowerCase(); // remove attributes
        tagstack[++taglevel] = shortlowertagname;
        valstack[taglevel] = "";
        managecontentpostpush(shortlowertagname, tagcontents, flushwhere);
      }
    }
  
    matchtuple = remain.match("<[^>]*>");
    if (matchtuple == null) break;
    ++ntags;
    if (ntags > 10000) break; // a crazy number of tags, I quit
  }
  flushobuffer(flushwhere);

}

function managecontentprepop(flushwhere) {
  // called during a pop before tag is popped
  if (tagstack[taglevel] == "guid") {
    lastguid = valstack[taglevel];
  }
  var thispath0 = tagstack[taglevel] + ">" + tagstack[taglevel-1];
  var thispath1 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2];
  var thispath2 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2] + ">" + tagstack[taglevel-3] ;
  var newname = valstack[taglevel];
  if (thispath1 == "microtheory>assertion>assertions") {
    // write or extend an mt block
    var comparemt = newmt.replace(/<[^>]*>/g, ''); // strip html for comparison because it contains unequal rownums
    if (true) {
      obuffer = '';
      ++mtnum;
      bdw("<font style=\"line-height:50%\">&nbsp;</font><br>");
      bdw("<span id=\"mtrow\" class=\"inline-edit-mt\">Mt :"); 
      ++rownum;
      for (i in newmttextmeset) textme2row[i] = rownum; // fix textme's so they point to this new box // they were set earlier, but the mt now claims its pieces
      for (i in newmttextmeset) textisamt[i] = true; // all textme's in this set are considered mt textme's
      for (i in newmttextmeset) delete newmttextmeset[i];
      bdw("<span id=\"cycl" + rownum + "\">");
      bdw(newmt);
      bdw("</span>"); // end cycl
      bdw("</span>"); // end mt
      flushobuffer(flushwhere);
      lastmt = comparemt;
      workingonmtflag = false;
    }
  }

  if (thispath0 == "name>symbol") {
    // can we check that we just popped off <package>KEWYORD</package>?
    newname = ':' + newname;
  }

  if (tagstack[taglevel] == "name") {
    if (true) {
      newterm();
      accbdw("<font class=\"popmelinkme_class\" id=\"popme" + ntextme + "\"><font class=\"textme_class\" id=\"textme" + ntextme + "\" style=\"left:0\" onClick=\"popme(" + ntextme + ");\">" + newname + "</font>");
      createnewinputset(ntextme);
      accbdw("</font>");
      linktable[newname] = "cg?cb-cf&" + lastguid;
      thislinetermcharcount += newname.length;
    }
    nextspaceflag = true;
  }

  if (tagstack[taglevel] == "string") {
    newterm();
    // text comes to us with unprotected quotes
    newname = newname.replace(/"/g, '\\"');
    accbdw("<font class=\"popme2linkme_class\" id=\"popme2_" + ntextme + "\"><font class=\"textme_class\" id=\"textme" + ntextme + "\" style=\"left:0\" onClick=\"popme2string(" + ntextme + ");\">" + "\"" + protecttextarea(newname) + "\"" + "</font></font>");
    nextspaceflag = true;
    thislinetermcharcount += newname.length;
  }

  if (tagstack[taglevel] == "number") {
    newterm();
    accbdw("<font class=\"popme2linkme_class\" id=\"popme2_" + ntextme + "\">");
    if (extraspacefornumbersflag == true && newname.length == 1) accbdw("&nbsp;");
    accbdw("<font class=\"textme_class\" id=\"textme" + ntextme + "\" style=\"left:0\" onClick=\"popme2number(" + ntextme + ");\">" + newname + "</font>");
    if (extraspacefornumbersflag == true && newname.length == 1) accbdw("&nbsp;");
    accbdw("</font>");
    nextspaceflag = true;
    thislinetermcharcount += newname.length;
  }

  if (tagstack[taglevel] == "variable") {
    if (nextspaceflag) accbdw("&nbsp;")
    accbdw(newname);
    nextspaceflag = true;
    thislinetermcharcount += newname.length;
  }

  if (tagstack[taglevel] == "function") {
    // end a function // ignore nextspaceflag
    accbdw(")");
    nextspaceflag = true;
  }

  if (thispath0 == "sentence>assertions") {
    // this ends a top level sentence // ignore nextspaceflag
    bdw(")");
    nextspaceflag = true;
    bdw("</span></span></div>");
    flushobuffer(flushwhere);
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
  // nextspaceflag = true;
  textme2row[ntextme] = rownum;
}

function managecontentpostpush(shortlowertagname, tagcontents, flushwhere) {
  // called during a pop before tag is popped
  var thispath0 = tagstack[taglevel] + ">" + tagstack[taglevel-1];
  var thispath1 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2];
  var thispath2 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2] + ">" + tagstack[taglevel-3] ;
  var thispath2gap = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + ">" + tagstack[taglevel-3] ;
  var thispath3 = tagstack[taglevel] + ">" + tagstack[taglevel-1] + ">" + tagstack[taglevel-2] + ">" + tagstack[taglevel-3] + ">" + tagstack[taglevel-4];
  if (thispath0 == "sentence>assertions") {
    // generate new predicate or embedded sentence at the assertion level, hence it gets a new row
    ++rownum;
    bdw("<div id =\"divrow" + rownum + "\" style=\"width:90%\"><span id=\"spanrow" + rownum + "\" style=\"line-height:150%;width:50\">");
    bdw("<span id=\"cycl" + rownum + "\">");  
    nextspaceflag = false;
  } 

  if (thispath2gap == "name>constant>>sentence" && thispath2 != "name>constant>function>sentence" && tagstack[taglevel-2] != "sentence") {
    // this starts an embedded sentence; the gap is a logical operator such as implies, and, or
    if (nextspaceflag) bdw("&nbsp;")
    accbdw("(");
    nextspaceflag = false;
  }

  if (shortlowertagname == "microtheory") {
    flushobuffer(flushwhere);
    workingonmtflag = true;
    newmt = '';
  }

  if (shortlowertagname == "function") {
    // generate new function
    if (thislinetermcharcount > startwraptermcharcount) {
      accbdw("<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;("); // might want to count indent depth, but right now this is just for visual relief
      thislinetermcharcount = 0;
      startwraptermcharcount = 60; // 30 for the first line, then shift to 60
    } else accbdw("&nbsp;(");
    nextspaceflag = false;
  }
}

function createnewinputset(n) {
  accbdw("<font id=\"csi-ac-" + n + "\" class=\"yui-ac invisible\" style=\"white-space:nowrap\">");
  accbdw("<input id=\"csi-input-" + n + "\" type=\"text\" value=\"\" size=\"0\" class=\"yui-ac-input inline-edit-input invisible\" onKeyUp=\"this.size=maxmin2size(this.value.length)\" onClick=\"this.size=maxmin2size(this.value.length);\" onBlur=\"clearsearchresults(" + n + ");\" style=\"padding:0px\">");
  accbdw("<input id=\"searchbutton-" + n + "\" class=\"searchbutton invisible\" type=\"button\" value=\"Search\" onClick=\"dosearch();\" style=\"padding:0px\">");
  accbdw("<span id=\"csi-ac-results-" + n + "\" class=\"yui-ac-container ac-results invisible\" style=\"padding:0px\"></span>");
  accbdw("<span id=\"search-results-" + n + "\" class=\"yui-ac-container search-results invisible\" style=\"padding:0px\"></span>");
  accbdw("<span id=\"end-search-results-" + n + "\" style=\"visibility:visible;width:0px;height:0px;font-size:0pt;padding:0px\"><span>");  // note that we use the csi-ac-results-n to end-search-results-n strings to ID the old AC results when clearing
  accbdw("</font>");
}

var inlineEditOpenMasterFlag = false;
var inlineEditChangePoint; // used to make a global out of where passed to genchange
var inlineEditSaveInnerHTML = '';
var inlineEditFirstEntry = true;
var inlineEditRemoveBoolean = false;
var inlineEditassertionid;

// this is what the page calls

function genchange(where, assertionid, removeboolean) {

  if (inlineEditOpenMasterFlag) return;
  if (inlineEditFirstEntry) {
    // document.body.setAttribute("onKeyUp","checkbodykey(event);"); // could be attached to something smaller, like the innerchange
//    writecss();
    inlineEditFirstEntry = false;
  }
  inlineEditChangePoint = where;
  inlineEditAssertionId = assertionid;
  inlineEditRemoveBoolean = removeboolean;

  var sUrl = 'cg?xml-assertion-info&assertionId=' + assertionid;
  // sUrl = 'http://achernar:3662/cgi-bin/cyccgi/cg?xml-assertion-info&assertionId=6189511';
  var assertioncallback = {
    success:handleAssertionSuccess,
    failure:handleAssertionFailure
  };
			      //  assertioncallback.argument.finalSuccessCallback = finalSuccessCallback;
  var termXMLrequest = YAHOO.util.Connect.asyncRequest('GET', sUrl, assertioncallback);

  // the rest of this function executes after the callback

}

function clearierr() {
  if (dobject('innerchangeerrmsg') != undefined) dobject('innerchangeerrmsg').innerHTML = '';
}

// this is what you write when you cancel the box

function cancel(force, oArgs) {
  if (force || inlineEditOpenMasterFlag) {
    // dobject(inlineEditChangePoint).innerHTML = '<font id=\"' + inlineEditChangePoint + '\" onClick=\"genchange(' + inlineEditChangePoint + ',' + assertionid ', true);\"> [CLICK TO CHANGE]</font>';
    var inlineedit = dobject(inlineEditChangePoint);
    //    inlineedit.innerHTML = inlineEditSaveInnerHTML;
    inlineEditOpenMasterFlag = false;
    if (oArgs) { //something happened
      var triggerElt = getTriggerElt(oArgs.containerElt, oArgs.oldAssertionId);
      if (oArgs.removeOriginal) {
        assertionRemovedEvent.fire(triggerElt, oArgs.containerElt, oArgs.oldAssertionId);
      }
      assertionAddedEvent.fire(triggerElt, oArgs.containerElt, oArgs.sentence);
    }
    /*
    {sentence: decodeURI(thissentence).replace(/&/g, '<hr>&'),
     sentencenum: i,
     oldAssertionId:inlineEditAssertionId,
     containerElt: containerElt,     
     removeOriginal: inlineEditRemoveBoolean};
    */
    inlineedit.parentNode.removeChild(inlineedit); //must be done after getting the triggerElt
  }
}

function getTriggerElt(elt, cycId) {
  var childElts = YAHOO.util.Dom.getElementsByClassName("assert", null, elt);
  for (var i = 0; i < childElts.length; i++) {
    var childId = childElts[i].getAttribute("cycid");
    if (childId == cycId) {
      return childElts[i];
    }
  }
  if (elt.parentNode) {
    return getTriggerElt(elt.parentNode, cycId);
  } else {
    return null;
  }
}
