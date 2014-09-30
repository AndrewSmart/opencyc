var Dom = YAHOO.util.Dom,
    Event = YAHOO.util.Event;

var onNatMouseOver = function (event, matchedEl, container) {
  var eltClass = matchedEl.className;
  var ids = eltClass.match("naut[0-9]+-[0-9]+");
  var id = ids[0];
  var elt = document.getElementById(id);
  Dom.addClass(elt, "term-highlight");
}

var onNatMouseOut = function (event, matchedEl, container) {
  var eltClass = matchedEl.className;
  var ids = eltClass.match("naut[0-9]+-[0-9]+");
  var id = ids[0];
  Dom.removeClass(document.getElementById(id), "term-highlight");
}

Event.delegate(document, "mouseover", onNatMouseOver, "a.nat");
Event.delegate(document, "mouseout", onNatMouseOut, "a.nat"); 

var gatherAssertions = function (limit) {
  if (limit == null || limit == undefined) {
    limit = 250;
  }
  var anchors = document.getElementsByTagName("a");
  var assertions = {lexical: [], nonLexical: [], rules: []};
  var assertionCount = 0;
  for (var i = 0; assertionCount <= limit && i < anchors.length; i++) {
    var curAnchor = anchors[i];
    if (Dom.hasClass(curAnchor, "assert")) {
      if (Dom.hasClass(curAnchor, "lex")) {
        assertions.lexical.push(curAnchor); 
        assertionCount++;
      } else if (Dom.hasClass(curAnchor, "rule")) {
        assertions.rules.push(curAnchor); 
        assertionCount++;
      } else {
        assertions.nonLexical.push(curAnchor);
        assertionCount++;
      }
    }
  }
  return assertions;
}
  
//these need to be global so they can be cleared when necessary.
var cbAssertionContextMenu;
var cbLexicalAssertionContextMenu;
var cbRuleAssertionContextMenu;

var addAssertionContextMenus = function (oType, aArgs, limit) {
  var assertions = gatherAssertions(limit);
  var nextIntValue = 0;
  function nextInt() {
    return nextIntValue ++;
  }
  
  //the following two functions are necessitated by a bug in YUI that causes 
  function getSubmenuData() {
    return {id: "editSubmenu" + nextInt(),
  	  itemdata: [{text: "Edit Assertion on new page", onclick: {fn: loadAssertionUrl, obj: {cmd: "edit"}}},
                       {text: "Edit Mt", onclick: {fn: loadAssertionUrl, obj: {cmd:"change-mt"}}},
                       {text: "Edit Direction", onclick: {fn: loadAssertionUrl, obj: {cmd:"change-direction"}}}]};
  }
  function getEditGroupData () {
    return [{text: "Edit Assertion", onclick: {fn: editInline, obj: {cmd:this}}, submenu: getSubmenuData()},
            {text: "Assert Similar", onclick: {fn: copyInline, obj: {cmd:this}}, submenu: assertSimilarSubmenuData},
            {text: "Assert Meta", onclick: {fn: loadAssertionUrl, obj: {cmd:"assert-meta"}}},
            {text: "Remove", submenu: unassertSubmenuData},
            {text: "Redo", submenu: redoSubmenuData} 
  	  ];
  }
  
  var assertSimilarSubmenuData = 
    {id: "assertSimilarSubmenu",
     itemdata: [{text: "Assert Similar on new page", onclick: {fn: loadAssertionUrl, obj: {cmd: "similar"}}}]};
  var unassertSubmenuData = 
    {id: "unassertSubmenu", 
     itemdata: [{text: "<span class='confirm'>Unassert</span>",  onclick: {fn: xhrLoadAssertionUrl, obj: {cmd:"unassert", evt: assertionRemovedEvent}}},
    {text: "<span class='confirm'>Blast</span>",  onclick: {fn: xhrLoadAssertionUrl, obj: {cmd:"blast", evt: assertionRemovedEvent}}}]};
  
  var redoSubmenuData = 
    {id: "redoSubmenu", 
     itemdata: [{text: "Reassert",  onclick: {fn: bgLoadAssertionUrl, obj: {cmd: "reassert"}}},
    {text: "Repropagate",  onclick: {fn: bgLoadAssertionUrl, obj: {cmd: "repropagate"}}},
    {text: "Repropagate and Browse",  onclick: {fn: loadAssertionUrl, obj: {cmd: "repropagate-browsably"}}},
    {text: "Recanonicalize",  onclick: {fn: bgLoadAssertionUrl, obj: {cmd: "recanonicalize"}}},
    {text: "Redo TMS",  onclick: {fn: bgLoadAssertionUrl, obj: {cmd: "assertion-tms"}}},
                ]};
  
  
  var miscGroupData = [
    {text: "Query Similar", onclick: {fn: loadAssertionUrl, obj: {cmd:"assertion-similar-query"}}},
    {text: "WFF Check", onclick: {fn: loadAssertionUrl, obj: {cmd:"wff-assertion"}}},
    {text: "HL Data", onclick: {fn: loadAssertionUrl, obj: {cmd:"describe-assertion"}}},
    {text: "Arguments", onclick: {fn: loadAssertionUrl, obj: {cmd:"arguments"}}}
  		       ];
  
  var lexical = [
                 {text: "Add to Webservice Lexicon", onclick: {fn: xhrLoadAssertionUrl, obj: {cmd: "add-to-webservice-lexicon"}}}
  		 ]; 
  var rulesGroupData = [
                        {text: "Use in Proof Checker", onclick: {fn: xhrLoadAssertionUrl, obj: {cmd: "use-in-proof"}}}
  			]; 
    if (cbAssertionContextMenu) {
      cbAssertionContextMenu.destroy();
    }
    cbAssertionContextMenu = new YAHOO.widget.ContextMenu(
  							"assertioncontextmenu", 
      { trigger: assertions.nonLexical,
        itemdata: [getEditGroupData(), miscGroupData],
        lazyload: true
      });
    cbAssertionContextMenu.beforeShowEvent.subscribe(setDisabledContextMenuOptions); 
  
    if (cbLexicalAssertionContextMenu) {
      cbLexicalAssertionContextMenu.destroy();
    }
    cbLexicalAssertionContextMenu = new YAHOO.widget.ContextMenu("lexicalassertioncontextmenu", 
      { trigger: assertions.lexical,
        itemdata: [getEditGroupData(), miscGroupData, lexical],
        lazyload: true
      });
    cbLexicalAssertionContextMenu.beforeShowEvent.subscribe(setDisabledContextMenuOptions); 
  
    if (cbRuleAssertionContextMenu) {
      cbRuleAssertionContextMenu.destroy();
    }
    cbRuleAssertionContextMenu = new YAHOO.widget.ContextMenu("ruleassertioncontextmenu", 
      { trigger: assertions.rules,
        itemdata: [getEditGroupData(), miscGroupData, rulesGroupData],
        lazyload: true
      });
    cbRuleAssertionContextMenu.beforeShowEvent.subscribe(setDisabledContextMenuOptions); 
  }


  function setDisabledContextMenuOptions(arg1, arg2, oObj) {
    var editable = ["Edit Assertion", "Edit Mt", "Edit Direction", "Reassert", "<span class='confirm'>Unassert</span>", "Recanonicalize"];
    var nonEditable = ["<span class='confirm'>Blast</span>", "Redo TMS"];
    var lexical     = ["Add to Webservice Lexicon"];
    var assertElt   = Dom.getAncestorByClassName(getMenuTarget(this), "assert");
    var isEditable  = Dom.hasClass(assertElt, "edit");
    var menuItems   = this.getItems(); 
    var subMenus    = this.getSubmenus();
    for (var j = 0; j < subMenus.length; j++) {
            subMenus[j].beforeShowEvent.subscribe(setDisabledContextMenuOptions);
    }
    for (i=0; i < menuItems.length; i++) {
      var itemText = menuItems[i].cfg.getProperty("text");
      if (editable.indexOf(itemText) >= 0) {
	menuItems[i].cfg.setProperty("disabled", !isEditable);
      } else if (nonEditable.indexOf(itemText) >= 0) {
        menuItems[i].cfg.setProperty("disabled", isEditable);
      }
    }
  }

var loadAssertionUrl = function (arg1, arg2, oObj) {
  var clickedElt = getMenuTarget(this);
  var assertElt = Dom.getAncestorByClassName(clickedElt, "assert");
  var assertId = assertElt.getAttribute("cycid");
  var assertTarget = assertElt.getAttribute("target");
  var targetFrame = getAncestorFrameByName(window, assertTarget);
  var href = null;
  if (oObj.cmd) {
    var href = "cg?cb-" + oObj.cmd + "&" + assertId; 
    targetFrame.location.href = href;
  }
}

//load this url in the background, and then reload the current window.
  var bgLoadAssertionUrl = function (arg1, arg2, oObj) {
    var clickedElt = getMenuTarget(this);
    var assertElt = Dom.getAncestorByClassName(clickedElt, "assert");
    var assertId = assertElt.getAttribute("cycid");
    var assertTarget = assertElt.getAttribute("target");
    var targetFrame = getAncestorFrameByName(window, assertTarget);
    if (oObj.cmd) {
      var url = "cg?cb-" + oObj.cmd + "&" + assertId; 
      var callback = {success: handlePageReload,
		      failure: handleFailedBgLoad,
		      argument: {target: targetFrame}
      };
      YAHOO.util.Connect.asyncRequest("get", url, callback);
    }
  }

function handlePageReload (o) {
  o.argument.target.location.reload();
}

function handleFailedBgLoad(o) {
  alert("Unable to perform menu action");
}


  var copyInline = function (arg1, arg2, oObj) {
    return editInlineInt(false, this, arg1, arg2, oObj);
  }

  var editInline = function (arg1, arg2, oObj) {
    return editInlineInt(true, this, arg1, arg2, oObj);
  }

  var editInlineInt = function (bEdit, oMenuItem, arg1, arg2, oObj) {
    var clickedElt = getMenuTarget(oMenuItem);
    var assertElt = Dom.getAncestorByClassName(clickedElt, "assert");
    var assertId = assertElt.getAttribute("cycid");
    var eltId = assertElt.getAttribute("id");
    var menu = getEnclosingMenu(oMenuItem);
    menu.cfg.setProperty("visible", false);
    genchange(eltId, assertId, bEdit);
  }

//assumes that o is a menuItem
    function getEnclosingMenu(o) {
      if (o.CSS_CLASS_NAME == "yuimenu") {
        return o;
      } else if (o.parent && o.parent != o) {
        return getEnclosingMenu(o.parent);
      } else return null;
    }
 
//load this url in the background, and show the results in an alert window
var xhrLoadAssertionUrl = function (arg1, arg2, oObj) {
  var clickedElt = getMenuTarget(this);
  var assertElt = Dom.getAncestorByClassName(clickedElt, "assert");
  var assertId = assertElt.getAttribute("cycid");
  var assertTarget = assertElt.getAttribute("target");
  var targetFrame = getAncestorFrameByName(window, assertTarget);
  if (oObj.cmd) {
    var url = "cg?cb-" + oObj.cmd + "&" + assertId; 
    var callback = {success: handleXHRLoad,
                    failure: handleFailedBgLoad,
                    argument: {target: targetFrame, evt: oObj.evt, assertId: assertId, assertElt: assertElt}
    };
    YAHOO.util.Connect.asyncRequest("get", url, callback);
  }
}

function handleXHRLoad (oObj) {
  if (oObj.argument.evt == undefined || oObj.argument.evt == null) {
    alert(oObj.responseText);
  } else {
    oObj.argument.evt.fire(oObj.argument.assertElt, null, oObj.argument.assertId);
  }
} 
  function getAncestorFrameByName(elt, frameName) {
    if (elt.frameElement && elt.frameElement.name == frameName) {
      return elt;
    } else if (elt.parent && elt.parent != elt) {
      return getAncestorFrameByName(elt.parent, frameName);
    } else return null;
  }

function getMenuTarget(oMenuElt) {
  if (oMenuElt.contextEventTarget) {
    return oMenuElt.contextEventTarget;
  } else if (oMenuElt.parent) {
    return getMenuTarget(oMenuElt.parent);
  } else return null;
}  

  Event.onDOMReady(addAssertionContextMenus, 250);

/* Monkeying around with the HTML of an existing page */

var assertionAddedEvent = new YAHOO.util.CustomEvent("assertionAdded"); //arg-signature: (istSentenceString, eltId), where eltId is the id for the element where this came from(i.e. near where it should be added back in)
var assertionRemovedEvent = new YAHOO.util.CustomEvent("assertionRemoved");//arg-signature (istSentenceString, eltId)



assertionRemovedEvent.subscribe(handleRemoveAssertionFromPage);
assertionAddedEvent.subscribe(handleAddAssertionToPage);

function handleRemoveAssertionFromPage(sType, aArgs, oObj) {
  var triggerElt = aArgs[0];
  var sourceElt = aArgs[1];
  var oldAssertionId = aArgs[2];
  YAHOO.util.Dom.setStyle(getSubsumingAssertionElt(triggerElt), "text-decoration", "line-through");
  possiblyActivateFrameReloadButton();
}

function handleAddAssertionToPage(sType, aArgs, oObj) {
  var triggerElt=aArgs[0];
  var sourceElt=aArgs[1];
  var newSentence =aArgs[2];
  var i = 0;
  var assertionElt = getSubsumingAssertionElt(triggerElt);
  if (newSentence && assertionElt) {
    var callbackFn = function(o) {
      var newElt = document.createElement("span");
      newElt.innerHTML = "<br>" + o.responseText;
      assertionElt.parentNode.insertBefore(newElt, assertionElt.nextSibling);
      var newId = YAHOO.util.Dom.generateId(YAHOO.util.Dom.getElementsByClassName("assert", null, newElt)[0]);
      addAssertionContextMenus(null, null, 1000);
      possiblyActivateFrameReloadButton();
    }
    var url = "cg?cb-form-smart&cycl=" + newSentence; //@TODO retrigger context menus for new items
    var callback = {success: callbackFn,
                    failure: handleFailedBgLoad
    };
    YAHOO.util.Connect.asyncRequest("get", url, callback);
  }
}

function getSubsumingAssertionElt(elt) {
  var assertionElt = YAHOO.util.Dom.getAncestorByClassName(elt, "assertion");
  if (assertionElt == null) {
    assertionElt = YAHOO.util.Dom.getAncestorByClassName(elt, "assert-sent");
  }
  return assertionElt;
}

function possiblyActivateFrameReloadButton () {
  var button = document.getElementById("reloadFrameButton");
  if (button) {
    button.setAttribute("class", "visible");
  }
}

function reloadCurrentFrame(eltId) {
  var frame= getAncestorFrame(window);
  if (frame) {
    frame.location.reload();
  }
}

  function getAncestorFrame(elt) {
    if (elt.frameElement) {
      return elt;
    } else if (elt.parentNode && elt.parentNode != elt) {
      return getAncestorFrame(elt.parentNode);
    } else return null;
  }


/* Proof-views */

function toggleCycLVisible (checkbox, eltId) {
  toggleEltClass(checkbox, eltId, "showcycl");
}  
function toggleDebug (checkbox, eltId) {
  toggleEltClass(checkbox, eltId, "pf-debug");
}  

function toggleEltClass (checkbox, eltId, newClass) {
  if (checkbox.checked) {
   Dom.addClass(eltId, newClass);
  } else {
    Dom.removeClass(eltId, newClass);
  }
}

function initProofView () {
  if (document.getElementById("justifications")) {
    var justificationTabView = new YAHOO.widget.TabView(document.getElementById("justifications"));
  }
    if (document.getElementById("pf-buttonbar")) {
      var pfButtonBar = new YAHOO.widget.Overlay("pf-buttonbar", {x: 1500, constraintoviewport: true, visible:true, width:"300px"} );
      uncheck("pf-show-debugging-checkbox");
      uncheck("pf-show-cycl-checkbox");
    pfButtonBar.render();
    pfButtonBar.show();
  }
}

function uncheck(checkboxName) {
  if (document.getElementById(checkboxName) != null) {
    document.getElementById(checkboxName).checked = false;
  }
}

Event.onDOMReady(initProofView);

