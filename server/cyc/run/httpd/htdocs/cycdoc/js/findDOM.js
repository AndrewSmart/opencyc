var isDHTML = 0;
var isLayers = 0;
var isAll = 0;
var isID = 0;

if (document.getElementById) {isID = 1; isDHTML = 1;}
else {
if (document.all) {isAll = 1; isDHTML = 1;}
else {
browserVersion = parseInt(navigator.appVersion);
if ((navigator.appName.indexOf('Netscape') != -1) && (browserVersion == 4)) {isLayers = 1; isDHTML = 1;}
}}

var isNav4, isNav6, isIE4;

/*
 * Browser version snooper; determines your browser
 * (Navigator 4, Navigator 6, or Internet Explorer 4/5)
 */
function setBrowser()
{
    if (navigator.appVersion.charAt(0) == "4")
    {
        if (navigator.appName.indexOf("Explorer") >= 0)
        {
            isIE4 = true;
        }
        else
        {
            isNav4 = true;
        }
    }
    else if (navigator.appVersion.charAt(0) > "4")
    {
        isNav6 = true;
    }
}

setBrowser();

/*
 *
 * Given a selector string, return a style object
 * by searching through stylesheets. Return null if
 * none found
 *
 */
function getStyleBySelector( selector )
{
    if (!isNav6)
    {
        return null;
    }
    var sheetList = document.styleSheets;
    var ruleList;
    var i, j;

    /* look through stylesheets in reverse order that
       they appear in the document */
    for (i=sheetList.length-1; i >= 0; i--)
    {
        ruleList = sheetList[i].cssRules;
        for (j=0; j<ruleList.length; j++)
        {
            if (ruleList[j].type == CSSRule.STYLE_RULE &&
                ruleList[j].selectorText == selector)
            {
                return ruleList[j].style;
            }   
        }
    }
    return null;
}

/*
 *
 * Given an id and a property (as strings), return
 * the given property of that id.  Navigator 6 will
 * first look for the property in a tag; if not found,
 * it will look through the stylesheet.
 *
 * Note: do not precede the id with a # -- it will be
 * appended when searching the stylesheets
 *
 */
function getIdProperty( id, property )
{
    if (isNav6)
    {
        var styleObject = document.getElementById( id );
        if (styleObject != null)
        {
            styleObject = styleObject.style;
            if (styleObject[property])
            {
                return styleObject[ property ];
            }
        }
        styleObject = getStyleBySelector( "#" + id );
        return (styleObject != null) ?
            styleObject[property] :
            null;
    }
    else if (isNav4)
    {
        return document[id][property];
    }
    else
    {
        return document.all[id].style[property];
    }
}

/*
 *
 * Given an id and a property (as strings), set
 * the given property of that id to the value provided.
 *
 * The property is set directly on the tag, not in the
 * stylesheet.
 *
 */

function setIdProperty( id, property, value )
{
    if (isNav6)
    {
        var styleObject = document.getElementById( id );
        if (styleObject != null)
        {
            styleObject = styleObject.style;
            styleObject[ property ] = value;
        }
        
    }
    else if (isNav4)
    {
        document[id][property] = value;
    }
    else if (isIE4)
    {
         document.all[id].style[property] = value;
    }
}

function findDOM(objectID,withStyle) {
	if (withStyle == 1) {
		if (isID) { return (document.getElementById(objectID).style) ; }
		else { 
			if (isAll) { return (document.all[objectID].style); }
		else {
			if (isLayers) { return (document.layers[objectID]); }
		};}
	}
	else {
		if (isID) { return (document.getElementById(objectID)) ; }
		else { 
			if (isAll) { return (document.all[objectID]); }
		else {
			if (isLayers) { return (document.layers[objectID]); }
		};}
	}
}

function whoAmI(objectID) {
	domStyle = findDOM(objectID,1)
	dom = findDOM(objectID,0);
	if (domStyle.pixelTop != null) { alert(domStyle.pixelTop); }
	else { alert(domStyle.top); }
	alert(dom.id);
}
