function ndeSetTextSize(chgsize,rs) 
{
  var startSize;
  var newSize;

  if (!document.getElementsByTagName)
  {
    return;
  }

  startSize = parseInt(ndeGetDocTextSize());

  if (!startSize)
  {
    startSize = 12;
  }

  switch (chgsize)
  {
  case 'incr':
    newSize = startSize + 2;
    break;

  case 'decr':
    newSize = startSize - 2;
    break;

  case 'reset':
    if (rs) {newSize = rs;} else {newSize = 12;}
    break;

  default:
    newSize = parseInt(ndeReadCookie("nde-textsize", true));
    if (!newSize)
    {
      newSize = startSize;
    }
    break;

  }

  if (newSize < 10) 
  {
    newSize = 10;
  }

  newSize += 'px';

  document.getElementsByTagName('html')[0].style.fontSize = newSize;
  document.getElementsByTagName('body')[0].style.fontSize = newSize;

  ndeCreateCookie("nde-textsize", newSize, 365, true);
}

function ndeGetDocTextSize() 
{
  if (!document.getElementsByTagName)
  {
    return 0;
  }

  var size = 0;
  var body = document.getElementsByTagName('body')[0];

  if (body.style && body.style.fontSize)
  {
    size = body.style.fontSize;
  }
  else if (typeof(getComputedStyle) != 'undefined')
  {
    size = getComputedStyle(body,'').getPropertyValue('font-size');
  }
  else if (body.currentStyle)
  {
    size = body.currentStyle.fontSize;
  }
  return size;
}

function ndeSetStyleSheet(newtitle) 
{
  // this function exists solely to 
  // distinguish when the user chooses a new
  // theme. IE does not properly reflow the page
  // when a new theme is chosen and we must force
  // a reload when IE users choose a new theme.

  ndeSetStyleSheetInternal(newtitle);
  if (navigator.userAgent.indexOf('MSIE') != -1 && 
      !window.opera && 
      navigator.product != 'Gecko')

  {
    history.go(0);
  }

}

function ndeSetStyleSheetInternal(newtitle) 
{
  if (!document.getElementsByTagName)
  {
    return;
  }

  var i;
  var savedtitle = '';
  var links = document.getElementsByTagName("link");

  if (!newtitle)
  {
    newtitle = savedtitle = ndeReadCookie("nde-style", false);
  }

  if (!newtitle)
  {
    newtitle = ndeGetPreferredStyleSheet();
  }

  var activesheet = null;
  for (i = 0; i < links.length; i++) 
  {
    var a = links[i];
    var rel = a.getAttribute('rel');
    var title = a.getAttribute('title');

    if (rel.indexOf("style") != -1 && title) 
    {
      a.disabled = true;
      if (title == newtitle) 
      {
        activesheet = a;
      }
    }
  }
  if (activesheet)
  {
    activesheet.disabled = false;
  }
  if (newtitle != savedtitle)
  {
    ndeCreateCookie("nde-style", newtitle, 365, false);
  }
}

function ndeGetActiveStyleSheet() 
{
  if (!document.getElementsByTagName)
  {
    return null;
  }

  var i;
  var links = document.getElementsByTagName("link");

  for(i = 0; i < links.length; i++) 
  {
    var a = links[i];
    var rel = a.getAttribute('rel');
    var title = a.getAttribute('title');

    if (rel.indexOf("style") != -1 && title && !a.disabled) 
    {
      return title;
    }
  }
  return null;
}

function ndeGetPreferredStyleSheet() 
{
  if (!document.getElementsByTagName)
  {
    return null;
  }

  var i, a;
  var links = document.getElementsByTagName("link");

  for (i = 0; i <  links.length; i++) 
  {
    a = links[i];
    var rel = a.getAttribute('rel');
    var title = a.getAttribute('title');

    if (rel.indexOf("style") != -1 && rel.indexOf("alt") == -1 && title)
    {
      return title;
    }
  }
  return null;
}

function ndeCreateCookie(name,value,days,useLang) 
{

  var langString = useLang ? ndeGetLang() : "";

  var cookie = name + langString + "=" + value + ";";

  if (days) 
  {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    cookie += " expires=" + date.toGMTString() + ";";
  }
  cookie += " path=/";

  document.cookie = cookie;
}

function ndeReadCookie(name, useLang) 
{

  var langString = useLang ? ndeGetLang() : "";

  var nameEQ = name + langString + "=";
  var ca = document.cookie.split(';');

  for(var i = 0; i < ca.length; i++) 
  {
    var c = ca[i];
    while (c.charAt(0) == ' ') 
    {
      c = c.substring(1, c.length);
    }

    if (c.indexOf(nameEQ) == 0) 
    {
      return c.substring(nameEQ.length,c.length);
    }
  }
  return null;
}

function ndeSetTheme()
{
  ndeSetStyleSheetInternal();
  ndeSetTextSize();
  return true;
}

function ndeGetLang()
{
  var langString = "";

  if (document.documentElement){
    langString = document.documentElement.lang;
    if (langString != ""){
      langString = "-" + langString;
    }
  }  
  return langString;
}

function init() 
{
  var rvValue = -1;
  if (navigator.product == 'Gecko')
  {
    rvValue = 0;
    var ua      = navigator.userAgent.toLowerCase();
    var rvStart = ua.indexOf('rv:');
    var rvEnd   = ua.indexOf(')', rvStart);
    var rv      = ua.substring(rvStart+3, rvEnd);
    var rvParts = rv.split('.');
    var exp     = 1;

    for (var i = 0; i < rvParts.length; i++)
    {
      var val = parseInt(rvParts[i]);
      rvValue += val / exp;
      exp *= 100;
    }
  }

  if (!document.getElementById || ( rvValue >= 0 && rvValue < 1.0))
  {
    var updateMessageShown = ndeReadCookie('upgrade');
    if (!updateMessageShown)
    {
      ndeCreateCookie('upgrade','1', 90);
      document.location = 'http://devedge.netscape.com/upgrade.html';
    }
  }

  cssjsmenu('navbar');
  cssjsmenu('nde-config');
  if (document.getElementById)
  {
    var kill = document.getElementById('hoverJS'); 
    kill.disabled = true;
  }
  ndeSetStyleSheetInternal();
}
