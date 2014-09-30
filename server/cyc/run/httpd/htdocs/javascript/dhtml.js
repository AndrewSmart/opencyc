function elementContains(elmOuter, elmInner)
{
  while (elmInner && elmInner != elmOuter)
  {
    elmInner = elmInner.parentNode;
  }
  if (elmInner == elmOuter)
  {
    return true;
  }
  return false;
}

function getPageXY(elm)
{
  var point = { x: 0, y: 0 };
  while (elm)
  {
    point.x += elm.offsetLeft;
    point.y += elm.offsetTop;
    elm = elm.offsetParent;
  }
  return point;
}

function setPageXY(elm, x, y)
{
  var parentXY = {x: 0, y: 0 };

  if (elm.offsetParent)
  {
    parentXY = getPageXY(elm.offsetParent);
  }

  elm.style.left = (x - parentXY.x) + 'px';
  elm.style.top  = (y - parentXY.y) + 'px';
}
