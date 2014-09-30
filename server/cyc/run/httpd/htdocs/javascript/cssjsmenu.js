function cssjsmenu(menuid)
{
  var i;
  var j;
  var node;
  var child;
  var parent;

  // if the browser doesn't even support
  // document.getElementById, give up now.
  if (!document.getElementById)
  {
    return true;
  }

  // check for downlevel browsers
  // Opera 6, IE 5/Mac are not supported

  var version;
  var offset;

  offset = navigator.userAgent.indexOf('Opera');
  if (offset != -1)
  {
    version = parseInt('0' + navigator.userAgent.substr(offset + 6), 10);
    if (version < 7)
    {
      return true;
    }
  }

  offset = navigator.userAgent.indexOf('MSIE');
  if (offset != -1)
  {
    if (navigator.userAgent.indexOf('Mac') != -1)
    {
      return true;
    }
  }

  var menudiv = document.getElementById(menuid);

  // ul
  var ul = new Array();

  for (i = 0; i < menudiv.childNodes.length; i++)
  {
    node = menudiv.childNodes[i];
    if (node.nodeName == 'UL')
    {
      ul[ul.length] = node;
    }
  }

  // ul > li
  var ul_gt_li = new Array();

  for (i = 0; i < ul.length; i++)
  {
    node = ul[i];
    for (j = 0; j < node.childNodes.length; j++)
    {
      child = node.childNodes[j];
      if (child.nodeName == 'LI')
      {
        ul_gt_li[ul_gt_li.length] = child;
        child.style.display = 'inline';
        child.style.listStyle = 'none';
        child.style.position = 'static';
      }
    }
  }

  // ul > li > ul
  var ul_gt_li_gt_ul = new Array();

  for (i = 0; i < ul_gt_li.length; i++)
  {
    node = ul_gt_li[i];
    for (j = 0; j < node.childNodes.length; j++)
    {
      child = node.childNodes[j];
      if (child.nodeName == 'UL')
      {
        ul_gt_li_gt_ul[ul_gt_li_gt_ul.length] = child;
        child.style.position = 'absolute';
        child.style.left = '-13em';
        child.style.visibility = 'hidden';

        // attach hover to parent li
        parent = child.parentNode;
        parent.onmouseover = function (e)
        {
          var i;
          var child;
          var point;

          // stop the pure css hover effect
          this.style.paddingBottom = '0';

          for (i = 0; i < this.childNodes.length; i++)
          {
            child = this.childNodes[i];
            if (child.nodeName == 'UL')
            {
              point = getPageXY(this);
              setPageXY(child, point.x, point.y + this.offsetHeight);
              child.style.visibility = 'visible';
            }
          }
          return false;
        };
        parent.onmouseout = function (e)
        {
          var relatedTarget = null;
          if (e)
          {
            relatedTarget = e.relatedTarget;
            // work around Gecko Linux only bug where related target is null
            // when clicking on menu links or when right clicking and moving
            // into a context menu.
	    if (navigator.product == 'Gecko' && navigator.platform.indexOf('Linux') != -1 && !relatedTarget)
	    {
	      relatedTarget = e.originalTarget;
	    }
          }
          else if (window.event)
          {
            relatedTarget = window.event.toElement;
          }

          if (elementContains(this, relatedTarget))
          {
            return false;
          }

          var i;
          var child;
          for (i = 0; i < this.childNodes.length; i++)
          {
            child = this.childNodes[i];
            if (child.nodeName == 'UL')
            {
                child.style.visibility = 'hidden';
            }
          }
          return false;
        };
      }
    }
  }
  return true;
}

