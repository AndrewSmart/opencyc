
if (document.layers) {
	origWidth = innerWidth;
	origHeight = innerHeight;
	}

function reloadPage() {
	if (innerWidth != origWidth || innerHeight != origHeight) 
	location.reload();
	}

if (document.layers) onresize = reloadPage;
				
function setVisibility(objectID,state) {
    setIdProperty(objectID, "display", state);
}

function toggleVisibility(objectID) {

  var state = getIdProperty( objectID, "display");
  if (state == 'none')
     setIdProperty(objectID, "display", 'block');
  else if (state == 'block')
     setIdProperty( objectID, "display", 'none');
  else 
     setIdProperty( objectID, "display", 'block');
}
