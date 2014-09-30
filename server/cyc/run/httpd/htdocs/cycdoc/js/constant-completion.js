////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1997 - 2008 Cycorp, Inc.  All rights reserved.
// @created 1997/03/25
// @version "$Id: constant-completion.js 129172 2009-11-11 22:03:25Z rck $"
// @creator goolsbey
////////////////////////////////////////////////////////////////////////////////
// Code to support constant completion widgets in the Cyc browser
////////////////////////////////////////////////////////////////////////////////

// identifying the input element

var form_number;
var input_number;
var form_element;
var ele_value;

function identify_input_element(input) {
  var win_forms = window.document.forms;
  form_number  = -1;
  input_number = -1;

  // find the form and element number
  for (var form_i = 0; form_i < win_forms.length; form_i++) {
   var win_form = win_forms[form_i];
   form_element = win_form.elements[input];
   if (form_element) {
     for (var element_i = 0; element_i < win_form.elements.length; element_i++) {
        if (win_form.elements[element_i] == form_element) {
          form_number = form_i;
          input_number = element_i;
          break;
        }
     }
   }

   if (form_number >= 0)
     break;
  }

  if (form_number == -1) {
    window.status = 'Cannot locate field ' + input + '.';
  }
  else {
    form_element = 
      window.document.forms[form_number].elements[input_number];
    ele_value = form_element.value;
  }
  return false;
}

// extracting string to complete

var minimum_completion_length = 2;
var valid_constant_chars = 
  'abcdefghijklmnopqrstuvwxyz' + 
  'ABCDEFGHIJKLMNOPQRSTUVWXYZ' + 
  '1234567890' + '-_?';

function valid_constant_char(character) {
  return valid_constant_chars.indexOf(character) != -1;
}

function string_to_complete(string) {
  var length = string.length;
  if (length == 0) return '';
  if (!valid_constant_char(string.charAt(length - 1)))
    return '';
  for (var index = length - 1; index >= 0; index--) {
    if (!valid_constant_char(string.charAt(index)))
      return string.substring(index + 1, length);
  }
  return string;
}

// completion window manipulation

function open_completion_window(url) {
  var new_window = 
        window.open(url, completion_frame_name, 
                   'scrollbars=yes,resizable=yes,width=200,height=400');
  if (new_window)
    if (!new_window.opener)
      new_window.opener = self;
  return new_window;
}

function display_completion_window(url) {
  // first get completion window
  var completion_window = open_completion_window('');

  if (completion_window) {
    if (completion_window.opener) {
      if (completion_window.opener != self) {
        completion_window.close();
        setTimeout('open_completion_window(url)', 1000);
      }
      else {
        open_completion_window(url);
      }
    }
  }
  else {
    window.status = 'Cannot open a window for completions.';
  }
  return false;
}

function constant_complete(input, submit, filter, choices, lexical) {
  identify_input_element(input);

  if (form_number == -1) {
    return false;
  }
  else {
   var prefix = string_to_complete(ele_value);

// @todo rehabilitate this section
//    if (prefix.length < minimum_completion_length) {
//      if (ele_value.length == 0)
//        window.status = 'Nothing in field to complete upon.';
//      else if (prefix.length == 0) 
//        window.status = 'Cannot complete on last character in field.';
//      else
//        window.status = 'At least ' + minimum_completion_length +
//                        ' characters are required for completion.';
//      if (form_element.focus) form_element.focus();
//      return false;
//    }

    var new_url = cgi_program + '?cb-complete' + 
                  '|form='    + form_number + 
                  '|input='   + input_number + 
                  '|prefix='  + prefix +
                  '|submit='  + submit +
                  '|filter='  + filter +
                  '|choices=' + choices +
		  '|lexical=' + lexical;

    return display_completion_window(new_url);
  }
}

function cyclify(input) {
  identify_input_element(input);

  if (form_number == -1) {
    return false;
  }
  else {
    ele_value = escape(ele_value);
    // replace +'s with %2b's since escape doesn't convert them and o/w they turn to spaces
    ele_value = ele_value.replace(/\+/g,"%2b"); 
    var new_url = cgi_program + '?cb-cyclify' + 
                  '|form=' + form_number + 
                  '|input=' + input_number + 
                  '|string=' + ele_value;

    return display_completion_window(new_url);
  }
}
