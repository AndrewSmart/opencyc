/*
Source of code: https://code.google.com/p/rangyinputs/source/browse/trunk/rangyinputs_jquery.js
The MIT License (MIT)

Copyright 2013, Tim Down
Modified work © 2014 Andrew Smart

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

This javascript allows the cursor position within a textarea to be known.
External call: var cursorPosition = getSelection(textareaYUINode);
*/

var UNDEF = "undefined";
var getSelection;
// Trio of isHost* functions taken from Peter Michaux's article:
// http://peter.michaux.ca/articles/feature-detection-state-of-the-art-browser-scripting
function isHostMethod(object, property) {
    var t = typeof object[property];
    return t === "function" || (!!(t == "object" && object[property])) || t == "unknown";
}

function isHostProperty(object, property) {
    return typeof(object[property]) != UNDEF;
}

function isHostObject(object, property) {
    return !!(typeof(object[property]) == "object" && object[property]);
}

function makeSelection(el, start, end) {
    return {
        start: start,
        end: end,
        length: end - start //,
        //text: el.value.slice(start, end)
    };
}

function getBody() {
    return isHostObject(document, "body") ? document.body : document.getElementsByTagName("body")[0];
}

//TODO: Find a way to call this within this .js file after DOM is ready;
// as document.addEventListener("DOMContentLoaded", function(event) {}); doesn't work!
var handleGetTextAreaSelectionAfterDOMReady = function() {
    var testTextArea = document.createElement("textarea");
    getBody().appendChild(testTextArea);
    if (isHostProperty(testTextArea, "selectionStart") && isHostProperty(testTextArea, "selectionEnd")) {
        getSelection = function(el) {
            var start = el.selectionStart, end = el.selectionEnd;
            return makeSelection(el, start, end);
        };
    } else if (isHostMethod(testTextArea, "createTextRange") && isHostObject(document, "selection") &&
               isHostMethod(document.selection, "createRange")) {
        getSelection = function(el) {
            var start = 0, end = 0, normalizedValue, textInputRange, len, endRange;
            var range = document.selection.createRange();
            if (range && range.parentElement() == el) {
                len = el.value.length;
                normalizedValue = el.value.replace(/\r\n/g, "\n");
                textInputRange = el.createTextRange();
                textInputRange.moveToBookmark(range.getBookmark());
                endRange = el.createTextRange();
                endRange.collapse(false);
                if (textInputRange.compareEndPoints("StartToEnd", endRange) > -1) {
                    start = end = len;
                } else {
                    start = -textInputRange.moveStart("character", -len);
                    start += normalizedValue.slice(0, start).split("\n").length - 1;
                    if (textInputRange.compareEndPoints("EndToEnd", endRange) > -1) {
                        end = len;
                    } else {
                        end = -textInputRange.moveEnd("character", -len);
                        end += normalizedValue.slice(0, end).split("\n").length - 1;
                    }
                }
            }
            return makeSelection(el, start, end);
        };
    } else {
        getBody().removeChild(testTextArea);
        Y.log("No means of finding text input caret position");
        return;
    }
    getBody().removeChild(testTextArea);
}
