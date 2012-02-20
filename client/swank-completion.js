// -*- mode: js2 -*-
//
// Copyright (c) 2012 Robert Krahn. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following
// disclaimer in the documentation and/or other materials
// provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

SwankJS.doCompletion = function doCompletion (str) {
  var r = [], obj, end_idx = str.length - 1;
  function dotCompletion(str, regex, skipPrefix) {
    var addPrefix = skipPrefix ? "" : str + ".";
    obj = window.eval(str);
    if (obj) {
      for (var name in obj) {
        if (regex) {
          if (name.match(regex))
            r.push(addPrefix + name);
        } else {
            r.push(addPrefix + name);
        }
      }
    }
    return r;
  }
  function nameCompletion() {
    var dotIndex = str.lastIndexOf(".");
    var parent = dotIndex > -1 ? str.substring(0, dotIndex) : "window";
    var strToComplete = str.substring(dotIndex + 1, str.length);
    return dotCompletion(parent, new RegExp("^" + strToComplete), dotIndex < 0);
  }

  return (str[end_idx] == ".") ? dotCompletion(str.substring(0, end_idx)) : nameCompletion();
};