/*
 * SVG_MathJax
 * 
 * Copyright 2014 Jason M. Sachs
 * Based loosely on an approach outlined by Martin Clark
 * in http://stackoverflow.com/a/21923030/44330
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
Svg_MathJax = (function() {
    // apply a function to elements of an array x
    function forEach(x,f) { 
        var n = x.length; for (var i = 0; i < n; ++i) { f(x[i]); } 
    }

    // find all the SVG text elements that are delimited by 
    // \( \) or $ $ MathJax delimiters 
    // (with optional whitespace before/after)
    function findSVGMathJax(f, context)
    {
        var re = /^\s*([LlRrCc]?)(\\\(.*\\\)|\$.*\$)\s*$/;
        context = context || document;
        forEach(context.getElementsByTagName('svg'), function(svg) {
            forEach(svg.getElementsByTagName('text'), function(t) {
                var m = t.textContent.match(re);
                if (m)
                {
                    f(svg, t, m);
                }
            });
        });
    }

    function _install(options) {
        var items = [];

        // Move the raw MathJax items to a temporary element
        MathJax.Hub.Register.StartupHook("Begin Typeset",function () { 
            var mathbucket = document.createElement('div');
            mathbucket.setAttribute('id','mathjax_svg_bucket');
            document.body.appendChild(mathbucket);
            findSVGMathJax(function(svg, t, m) { 
                var d = document.createElement('div');
                mathbucket.appendChild(d);
                var mathmarkup = m[2].replace(/^\$(.*)\$$/,'\\($1\\)');
                d.appendChild(document.createTextNode(mathmarkup));
                t.textContent = '';	
                items.push([t,d,m[1]]);
            });
        });
        MathJax.Hub.Register.StartupHook("End Typeset",function() {
            forEach(items, function(x) {
                var svgdest = x[0]; 
                var mathjaxdiv = x[1]; 
                var justification = x[2];
                var svgmath = 
                     mathjaxdiv.getElementsByClassName('MathJax_SVG')[0]
                               .getElementsByTagName('svg')[0];
                var svgmathinfo = {
                  width: svgmath.viewBox.baseVal.width, 
                  height: svgmath.viewBox.baseVal.height
                };
                // get graphics nodes
                var gnodes = 
                    svgmath.getElementsByTagName('g')[0].cloneNode(true);
                var fontsize = svgdest.getAttribute('font-size');
                var scale = options.scale*fontsize;
                var x =  +svgdest.getAttribute('x');
                var y =  +svgdest.getAttribute('y');

                var x0 = x;
                var y0 = y;
                var x1;
                switch (justification.toUpperCase())
                {
                case 'L': x1 = 0; break;
                case 'R': x1 = -svgmathinfo.width; break;
                case 'C': // default to center
                default:  x1 = -svgmathinfo.width * 0.5; break;
                }
                var y1 = svgmathinfo.height*0;
                gnodes.setAttribute('transform', 'translate('+x0+' '+y0+')'
                     +' scale('+scale+') translate('+x1+' '+y1+')'
                     +' matrix(1 0 0 -1 0 0)');
                if (options.escape_clip)
                    svgdest.parentNode.removeAttribute('clip-path');
                svgdest.parentNode.replaceChild(gnodes,svgdest);
            });
            // remove the temporary items
            var mathbucket = document.getElementById('mathjax_svg_bucket');
            mathbucket.parentNode.removeChild(mathbucket);
        });
    }
    
    var F = function()
    {
        this.scale = 0.0016;
        this.escape_clip = false;
    };
    F.prototype.install = function() { _install(this); }
    return F;
})();
