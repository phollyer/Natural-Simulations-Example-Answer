!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function i(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(i){return n(r,t,e,u,i)}}}}}))}function f(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(i){return function(f){return n(r,t,e,u,i,f)}}}}}}))}function a(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}function s(n,r,t,e,u,i,f){return 6===n.a?n.f(r,t,e,u,i,f):n(r)(t)(e)(u)(i)(f)}function b(n,r){for(var t,e=[],u=d(n,r,0,e);u&&(t=e.pop());u=d(t.a,t.b,0,e));return u}function d(n,r,t,e){if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&C(5),!1;if(t>100)return e.push($(n,r)),!0;for(var u in 0>n.$&&(n=sr(n),r=sr(r)),n)if(!d(n[u],r[u],t+1,e))return!1;return!0}function l(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=l(n.a,r.a))||(t=l(n.b,r.b))?t:l(n.c,r.c);for(;n.b&&r.b&&!(t=l(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var h=t((function(n,r){var t=l(n,r);return 0>t?or:t?ar:fr}));function $(n,r){return{a:n,b:r}}function g(n,r,t){return{a:n,b:r,c:t}}var p={$:0};function m(n,r){return{$:1,a:n,b:r}}var y=t(m);function w(n){for(var r=p,t=n.length;t--;)r=m(n[t],r);return r}function A(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var j=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),_=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,$(t,r)})),k=t((function(n,r){return r[n]})),N=e((function(n,r,t){for(var e=t.length,u=Array(e),i=0;e>i;i++)u[i]=t[i];return u[n]=r,u})),T=t((function(n,r){for(var t=r.length,e=Array(t+1),u=0;t>u;u++)e[u]=r[u];return e[t]=n,e})),E=e((function(n,r,t){for(var e=t.length,u=0;e>u;u++)r=a(n,t[u],r);return r})),x=e((function(n,r,t){for(var e=t.length-1;e>=0;e--)r=a(n,t[e],r);return r})),L=t((function(n,r){for(var t=r.length,e=Array(t),u=0;t>u;u++)e[u]=n(r[u]);return e})),q=e((function(n,r,t){return t.slice(n,r)})),F=e((function(n,r,t){var e=r.length,u=n-e;u>t.length&&(u=t.length);for(var i=Array(e+u),f=0;e>f;f++)i[f]=r[f];for(f=0;u>f;f++)i[f+e]=t[f];return i}));function C(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var B=t((function(n,r){var t=r%n;return 0===n?C(11):t>0&&0>n||0>t&&n>0?t+n:t}));var O=Math.ceil,z=Math.floor,I=Math.round,R=Math.sqrt,S=Math.log;var D=t((function(n,r){return r.join(n)}));function M(n){return n+""}function P(n){return{$:2,b:n}}P((function(n){return"number"!=typeof n?H("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?mr(n):!isFinite(n)||n%1?H("an INT",n):mr(n)})),P((function(n){return"boolean"==typeof n?mr(n):H("a BOOL",n)})),P((function(n){return"number"==typeof n?mr(n):H("a FLOAT",n)})),P((function(n){return mr(n)})),P((function(n){return"string"==typeof n?mr(n):n instanceof String?mr(n+""):H("a STRING",n)}));var W=t((function(n,r){return J(n,r)}));function J(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?mr(n.c):H("null",r);case 3:return V(r)?G(n.b,r,w):H("a LIST",r);case 4:return V(r)?G(n.b,r,Y):H("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return H("an OBJECT with a field named `"+t+"`",r);var e=J(n.b,r[t]);return Vr(e)?e:hr(a(gr,t,e.a));case 7:var u=n.e;if(!V(r))return H("an ARRAY",r);if(u>=r.length)return H("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=J(n.b,r[u]);return Vr(e)?e:hr(a(pr,u,e.a));case 8:if("object"!=typeof r||null===r||V(r))return H("an OBJECT",r);var i=p;for(var f in r)if(r.hasOwnProperty(f)){e=J(n.b,r[f]);if(!Vr(e))return hr(a(gr,f,e.a));i=m($(f,e.a),i)}return mr(Tr(i));case 9:for(var o=n.f,c=n.g,v=0;c.length>v;v++){e=J(c[v],r);if(!Vr(e))return e;o=o(e.a)}return mr(o);case 10:e=J(n.b,r);return Vr(e)?J(n.h(e.a),r):e;case 11:for(var s=p,b=n.g;b.b;b=b.b){e=J(b.a,r);if(Vr(e))return e;s=m(e.a,s)}return hr(yr(Tr(s)));case 1:return hr(a($r,n.a,r));case 0:return mr(n.a)}}function G(n,r,t){for(var e=r.length,u=Array(e),i=0;e>i;i++){var f=J(n,r[i]);if(!Vr(f))return hr(a(pr,i,f.a));u[i]=f.a}return mr(t(u))}function V(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function Y(n){return a(Gr,n.length,(function(r){return n[r]}))}function H(n,r){return hr(a($r,"Expecting "+n,r))}function K(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return K(n.b,r.b);case 6:return n.d===r.d&&K(n.b,r.b);case 7:return n.e===r.e&&K(n.b,r.b);case 9:return n.f===r.f&&Q(n.g,r.g);case 10:return n.h===r.h&&K(n.b,r.b);case 11:return Q(n.g,r.g)}}function Q(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!K(n[e],r[e]))return!1;return!0}function U(n){return{$:0,a:n}}function X(n){return{$:2,b:n,c:null}}var Z=t((function(n,r){return{$:3,b:n,d:r}}));var nn=0;function rn(n){var r={$:0,e:nn++,f:n,g:null,h:[]};return on(r),r}function tn(n){return X((function(r){r(U(rn(n)))}))}function en(n,r){n.h.push(r),on(n)}var un=t((function(n,r){return X((function(t){en(n,r),t(U(0))}))}));var fn=!1,an=[];function on(n){if(an.push(n),!fn){for(fn=!0;n=an.shift();)cn(n);fn=!1}}function cn(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,on(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function vn(n,r,t,e,u,i){var f=a(W,n,r?r.flags:void 0);Vr(f)||C(2);var o={},c=(f=t(f.a)).a,v=i(b,c),s=function(n,r){var t;for(var e in sn){var u=sn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=dn(u,r)}return t}(o,b);function b(n,r){v(c=(f=a(e,n,c)).a,r),yn(o,f.b,u(c))}return yn(o,f.b,u(c)),s?{ports:s}:{}}var sn={};function bn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function dn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,i=n.e,f=n.f;function v(n){return a(Z,v,{$:5,b:function(r){var a=r.a;return 0===r.$?o(u,t,a,n):i&&f?c(e,t,a.i,a.j,n):o(e,t,i?a.i:a.j,n)}})}return t.h=rn(a(Z,v,n.b))}var ln=t((function(n,r){return X((function(t){n.g(r),t(U(0))}))})),hn=t((function(n,r){return a(un,n.h,{$:0,a:r})}));function $n(n){return function(r){return{$:1,k:n,l:r}}}function gn(n){return{$:2,m:n}}var pn=[],mn=!1;function yn(n,r,t){if(pn.push({p:n,q:r,r:t}),!mn){mn=!0;for(var e;e=pn.shift();)wn(e.p,e.q,e.r);mn=!1}}function wn(n,r,t){var e={};for(var u in An(!0,r,e,null),An(!1,t,e,null),n)en(n[u],{$:"fx",a:e[u]||{i:p,j:p}})}function An(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return a(n?sn[r].e:sn[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:p,j:p},n?t.i=m(r,t.i):t.j=m(r,t.j),t}(n,i,t[u]));case 2:for(var f=r.m;f.b;f=f.b)An(n,f.a,t,e);return;case 3:return void An(n,r.o,t,{s:r.n,t:e})}}var jn;var _n="undefined"!=typeof document?document:{};function kn(n,r){n.appendChild(r)}function Nn(n){return{$:0,a:n}}var Tn=t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var f=e.a;i+=f.b||0,u.push(f)}return i+=u.length,{$:1,c:r,d:qn(t),e:u,f:n,b:i}}))})),En=Tn(void 0);t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var f=e.a;i+=f.b.b||0,u.push(f)}return i+=u.length,{$:2,c:r,d:qn(t),e:u,f:n,b:i}}))}))(void 0);var xn=t((function(n,r){return{$:"a3",n:n,o:r}}));var Ln;function qn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var f=r[e]||(r[e]={});"a3"===e&&"class"===u?Fn(f,u,i):f[u]=i}else"className"===u?Fn(r,u,i):r[u]=i}return r}function Fn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Cn(n,r){var t=n.$;if(5===t)return Cn(n.k||(n.k=n.m()),r);if(0===t)return _n.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(f=Cn(e,i)).elm_event_node_ref=i,f}if(3===t)return Bn(f=n.h(n.g),r,n.d),f;var f=n.f?_n.createElementNS(n.f,n.c):_n.createElement(n.c);jn&&"a"==n.c&&f.addEventListener("click",jn(f)),Bn(f,r,n.d);for(var a=n.e,o=0;a.length>o;o++)kn(f,Cn(1===t?a[o]:a[o].b,r));return f}function Bn(n,r,t){for(var e in t){var u=t[e];"a1"===e?On(n,u):"a0"===e?Rn(n,r,u):"a3"===e?zn(n,u):"a4"===e?In(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function On(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function zn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function In(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function Rn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],f=e[u];if(i){if(f){if(f.q.$===i.$){f.q=i;continue}n.removeEventListener(u,f)}f=Sn(r,i),n.addEventListener(u,f,Ln&&{passive:2>Hr(i)}),e[u]=f}else n.removeEventListener(u,f),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Ln=!0}}))}catch(n){}function Sn(n,r){function t(r){var e=t.q,u=J(e.a,r);if(Vr(u)){for(var i,f=Hr(e),a=u.a,o=f?3>f?a.a:a.y:a,c=1==f?a.b:3==f&&a.aJ,v=(c&&r.stopPropagation(),(2==f?a.b:3==f&&a.aD)&&r.preventDefault(),n);i=v.j;){if("function"==typeof i)o=i(o);else for(var s=i.length;s--;)o=i[s](o);v=v.p}v(o,c)}}return t.q=r,t}function Dn(n,r){return n.$==r.$&&K(n.a,r.a)}function Mn(n,r){var t=[];return Wn(n,r,t,0),t}function Pn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Wn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void Pn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var f=n.l,a=r.l,o=f.length,c=o===a.length;c&&o--;)c=f[o]===a[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Wn(n.k,r.k,v,0),void(v.length>0&&Pn(t,1,e,v));case 4:for(var s=n.j,b=r.j,d=!1,l=n.k;4===l.$;)d=!0,"object"!=typeof s?s=[s,l.j]:s.push(l.j),l=l.k;for(var h=r.k;4===h.$;)d=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return d&&s.length!==b.length?void Pn(t,0,e,r):((d?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Pn(t,2,e,b),void Wn(l,h,t,e+1));case 0:return void(n.a!==r.a&&Pn(t,3,e,r.a));case 1:return void Jn(n,r,t,e,Vn);case 2:return void Jn(n,r,t,e,Yn);case 3:if(n.h!==r.h)return void Pn(t,0,e,r);var $=Gn(n.d,r.d);$&&Pn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Pn(t,5,e,g))}}}function Jn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=Gn(n.d,r.d);i&&Pn(t,4,e,i),u(n,r,t,e)}else Pn(t,0,e,r)}function Gn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],f=r[u];i===f&&"value"!==u&&"checked"!==u||"a0"===t&&Dn(i,f)||((e=e||{})[u]=f)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var a=Gn(n[u],r[u]||{},u);a&&((e=e||{})[u]=a)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Vn(n,r,t,e){var u=n.e,i=r.e,f=u.length,a=i.length;f>a?Pn(t,6,e,{v:a,i:f-a}):a>f&&Pn(t,7,e,{v:f,e:i});for(var o=a>f?f:a,c=0;o>c;c++){var v=u[c];Wn(v,i[c],t,++e),e+=v.b||0}}function Yn(n,r,t,e){for(var u=[],i={},f=[],a=n.e,o=r.e,c=a.length,v=o.length,s=0,b=0,d=e;c>s&&v>b;){var l=(N=a[s]).a,h=(T=o[b]).a,$=N.b,g=T.b,p=void 0,m=void 0;if(l!==h){var y=a[s+1],w=o[b+1];if(y){var A=y.a,j=y.b;m=h===A}if(w){var _=w.a,k=w.b;p=l===_}if(p&&m)Wn($,k,u,++d),Hn(i,u,l,g,b,f),d+=$.b||0,Kn(i,u,l,j,++d),d+=j.b||0,s+=2,b+=2;else if(p)d++,Hn(i,u,h,g,b,f),Wn($,k,u,d),d+=$.b||0,s+=1,b+=2;else if(m)Kn(i,u,l,$,++d),d+=$.b||0,Wn(j,g,u,++d),d+=j.b||0,s+=2,b+=1;else{if(!y||A!==_)break;Kn(i,u,l,$,++d),Hn(i,u,h,g,b,f),d+=$.b||0,Wn(j,k,u,++d),d+=j.b||0,s+=2,b+=2}}else Wn($,g,u,++d),d+=$.b||0,s++,b++}for(;c>s;){var N;d++,Kn(i,u,(N=a[s]).a,$=N.b,d),d+=$.b||0,s++}for(;v>b;){var T,E=E||[];Hn(i,u,(T=o[b]).a,T.b,void 0,E),b++}(u.length>0||f.length>0||E)&&Pn(t,8,e,{w:u,x:f,y:E})}function Hn(n,r,t,e,u,i){var f=n[t];if(!f)return i.push({r:u,A:f={c:0,z:e,r:u,s:void 0}}),void(n[t]=f);if(1===f.c){i.push({r:u,A:f}),f.c=2;var a=[];return Wn(f.z,e,a,f.r),f.r=u,void(f.s.s={w:a,A:f})}Hn(n,r,t+"_elmW6BL",e,u,i)}function Kn(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var f=[];return Wn(e,i.z,f,u),void Pn(r,9,u,{w:f,A:i})}Kn(n,r,t+"_elmW6BL",e,u)}else{var a=Pn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:a}}}function Qn(n,r,t,e){!function n(r,t,e,u,i,f,a){var o=e[u],c=o.r;for(;c===i;){var v=o.$;if(1===v)Qn(r,t.k,o.s,a);else if(8===v){o.t=r,o.u=a,(s=o.s.w).length>0&&n(r,t,s,0,i,f,a)}else if(9===v){o.t=r,o.u=a;var s,b=o.s;if(b)b.A.s=r,(s=b.w).length>0&&n(r,t,s,0,i,f,a)}else o.t=r,o.u=a;if(u++,!(o=e[u])||(c=o.r)>f)return u}var d=t.$;if(4===d){for(var l=t.k;4===l.$;)l=l.k;return n(r,l,e,u,i+1,f,r.elm_event_node_ref)}for(var h=t.e,$=r.childNodes,g=0;h.length>g;g++){i++;var p=1===d?h[g]:h[g].b,m=i+(p.b||0);if(c>=i&&m>=c&&(u=n($[g],p,e,u,i,m,a),!(o=e[u])||(c=o.r)>f))return u;i=m}return u}(n,r,t,0,0,r.b,e)}function Un(n,r,t,e){return 0===t.length?n:(Qn(n,r,t,e),Xn(n,t))}function Xn(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,i=Zn(u,e);u===n&&(n=i)}return n}function Zn(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=Cn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return Bn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Xn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(Cn(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var f=t.A;return void 0!==f.r&&n.parentNode.removeChild(n),f.s=Xn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=_n.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;kn(t,2===u.c?u.s:Cn(u.z,r.u))}return t}(t.y,r);n=Xn(n,t.w);for(var u=t.x,i=0;u.length>i;i++){var f=u[i],a=f.A,o=2===a.c?a.s:Cn(a.z,r.u);n.insertBefore(o,n.childNodes[f.r])}e&&kn(n,e);return n}(n,r);case 5:return r.s(n);default:C(10)}}function nr(n){if(3===n.nodeType)return Nn(n.textContent);if(1!==n.nodeType)return Nn("");for(var r=p,t=n.attributes,e=t.length;e--;){var u=t[e];r=m(a(xn,u.name,u.value),r)}var i=n.tagName.toLowerCase(),f=p,c=n.childNodes;for(e=c.length;e--;)f=m(nr(c[e]),f);return o(En,i,r,f)}var rr=u((function(n,r,t,e){return vn(r,e,n.b1,n.cw,n.cq,(function(r,t){var u=n.cx,i=e.node,f=nr(i);return er(t,(function(n){var t=u(n),e=Mn(f,t);i=Un(i,f,e,r),f=t}))}))})),tr=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function er(n,r){r(n);var t=0;function e(){t=1===t?0:(tr(e),r(n),1)}return function(u,i){n=u,i?(r(n),2===t&&(t=1)):(0===t&&tr(e),t=2)}}var ur={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var ir=t((function(n,r){return X((function(){var t=setInterval((function(){rn(r)}),n);return function(){clearInterval(t)}}))}));var fr=1,ar=2,or=0,cr=y,vr=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,i=o(n,t.b,t.c,o(vr,n,r,t.e));n=u,r=i,t=e}})),sr=function(n){return o(vr,e((function(n,r,t){return a(cr,$(n,r),t)})),p,n)},br=x,dr=e((function(n,r,e){var u=e.c,i=e.d,f=t((function(r,t){return o(br,r.$?n:f,t,r.a)}));return o(br,f,o(br,n,r,i),u)})),lr=function(n){return o(dr,cr,p,n)},hr=function(n){return{$:1,a:n}},$r=t((function(n,r){return{$:3,a:n,b:r}})),gr=t((function(n,r){return{$:0,a:n,b:r}})),pr=t((function(n,r){return{$:1,a:n,b:r}})),mr=function(n){return{$:0,a:n}},yr=function(n){return{$:2,a:n}},wr=function(n){return{$:0,a:n}},Ar={$:1},jr=t((function(n,r){return a(D,n,A(r))})),_r=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=a(n,t.a,r);n=u,r=i,t=e}})),kr=e((function(n,r,t){for(;;){if(l(n,r)>=1)return t;var e=n,u=r-1,i=a(cr,r,t);n=e,r=u,t=i}})),Nr=t((function(n,r){return o(kr,n,r,p)})),Tr=function(n){return o(_r,cr,p,n)},Er=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),xr=[],Lr=O,qr=t((function(n,r){return S(r)/S(n)})),Fr=Lr(a(qr,2,32)),Cr=c(Er,0,Fr,xr,xr),Br=j,Or=function(n){return{$:1,a:n}},zr=z,Ir=function(n){return n.length},Rr=t((function(n,r){return l(n,r)>0?n:r})),Sr=function(n){return{$:0,a:n}},Dr=_,Mr=t((function(n,r){for(;;){var t=a(Dr,32,n),e=t.b,u=a(cr,Sr(t.a),r);if(!e.b)return Tr(u);n=e,r=u}})),Pr=t((function(n,r){for(;;){var t=Lr(r/32);if(1===t)return a(Dr,32,n).a;n=a(Mr,n,p),r=t}})),Wr=t((function(n,r){if(r.a){var t=32*r.a,e=zr(a(qr,32,t-1)),u=n?Tr(r.d):r.d,i=a(Pr,u,r.a);return c(Er,Ir(r.c)+t,a(Rr,5,e*Fr),i,r.c)}return c(Er,Ir(r.c),Fr,xr,r.c)})),Jr=i((function(n,r,t,e,u){for(;;){if(0>r)return a(Wr,!1,{d:e,a:t/32|0,c:u});var i=Or(o(Br,32,r,n));n=n,r=r-32,t=t,e=a(cr,i,e),u=u}})),Gr=t((function(n,r){if(n>0){var t=n%32;return v(Jr,r,n-t-32,n,p,o(Br,t,n-t,r))}return Cr})),Vr=function(n){return!n.$},Yr=function(n){return{$:0,a:n}},Hr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Kr=function(n){return n},Qr=U,Ur=Qr(0),Xr=u((function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var f=i.a,v=i.b;if(v.b){var s=v.a,b=v.b;if(b.b){var d=b.b;return a(n,u,a(n,f,a(n,s,a(n,b.a,t>500?o(_r,n,r,Tr(d)):c(Xr,n,r,t+1,d)))))}return a(n,u,a(n,f,a(n,s,r)))}return a(n,u,a(n,f,r))}return a(n,u,r)}return r})),Zr=e((function(n,r,t){return c(Xr,n,r,0,t)})),nt=t((function(n,r){return o(Zr,t((function(r,t){return a(cr,n(r),t)})),p,r)})),rt=Z,tt=t((function(n,r){return a(rt,(function(r){return Qr(n(r))}),r)})),et=e((function(n,r,t){return a(rt,(function(r){return a(rt,(function(t){return Qr(a(n,r,t))}),t)}),r)})),ut=function(n){return o(Zr,et(cr),Qr(p),n)},it=ln,ft=t((function(n,r){var t=r;return tn(a(rt,it(n),t))}));sn.Task=bn(Ur,e((function(n,r){return a(tt,(function(){return 0}),ut(a(nt,ft(n),r)))})),e((function(){return Qr(0)})),t((function(n,r){return a(tt,n,r)})));$n("Task");var at,ot=rr,ct=gn(p),vt=gn,st=t((function(n,r){return{$:0,a:n,b:r}})),bt=t((function(n,r){return{bp:r,bx:n}})),dt={$:-2},lt=dt,ht=Qr(a(bt,lt,lt)),$t=h,gt=t((function(n,r){n:for(;;){if(-2===r.$)return Ar;var t=r.c,e=r.d,u=r.e;switch(a($t,n,r.b)){case 0:n=n,r=e;continue n;case 1:return wr(t);default:n=n,r=u;continue n}}})),pt=i((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),mt=i((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v(pt,n,r,t,e,u);var i=e.d;s=e.e;return v(pt,0,e.b,e.c,v(pt,1,i.b,i.c,i.d,i.e),v(pt,1,r,t,s,u))}var f=u.b,a=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return v(pt,n,f,a,v(pt,0,r,t,e,o),c);var s;return v(pt,0,r,t,v(pt,1,e.b,e.c,e.d,s=e.e),v(pt,1,f,a,o,c))})),yt=e((function(n,r,t){if(-2===t.$)return v(pt,0,n,r,dt,dt);var e=t.a,u=t.b,i=t.c,f=t.d,c=t.e;switch(a($t,n,u)){case 0:return v(mt,e,u,i,o(yt,n,r,f),c);case 1:return v(pt,e,u,r,f,c);default:return v(mt,e,u,i,f,o(yt,n,r,c))}})),wt=e((function(n,r,t){var e=o(yt,n,r,t);if(-1!==e.$||e.a)return e;return v(pt,1,e.b,e.c,e.d,e.e)})),At=t((function(n,r){var t=n.a,e=n.b,u=a(gt,t,r);return o(wt,t,1===u.$?w([e]):a(cr,e,u.a),r)})),jt=function(n){return X((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(U(0))}))},_t=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,i=o(n,t.b,t.c,o(_t,n,r,t.d));n=u,r=i,t=e}})),kt=f((function(n,r,u,i,f,a){var v=o(_t,e((function(t,e,i){n:for(;;){var f=i.a,a=i.b;if(f.b){var v=f.a,s=v.a,b=v.b,d=f.b;if(0>l(s,t)){t=t,e=e,i=$(d,o(n,s,b,a));continue n}return l(s,t)>0?$(f,o(u,t,e,a)):$(d,c(r,s,b,e,a))}return $(f,o(u,t,e,a))}})),$(sr(i),a),f),s=v.a,b=v.b;return o(_r,t((function(r,t){return o(n,r.a,r.b,t)})),b,s)})),Nt=hn,Tt=ir,Et=tn,xt=e((function(n,r,t){if(r.b){var e=r.a,u=r.b,i=Et(a(Tt,e,a(Nt,n,e)));return a(rt,(function(r){return o(xt,n,u,o(wt,e,r,t))}),i)}return Qr(t)})),Lt=e((function(n,r,t){var i=t.bp,f=e((function(n,r,t){var e=t.c;return g(t.a,t.b,a(rt,(function(){return e}),jt(r)))})),c=o(_r,At,lt,r),v=s(kt,e((function(n,r,t){var e=t.b,u=t.c;return g(a(cr,n,t.a),e,u)})),u((function(n,r,t,e){var u=e.c;return g(e.a,o(wt,n,t,e.b),u)})),f,c,i,g(p,lt,Qr(0))),b=v.a,d=v.b;return a(rt,(function(n){return Qr(a(bt,c,n))}),a(rt,(function(){return o(xt,n,b,d)}),v.c))})),qt=(at=Kr,X((function(n){n(U(at(Date.now())))}))),Ft=e((function(n,r,t){var e=a(gt,r,t.bx);if(1===e.$)return Qr(t);var u=e.a;return a(rt,(function(){return Qr(t)}),a(rt,(function(r){return ut(a(nt,(function(t){return a(it,n,t(r))}),u))}),qt))})),Ct=e((function(n,r,t){return n(r(t))}));sn.Time=bn(ht,Lt,Ft,0,t((function(n,r){return a(st,r.a,a(Ct,n,r.b))})));var Bt,Ot,zt,It,Rt,St=$n("Time"),Dt=t((function(n,r){return St(a(st,n,r))})),Mt=i((function(n,r,t,e,u){var i;return t+(0>(i=(u-n)/(r-n))?-i:i)*(e-t)})),Pt=R,Wt=.5*(Pt(3)-1),Jt=(3-Pt(3))/6,Gt=t((function(n,r){return l(n,r)>0?$(1,0):$(0,1)})),Vt=4294967295>>>32-Fr,Yt=k,Ht=e((function(n,r,t){for(;;){var e=a(Yt,Vt&r>>>n,t);if(e.$)return a(Yt,Vt&r,e.a);n=n-Fr,r=r,t=e.a}})),Kt=t((function(n,r){var t=r.a,e=r.b,u=r.c,i=r.d;return 0>n||l(n,t)>-1?Ar:l(n,function(n){return n>>>5<<5}(t))>-1?wr(a(Yt,Vt&n,i)):wr(o(Ht,e,n,u))})),Qt=t((function(n,r){var t=a(Kt,r,n);return t.$?0:t.a})),Ut=e((function(n,r,t){for(;;){var e=a(Dr,32,n),u=e.a,i=e.b;if(0>l(Ir(u),32))return a(Wr,!0,{d:r,a:t,c:u});n=i,r=a(cr,Or(u),r),t=t+1}})),Xt=function(n){return n.b?o(Ut,n,p,0):Cr},Zt=Xt(w([1,1,0,-1,1,0,1,-1,0,-1,-1,0,1,0,1,-1,0,1,1,0,-1,-1,0,-1,0,1,1,0,-1,1,0,1,-1,0,-1,-1])),ne=f((function(n,r,t,e,u,i){var f=.5-n*n-r*r;if(0>f)return 0;var o=f*f,c=3*a(Qt,i,t+a(Qt,u,e));return o*o*(a(Qt,Zt,c)*n+a(Qt,Zt,c+1)*r)})),re=e((function(n,r,t){var e=n.V,u=n.W,i=(r+t)*Wt,f=zr(t+i),o=255&f,c=zr(r+i),v=255&c,b=(c+f)*Jt,d=t-(f-b),l=d-1+2*Jt,h=r-(c-b),$=s(ne,h,d,v,o,e,u),g=s(ne,h-1+2*Jt,l,v+1,o+1,e,u),p=a(Gt,h,d),m=p.a,y=p.b;return 70*($+s(ne,h-m+Jt,d-y+Jt,v+m,o+y,e,u)+g)})),te=t((function(n,r){return o(re,n,r,0)})),ee=t((function(n,r){return{$:0,a:n,b:r}})),ue=function(n){var r=n.b;return a(ee,1664525*n.a+r>>>0,r)},ie=F,fe=q,ae=t((function(n,r){var t=Ir(n),e=32-Ir(r.c)-t,u=o(ie,32,r.c,n);return 0>e?{d:a(cr,Or(u),r.d),a:r.a+1,c:o(fe,e,t,n)}:e?{d:r.d,a:r.a,c:u}:{d:a(cr,Or(u),r.d),a:r.a+1,c:xr}})),oe=T,ce=function(n){return[n]},ve=N,se=u((function(n,r,t,e){var u=Vt&r>>>n;if(l(u,Ir(e))>-1){if(5===n)return a(oe,Or(t),e);var i=Sr(c(se,n-Fr,r,t,xr));return a(oe,i,e)}var f=a(Yt,u,e);if(f.$){i=Sr(c(se,n-Fr,r,t,ce(f)));return o(ve,u,i,e)}var i=Sr(c(se,n-Fr,r,t,f.a));return o(ve,u,i,e)})),be=t((function(n,r){var t=r.a,e=r.b,u=r.c,i=Ir(r.d),f=Ir(n),a=t+(f-i);if(b(f,32)){if(l(a>>>Fr,1<<e)>0){var o=e+Fr,v=c(se,o,t,n,ce(Sr(u)));return c(Er,a,o,v,xr)}return c(Er,a,e,c(se,e,t,n,u),xr)}return c(Er,a,e,u,n)})),de=t((function(n,r){var t=r.d,e=Ir(n),u=32-Ir(t)-e,i=a(be,o(ie,32,t,n),r);return 0>u?a(be,o(fe,u,e,n),i):i})),le=E,he=t((function(n,r){var e,u=r.c,i=r.d;return 1>l(r.a,128)?a(de,i,o(le,e=t((function(n,r){return n.$?a(de,n.a,r):o(le,e,r,n.a)})),n,u)):a(Wr,!0,a(ae,i,o(le,e=t((function(n,r){return n.$?a(ae,n.a,r):o(le,e,r,n.a)})),function(n){var r=n.a,e=n.c,u=n.d,i=t((function(n,r){return n.$?a(cr,n,r):o(le,i,r,n.a)}));return{d:o(le,i,p,e),a:r/32|0,c:u}}(n),u)))})),$e=L,ge=t((function(n,r){var t=r.d,e=function(r){return r.$?Or(a($e,n,r.a)):Sr(a($e,e,r.a))};return c(Er,r.a,r.b,a($e,e,r.c),a($e,n,t))})),pe=function(n){return a(ge,(function(n){return n%12}),n)},me=e((function(n,r,t){return r(n(t))})),ye=function(n){var r=n.a,t=277803737*(r^r>>>4+(r>>>28));return(t>>>22^t)>>>0},we=t((function(n,r){return function(t){var e=0>l(n,r)?$(n,r):$(r,n),u=e.a,i=e.b-u+1;if(i-1&i){var f=(-i>>>0)%i>>>0;return function(n){for(;;){var r=ye(n),t=ue(n);if(l(r,f)>=0)return $(r%i+u,t);n=t}}(t)}return $(((i-1&ye(t))>>>0)+u,ue(t))}})),Ae=function(n){return n.a},je=u((function(n,r,t,e){for(;;){if(1>r)return $(n,e);var u=t(e),i=u.b;n=a(cr,u.a,n),r=r-1,t=t,e=i}})),_e=t((function(n,r){var t=r;return function(r){return c(je,p,n,t,r)}})),ke=t((function(n,r){var t=r;return function(r){var e=t(r),u=e.b;return $(n(e.a),u)}})),Ne=t((function(n,r){for(;;){var t=a(gt,n,r);if(1===t.$)return n;var e=t.a;if(b(n,e))return n;n=e,r=r}})),Te=t((function(n,r){return a(Ne,n,r.b)})),Ee=B,xe=t((function(n,r){return{$:0,a:n,b:r}})),Le=a(xe,0,lt),qe=t((function(n,r){var t=a(gt,n,r);if(1===t.$)return $(n,o(wt,n,n,r));var e=t.a;if(b(n,e))return $(n,r);var u=a(qe,e,r),i=u.a;return $(i,o(wt,n,i,u.b))})),Fe=e((function(n,r,t){var e=t.a,u=a(qe,n,t.b),i=u.a,f=a(qe,r,u.b),c=f.a,v=f.b;return b(i,c)?a(xe,e,v):a(xe,e+1,o(wt,i,c,v))})),Ce=t((function(n,r){var e=Ee(Ae(n)),u=t((function(r,t){var u=t.a,i=t.b,f=a(Te,r,u),c=a(Te,e(f+1),u),v=a(Kt,f,n);if(1===v.$)return $(u,i);var s=v.a;return $(o(Fe,f,c,u),a(cr,s,i))}));return n.a?o(Zr,u,$(Le,p),r).b:p})),Be=(Bt=Xt(a(Nr,0,255)),Ot=Ae(Bt),a(ke,a(me,Ce(Bt),Xt),a(_e,Ot,a(we,0,Ot-1)))),Oe=t((function(n,r){return n(r)})),ze=function(n){var r,t,e,u=(e=(r=a(Oe,Be,n)).b,$(a(he,t=r.a,Xt(Tr(lr(t)))),e)),i=u.a,f=u.b;return $({V:i,W:pe(i)},f)}((zt=42,It=ue(a(ee,0,1013904223)),ue(a(ee,It.a+zt>>>0,It.b)))).a,Ie=t((function(n,r){var t,e=r.am+.01;return $(function(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}(r,{T:a(cr,$(e,(t=e,v(Mt,-1,1,300,600,a(te,ze,t)))),r.T),am:e}),ct)})),Re=c(u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),0,0,0,1),Se=t((function(n,r){return a(xn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))})),De=M,Me=I,Pe=function(n){var r,t=n.b,e=n.c,u=n.d,i=function(n){return Me(1e4*n)/100};return r=w(["rgba(",De(i(n.a)),"%,",De(i(t)),"%,",De(i(e)),"%,",De(function(n){return Me(1e3*n)/1e3}(u)),")"]),a(jr,"",r)},We=function(n){return n.$?"none":Pe(n.a)},Je=a(Ct,Se("fill"),We),Ge=function(n){switch(n.$){case 0:return De(n.a)+"cm";case 1:return De(n.a)+"em";case 2:return De(n.a)+"ex";case 3:return De(n.a)+"in";case 4:return De(n.a)+"mm";case 5:return De(n.a);case 6:return De(n.a)+"pc";case 7:return De(n.a)+"%";case 8:return De(n.a)+"pt";default:return De(n.a)+"px"}},Ve=function(n){return a(Se,"height",Ge(n))},Ye=function(n){return{$:9,a:n}},He=Tn(function(n){return"script"==n?"p":n}("http://www.w3.org/2000/svg")),Ke=He("rect"),Qe=function(n){return a(Se,"width",Ge(n))},Ue=function(n){return a(Se,"x",Ge(n))},Xe=function(n){return a(Se,"y",Ge(n))},Ze=function(n){var r,t=n.a,e=n.b;return a(Ke,w([Ue(Ye(0)),Xe(Ye(100*t)),Qe(Ye(e)),Ve(Ye(1)),Je((r=Re,{$:0,a:r}))]),p)},nu=He("svg"),ru=u((function(n,r,t,e){return a(Se,"viewBox",a(jr," ",a(nt,De,w([n,r,t,e]))))})),tu=ot({b1:function(){return $({T:p,am:0},ct)},cq:function(){return vt(w([a(Dt,10,Kr)]))},cw:Ie,cx:function(n){return a(nu,w([Qe(Ye(600)),Ve(Ye(600)),c(ru,0,0,600,600)]),a(nt,Ze,n.T))}});Rt={Noise:{Perlin:{init:tu(Yr(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?C(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,Rt):n.Elm=Rt}(window);