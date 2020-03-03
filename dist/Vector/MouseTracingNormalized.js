!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function i(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(i){return n(r,t,e,u,i)}}}}}))}function a(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(i){return function(a){return n(r,t,e,u,i,a)}}}}}}))}function f(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}function s(n,r,t,e,u,i,a){return 6===n.a?n.f(r,t,e,u,i,a):n(r)(t)(e)(u)(i)(a)}function b(n,r){for(var t,e=[],u=l(n,r,0,e);u&&(t=e.pop());u=l(t.a,t.b,0,e));return u}function l(n,r,t,e){if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&F(5),!1;if(t>100)return e.push(g(n,r)),!0;for(var u in 0>n.$&&(n=dr(n),r=dr(r)),n)if(!l(n[u],r[u],t+1,e))return!1;return!0}function d(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=d(n.a,r.a))||(t=d(n.b,r.b))?t:d(n.c,r.c);for(;n.b&&r.b&&!(t=d(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var h=t((function(n,r){var t=d(n,r);return 0>t?sr:t?vr:cr}));function g(n,r){return{a:n,b:r}}function $(n,r,t){return{a:n,b:r,c:t}}function p(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=y(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=y(n.a,r);return t}var m={$:0};function y(n,r){return{$:1,a:n,b:r}}var w=t(y);function A(n){for(var r=m,t=n.length;t--;)r=y(n[t],r);return r}function j(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var k=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),_=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,g(t,r)}));function F(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var E=Math.ceil,N=Math.floor,L=Math.round,T=Math.log;var x=t((function(n,r){return r.join(n)}));function B(n){return n+""}function q(n){return{$:2,b:n}}var C=q((function(n){return"number"!=typeof n?Y("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?mr(n):!isFinite(n)||n%1?Y("an INT",n):mr(n)}));q((function(n){return"boolean"==typeof n?mr(n):Y("a BOOL",n)})),q((function(n){return"number"==typeof n?mr(n):Y("a FLOAT",n)})),q((function(n){return mr(n)})),q((function(n){return"string"==typeof n?mr(n):n instanceof String?mr(n+""):Y("a STRING",n)}));var O=t((function(n,r){return{$:6,d:n,b:r}}));function R(n,r){return{$:9,f:n,g:r}}var z=t((function(n,r){return R(n,[r])})),M=e((function(n,r,t){return R(n,[r,t])})),S=t((function(n,r){return I(n,r)}));function I(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?mr(n.c):Y("null",r);case 3:return D(r)?W(n.b,r,A):Y("a LIST",r);case 4:return D(r)?W(n.b,r,P):Y("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return Y("an OBJECT with a field named `"+t+"`",r);var e=I(n.b,r[t]);return Dr(e)?e:hr(f($r,t,e.a));case 7:var u=n.e;if(!D(r))return Y("an ARRAY",r);if(u>=r.length)return Y("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=I(n.b,r[u]);return Dr(e)?e:hr(f(pr,u,e.a));case 8:if("object"!=typeof r||null===r||D(r))return Y("an OBJECT",r);var i=m;for(var a in r)if(r.hasOwnProperty(a)){e=I(n.b,r[a]);if(!Dr(e))return hr(f($r,a,e.a));i=y(g(a,e.a),i)}return mr(_r(i));case 9:for(var o=n.f,c=n.g,v=0;c.length>v;v++){e=I(c[v],r);if(!Dr(e))return e;o=o(e.a)}return mr(o);case 10:e=I(n.b,r);return Dr(e)?I(n.h(e.a),r):e;case 11:for(var s=m,b=n.g;b.b;b=b.b){e=I(b.a,r);if(Dr(e))return e;s=y(e.a,s)}return hr(yr(_r(s)));case 1:return hr(f(gr,n.a,r));case 0:return mr(n.a)}}function W(n,r,t){for(var e=r.length,u=Array(e),i=0;e>i;i++){var a=I(n,r[i]);if(!Dr(a))return hr(f(pr,i,a.a));u[i]=a.a}return mr(t(u))}function D(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function P(n){return f(Wr,n.length,(function(r){return n[r]}))}function Y(n,r){return hr(f(gr,"Expecting "+n,r))}function G(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return G(n.b,r.b);case 6:return n.d===r.d&&G(n.b,r.b);case 7:return n.e===r.e&&G(n.b,r.b);case 9:return n.f===r.f&&J(n.g,r.g);case 10:return n.h===r.h&&G(n.b,r.b);case 11:return J(n.g,r.g)}}function J(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!G(n[e],r[e]))return!1;return!0}function Z(n){return{$:0,a:n}}function H(n){return{$:2,b:n,c:null}}var V=t((function(n,r){return{$:3,b:n,d:r}}));var X=0;function K(n){var r={$:0,e:X++,f:n,g:null,h:[]};return en(r),r}function Q(n){return H((function(r){r(Z(K(n)))}))}function U(n,r){n.h.push(r),en(n)}var nn=t((function(n,r){return H((function(t){U(n,r),t(Z(0))}))}));var rn=!1,tn=[];function en(n){if(tn.push(n),!rn){for(rn=!0;n=tn.shift();)un(n);rn=!1}}function un(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,en(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function an(n,r,t,e,u,i){var a=f(S,n,r?r.flags:void 0);Dr(a)||F(2);var o={},c=(a=t(a.a)).a,v=i(b,c),s=function(n,r){var t;for(var e in fn){var u=fn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=cn(u,r)}return t}(o,b);function b(n,r){v(c=(a=f(e,n,c)).a,r),hn(o,a.b,u(c))}return hn(o,a.b,u(c)),s?{ports:s}:{}}var fn={};function on(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function cn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,i=n.e,a=n.f;function v(n){return f(V,v,{$:5,b:function(r){var f=r.a;return 0===r.$?o(u,t,f,n):i&&a?c(e,t,f.i,f.j,n):o(e,t,i?f.i:f.j,n)}})}return t.h=K(f(V,v,n.b))}var vn=t((function(n,r){return H((function(t){n.g(r),t(Z(0))}))})),sn=t((function(n,r){return f(nn,n.h,{$:0,a:r})}));function bn(n){return function(r){return{$:1,k:n,l:r}}}var ln=[],dn=!1;function hn(n,r,t){if(ln.push({p:n,q:r,r:t}),!dn){dn=!0;for(var e;e=ln.shift();)gn(e.p,e.q,e.r);dn=!1}}function gn(n,r,t){var e={};for(var u in $n(!0,r,e,null),$n(!1,t,e,null),n)U(n[u],{$:"fx",a:e[u]||{i:m,j:m}})}function $n(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return f(n?fn[r].e:fn[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:m,j:m},n?t.i=y(r,t.i):t.j=y(r,t.j),t}(n,i,t[u]));case 2:for(var a=r.m;a.b;a=a.b)$n(n,a.a,t,e);return;case 3:return void $n(n,r.o,t,{s:r.n,t:e})}}var pn;var mn="undefined"!=typeof document?document:{};function yn(n,r){n.appendChild(r)}function wn(n){return{$:0,a:n}}var An=t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var a=e.a;i+=a.b||0,u.push(a)}return i+=u.length,{$:1,c:r,d:Fn(t),e:u,f:n,b:i}}))})),jn=An(void 0);t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var a=e.a;i+=a.b.b||0,u.push(a)}return i+=u.length,{$:2,c:r,d:Fn(t),e:u,f:n,b:i}}))}))(void 0);var kn=t((function(n,r){return{$:"a3",n:n,o:r}}));var _n;function Fn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var a=r[e]||(r[e]={});"a3"===e&&"class"===u?En(a,u,i):a[u]=i}else"className"===u?En(r,u,i):r[u]=i}return r}function En(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Nn(n,r){var t=n.$;if(5===t)return Nn(n.k||(n.k=n.m()),r);if(0===t)return mn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(a=Nn(e,i)).elm_event_node_ref=i,a}if(3===t)return Ln(a=n.h(n.g),r,n.d),a;var a=n.f?mn.createElementNS(n.f,n.c):mn.createElement(n.c);pn&&"a"==n.c&&a.addEventListener("click",pn(a)),Ln(a,r,n.d);for(var f=n.e,o=0;f.length>o;o++)yn(a,Nn(1===t?f[o]:f[o].b,r));return a}function Ln(n,r,t){for(var e in t){var u=t[e];"a1"===e?Tn(n,u):"a0"===e?qn(n,r,u):"a3"===e?xn(n,u):"a4"===e?Bn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function Tn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function xn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Bn(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function qn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],a=e[u];if(i){if(a){if(a.q.$===i.$){a.q=i;continue}n.removeEventListener(u,a)}a=Cn(r,i),n.addEventListener(u,a,_n&&{passive:2>Jr(i)}),e[u]=a}else n.removeEventListener(u,a),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){_n=!0}}))}catch(n){}function Cn(n,r){function t(r){var e=t.q,u=I(e.a,r);if(Dr(u)){for(var i,a=Jr(e),f=u.a,o=a?3>a?f.a:f.y:f,c=1==a?f.b:3==a&&f.aH,v=(c&&r.stopPropagation(),(2==a?f.b:3==a&&f.aB)&&r.preventDefault(),n);i=v.j;){if("function"==typeof i)o=i(o);else for(var s=i.length;s--;)o=i[s](o);v=v.p}v(o,c)}}return t.q=r,t}function On(n,r){return n.$==r.$&&G(n.a,r.a)}function Rn(n,r){var t=[];return Mn(n,r,t,0),t}function zn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Mn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void zn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var a=n.l,f=r.l,o=a.length,c=o===f.length;c&&o--;)c=a[o]===f[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Mn(n.k,r.k,v,0),void(v.length>0&&zn(t,1,e,v));case 4:for(var s=n.j,b=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return l&&s.length!==b.length?void zn(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||zn(t,2,e,b),void Mn(d,h,t,e+1));case 0:return void(n.a!==r.a&&zn(t,3,e,r.a));case 1:return void Sn(n,r,t,e,Wn);case 2:return void Sn(n,r,t,e,Dn);case 3:if(n.h!==r.h)return void zn(t,0,e,r);var g=In(n.d,r.d);g&&zn(t,4,e,g);var $=r.i(n.g,r.g);return void($&&zn(t,5,e,$))}}}function Sn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=In(n.d,r.d);i&&zn(t,4,e,i),u(n,r,t,e)}else zn(t,0,e,r)}function In(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],a=r[u];i===a&&"value"!==u&&"checked"!==u||"a0"===t&&On(i,a)||((e=e||{})[u]=a)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=In(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Wn(n,r,t,e){var u=n.e,i=r.e,a=u.length,f=i.length;a>f?zn(t,6,e,{v:f,i:a-f}):f>a&&zn(t,7,e,{v:a,e:i});for(var o=f>a?a:f,c=0;o>c;c++){var v=u[c];Mn(v,i[c],t,++e),e+=v.b||0}}function Dn(n,r,t,e){for(var u=[],i={},a=[],f=n.e,o=r.e,c=f.length,v=o.length,s=0,b=0,l=e;c>s&&v>b;){var d=(F=f[s]).a,h=(E=o[b]).a,g=F.b,$=E.b,p=void 0,m=void 0;if(d!==h){var y=f[s+1],w=o[b+1];if(y){var A=y.a,j=y.b;m=h===A}if(w){var k=w.a,_=w.b;p=d===k}if(p&&m)Mn(g,_,u,++l),Pn(i,u,d,$,b,a),l+=g.b||0,Yn(i,u,d,j,++l),l+=j.b||0,s+=2,b+=2;else if(p)l++,Pn(i,u,h,$,b,a),Mn(g,_,u,l),l+=g.b||0,s+=1,b+=2;else if(m)Yn(i,u,d,g,++l),l+=g.b||0,Mn(j,$,u,++l),l+=j.b||0,s+=2,b+=1;else{if(!y||A!==k)break;Yn(i,u,d,g,++l),Pn(i,u,h,$,b,a),l+=g.b||0,Mn(j,_,u,++l),l+=j.b||0,s+=2,b+=2}}else Mn(g,$,u,++l),l+=g.b||0,s++,b++}for(;c>s;){var F;l++,Yn(i,u,(F=f[s]).a,g=F.b,l),l+=g.b||0,s++}for(;v>b;){var E,N=N||[];Pn(i,u,(E=o[b]).a,E.b,void 0,N),b++}(u.length>0||a.length>0||N)&&zn(t,8,e,{w:u,x:a,y:N})}function Pn(n,r,t,e,u,i){var a=n[t];if(!a)return i.push({r:u,A:a={c:0,z:e,r:u,s:void 0}}),void(n[t]=a);if(1===a.c){i.push({r:u,A:a}),a.c=2;var f=[];return Mn(a.z,e,f,a.r),a.r=u,void(a.s.s={w:f,A:a})}Pn(n,r,t+"_elmW6BL",e,u,i)}function Yn(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var a=[];return Mn(e,i.z,a,u),void zn(r,9,u,{w:a,A:i})}Yn(n,r,t+"_elmW6BL",e,u)}else{var f=zn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function Gn(n,r,t,e){!function n(r,t,e,u,i,a,f){var o=e[u],c=o.r;for(;c===i;){var v=o.$;if(1===v)Gn(r,t.k,o.s,f);else if(8===v){o.t=r,o.u=f,(s=o.s.w).length>0&&n(r,t,s,0,i,a,f)}else if(9===v){o.t=r,o.u=f;var s,b=o.s;if(b)b.A.s=r,(s=b.w).length>0&&n(r,t,s,0,i,a,f)}else o.t=r,o.u=f;if(u++,!(o=e[u])||(c=o.r)>a)return u}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,i+1,a,r.elm_event_node_ref)}for(var h=t.e,g=r.childNodes,$=0;h.length>$;$++){i++;var p=1===l?h[$]:h[$].b,m=i+(p.b||0);if(c>=i&&m>=c&&(u=n(g[$],p,e,u,i,m,f),!(o=e[u])||(c=o.r)>a))return u;i=m}return u}(n,r,t,0,0,r.b,e)}function Jn(n,r,t,e){return 0===t.length?n:(Gn(n,r,t,e),Zn(n,t))}function Zn(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,i=Hn(u,e);u===n&&(n=i)}return n}function Hn(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=Nn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return Ln(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Zn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(Nn(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var a=t.A;return void 0!==a.r&&n.parentNode.removeChild(n),a.s=Zn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=mn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;yn(t,2===u.c?u.s:Nn(u.z,r.u))}return t}(t.y,r);n=Zn(n,t.w);for(var u=t.x,i=0;u.length>i;i++){var a=u[i],f=a.A,o=2===f.c?f.s:Nn(f.z,r.u);n.insertBefore(o,n.childNodes[a.r])}e&&yn(n,e);return n}(n,r);case 5:return r.s(n);default:F(10)}}function Vn(n){if(3===n.nodeType)return wn(n.textContent);if(1!==n.nodeType)return wn("");for(var r=m,t=n.attributes,e=t.length;e--;){var u=t[e];r=y(f(kn,u.name,u.value),r)}var i=n.tagName.toLowerCase(),a=m,c=n.childNodes;for(e=c.length;e--;)a=y(Vn(c[e]),a);return o(jn,i,r,a)}var Xn=u((function(n,r,t,e){return an(r,e,n.bZ,n.cs,n.cm,(function(r,t){var u=n.ct,i=e.node,a=Vn(i);return Qn(t,(function(n){var t=u(n),e=Rn(a,t);i=Jn(i,a,e,r),a=t}))}))})),Kn=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function Qn(n,r){r(n);var t=0;function e(){t=1===t?0:(Kn(e),r(n),1)}return function(u,i){n=u,i?(r(n),2===t&&(t=1)):(0===t&&Kn(e),t=2)}}var Un={addEventListener:function(){},removeEventListener:function(){}},nr="undefined"!=typeof document?document:Un,rr="undefined"!=typeof window?window:Un,tr=e((function(n,r,t){return Q(H((function(){function e(n){K(t(n))}return n.addEventListener(r,e,_n&&{passive:!0}),function(){n.removeEventListener(r,e)}})))})),er=t((function(n,r){var t=I(n,r);return Dr(t)?wr(t.a):Ar}));var ur=t((function(n,r){return new Float64Array([n,r])})),ir=t((function(n,r){var t=new Float64Array(2);return t[0]=n[0]+r[0],t[1]=n[1]+r[1],t})),ar=t((function(n,r){var t=new Float64Array(2);return t[0]=n[0]-r[0],t[1]=n[1]-r[1],t}));function fr(n){return Math.sqrt(n[0]*n[0]+n[1]*n[1])}var or=t((function(n,r){var t=new Float64Array(2);return t[0]=r[0]*n,t[1]=r[1]*n,t}));new Float64Array(3),new Float64Array(3),new Float64Array(3);new Float64Array(16),new Float64Array(16),new Float64Array([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]);var cr=1,vr=2,sr=0,br=w,lr=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,i=o(n,t.b,t.c,o(lr,n,r,t.e));n=u,r=i,t=e}})),dr=function(n){return o(lr,e((function(n,r,t){return f(br,g(n,r),t)})),m,n)},hr=function(n){return{$:1,a:n}},gr=t((function(n,r){return{$:3,a:n,b:r}})),$r=t((function(n,r){return{$:0,a:n,b:r}})),pr=t((function(n,r){return{$:1,a:n,b:r}})),mr=function(n){return{$:0,a:n}},yr=function(n){return{$:2,a:n}},wr=function(n){return{$:0,a:n}},Ar={$:1},jr=t((function(n,r){return f(x,n,j(r))})),kr=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=f(n,t.a,r);n=u,r=i,t=e}})),_r=function(n){return o(kr,br,m,n)},Fr=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),Er=[],Nr=E,Lr=t((function(n,r){return T(r)/T(n)})),Tr=Nr(f(Lr,2,32)),xr=c(Fr,0,Tr,Er,Er),Br=k,qr=N,Cr=function(n){return n.length},Or=t((function(n,r){return d(n,r)>0?n:r})),Rr=_,zr=t((function(n,r){for(;;){var t=f(Rr,32,n),e=t.b,u=f(br,{$:0,a:t.a},r);if(!e.b)return _r(u);n=e,r=u}})),Mr=t((function(n,r){for(;;){var t=Nr(r/32);if(1===t)return f(Rr,32,n).a;n=f(zr,n,m),r=t}})),Sr=t((function(n,r){if(r.a){var t=32*r.a,e=qr(f(Lr,32,t-1)),u=n?_r(r.d):r.d,i=f(Mr,u,r.a);return c(Fr,Cr(r.c)+t,f(Or,5,e*Tr),i,r.c)}return c(Fr,Cr(r.c),Tr,Er,r.c)})),Ir=i((function(n,r,t,e,u){for(;;){if(0>r)return f(Sr,!1,{d:e,a:t/32|0,c:u});var i={$:1,a:o(Br,32,r,n)};n=n,r=r-32,t=t,e=f(br,i,e),u=u}})),Wr=t((function(n,r){if(n>0){var t=n%32;return v(Ir,r,n-t-32,n,m,o(Br,t,n-t,r))}return xr})),Dr=function(n){return!n.$},Pr=z,Yr=M,Gr=function(n){return{$:0,a:n}},Jr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Zr=Z,Hr=Zr(0),Vr=u((function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var a=i.a,v=i.b;if(v.b){var s=v.a,b=v.b;if(b.b){var l=b.b;return f(n,u,f(n,a,f(n,s,f(n,b.a,t>500?o(kr,n,r,_r(l)):c(Vr,n,r,t+1,l)))))}return f(n,u,f(n,a,f(n,s,r)))}return f(n,u,f(n,a,r))}return f(n,u,r)}return r})),Xr=e((function(n,r,t){return c(Vr,n,r,0,t)})),Kr=t((function(n,r){return o(Xr,t((function(r,t){return f(br,n(r),t)})),m,r)})),Qr=V,Ur=t((function(n,r){return f(Qr,(function(r){return Zr(n(r))}),r)})),nt=e((function(n,r,t){return f(Qr,(function(r){return f(Qr,(function(t){return Zr(f(n,r,t))}),t)}),r)})),rt=function(n){return o(Xr,nt(br),Zr(m),n)},tt=vn,et=t((function(n,r){var t=r;return Q(f(Qr,tt(n),t))}));fn.Task=on(Hr,e((function(n,r){return f(Ur,(function(){return 0}),rt(f(Kr,et(n),r)))})),e((function(){return Zr(0)})),t((function(n,r){return f(Ur,n,r)})));bn("Task");var ut=Xn,it=ur,at=f(it,300,300),ft=function(n){return{$:2,m:n}}(m),ot=t((function(n,r){return{$:0,a:n,b:r}})),ct=O,vt=C,st=e((function(n,r,t){return{$:0,a:n,b:r,c:t}})),bt=t((function(n,r){return{bk:r,bu:n}})),lt={$:-2},dt=lt,ht=Zr(f(bt,m,dt)),gt=function(n){var r=n.b;return g(p(function(n){return n?"w_":"d_"}(n.a),r),n)},$t=i((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),pt=i((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v($t,n,r,t,e,u);var i=e.d;s=e.e;return v($t,0,e.b,e.c,v($t,1,i.b,i.c,i.d,i.e),v($t,1,r,t,s,u))}var a=u.b,f=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return v($t,n,a,f,v($t,0,r,t,e,o),c);var s;return v($t,0,r,t,v($t,1,e.b,e.c,e.d,s=e.e),v($t,1,a,f,o,c))})),mt=h,yt=e((function(n,r,t){if(-2===t.$)return v($t,0,n,r,lt,lt);var e=t.a,u=t.b,i=t.c,a=t.d,c=t.e;switch(f(mt,n,u)){case 0:return v(pt,e,u,i,o(yt,n,r,a),c);case 1:return v($t,e,u,r,a,c);default:return v(pt,e,u,i,a,o(yt,n,r,c))}})),wt=e((function(n,r,t){var e=o(yt,n,r,t);if(-1!==e.$||e.a)return e;return v($t,1,e.b,e.c,e.d,e.e)})),At=function(n){return o(kr,t((function(n,r){return o(wt,n.a,n.b,r)})),dt,n)},jt=function(n){return H((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(Z(0))}))},kt=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,i=o(n,t.b,t.c,o(kt,n,r,t.d));n=u,r=i,t=e}})),_t=a((function(n,r,u,i,a,f){var v=o(kt,e((function(t,e,i){n:for(;;){var a=i.a,f=i.b;if(a.b){var v=a.a,s=v.a,b=v.b,l=a.b;if(0>d(s,t)){t=t,e=e,i=g(l,o(n,s,b,f));continue n}return d(s,t)>0?g(a,o(u,t,e,f)):g(l,c(r,s,b,e,f))}return g(a,o(u,t,e,f))}})),g(dr(i),f),a),s=v.a,b=v.b;return o(kr,t((function(r,t){return o(n,r.a,r.b,t)})),b,s)})),Ft=t((function(n,r){return{aR:r,aW:n}})),Et=sn,Nt=e((function(n,r,t){return f(Ur,(function(n){return g(r,n)}),o(tr,t.a?rr:nr,t.b,(function(t){return f(Et,n,f(Ft,r,t))})))})),Lt=t((function(n,r){return o(kt,wt,r,n)})),Tt=e((function(n,r,t){var i=e((function(r,t,e){var u=e.c;return $(e.a,e.b,f(br,o(Nt,n,r,t),u))})),a=e((function(n,r,t){var e=t.b,u=t.c;return $(f(br,r,t.a),e,u)})),c=u((function(n,r,t,e){var u=e.c;return $(e.a,o(wt,n,r,e.b),u)})),v=f(Kr,gt,r),b=s(_t,a,c,i,t.bk,At(v),$(m,dt,m)),l=b.b,d=b.c;return f(Qr,(function(n){return Zr(f(bt,v,f(Lt,l,At(n))))}),f(Qr,(function(){return rt(d)}),rt(f(Kr,jt,b.a))))})),xt=e((function(n,r,t){var e=n(r);return e.$?t:f(br,e.a,t)})),Bt=t((function(n,r){return o(Xr,xt(n),m,r)}));fn["Browser.Events"]=on(ht,Tt,e((function(n,r,t){var e=r.aW,u=r.aR,i=f(Bt,(function(n){var r=n.b,t=r.c;return b(n.a,e)?f(er,t,u):Ar}),t.bu);return f(Qr,(function(){return Zr(t)}),rt(f(Kr,tt(n),i)))})),0,t((function(n,r){return o(st,r.a,r.b,f(Pr,n,r.c))})));var qt,Ct=bn("Browser.Events"),Ot=f(e((function(n,r,t){return Ct(o(st,n,r,t))})),0,"mousemove"),Rt=t((function(n,r){return g(function(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}(r,{ad:f(it,n.a,n.b)}),ft)})),zt=c(u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),0,0,0,1),Mt=function(n){return n[0]},St=function(n){return n[1]},It=An(function(n){return"script"==n?"p":n}("http://www.w3.org/2000/svg")),Wt=It("line"),Dt=ir,Pt=function(n){var r=new Float64Array(2),t=1/fr(n);return r[0]=n[0]*t,r[1]=n[1]*t,r},Yt=or,Gt=ar,Jt=t((function(n,r){return f(Dt,at,f(Yt,n,Pt(f(Gt,r,at))))})),Zt=function(n){return{$:9,a:n}},Ht=t((function(n,r){return f(kn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))})),Vt=B,Xt=L,Kt=function(n){var r,t,e=n.b,u=n.c,i=n.d,a=function(n){return Xt(1e4*n)/100};return r=A(["rgba(",Vt(a(n.a)),"%,",Vt(a(e)),"%,",Vt(a(u)),"%,",Vt((t=i,Xt(1e3*t)/1e3)),")"]),f(jr,"",r)},Qt=function(n){switch(n.$){case 0:return Vt(n.a)+"cm";case 1:return Vt(n.a)+"em";case 2:return Vt(n.a)+"ex";case 3:return Vt(n.a)+"in";case 4:return Vt(n.a)+"mm";case 5:return Vt(n.a);case 6:return Vt(n.a)+"pc";case 7:return Vt(n.a)+"%";case 8:return Vt(n.a)+"pt";default:return Vt(n.a)+"px"}},Ut=function(n){return{cv:n[0],cy:n[1]}},ne=function(n){return f(Ht,"x1",Qt(n))},re=function(n){return f(Ht,"x2",Qt(n))},te=function(n){return f(Ht,"y1",Qt(n))},ee=function(n){return f(Ht,"y2",Qt(n))},ue=function(n){var r,t,e=Ut(f(Jt,100,n)),u=e.cy;return f(Wt,A([ne(Zt(e.cv)),te(Zt(u)),re(Zt(Mt(at))),ee(Zt(St(at))),(t=zt,f(Ht,"stroke",Kt(t))),(r=Zt(3),f(Ht,"stroke-width",Qt(r)))]),m)},ie=function(n){return f(Ht,"height",Qt(n))},ae=It("svg"),fe=u((function(n,r,t,e){return f(Ht,"viewBox",f(jr," ",f(Kr,Vt,A([n,r,t,e]))))})),oe=ut({bZ:function(){return g({ad:at},ft)},cm:function(){return Ot(o(Yr,ot,f(ct,"pageX",vt),f(ct,"pageY",vt)))},cs:Rt,ct:function(n){return f(ae,A([(r=Zt(600),f(Ht,"width",Qt(r))),ie(Zt(600)),c(fe,0,0,600,600)]),A([ue(n.ad)]));var r}});qt={Vector:{MouseTracingNormalized:{init:oe(Gr(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?F(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,qt):n.Elm=qt}(window);