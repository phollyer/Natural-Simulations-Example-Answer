!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function i(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(i){return n(r,t,e,u,i)}}}}}))}function a(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(i){return function(a){return n(r,t,e,u,i,a)}}}}}}))}function f(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}function s(n,r,t,e,u,i,a){return 6===n.a?n.f(r,t,e,u,i,a):n(r)(t)(e)(u)(i)(a)}function b(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=b(n.a,r.a))||(t=b(n.b,r.b))?t:b(n.c,r.c);for(;n.b&&r.b&&!(t=b(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var l=t((function(n,r){var t=b(n,r);return 0>t?Kn:t?Xn:Hn}));function d(n,r){return{a:n,b:r}}function h(n,r,t){return{a:n,b:r,c:t}}var $={$:0};function g(n,r){return{$:1,a:n,b:r}}var m=t(g);function p(n){for(var r=$,t=n.length;t--;)r=g(n[t],r);return r}function y(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var w=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),A=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,d(t,r)}));function j(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var k=Math.sin;var _=Math.ceil,E=Math.floor,N=Math.round,L=Math.log;var x=t((function(n,r){return r.join(n)}));function T(n){return n+""}function F(n){return{$:2,b:n}}F((function(n){return"number"!=typeof n?B("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?ur(n):!isFinite(n)||n%1?B("an INT",n):ur(n)})),F((function(n){return"boolean"==typeof n?ur(n):B("a BOOL",n)})),F((function(n){return"number"==typeof n?ur(n):B("a FLOAT",n)})),F((function(n){return ur(n)})),F((function(n){return"string"==typeof n?ur(n):n instanceof String?ur(n+""):B("a STRING",n)}));var S=t((function(n,r){return z(n,r)}));function z(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?ur(n.c):B("null",r);case 3:return O(r)?C(n.b,r,p):B("a LIST",r);case 4:return O(r)?C(n.b,r,q):B("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return B("an OBJECT with a field named `"+t+"`",r);var e=z(n.b,r[t]);return Nr(e)?e:nr(f(tr,t,e.a));case 7:var u=n.e;if(!O(r))return B("an ARRAY",r);if(u>=r.length)return B("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=z(n.b,r[u]);return Nr(e)?e:nr(f(er,u,e.a));case 8:if("object"!=typeof r||null===r||O(r))return B("an OBJECT",r);var i=$;for(var a in r)if(r.hasOwnProperty(a)){e=z(n.b,r[a]);if(!Nr(e))return nr(f(tr,a,e.a));i=g(d(a,e.a),i)}return ur(vr(i));case 9:for(var o=n.f,c=n.g,v=0;c.length>v;v++){e=z(c[v],r);if(!Nr(e))return e;o=o(e.a)}return ur(o);case 10:e=z(n.b,r);return Nr(e)?z(n.h(e.a),r):e;case 11:for(var s=$,b=n.g;b.b;b=b.b){e=z(b.a,r);if(Nr(e))return e;s=g(e.a,s)}return nr(ir(vr(s)));case 1:return nr(f(rr,n.a,r));case 0:return ur(n.a)}}function C(n,r,t){for(var e=r.length,u=Array(e),i=0;e>i;i++){var a=z(n,r[i]);if(!Nr(a))return nr(f(er,i,a.a));u[i]=a.a}return ur(t(u))}function O(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function q(n){return f(Er,n.length,(function(r){return n[r]}))}function B(n,r){return nr(f(rr,"Expecting "+n,r))}function I(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return I(n.b,r.b);case 6:return n.d===r.d&&I(n.b,r.b);case 7:return n.e===r.e&&I(n.b,r.b);case 9:return n.f===r.f&&M(n.g,r.g);case 10:return n.h===r.h&&I(n.b,r.b);case 11:return M(n.g,r.g)}}function M(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!I(n[e],r[e]))return!1;return!0}function R(n){return{$:0,a:n}}function D(n){return{$:2,b:n,c:null}}var V=t((function(n,r){return{$:3,b:n,d:r}}));var P=0;function W(n){var r={$:0,e:P++,f:n,g:null,h:[]};return K(r),r}function Y(n){return D((function(r){r(R(W(n)))}))}function G(n,r){n.h.push(r),K(n)}var J=t((function(n,r){return D((function(t){G(n,r),t(R(0))}))}));var H=!1,X=[];function K(n){if(X.push(n),!H){for(H=!0;n=X.shift();)Q(n);H=!1}}function Q(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,K(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function U(n,r,t,e,u,i){var a=f(S,n,r?r.flags:void 0);Nr(a)||j(2);var o={},c=(a=t(a.a)).a,v=i(b,c),s=function(n,r){var t;for(var e in Z){var u=Z[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=rn(u,r)}return t}(o,b);function b(n,r){v(c=(a=f(e,n,c)).a,r),on(o,a.b,u(c))}return on(o,a.b,u(c)),s?{ports:s}:{}}var Z={};function nn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function rn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,i=n.e,a=n.f;function v(n){return f(V,v,{$:5,b:function(r){var f=r.a;return 0===r.$?o(u,t,f,n):i&&a?c(e,t,f.i,f.j,n):o(e,t,i?f.i:f.j,n)}})}return t.h=W(f(V,v,n.b))}var tn=t((function(n,r){return D((function(t){n.g(r),t(R(0))}))})),en=t((function(n,r){return f(J,n.h,{$:0,a:r})}));function un(n){return function(r){return{$:1,k:n,l:r}}}var an=[],fn=!1;function on(n,r,t){if(an.push({p:n,q:r,r:t}),!fn){fn=!0;for(var e;e=an.shift();)cn(e.p,e.q,e.r);fn=!1}}function cn(n,r,t){var e={};for(var u in vn(!0,r,e,null),vn(!1,t,e,null),n)G(n[u],{$:"fx",a:e[u]||{i:$,j:$}})}function vn(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return f(n?Z[r].e:Z[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:$,j:$},n?t.i=g(r,t.i):t.j=g(r,t.j),t}(n,i,t[u]));case 2:for(var a=r.m;a.b;a=a.b)vn(n,a.a,t,e);return;case 3:return void vn(n,r.o,t,{s:r.n,t:e})}}var sn;var bn="undefined"!=typeof document?document:{};function ln(n,r){n.appendChild(r)}function dn(n){return{$:0,a:n}}var hn=t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var a=e.a;i+=a.b||0,u.push(a)}return i+=u.length,{$:1,c:r,d:pn(t),e:u,f:n,b:i}}))})),$n=hn(void 0);t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var a=e.a;i+=a.b.b||0,u.push(a)}return i+=u.length,{$:2,c:r,d:pn(t),e:u,f:n,b:i}}))}))(void 0);var gn=t((function(n,r){return{$:"a3",n:n,o:r}}));var mn;function pn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var a=r[e]||(r[e]={});"a3"===e&&"class"===u?yn(a,u,i):a[u]=i}else"className"===u?yn(r,u,i):r[u]=i}return r}function yn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function wn(n,r){var t=n.$;if(5===t)return wn(n.k||(n.k=n.m()),r);if(0===t)return bn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(a=wn(e,i)).elm_event_node_ref=i,a}if(3===t)return An(a=n.h(n.g),r,n.d),a;var a=n.f?bn.createElementNS(n.f,n.c):bn.createElement(n.c);sn&&"a"==n.c&&a.addEventListener("click",sn(a)),An(a,r,n.d);for(var f=n.e,o=0;f.length>o;o++)ln(a,wn(1===t?f[o]:f[o].b,r));return a}function An(n,r,t){for(var e in t){var u=t[e];"a1"===e?jn(n,u):"a0"===e?En(n,r,u):"a3"===e?kn(n,u):"a4"===e?_n(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function jn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function kn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function _n(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function En(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],a=e[u];if(i){if(a){if(a.q.$===i.$){a.q=i;continue}n.removeEventListener(u,a)}a=Nn(r,i),n.addEventListener(u,a,mn&&{passive:2>xr(i)}),e[u]=a}else n.removeEventListener(u,a),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){mn=!0}}))}catch(n){}function Nn(n,r){function t(r){var e=t.q,u=z(e.a,r);if(Nr(u)){for(var i,a=xr(e),f=u.a,o=a?3>a?f.a:f.y:f,c=1==a?f.b:3==a&&f.aL,v=(c&&r.stopPropagation(),(2==a?f.b:3==a&&f.aF)&&r.preventDefault(),n);i=v.j;){if("function"==typeof i)o=i(o);else for(var s=i.length;s--;)o=i[s](o);v=v.p}v(o,c)}}return t.q=r,t}function Ln(n,r){return n.$==r.$&&I(n.a,r.a)}function xn(n,r){var t=[];return Fn(n,r,t,0),t}function Tn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Fn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void Tn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var a=n.l,f=r.l,o=a.length,c=o===f.length;c&&o--;)c=a[o]===f[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Fn(n.k,r.k,v,0),void(v.length>0&&Tn(t,1,e,v));case 4:for(var s=n.j,b=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return l&&s.length!==b.length?void Tn(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Tn(t,2,e,b),void Fn(d,h,t,e+1));case 0:return void(n.a!==r.a&&Tn(t,3,e,r.a));case 1:return void Sn(n,r,t,e,Cn);case 2:return void Sn(n,r,t,e,On);case 3:if(n.h!==r.h)return void Tn(t,0,e,r);var $=zn(n.d,r.d);$&&Tn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Tn(t,5,e,g))}}}function Sn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=zn(n.d,r.d);i&&Tn(t,4,e,i),u(n,r,t,e)}else Tn(t,0,e,r)}function zn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],a=r[u];i===a&&"value"!==u&&"checked"!==u||"a0"===t&&Ln(i,a)||((e=e||{})[u]=a)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=zn(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Cn(n,r,t,e){var u=n.e,i=r.e,a=u.length,f=i.length;a>f?Tn(t,6,e,{v:f,i:a-f}):f>a&&Tn(t,7,e,{v:a,e:i});for(var o=f>a?a:f,c=0;o>c;c++){var v=u[c];Fn(v,i[c],t,++e),e+=v.b||0}}function On(n,r,t,e){for(var u=[],i={},a=[],f=n.e,o=r.e,c=f.length,v=o.length,s=0,b=0,l=e;c>s&&v>b;){var d=(E=f[s]).a,h=(N=o[b]).a,$=E.b,g=N.b,m=void 0,p=void 0;if(d!==h){var y=f[s+1],w=o[b+1];if(y){var A=y.a,j=y.b;p=h===A}if(w){var k=w.a,_=w.b;m=d===k}if(m&&p)Fn($,_,u,++l),qn(i,u,d,g,b,a),l+=$.b||0,Bn(i,u,d,j,++l),l+=j.b||0,s+=2,b+=2;else if(m)l++,qn(i,u,h,g,b,a),Fn($,_,u,l),l+=$.b||0,s+=1,b+=2;else if(p)Bn(i,u,d,$,++l),l+=$.b||0,Fn(j,g,u,++l),l+=j.b||0,s+=2,b+=1;else{if(!y||A!==k)break;Bn(i,u,d,$,++l),qn(i,u,h,g,b,a),l+=$.b||0,Fn(j,_,u,++l),l+=j.b||0,s+=2,b+=2}}else Fn($,g,u,++l),l+=$.b||0,s++,b++}for(;c>s;){var E;l++,Bn(i,u,(E=f[s]).a,$=E.b,l),l+=$.b||0,s++}for(;v>b;){var N,L=L||[];qn(i,u,(N=o[b]).a,N.b,void 0,L),b++}(u.length>0||a.length>0||L)&&Tn(t,8,e,{w:u,x:a,y:L})}function qn(n,r,t,e,u,i){var a=n[t];if(!a)return i.push({r:u,A:a={c:0,z:e,r:u,s:void 0}}),void(n[t]=a);if(1===a.c){i.push({r:u,A:a}),a.c=2;var f=[];return Fn(a.z,e,f,a.r),a.r=u,void(a.s.s={w:f,A:a})}qn(n,r,t+"_elmW6BL",e,u,i)}function Bn(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var a=[];return Fn(e,i.z,a,u),void Tn(r,9,u,{w:a,A:i})}Bn(n,r,t+"_elmW6BL",e,u)}else{var f=Tn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function In(n,r,t,e){!function n(r,t,e,u,i,a,f){var o=e[u],c=o.r;for(;c===i;){var v=o.$;if(1===v)In(r,t.k,o.s,f);else if(8===v){o.t=r,o.u=f,(s=o.s.w).length>0&&n(r,t,s,0,i,a,f)}else if(9===v){o.t=r,o.u=f;var s,b=o.s;if(b)b.A.s=r,(s=b.w).length>0&&n(r,t,s,0,i,a,f)}else o.t=r,o.u=f;if(u++,!(o=e[u])||(c=o.r)>a)return u}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,i+1,a,r.elm_event_node_ref)}for(var h=t.e,$=r.childNodes,g=0;h.length>g;g++){i++;var m=1===l?h[g]:h[g].b,p=i+(m.b||0);if(c>=i&&p>=c&&(u=n($[g],m,e,u,i,p,f),!(o=e[u])||(c=o.r)>a))return u;i=p}return u}(n,r,t,0,0,r.b,e)}function Mn(n,r,t,e){return 0===t.length?n:(In(n,r,t,e),Rn(n,t))}function Rn(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,i=Dn(u,e);u===n&&(n=i)}return n}function Dn(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=wn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return An(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Rn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(wn(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var a=t.A;return void 0!==a.r&&n.parentNode.removeChild(n),a.s=Rn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=bn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;ln(t,2===u.c?u.s:wn(u.z,r.u))}return t}(t.y,r);n=Rn(n,t.w);for(var u=t.x,i=0;u.length>i;i++){var a=u[i],f=a.A,o=2===f.c?f.s:wn(f.z,r.u);n.insertBefore(o,n.childNodes[a.r])}e&&ln(n,e);return n}(n,r);case 5:return r.s(n);default:j(10)}}function Vn(n){if(3===n.nodeType)return dn(n.textContent);if(1!==n.nodeType)return dn("");for(var r=$,t=n.attributes,e=t.length;e--;){var u=t[e];r=g(f(gn,u.name,u.value),r)}var i=n.tagName.toLowerCase(),a=$,c=n.childNodes;for(e=c.length;e--;)a=g(Vn(c[e]),a);return o($n,i,r,a)}var Pn=u((function(n,r,t,e){return U(r,e,n.b3,n.cy,n.cs,(function(r,t){var u=n.cz,i=e.node,a=Vn(i);return Yn(t,(function(n){var t=u(n),e=xn(a,t);i=Mn(i,a,e,r),a=t}))}))})),Wn=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function Yn(n,r){r(n);var t=0;function e(){t=1===t?0:(Wn(e),r(n),1)}return function(u,i){n=u,i?(r(n),2===t&&(t=1)):(0===t&&Wn(e),t=2)}}var Gn={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Jn=t((function(n,r){return D((function(){var t=setInterval((function(){W(r)}),n);return function(){clearInterval(t)}}))}));var Hn=1,Xn=2,Kn=0,Qn=m,Un=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,i=o(n,t.b,t.c,o(Un,n,r,t.e));n=u,r=i,t=e}})),Zn=function(n){return o(Un,e((function(n,r,t){return f(Qn,d(n,r),t)})),$,n)},nr=function(n){return{$:1,a:n}},rr=t((function(n,r){return{$:3,a:n,b:r}})),tr=t((function(n,r){return{$:0,a:n,b:r}})),er=t((function(n,r){return{$:1,a:n,b:r}})),ur=function(n){return{$:0,a:n}},ir=function(n){return{$:2,a:n}},ar=function(n){return{$:0,a:n}},fr={$:1},or=t((function(n,r){return f(x,n,y(r))})),cr=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=f(n,t.a,r);n=u,r=i,t=e}})),vr=function(n){return o(cr,Qn,$,n)},sr=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),br=[],lr=_,dr=t((function(n,r){return L(r)/L(n)})),hr=lr(f(dr,2,32)),$r=c(sr,0,hr,br,br),gr=w,mr=E,pr=function(n){return n.length},yr=t((function(n,r){return b(n,r)>0?n:r})),wr=A,Ar=t((function(n,r){for(;;){var t=f(wr,32,n),e=t.b,u=f(Qn,{$:0,a:t.a},r);if(!e.b)return vr(u);n=e,r=u}})),jr=t((function(n,r){for(;;){var t=lr(r/32);if(1===t)return f(wr,32,n).a;n=f(Ar,n,$),r=t}})),kr=t((function(n,r){if(r.a){var t=32*r.a,e=mr(f(dr,32,t-1)),u=n?vr(r.d):r.d,i=f(jr,u,r.a);return c(sr,pr(r.c)+t,f(yr,5,e*hr),i,r.c)}return c(sr,pr(r.c),hr,br,r.c)})),_r=i((function(n,r,t,e,u){for(;;){if(0>r)return f(kr,!1,{d:e,a:t/32|0,c:u});var i={$:1,a:o(gr,32,r,n)};n=n,r=r-32,t=t,e=f(Qn,i,e),u=u}})),Er=t((function(n,r){if(n>0){var t=n%32;return v(_r,r,n-t-32,n,$,o(gr,t,n-t,r))}return $r})),Nr=function(n){return!n.$},Lr=function(n){return{$:0,a:n}},xr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Tr=function(n){return n},Fr=R,Sr=Fr(0),zr=u((function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var a=i.a,v=i.b;if(v.b){var s=v.a,b=v.b;if(b.b){var l=b.b;return f(n,u,f(n,a,f(n,s,f(n,b.a,t>500?o(cr,n,r,vr(l)):c(zr,n,r,t+1,l)))))}return f(n,u,f(n,a,f(n,s,r)))}return f(n,u,f(n,a,r))}return f(n,u,r)}return r})),Cr=e((function(n,r,t){return c(zr,n,r,0,t)})),Or=t((function(n,r){return o(Cr,t((function(r,t){return f(Qn,n(r),t)})),$,r)})),qr=V,Br=t((function(n,r){return f(qr,(function(r){return Fr(n(r))}),r)})),Ir=e((function(n,r,t){return f(qr,(function(r){return f(qr,(function(t){return Fr(f(n,r,t))}),t)}),r)})),Mr=function(n){return o(Cr,Ir(Qn),Fr($),n)},Rr=tn,Dr=t((function(n,r){var t=r;return Y(f(qr,Rr(n),t))}));Z.Task=nn(Sr,e((function(n,r){return f(Br,(function(){return 0}),Mr(f(Or,Dr(n),r)))})),e((function(){return Fr(0)})),t((function(n,r){return f(Br,n,r)})));un("Task");var Vr,Pr=Pn,Wr=function(n){return{$:2,m:n}}($),Yr=t((function(n,r){return{$:0,a:n,b:r}})),Gr=t((function(n,r){return{br:r,bz:n}})),Jr={$:-2},Hr=Jr,Xr=Fr(f(Gr,Hr,Hr)),Kr=l,Qr=t((function(n,r){n:for(;;){if(-2===r.$)return fr;var t=r.c,e=r.d,u=r.e;switch(f(Kr,n,r.b)){case 0:n=n,r=e;continue n;case 1:return ar(t);default:n=n,r=u;continue n}}})),Ur=i((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),Zr=i((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v(Ur,n,r,t,e,u);var i=e.d;s=e.e;return v(Ur,0,e.b,e.c,v(Ur,1,i.b,i.c,i.d,i.e),v(Ur,1,r,t,s,u))}var a=u.b,f=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return v(Ur,n,a,f,v(Ur,0,r,t,e,o),c);var s;return v(Ur,0,r,t,v(Ur,1,e.b,e.c,e.d,s=e.e),v(Ur,1,a,f,o,c))})),nt=e((function(n,r,t){if(-2===t.$)return v(Ur,0,n,r,Jr,Jr);var e=t.a,u=t.b,i=t.c,a=t.d,c=t.e;switch(f(Kr,n,u)){case 0:return v(Zr,e,u,i,o(nt,n,r,a),c);case 1:return v(Ur,e,u,r,a,c);default:return v(Zr,e,u,i,a,o(nt,n,r,c))}})),rt=e((function(n,r,t){var e=o(nt,n,r,t);if(-1!==e.$||e.a)return e;return v(Ur,1,e.b,e.c,e.d,e.e)})),tt=t((function(n,r){var t=n.a,e=n.b,u=f(Qr,t,r);return o(rt,t,1===u.$?p([e]):f(Qn,e,u.a),r)})),et=function(n){return D((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(R(0))}))},ut=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,i=o(n,t.b,t.c,o(ut,n,r,t.d));n=u,r=i,t=e}})),it=a((function(n,r,u,i,a,f){var v=o(ut,e((function(t,e,i){n:for(;;){var a=i.a,f=i.b;if(a.b){var v=a.a,s=v.a,l=v.b,h=a.b;if(0>b(s,t)){t=t,e=e,i=d(h,o(n,s,l,f));continue n}return b(s,t)>0?d(a,o(u,t,e,f)):d(h,c(r,s,l,e,f))}return d(a,o(u,t,e,f))}})),d(Zn(i),f),a),s=v.a,l=v.b;return o(cr,t((function(r,t){return o(n,r.a,r.b,t)})),l,s)})),at=en,ft=Jn,ot=Y,ct=e((function(n,r,t){if(r.b){var e=r.a,u=r.b,i=ot(f(ft,e,f(at,n,e)));return f(qr,(function(r){return o(ct,n,u,o(rt,e,r,t))}),i)}return Fr(t)})),vt=e((function(n,r,t){var i=t.br,a=e((function(n,r,t){var e=t.c;return h(t.a,t.b,f(qr,(function(){return e}),et(r)))})),c=o(cr,tt,Hr,r),v=s(it,e((function(n,r,t){var e=t.b,u=t.c;return h(f(Qn,n,t.a),e,u)})),u((function(n,r,t,e){var u=e.c;return h(e.a,o(rt,n,t,e.b),u)})),a,c,i,h($,Hr,Fr(0))),b=v.a,l=v.b;return f(qr,(function(n){return Fr(f(Gr,c,n))}),f(qr,(function(){return o(ct,n,b,l)}),v.c))})),st=(Vr=Tr,D((function(n){n(R(Vr(Date.now())))}))),bt=e((function(n,r,t){var e=f(Qr,r,t.bz);if(1===e.$)return Fr(t);var u=e.a;return f(qr,(function(){return Fr(t)}),f(qr,(function(r){return Mr(f(Or,(function(t){return f(Rr,n,t(r))}),u))}),st))})),lt=e((function(n,r,t){return n(r(t))}));Z.Time=nn(Xr,vt,bt,0,t((function(n,r){return f(Yr,r.a,f(lt,n,r.b))})));var dt,ht=un("Time"),$t=t((function(n,r){return ht(f(Yr,n,r))})),gt=k,mt=t((function(n,r){var t=r.ap*gt(r.S);return d(function(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}(r,{S:r.S+r.aE,V:t}),Wr)})),pt=c(u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),0,0,0,1),yt=t((function(n,r){return f(gn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))})),wt=T,At=function(n){switch(n.$){case 0:return wt(n.a)+"cm";case 1:return wt(n.a)+"em";case 2:return wt(n.a)+"ex";case 3:return wt(n.a)+"in";case 4:return wt(n.a)+"mm";case 5:return wt(n.a);case 6:return wt(n.a)+"pc";case 7:return wt(n.a)+"%";case 8:return wt(n.a)+"pt";default:return wt(n.a)+"px"}},jt=function(n){return f(yt,"height",At(n))},kt=function(n){return f(or,"",n)},_t=N,Et=function(n){var r,t=n.b,e=n.c,u=n.d,i=function(n){return _t(1e4*n)/100};return kt(p(["rgba(",wt(i(n.a)),"%,",wt(i(t)),"%,",wt(i(e)),"%,",wt((r=u,_t(1e3*r)/1e3)),")"]))},Nt=function(n){return n.$?"none":Et(n.a)},Lt=f(lt,yt("fill"),Nt),xt=Lt({$:1}),Tt=function(n){return{$:9,a:n}},Ft=hn(function(n){return"script"==n?"p":n}("http://www.w3.org/2000/svg")),St=Ft("rect"),zt=function(n){return f(yt,"stroke",Et(n))},Ct=function(n){return f(yt,"stroke-width",At(n))},Ot=function(n){return f(yt,"width",At(n))},qt=f(St,p([Ot(Tt(600)),jt(Tt(600)),xt,zt(pt),Ct(Tt(3))]),$),Bt=Ft("svg"),It=t((function(n,r){return{$:5,a:n,b:r}})),Mt=Ft("circle"),Rt=function(n){return f(yt,"cy",At(n))},Dt=function(n){return f(yt,"r",At(n))},Vt=function(n){var r=t((function(n,r){return kt(p([n,"(",f(or," ",f(Or,wt,r)),")"]))}));switch(n.$){case 0:return f(r,"matrix",p([n.a,n.b,n.c,n.d,n.e,n.f]));case 1:return f(r,"rotate",p([n.a,n.b,n.c]));case 2:return f(r,"scale",p([n.a,n.b]));case 3:return f(r,"skewX",p([n.a]));case 4:return f(r,"skewY",p([n.a]));default:return f(r,"translate",p([n.a,n.b]))}},Pt=function(n){return f(yt,"transform",f(or," ",f(Or,Vt,n)))},Wt=t((function(n,r){return f(Mt,p([Pt(p([f(It,300,300)])),(e=Tt(r),f(yt,"cx",At(e))),Rt(Tt(0)),Dt(Tt(n)),Lt((t=pt,{$:0,a:t}))]),$);var t,e})),Yt=u((function(n,r,t,e){return f(yt,"viewBox",f(or," ",f(Or,wt,p([n,r,t,e]))))})),Gt=Ft("line"),Jt=function(n){return f(yt,"x2",At(n))},Ht=function(n){return f(yt,"y1",At(n))},Xt=function(n){return f(yt,"y2",At(n))},Kt=Pr({b3:function(){return d({ap:200,S:0,as:20,aE:.020943951023931952,V:0},Wr)},cs:function(){return f($t,10,Tr)},cy:mt,cz:function(n){return f(Bt,p([Ot(Tt(600)),jt(Tt(600)),c(Yt,0,0,600,600)]),p([qt,(r=n.V,f(Gt,p([Pt(p([f(It,300,300)])),(t=Tt(0),f(yt,"x1",At(t))),Ht(Tt(0)),Jt(Tt(r)),Xt(Tt(0)),zt(pt),Ct(Tt(2))]),$)),f(Wt,n.as,n.V)]));var r,t}});dt={Oscillations:{SimpleHarmonicMotionWithAngle:{init:Kt(Lr(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?j(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,dt):n.Elm=dt}(window);