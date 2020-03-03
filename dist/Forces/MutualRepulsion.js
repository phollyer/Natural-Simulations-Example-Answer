!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function a(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(a){return n(r,t,e,u,a)}}}}}))}function i(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(a){return function(i){return n(r,t,e,u,a,i)}}}}}}))}function f(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,a){return 5===n.a?n.f(r,t,e,u,a):n(r)(t)(e)(u)(a)}function b(n,r,t,e,u,a,i){return 6===n.a?n.f(r,t,e,u,a,i):n(r)(t)(e)(u)(a)(i)}function s(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=s(n.a,r.a))||(t=s(n.b,r.b))?t:s(n.c,r.c);for(;n.b&&r.b&&!(t=s(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var l=t((function(n,r){var t=s(n,r);return 0>t?fr:t?ir:ar}));function d(n,r){return{a:n,b:r}}function h(n,r,t){return{a:n,b:r,c:t}}function $(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}function g(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=m(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=m(n.a,r);return t}var p={$:0};function m(n,r){return{$:1,a:n,b:r}}var w=t(m);function y(n){for(var r=p,t=n.length;t--;)r=m(n[t],r);return r}function A(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var k=e((function(n,r,t){for(var e=[];r.b&&t.b;r=r.b,t=t.b)e.push(f(n,r.a,t.a));return y(e)}));var F=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),j=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,d(t,r)}));function _(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var N=Math.cos;var E=Math.ceil,L=Math.floor,T=Math.round,x=Math.sqrt,O=Math.log;var q=t((function(n,r){return r.join(n)}));function C(n){return n+""}function z(n){return{$:2,b:n}}z((function(n){return"number"!=typeof n?S("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?hr(n):!isFinite(n)||n%1?S("an INT",n):hr(n)})),z((function(n){return"boolean"==typeof n?hr(n):S("a BOOL",n)})),z((function(n){return"number"==typeof n?hr(n):S("a FLOAT",n)})),z((function(n){return hr(n)})),z((function(n){return"string"==typeof n?hr(n):n instanceof String?hr(n+""):S("a STRING",n)}));var D=t((function(n,r){return R(n,r)}));function R(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?hr(n.c):S("null",r);case 3:return M(r)?B(n.b,r,y):S("a LIST",r);case 4:return M(r)?B(n.b,r,I):S("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return S("an OBJECT with a field named `"+t+"`",r);var e=R(n.b,r[t]);return Gr(e)?e:br(f(lr,t,e.a));case 7:var u=n.e;if(!M(r))return S("an ARRAY",r);if(u>=r.length)return S("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=R(n.b,r[u]);return Gr(e)?e:br(f(dr,u,e.a));case 8:if("object"!=typeof r||null===r||M(r))return S("an OBJECT",r);var a=p;for(var i in r)if(r.hasOwnProperty(i)){e=R(n.b,r[i]);if(!Gr(e))return br(f(lr,i,e.a));a=m(d(i,e.a),a)}return hr(_r(a));case 9:for(var o=n.f,c=n.g,v=0;c.length>v;v++){e=R(c[v],r);if(!Gr(e))return e;o=o(e.a)}return hr(o);case 10:e=R(n.b,r);return Gr(e)?R(n.h(e.a),r):e;case 11:for(var b=p,s=n.g;s.b;s=s.b){e=R(s.a,r);if(Gr(e))return e;b=m(e.a,b)}return br($r(_r(b)));case 1:return br(f(sr,n.a,r));case 0:return hr(n.a)}}function B(n,r,t){for(var e=r.length,u=Array(e),a=0;e>a;a++){var i=R(n,r[a]);if(!Gr(i))return br(f(dr,a,i.a));u[a]=i.a}return hr(t(u))}function M(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function I(n){return f(Pr,n.length,(function(r){return n[r]}))}function S(n,r){return br(f(sr,"Expecting "+n,r))}function P(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return P(n.b,r.b);case 6:return n.d===r.d&&P(n.b,r.b);case 7:return n.e===r.e&&P(n.b,r.b);case 9:return n.f===r.f&&G(n.g,r.g);case 10:return n.h===r.h&&P(n.b,r.b);case 11:return G(n.g,r.g)}}function G(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!P(n[e],r[e]))return!1;return!0}function J(n){return{$:0,a:n}}function W(n){return{$:2,b:n,c:null}}var Y=t((function(n,r){return{$:3,b:n,d:r}}));var K=0;function H(n){var r={$:0,e:K++,f:n,g:null,h:[]};return nn(r),r}function Q(n){return W((function(r){r(J(H(n)))}))}function U(n,r){n.h.push(r),nn(n)}var V=t((function(n,r){return W((function(t){U(n,r),t(J(0))}))}));var X=!1,Z=[];function nn(n){if(Z.push(n),!X){for(X=!0;n=Z.shift();)rn(n);X=!1}}function rn(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,nn(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function tn(n,r,t,e,u,a){var i=f(D,n,r?r.flags:void 0);Gr(i)||_(2);var o={},c=(i=t(i.a)).a,v=a(s,c),b=function(n,r){var t;for(var e in en){var u=en[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=an(u,r)}return t}(o,s);function s(n,r){v(c=(i=f(e,n,c)).a,r),ln(o,i.b,u(c))}return ln(o,i.b,u(c)),b?{ports:b}:{}}var en={};function un(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function an(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,a=n.e,i=n.f;function v(n){return f(Y,v,{$:5,b:function(r){var f=r.a;return 0===r.$?o(u,t,f,n):a&&i?c(e,t,f.i,f.j,n):o(e,t,a?f.i:f.j,n)}})}return t.h=H(f(Y,v,n.b))}var fn=t((function(n,r){return W((function(t){n.g(r),t(J(0))}))})),on=t((function(n,r){return f(V,n.h,{$:0,a:r})}));function cn(n){return function(r){return{$:1,k:n,l:r}}}function vn(n){return{$:2,m:n}}var bn=[],sn=!1;function ln(n,r,t){if(bn.push({p:n,q:r,r:t}),!sn){sn=!0;for(var e;e=bn.shift();)dn(e.p,e.q,e.r);sn=!1}}function dn(n,r,t){var e={};for(var u in hn(!0,r,e,null),hn(!1,t,e,null),n)U(n[u],{$:"fx",a:e[u]||{i:p,j:p}})}function hn(n,r,t,e){switch(r.$){case 1:var u=r.k,a=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return f(n?en[r].e:en[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:p,j:p},n?t.i=m(r,t.i):t.j=m(r,t.j),t}(n,a,t[u]));case 2:for(var i=r.m;i.b;i=i.b)hn(n,i.a,t,e);return;case 3:return void hn(n,r.o,t,{s:r.n,t:e})}}var $n;var gn="undefined"!=typeof document?document:{};function pn(n,r){n.appendChild(r)}function mn(n){return{$:0,a:n}}var wn=t((function(n,r){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b||0,u.push(i)}return a+=u.length,{$:1,c:r,d:Fn(t),e:u,f:n,b:a}}))})),yn=wn(void 0);t((function(n,r){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b.b||0,u.push(i)}return a+=u.length,{$:2,c:r,d:Fn(t),e:u,f:n,b:a}}))}))(void 0);var An=t((function(n,r){return{$:"a3",n:n,o:r}}));var kn;function Fn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=r[e]||(r[e]={});"a3"===e&&"class"===u?jn(i,u,a):i[u]=a}else"className"===u?jn(r,u,a):r[u]=a}return r}function jn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function _n(n,r){var t=n.$;if(5===t)return _n(n.k||(n.k=n.m()),r);if(0===t)return gn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:r};return(i=_n(e,a)).elm_event_node_ref=a,i}if(3===t)return Nn(i=n.h(n.g),r,n.d),i;var i=n.f?gn.createElementNS(n.f,n.c):gn.createElement(n.c);$n&&"a"==n.c&&i.addEventListener("click",$n(i)),Nn(i,r,n.d);for(var f=n.e,o=0;f.length>o;o++)pn(i,_n(1===t?f[o]:f[o].b,r));return i}function Nn(n,r,t){for(var e in t){var u=t[e];"a1"===e?En(n,u):"a0"===e?xn(n,r,u):"a3"===e?Ln(n,u):"a4"===e?Tn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function En(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Ln(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Tn(n,r){for(var t in r){var e=r[t],u=e.f,a=e.o;void 0!==a?n.setAttributeNS(u,t,a):n.removeAttributeNS(u,t)}}function xn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}n.removeEventListener(u,i)}i=On(r,a),n.addEventListener(u,i,kn&&{passive:2>Wr(a)}),e[u]=i}else n.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){kn=!0}}))}catch(n){}function On(n,r){function t(r){var e=t.q,u=R(e.a,r);if(Gr(u)){for(var a,i=Wr(e),f=u.a,o=i?3>i?f.a:f.y:f,c=1==i?f.b:3==i&&f.aK,v=(c&&r.stopPropagation(),(2==i?f.b:3==i&&f.aE)&&r.preventDefault(),n);a=v.j;){if("function"==typeof a)o=a(o);else for(var b=a.length;b--;)o=a[b](o);v=v.p}v(o,c)}}return t.q=r,t}function qn(n,r){return n.$==r.$&&P(n.a,r.a)}function Cn(n,r){var t=[];return Dn(n,r,t,0),t}function zn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Dn(n,r,t,e){if(n!==r){var u=n.$,a=r.$;if(u!==a){if(1!==u||2!==a)return void zn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),a=1}switch(a){case 5:for(var i=n.l,f=r.l,o=i.length,c=o===f.length;c&&o--;)c=i[o]===f[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Dn(n.k,r.k,v,0),void(v.length>0&&zn(t,1,e,v));case 4:for(var b=n.j,s=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof b?b=[b,d.j]:b.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof s?s=[s,h.j]:s.push(h.j),h=h.k;return l&&b.length!==s.length?void zn(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(b,s):b===s)||zn(t,2,e,s),void Dn(d,h,t,e+1));case 0:return void(n.a!==r.a&&zn(t,3,e,r.a));case 1:return void Rn(n,r,t,e,Mn);case 2:return void Rn(n,r,t,e,In);case 3:if(n.h!==r.h)return void zn(t,0,e,r);var $=Bn(n.d,r.d);$&&zn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&zn(t,5,e,g))}}}function Rn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var a=Bn(n.d,r.d);a&&zn(t,4,e,a),u(n,r,t,e)}else zn(t,0,e,r)}function Bn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var a=n[u],i=r[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&qn(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=Bn(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Mn(n,r,t,e){var u=n.e,a=r.e,i=u.length,f=a.length;i>f?zn(t,6,e,{v:f,i:i-f}):f>i&&zn(t,7,e,{v:i,e:a});for(var o=f>i?i:f,c=0;o>c;c++){var v=u[c];Dn(v,a[c],t,++e),e+=v.b||0}}function In(n,r,t,e){for(var u=[],a={},i=[],f=n.e,o=r.e,c=f.length,v=o.length,b=0,s=0,l=e;c>b&&v>s;){var d=(_=f[b]).a,h=(N=o[s]).a,$=_.b,g=N.b,p=void 0,m=void 0;if(d!==h){var w=f[b+1],y=o[s+1];if(w){var A=w.a,k=w.b;m=h===A}if(y){var F=y.a,j=y.b;p=d===F}if(p&&m)Dn($,j,u,++l),Sn(a,u,d,g,s,i),l+=$.b||0,Pn(a,u,d,k,++l),l+=k.b||0,b+=2,s+=2;else if(p)l++,Sn(a,u,h,g,s,i),Dn($,j,u,l),l+=$.b||0,b+=1,s+=2;else if(m)Pn(a,u,d,$,++l),l+=$.b||0,Dn(k,g,u,++l),l+=k.b||0,b+=2,s+=1;else{if(!w||A!==F)break;Pn(a,u,d,$,++l),Sn(a,u,h,g,s,i),l+=$.b||0,Dn(k,j,u,++l),l+=k.b||0,b+=2,s+=2}}else Dn($,g,u,++l),l+=$.b||0,b++,s++}for(;c>b;){var _;l++,Pn(a,u,(_=f[b]).a,$=_.b,l),l+=$.b||0,b++}for(;v>s;){var N,E=E||[];Sn(a,u,(N=o[s]).a,N.b,void 0,E),s++}(u.length>0||i.length>0||E)&&zn(t,8,e,{w:u,x:i,y:E})}function Sn(n,r,t,e,u,a){var i=n[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(n[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var f=[];return Dn(i.z,e,f,i.r),i.r=u,void(i.s.s={w:f,A:i})}Sn(n,r,t+"_elmW6BL",e,u,a)}function Pn(n,r,t,e,u){var a=n[t];if(a){if(0===a.c){a.c=2;var i=[];return Dn(e,a.z,i,u),void zn(r,9,u,{w:i,A:a})}Pn(n,r,t+"_elmW6BL",e,u)}else{var f=zn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function Gn(n,r,t,e){!function n(r,t,e,u,a,i,f){var o=e[u],c=o.r;for(;c===a;){var v=o.$;if(1===v)Gn(r,t.k,o.s,f);else if(8===v){o.t=r,o.u=f,(b=o.s.w).length>0&&n(r,t,b,0,a,i,f)}else if(9===v){o.t=r,o.u=f;var b,s=o.s;if(s)s.A.s=r,(b=s.w).length>0&&n(r,t,b,0,a,i,f)}else o.t=r,o.u=f;if(u++,!(o=e[u])||(c=o.r)>i)return u}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,a+1,i,r.elm_event_node_ref)}for(var h=t.e,$=r.childNodes,g=0;h.length>g;g++){a++;var p=1===l?h[g]:h[g].b,m=a+(p.b||0);if(c>=a&&m>=c&&(u=n($[g],p,e,u,a,m,f),!(o=e[u])||(c=o.r)>i))return u;a=m}return u}(n,r,t,0,0,r.b,e)}function Jn(n,r,t,e){return 0===t.length?n:(Gn(n,r,t,e),Wn(n,t))}function Wn(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,a=Yn(u,e);u===n&&(n=a)}return n}function Yn(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=_n(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return Nn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Wn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,a=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(_n(u[e],r.u),a);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var i=t.A;return void 0!==i.r&&n.parentNode.removeChild(n),i.s=Wn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=gn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;pn(t,2===u.c?u.s:_n(u.z,r.u))}return t}(t.y,r);n=Wn(n,t.w);for(var u=t.x,a=0;u.length>a;a++){var i=u[a],f=i.A,o=2===f.c?f.s:_n(f.z,r.u);n.insertBefore(o,n.childNodes[i.r])}e&&pn(n,e);return n}(n,r);case 5:return r.s(n);default:_(10)}}function Kn(n){if(3===n.nodeType)return mn(n.textContent);if(1!==n.nodeType)return mn("");for(var r=p,t=n.attributes,e=t.length;e--;){var u=t[e];r=m(f(An,u.name,u.value),r)}var a=n.tagName.toLowerCase(),i=p,c=n.childNodes;for(e=c.length;e--;)i=m(Kn(c[e]),i);return o(yn,a,r,i)}var Hn=u((function(n,r,t,e){return tn(r,e,n.b1,n.cw,n.cq,(function(r,t){var u=n.cx,a=e.node,i=Kn(a);return Un(t,(function(n){var t=u(n),e=Cn(i,t);a=Jn(a,i,e,r),i=t}))}))})),Qn=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function Un(n,r){r(n);var t=0;function e(){t=1===t?0:(Qn(e),r(n),1)}return function(u,a){n=u,a?(r(n),2===t&&(t=1)):(0===t&&Qn(e),t=2)}}var Vn={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Xn=t((function(n,r){return W((function(){var t=setInterval((function(){H(r)}),n);return function(){clearInterval(t)}}))}));var Zn=t((function(n,r){return new Float64Array([n,r])})),nr=t((function(n,r){var t=new Float64Array(2);return t[0]=n[0]+r[0],t[1]=n[1]+r[1],t})),rr=t((function(n,r){var t=new Float64Array(2);return t[0]=n[0]-r[0],t[1]=n[1]-r[1],t}));function tr(n){return Math.sqrt(n[0]*n[0]+n[1]*n[1])}var er=tr,ur=t((function(n,r){var t=new Float64Array(2);return t[0]=r[0]*n,t[1]=r[1]*n,t}));new Float64Array(3),new Float64Array(3),new Float64Array(3);new Float64Array(16),new Float64Array(16),new Float64Array([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]);var ar=1,ir=2,fr=0,or=w,cr=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,a=o(n,t.b,t.c,o(cr,n,r,t.e));n=u,r=a,t=e}})),vr=function(n){return o(cr,e((function(n,r,t){return f(or,d(n,r),t)})),p,n)},br=function(n){return{$:1,a:n}},sr=t((function(n,r){return{$:3,a:n,b:r}})),lr=t((function(n,r){return{$:0,a:n,b:r}})),dr=t((function(n,r){return{$:1,a:n,b:r}})),hr=function(n){return{$:0,a:n}},$r=function(n){return{$:2,a:n}},gr=function(n){return{$:0,a:n}},pr={$:1},mr=t((function(n,r){return f(q,n,A(r))})),wr=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,a=f(n,t.a,r);n=u,r=a,t=e}})),yr=function(n){return o(wr,t((function(n,r){return r+1})),0,n)},Ar=k,kr=e((function(n,r,t){for(;;){if(s(n,r)>=1)return t;var e=n,u=r-1,a=f(or,r,t);n=e,r=u,t=a}})),Fr=t((function(n,r){return o(kr,n,r,p)})),jr=t((function(n,r){return o(Ar,n,f(Fr,0,yr(r)-1),r)})),_r=function(n){return o(wr,or,p,n)},Nr=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),Er=[],Lr=E,Tr=t((function(n,r){return O(r)/O(n)})),xr=Lr(f(Tr,2,32)),Or=c(Nr,0,xr,Er,Er),qr=F,Cr=L,zr=function(n){return n.length},Dr=t((function(n,r){return s(n,r)>0?n:r})),Rr=j,Br=t((function(n,r){for(;;){var t=f(Rr,32,n),e=t.b,u=f(or,{$:0,a:t.a},r);if(!e.b)return _r(u);n=e,r=u}})),Mr=t((function(n,r){for(;;){var t=Lr(r/32);if(1===t)return f(Rr,32,n).a;n=f(Br,n,p),r=t}})),Ir=t((function(n,r){if(r.a){var t=32*r.a,e=Cr(f(Tr,32,t-1)),u=n?_r(r.d):r.d,a=f(Mr,u,r.a);return c(Nr,zr(r.c)+t,f(Dr,5,e*xr),a,r.c)}return c(Nr,zr(r.c),xr,Er,r.c)})),Sr=a((function(n,r,t,e,u){for(;;){if(0>r)return f(Ir,!1,{d:e,a:t/32|0,c:u});var a={$:1,a:o(qr,32,r,n)};n=n,r=r-32,t=t,e=f(or,a,e),u=u}})),Pr=t((function(n,r){if(n>0){var t=n%32;return v(Sr,r,n-t-32,n,p,o(qr,t,n-t,r))}return Or})),Gr=function(n){return!n.$},Jr=function(n){return{$:0,a:n}},Wr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Yr=function(n){return n},Kr=J,Hr=Kr(0),Qr=u((function(n,r,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,v=a.b;if(v.b){var b=v.a,s=v.b;if(s.b){var l=s.b;return f(n,u,f(n,i,f(n,b,f(n,s.a,t>500?o(wr,n,r,_r(l)):c(Qr,n,r,t+1,l)))))}return f(n,u,f(n,i,f(n,b,r)))}return f(n,u,f(n,i,r))}return f(n,u,r)}return r})),Ur=e((function(n,r,t){return c(Qr,n,r,0,t)})),Vr=t((function(n,r){return o(Ur,t((function(r,t){return f(or,n(r),t)})),p,r)})),Xr=Y,Zr=t((function(n,r){return f(Xr,(function(r){return Kr(n(r))}),r)})),nt=e((function(n,r,t){return f(Xr,(function(r){return f(Xr,(function(t){return Kr(f(n,r,t))}),t)}),r)})),rt=function(n){return o(Ur,nt(or),Kr(p),n)},tt=fn,et=t((function(n,r){var t=r;return Q(f(Xr,tt(n),t))}));en.Task=un(Hr,e((function(n,r){return f(Zr,(function(){return 0}),rt(f(Vr,et(n),r)))})),e((function(){return Kr(0)})),t((function(n,r){return f(Zr,n,r)})));cn("Task");var ut,at=Hn,it=function(n){return{$:1,a:n}},ft=t((function(n,r){return{$:0,a:n,b:r}})),ot=function(n){var r=n.b;return f(ft,1664525*n.a+r>>>0,r)},ct=function(n){var r=n.a,t=277803737*(r^r>>>4+(r>>>28));return(t>>>22^t)>>>0},vt=t((function(n,r){return function(t){var e,u=ot(t),a=0>(e=r-n)?-e:e,i=ct(u);return d((134217728*(1*(67108863&ct(t)))+1*(134217727&i))/9007199254740992*a+n,ot(u))}})),bt=(ut=Yr,W((function(n){n(J(ut(Date.now())))}))),st=f(Xr,(function(n){return Kr((r=n,t=ot(f(ft,0,1013904223)),ot(f(ft,t.a+r>>>0,t.b))));var r,t}),bt),lt=t((function(n,r){return n(r)})),dt=e((function(n,r,t){if(r.b){var e=r.b,u=f(lt,r.a,t),a=u.b;return f(Xr,(function(){return o(dt,n,e,a)}),f(tt,n,u.a))}return Kr(t)})),ht=e((function(n,r,t){return Kr(t)})),$t=t((function(n,r){var t=r;return function(r){var e=t(r),u=e.b;return d(n(e.a),u)}}));en.Random=un(st,dt,ht,t((function(n,r){return f($t,n,r)})));var gt=cn("Random"),pt=t((function(n,r){return gt(f($t,n,r))})),mt=u((function(n,r,t,e){for(;;){if(1>r)return d(n,e);var u=t(e),a=u.b;n=f(or,u.a,n),r=r-1,t=t,e=a}})),wt=t((function(n,r){var t=r;return function(r){return c(mt,p,n,t,r)}})),yt=e((function(n,r,t){var e=r,u=t;return function(r){var t=e(r),a=t.a,i=u(t.b),o=i.b;return d(f(n,a,i.a),o)}})),At=N,kt=x,Ft=o(yt,t((function(n,r){return kt(-2*f(Tr,2.718281828459045,1-f(Dr,0,n)))*At(r)})),f(vt,0,1),f(vt,0,6.283185307179586)),jt=t((function(n,r){return f($t,(function(t){return t*r+n}),Ft)})),_t=Zn,Nt=f(pt,it,f(wt,10,o(yt,t((function(n,r){return{aO:f(_t,0,0),O:n,F:r,ap:f(_t,0,0)}})),f(jt,.7,.7),o(yt,_t,f(vt,0,600),f(vt,0,600))))),Et=function(n){return{$:0,a:n}},Lt=vn,Tt=t((function(n,r){return{$:0,a:n,b:r}})),xt=t((function(n,r){return{br:r,bz:n}})),Ot={$:-2},qt=Ot,Ct=Kr(f(xt,qt,qt)),zt=l,Dt=t((function(n,r){n:for(;;){if(-2===r.$)return pr;var t=r.c,e=r.d,u=r.e;switch(f(zt,n,r.b)){case 0:n=n,r=e;continue n;case 1:return gr(t);default:n=n,r=u;continue n}}})),Rt=a((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),Bt=a((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v(Rt,n,r,t,e,u);var a=e.d;b=e.e;return v(Rt,0,e.b,e.c,v(Rt,1,a.b,a.c,a.d,a.e),v(Rt,1,r,t,b,u))}var i=u.b,f=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return v(Rt,n,i,f,v(Rt,0,r,t,e,o),c);var b;return v(Rt,0,r,t,v(Rt,1,e.b,e.c,e.d,b=e.e),v(Rt,1,i,f,o,c))})),Mt=e((function(n,r,t){if(-2===t.$)return v(Rt,0,n,r,Ot,Ot);var e=t.a,u=t.b,a=t.c,i=t.d,c=t.e;switch(f(zt,n,u)){case 0:return v(Bt,e,u,a,o(Mt,n,r,i),c);case 1:return v(Rt,e,u,r,i,c);default:return v(Bt,e,u,a,i,o(Mt,n,r,c))}})),It=e((function(n,r,t){var e=o(Mt,n,r,t);if(-1!==e.$||e.a)return e;return v(Rt,1,e.b,e.c,e.d,e.e)})),St=t((function(n,r){var t=n.a,e=n.b,u=f(Dt,t,r);return o(It,t,1===u.$?y([e]):f(or,e,u.a),r)})),Pt=function(n){return W((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(J(0))}))},Gt=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,a=o(n,t.b,t.c,o(Gt,n,r,t.d));n=u,r=a,t=e}})),Jt=i((function(n,r,u,a,i,f){var v=o(Gt,e((function(t,e,a){n:for(;;){var i=a.a,f=a.b;if(i.b){var v=i.a,b=v.a,l=v.b,h=i.b;if(0>s(b,t)){t=t,e=e,a=d(h,o(n,b,l,f));continue n}return s(b,t)>0?d(i,o(u,t,e,f)):d(h,c(r,b,l,e,f))}return d(i,o(u,t,e,f))}})),d(vr(a),f),i),b=v.a,l=v.b;return o(wr,t((function(r,t){return o(n,r.a,r.b,t)})),l,b)})),Wt=on,Yt=Xn,Kt=Q,Ht=e((function(n,r,t){if(r.b){var e=r.a,u=r.b,a=Kt(f(Yt,e,f(Wt,n,e)));return f(Xr,(function(r){return o(Ht,n,u,o(It,e,r,t))}),a)}return Kr(t)})),Qt=e((function(n,r,t){var a=t.br,i=e((function(n,r,t){var e=t.c;return h(t.a,t.b,f(Xr,(function(){return e}),Pt(r)))})),c=o(wr,St,qt,r),v=b(Jt,e((function(n,r,t){var e=t.b,u=t.c;return h(f(or,n,t.a),e,u)})),u((function(n,r,t,e){var u=e.c;return h(e.a,o(It,n,t,e.b),u)})),i,c,a,h(p,qt,Kr(0))),s=v.a,l=v.b;return f(Xr,(function(n){return Kr(f(xt,c,n))}),f(Xr,(function(){return o(Ht,n,s,l)}),v.c))})),Ut=e((function(n,r,t){var e=f(Dt,r,t.bz);if(1===e.$)return Kr(t);var u=e.a;return f(Xr,(function(){return Kr(t)}),f(Xr,(function(r){return rt(f(Vr,(function(t){return f(tt,n,t(r))}),u))}),bt))})),Vt=e((function(n,r,t){return n(r(t))}));en.Time=un(Ct,Qt,Ut,0,t((function(n,r){return f(Tt,r.a,f(Vt,n,r.b))})));var Xt,Zt,ne,re=cn("Time"),te=t((function(n,r){return re(f(Tt,n,r))})),ee=vn(p),ue=t((function(n,r){return r.b?o(Ur,or,r,n):n})),ae=t((function(n,r){n:for(;;){if(n>0){if(r.b){n=n-1,r=r.b;continue n}return r}return r}})),ie=e((function(n,r,t){n:for(;;){if(n>0){if(r.b){var e=r.a;n=n-1,r=r.b,t=f(or,e,t);continue n}return t}return t}})),fe=t((function(n,r){return _r(o(ie,n,r,p))})),oe=e((function(n,r,t){if(r>0){var e=d(r,t);n:for(;;){r:for(;;){if(!e.b.b)return t;if(!e.b.b.b){if(1===e.a)break n;break r}switch(e.a){case 1:break n;case 2:var u=e.b;return y([u.a,u.b.a]);case 3:if(e.b.b.b.b){var a=e.b,i=a.b;return y([a.a,i.a,i.b.a])}break r;default:if(e.b.b.b.b&&e.b.b.b.b.b){var c=e.b,v=c.b,b=v.b,s=b.b,l=s.a,h=s.b;return f(or,c.a,f(or,v.a,f(or,b.a,f(or,l,n>1e3?f(fe,r-4,h):o(oe,n+1,r-4,h)))))}break r}}return t}return y([e.b.a])}return p})),ce=t((function(n,r){return o(oe,0,n,r)})),ve=t((function(n,r){if(0>n)return r;var t=function(n){if(n.b){return gr(n.b)}return pr}(f(ae,n,r));return 1===t.$?r:f(ue,f(ce,n,r),t.a)})),be=nr,se=ur,le=e((function(n,r,t){return f(be,t,f(se,1/r,n))})),de=function(n){return n[0]*n[0]+n[1]*n[1]},he=er,$e=e((function(n,r,t){var e=he(t);return s(e,r)>0?f(se,r/e,t):0>s(e,n)?f(se,n/e,t):t})),ge=function(n){var r=new Float64Array(2),t=1/tr(n);return r[0]=n[0]*t,r[1]=n[1]*t,r},pe=rr,me=t((function(n,r){var t=o($e,5,25,f(pe,r.F,n.F)),e=de(t),u=ge(t);return f(se,-5*r.O*r.O/e,u)})),we=t((function(n,r){var e=n,u=f(be,n.ap,o(wr,t((function(r,t){return o(le,f(me,n,r),r.O,t)})),f(_t,0,0),r));return $(e,{F:f(be,n.F,u),ap:u})})),ye=t((function(n,r){return d($(r,n.$?{D:n.a}:{D:f(jr,t((function(n,t){return f(we,t,f(ve,n,r.D))})),r.D)}),ee)})),Ae=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),ke=c(Ae,0,0,0,1),Fe=t((function(n,r){return f(An,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))})),je=C,_e=function(n){switch(n.$){case 0:return je(n.a)+"cm";case 1:return je(n.a)+"em";case 2:return je(n.a)+"ex";case 3:return je(n.a)+"in";case 4:return je(n.a)+"mm";case 5:return je(n.a);case 6:return je(n.a)+"pc";case 7:return je(n.a)+"%";case 8:return je(n.a)+"pt";default:return je(n.a)+"px"}},Ne=function(n){return f(Fe,"height",_e(n))},Ee=T,Le=function(n){var r,t,e=n.b,u=n.c,a=n.d,i=function(n){return Ee(1e4*n)/100};return r=y(["rgba(",je(i(n.a)),"%,",je(i(e)),"%,",je(i(u)),"%,",je((t=a,Ee(1e3*t)/1e3)),")"]),f(mr,"",r)},Te=function(n){return n.$?"none":Le(n.a)},xe=f(Vt,Fe("fill"),Te),Oe=xe({$:1}),qe=function(n){return{$:9,a:n}},Ce=wn(function(n){return"script"==n?"p":n}("http://www.w3.org/2000/svg")),ze=Ce("rect"),De=function(n){return f(Fe,"width",_e(n))},Re=f(ze,y([De(qe(600)),Ne(qe(600)),Oe,(Zt=ke,f(Fe,"stroke",Le(Zt))),(Xt=qe(3),f(Fe,"stroke-width",_e(Xt)))]),p),Be=Ce("svg"),Me=u((function(n,r,t,e){return f(Fe,"viewBox",f(mr," ",f(Vr,je,y([n,r,t,e]))))})),Ie=Ce("circle"),Se=function(n){return f(Fe,"cx",_e(n))},Pe=function(n){return f(Fe,"cy",_e(n))},Ge=c(Ae,211/255,215/255,207/255,1),Je=function(n){return f(Fe,"r",_e(n))},We=function(n){return{cz:n[0],cC:n[1]}},Ye=e((function(n,r,t){var e,u=We(t),a=u.cC;return f(Ie,g(y([Se(qe(u.cz)),Pe(qe(a)),Je(qe(r)),xe((e=Ge,{$:0,a:e}))]),n),p)})),Ke=function(n){var r=n.F;return o(Ye,p,function(n){return 20*n}(n.O),r)},He=at({b1:function(){return d({D:p},Nt)},cq:function(){return Lt(y([f(te,10,Et)]))},cw:ye,cx:function(n){return f(Be,y([De(qe(600)),Ne(qe(600)),c(Me,0,0,600,600)]),f(or,Re,f(Vr,Ke,n.D)))}});ne={Forces:{MutualRepulsion:{init:He(Jr(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?_(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,ne):n.Elm=ne}(window);