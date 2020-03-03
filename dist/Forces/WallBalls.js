!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function a(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(a){return n(r,t,e,u,a)}}}}}))}function i(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(a){return function(i){return n(r,t,e,u,a,i)}}}}}}))}function f(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,a){return 5===n.a?n.f(r,t,e,u,a):n(r)(t)(e)(u)(a)}function s(n,r,t,e,u,a,i){return 6===n.a?n.f(r,t,e,u,a,i):n(r)(t)(e)(u)(a)(i)}function b(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=b(n.a,r.a))||(t=b(n.b,r.b))?t:b(n.c,r.c);for(;n.b&&r.b&&!(t=b(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var l=t((function(n,r){var t=b(n,r);return 0>t?Zn:t?Xn:Vn}));function d(n,r){return{a:n,b:r}}function h(n,r,t){return{a:n,b:r,c:t}}function $(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}var g={$:0};function p(n,r){return{$:1,a:n,b:r}}var m=t(p);function y(n){for(var r=g,t=n.length;t--;)r=p(n[t],r);return r}function w(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var A=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),j=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,d(t,r)}));function k(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var _=Math.ceil,F=Math.floor,N=Math.round,E=Math.log;var O=t((function(n,r){return r.join(n)}));function L(n){return n+""}function T(n){return{$:2,b:n}}T((function(n){return"number"!=typeof n?z("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?fr(n):!isFinite(n)||n%1?z("an INT",n):fr(n)})),T((function(n){return"boolean"==typeof n?fr(n):z("a BOOL",n)})),T((function(n){return"number"==typeof n?fr(n):z("a FLOAT",n)})),T((function(n){return fr(n)})),T((function(n){return"string"==typeof n?fr(n):n instanceof String?fr(n+""):z("a STRING",n)}));var x=t((function(n,r){return B(n,r)}));function B(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?fr(n.c):z("null",r);case 3:return C(r)?D(n.b,r,y):z("a LIST",r);case 4:return C(r)?D(n.b,r,q):z("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return z("an OBJECT with a field named `"+t+"`",r);var e=B(n.b,r[t]);return Lr(e)?e:er(f(ar,t,e.a));case 7:var u=n.e;if(!C(r))return z("an ARRAY",r);if(u>=r.length)return z("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=B(n.b,r[u]);return Lr(e)?e:er(f(ir,u,e.a));case 8:if("object"!=typeof r||null===r||C(r))return z("an OBJECT",r);var a=g;for(var i in r)if(r.hasOwnProperty(i)){e=B(n.b,r[i]);if(!Lr(e))return er(f(ar,i,e.a));a=p(d(i,e.a),a)}return fr(lr(a));case 9:for(var o=n.f,c=n.g,v=0;c.length>v;v++){e=B(c[v],r);if(!Lr(e))return e;o=o(e.a)}return fr(o);case 10:e=B(n.b,r);return Lr(e)?B(n.h(e.a),r):e;case 11:for(var s=g,b=n.g;b.b;b=b.b){e=B(b.a,r);if(Lr(e))return e;s=p(e.a,s)}return er(or(lr(s)));case 1:return er(f(ur,n.a,r));case 0:return fr(n.a)}}function D(n,r,t){for(var e=r.length,u=Array(e),a=0;e>a;a++){var i=B(n,r[a]);if(!Lr(i))return er(f(ir,a,i.a));u[a]=i.a}return fr(t(u))}function C(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function q(n){return f(Or,n.length,(function(r){return n[r]}))}function z(n,r){return er(f(ur,"Expecting "+n,r))}function R(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return R(n.b,r.b);case 6:return n.d===r.d&&R(n.b,r.b);case 7:return n.e===r.e&&R(n.b,r.b);case 9:return n.f===r.f&&P(n.g,r.g);case 10:return n.h===r.h&&R(n.b,r.b);case 11:return P(n.g,r.g)}}function P(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!R(n[e],r[e]))return!1;return!0}function I(n){return{$:0,a:n}}function K(n){return{$:2,b:n,c:null}}var S=t((function(n,r){return{$:3,b:n,d:r}}));var M=0;function W(n){var r={$:0,e:M++,f:n,g:null,h:[]};return U(r),r}function G(n){return K((function(r){r(I(W(n)))}))}function J(n,r){n.h.push(r),U(n)}var Y=t((function(n,r){return K((function(t){J(n,r),t(I(0))}))}));var Q=!1,H=[];function U(n){if(H.push(n),!Q){for(Q=!0;n=H.shift();)V(n);Q=!1}}function V(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,U(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function X(n,r,t,e,u,a){var i=f(x,n,r?r.flags:void 0);Lr(i)||k(2);var o={},c=(i=t(i.a)).a,v=a(b,c),s=function(n,r){var t;for(var e in Z){var u=Z[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=rn(u,r)}return t}(o,b);function b(n,r){v(c=(i=f(e,n,c)).a,r),on(o,i.b,u(c))}return on(o,i.b,u(c)),s?{ports:s}:{}}var Z={};function nn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function rn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,a=n.e,i=n.f;function v(n){return f(S,v,{$:5,b:function(r){var f=r.a;return 0===r.$?o(u,t,f,n):a&&i?c(e,t,f.i,f.j,n):o(e,t,a?f.i:f.j,n)}})}return t.h=W(f(S,v,n.b))}var tn=t((function(n,r){return K((function(t){n.g(r),t(I(0))}))})),en=t((function(n,r){return f(Y,n.h,{$:0,a:r})}));function un(n){return function(r){return{$:1,k:n,l:r}}}var an=[],fn=!1;function on(n,r,t){if(an.push({p:n,q:r,r:t}),!fn){fn=!0;for(var e;e=an.shift();)cn(e.p,e.q,e.r);fn=!1}}function cn(n,r,t){var e={};for(var u in vn(!0,r,e,null),vn(!1,t,e,null),n)J(n[u],{$:"fx",a:e[u]||{i:g,j:g}})}function vn(n,r,t,e){switch(r.$){case 1:var u=r.k,a=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return f(n?Z[r].e:Z[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:g,j:g},n?t.i=p(r,t.i):t.j=p(r,t.j),t}(n,a,t[u]));case 2:for(var i=r.m;i.b;i=i.b)vn(n,i.a,t,e);return;case 3:return void vn(n,r.o,t,{s:r.n,t:e})}}var sn;var bn="undefined"!=typeof document?document:{};function ln(n,r){n.appendChild(r)}function dn(n){return{$:0,a:n}}var hn=t((function(n,r){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b||0,u.push(i)}return a+=u.length,{$:1,c:r,d:mn(t),e:u,f:n,b:a}}))})),$n=hn(void 0);t((function(n,r){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b.b||0,u.push(i)}return a+=u.length,{$:2,c:r,d:mn(t),e:u,f:n,b:a}}))}))(void 0);var gn=t((function(n,r){return{$:"a3",n:n,o:r}}));var pn;function mn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=r[e]||(r[e]={});"a3"===e&&"class"===u?yn(i,u,a):i[u]=a}else"className"===u?yn(r,u,a):r[u]=a}return r}function yn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function wn(n,r){var t=n.$;if(5===t)return wn(n.k||(n.k=n.m()),r);if(0===t)return bn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:r};return(i=wn(e,a)).elm_event_node_ref=a,i}if(3===t)return An(i=n.h(n.g),r,n.d),i;var i=n.f?bn.createElementNS(n.f,n.c):bn.createElement(n.c);sn&&"a"==n.c&&i.addEventListener("click",sn(i)),An(i,r,n.d);for(var f=n.e,o=0;f.length>o;o++)ln(i,wn(1===t?f[o]:f[o].b,r));return i}function An(n,r,t){for(var e in t){var u=t[e];"a1"===e?jn(n,u):"a0"===e?Fn(n,r,u):"a3"===e?kn(n,u):"a4"===e?_n(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function jn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function kn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function _n(n,r){for(var t in r){var e=r[t],u=e.f,a=e.o;void 0!==a?n.setAttributeNS(u,t,a):n.removeAttributeNS(u,t)}}function Fn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}n.removeEventListener(u,i)}i=Nn(r,a),n.addEventListener(u,i,pn&&{passive:2>xr(a)}),e[u]=i}else n.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){pn=!0}}))}catch(n){}function Nn(n,r){function t(r){var e=t.q,u=B(e.a,r);if(Lr(u)){for(var a,i=xr(e),f=u.a,o=i?3>i?f.a:f.y:f,c=1==i?f.b:3==i&&f.aK,v=(c&&r.stopPropagation(),(2==i?f.b:3==i&&f.aE)&&r.preventDefault(),n);a=v.j;){if("function"==typeof a)o=a(o);else for(var s=a.length;s--;)o=a[s](o);v=v.p}v(o,c)}}return t.q=r,t}function En(n,r){return n.$==r.$&&R(n.a,r.a)}function On(n,r){var t=[];return Tn(n,r,t,0),t}function Ln(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Tn(n,r,t,e){if(n!==r){var u=n.$,a=r.$;if(u!==a){if(1!==u||2!==a)return void Ln(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),a=1}switch(a){case 5:for(var i=n.l,f=r.l,o=i.length,c=o===f.length;c&&o--;)c=i[o]===f[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Tn(n.k,r.k,v,0),void(v.length>0&&Ln(t,1,e,v));case 4:for(var s=n.j,b=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return l&&s.length!==b.length?void Ln(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Ln(t,2,e,b),void Tn(d,h,t,e+1));case 0:return void(n.a!==r.a&&Ln(t,3,e,r.a));case 1:return void xn(n,r,t,e,Dn);case 2:return void xn(n,r,t,e,Cn);case 3:if(n.h!==r.h)return void Ln(t,0,e,r);var $=Bn(n.d,r.d);$&&Ln(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Ln(t,5,e,g))}}}function xn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var a=Bn(n.d,r.d);a&&Ln(t,4,e,a),u(n,r,t,e)}else Ln(t,0,e,r)}function Bn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var a=n[u],i=r[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&En(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=Bn(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Dn(n,r,t,e){var u=n.e,a=r.e,i=u.length,f=a.length;i>f?Ln(t,6,e,{v:f,i:i-f}):f>i&&Ln(t,7,e,{v:i,e:a});for(var o=f>i?i:f,c=0;o>c;c++){var v=u[c];Tn(v,a[c],t,++e),e+=v.b||0}}function Cn(n,r,t,e){for(var u=[],a={},i=[],f=n.e,o=r.e,c=f.length,v=o.length,s=0,b=0,l=e;c>s&&v>b;){var d=(F=f[s]).a,h=(N=o[b]).a,$=F.b,g=N.b,p=void 0,m=void 0;if(d!==h){var y=f[s+1],w=o[b+1];if(y){var A=y.a,j=y.b;m=h===A}if(w){var k=w.a,_=w.b;p=d===k}if(p&&m)Tn($,_,u,++l),qn(a,u,d,g,b,i),l+=$.b||0,zn(a,u,d,j,++l),l+=j.b||0,s+=2,b+=2;else if(p)l++,qn(a,u,h,g,b,i),Tn($,_,u,l),l+=$.b||0,s+=1,b+=2;else if(m)zn(a,u,d,$,++l),l+=$.b||0,Tn(j,g,u,++l),l+=j.b||0,s+=2,b+=1;else{if(!y||A!==k)break;zn(a,u,d,$,++l),qn(a,u,h,g,b,i),l+=$.b||0,Tn(j,_,u,++l),l+=j.b||0,s+=2,b+=2}}else Tn($,g,u,++l),l+=$.b||0,s++,b++}for(;c>s;){var F;l++,zn(a,u,(F=f[s]).a,$=F.b,l),l+=$.b||0,s++}for(;v>b;){var N,E=E||[];qn(a,u,(N=o[b]).a,N.b,void 0,E),b++}(u.length>0||i.length>0||E)&&Ln(t,8,e,{w:u,x:i,y:E})}function qn(n,r,t,e,u,a){var i=n[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(n[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var f=[];return Tn(i.z,e,f,i.r),i.r=u,void(i.s.s={w:f,A:i})}qn(n,r,t+"_elmW6BL",e,u,a)}function zn(n,r,t,e,u){var a=n[t];if(a){if(0===a.c){a.c=2;var i=[];return Tn(e,a.z,i,u),void Ln(r,9,u,{w:i,A:a})}zn(n,r,t+"_elmW6BL",e,u)}else{var f=Ln(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function Rn(n,r,t,e){!function n(r,t,e,u,a,i,f){var o=e[u],c=o.r;for(;c===a;){var v=o.$;if(1===v)Rn(r,t.k,o.s,f);else if(8===v){o.t=r,o.u=f,(s=o.s.w).length>0&&n(r,t,s,0,a,i,f)}else if(9===v){o.t=r,o.u=f;var s,b=o.s;if(b)b.A.s=r,(s=b.w).length>0&&n(r,t,s,0,a,i,f)}else o.t=r,o.u=f;if(u++,!(o=e[u])||(c=o.r)>i)return u}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,a+1,i,r.elm_event_node_ref)}for(var h=t.e,$=r.childNodes,g=0;h.length>g;g++){a++;var p=1===l?h[g]:h[g].b,m=a+(p.b||0);if(c>=a&&m>=c&&(u=n($[g],p,e,u,a,m,f),!(o=e[u])||(c=o.r)>i))return u;a=m}return u}(n,r,t,0,0,r.b,e)}function Pn(n,r,t,e){return 0===t.length?n:(Rn(n,r,t,e),In(n,t))}function In(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,a=Kn(u,e);u===n&&(n=a)}return n}function Kn(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=wn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return An(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return In(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,a=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(wn(u[e],r.u),a);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var i=t.A;return void 0!==i.r&&n.parentNode.removeChild(n),i.s=In(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=bn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;ln(t,2===u.c?u.s:wn(u.z,r.u))}return t}(t.y,r);n=In(n,t.w);for(var u=t.x,a=0;u.length>a;a++){var i=u[a],f=i.A,o=2===f.c?f.s:wn(f.z,r.u);n.insertBefore(o,n.childNodes[i.r])}e&&ln(n,e);return n}(n,r);case 5:return r.s(n);default:k(10)}}function Sn(n){if(3===n.nodeType)return dn(n.textContent);if(1!==n.nodeType)return dn("");for(var r=g,t=n.attributes,e=t.length;e--;){var u=t[e];r=p(f(gn,u.name,u.value),r)}var a=n.tagName.toLowerCase(),i=g,c=n.childNodes;for(e=c.length;e--;)i=p(Sn(c[e]),i);return o($n,a,r,i)}var Mn=u((function(n,r,t,e){return X(r,e,n.b3,n.cy,n.cs,(function(r,t){var u=n.cz,a=e.node,i=Sn(a);return Gn(t,(function(n){var t=u(n),e=On(i,t);a=Pn(a,i,e,r),i=t}))}))})),Wn=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function Gn(n,r){r(n);var t=0;function e(){t=1===t?0:(Wn(e),r(n),1)}return function(u,a){n=u,a?(r(n),2===t&&(t=1)):(0===t&&Wn(e),t=2)}}var Jn={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Yn=t((function(n,r){return K((function(){var t=setInterval((function(){W(r)}),n);return function(){clearInterval(t)}}))}));var Qn=t((function(n,r){return new Float64Array([n,r])})),Hn=t((function(n,r){var t=new Float64Array(2);return t[0]=n[0]+r[0],t[1]=n[1]+r[1],t}));var Un=t((function(n,r){var t=new Float64Array(2);return t[0]=r[0]*n,t[1]=r[1]*n,t}));new Float64Array(3),new Float64Array(3),new Float64Array(3);new Float64Array(16),new Float64Array(16),new Float64Array([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]);var Vn=1,Xn=2,Zn=0,nr=m,rr=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,a=o(n,t.b,t.c,o(rr,n,r,t.e));n=u,r=a,t=e}})),tr=function(n){return o(rr,e((function(n,r,t){return f(nr,d(n,r),t)})),g,n)},er=function(n){return{$:1,a:n}},ur=t((function(n,r){return{$:3,a:n,b:r}})),ar=t((function(n,r){return{$:0,a:n,b:r}})),ir=t((function(n,r){return{$:1,a:n,b:r}})),fr=function(n){return{$:0,a:n}},or=function(n){return{$:2,a:n}},cr=function(n){return{$:0,a:n}},vr={$:1},sr=t((function(n,r){return f(O,n,w(r))})),br=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,a=f(n,t.a,r);n=u,r=a,t=e}})),lr=function(n){return o(br,nr,g,n)},dr=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),hr=[],$r=_,gr=t((function(n,r){return E(r)/E(n)})),pr=$r(f(gr,2,32)),mr=c(dr,0,pr,hr,hr),yr=A,wr=F,Ar=function(n){return n.length},jr=t((function(n,r){return b(n,r)>0?n:r})),kr=j,_r=t((function(n,r){for(;;){var t=f(kr,32,n),e=t.b,u=f(nr,{$:0,a:t.a},r);if(!e.b)return lr(u);n=e,r=u}})),Fr=t((function(n,r){for(;;){var t=$r(r/32);if(1===t)return f(kr,32,n).a;n=f(_r,n,g),r=t}})),Nr=t((function(n,r){if(r.a){var t=32*r.a,e=wr(f(gr,32,t-1)),u=n?lr(r.d):r.d,a=f(Fr,u,r.a);return c(dr,Ar(r.c)+t,f(jr,5,e*pr),a,r.c)}return c(dr,Ar(r.c),pr,hr,r.c)})),Er=a((function(n,r,t,e,u){for(;;){if(0>r)return f(Nr,!1,{d:e,a:t/32|0,c:u});var a={$:1,a:o(yr,32,r,n)};n=n,r=r-32,t=t,e=f(nr,a,e),u=u}})),Or=t((function(n,r){if(n>0){var t=n%32;return v(Er,r,n-t-32,n,g,o(yr,t,n-t,r))}return mr})),Lr=function(n){return!n.$},Tr=function(n){return{$:0,a:n}},xr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Br=function(n){return n},Dr=I,Cr=Dr(0),qr=u((function(n,r,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,v=a.b;if(v.b){var s=v.a,b=v.b;if(b.b){var l=b.b;return f(n,u,f(n,i,f(n,s,f(n,b.a,t>500?o(br,n,r,lr(l)):c(qr,n,r,t+1,l)))))}return f(n,u,f(n,i,f(n,s,r)))}return f(n,u,f(n,i,r))}return f(n,u,r)}return r})),zr=e((function(n,r,t){return c(qr,n,r,0,t)})),Rr=t((function(n,r){return o(zr,t((function(r,t){return f(nr,n(r),t)})),g,r)})),Pr=S,Ir=t((function(n,r){return f(Pr,(function(r){return Dr(n(r))}),r)})),Kr=e((function(n,r,t){return f(Pr,(function(r){return f(Pr,(function(t){return Dr(f(n,r,t))}),t)}),r)})),Sr=function(n){return o(zr,Kr(nr),Dr(g),n)},Mr=tn,Wr=t((function(n,r){var t=r;return G(f(Pr,Mr(n),t))}));Z.Task=nn(Cr,e((function(n,r){return f(Ir,(function(){return 0}),Sr(f(Rr,Wr(n),r)))})),e((function(){return Dr(0)})),t((function(n,r){return f(Ir,n,r)})));un("Task");var Gr,Jr=Mn,Yr=function(n){return{$:1,a:n}},Qr=t((function(n,r){return{$:0,a:n,b:r}})),Hr=function(n){var r=n.b;return f(Qr,1664525*n.a+r>>>0,r)},Ur=function(n){var r=n.a,t=277803737*(r^r>>>4+(r>>>28));return(t>>>22^t)>>>0},Vr=t((function(n,r){return function(t){var e,u=Hr(t),a=0>(e=r-n)?-e:e,i=Ur(u);return d((134217728*(1*(67108863&Ur(t)))+1*(134217727&i))/9007199254740992*a+n,Hr(u))}})),Xr=(Gr=Br,K((function(n){n(I(Gr(Date.now())))}))),Zr=f(Pr,(function(n){return Dr((r=n,t=Hr(f(Qr,0,1013904223)),Hr(f(Qr,t.a+r>>>0,t.b))));var r,t}),Xr),nt=t((function(n,r){return n(r)})),rt=e((function(n,r,t){if(r.b){var e=r.b,u=f(nt,r.a,t),a=u.b;return f(Pr,(function(){return o(rt,n,e,a)}),f(Mr,n,u.a))}return Dr(t)})),tt=e((function(n,r,t){return Dr(t)})),et=t((function(n,r){var t=r;return function(r){var e=t(r),u=e.b;return d(n(e.a),u)}}));Z.Random=nn(Zr,rt,tt,t((function(n,r){return f(et,n,r)})));var ut=un("Random"),at=t((function(n,r){return ut(f(et,n,r))})),it=u((function(n,r,t,e){for(;;){if(1>r)return d(n,e);var u=t(e),a=u.b;n=f(nr,u.a,n),r=r-1,t=t,e=a}})),ft=t((function(n,r){var t=r;return function(r){return c(it,g,n,t,r)}})),ot=e((function(n,r,t){var e=r,u=t;return function(r){var t=e(r),a=t.a,i=u(t.b),o=i.b;return d(f(n,a,i.a),o)}})),ct=Qn,vt=f(at,Yr,f(ft,10,o(ot,t((function(n,r){return{aQ:f(ct,0,0),D:n,O:r,ap:f(ct,0,0)}})),f(Vr,.3,1),o(ot,ct,f(Vr,0,600),f(Vr,0,600))))),st=function(n){return{$:0,a:n}},bt=t((function(n,r){return{$:0,a:n,b:r}})),lt=t((function(n,r){return{bt:r,bB:n}})),dt={$:-2},ht=dt,$t=Dr(f(lt,ht,ht)),gt=l,pt=t((function(n,r){n:for(;;){if(-2===r.$)return vr;var t=r.c,e=r.d,u=r.e;switch(f(gt,n,r.b)){case 0:n=n,r=e;continue n;case 1:return cr(t);default:n=n,r=u;continue n}}})),mt=a((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),yt=a((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v(mt,n,r,t,e,u);var a=e.d;s=e.e;return v(mt,0,e.b,e.c,v(mt,1,a.b,a.c,a.d,a.e),v(mt,1,r,t,s,u))}var i=u.b,f=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return v(mt,n,i,f,v(mt,0,r,t,e,o),c);var s;return v(mt,0,r,t,v(mt,1,e.b,e.c,e.d,s=e.e),v(mt,1,i,f,o,c))})),wt=e((function(n,r,t){if(-2===t.$)return v(mt,0,n,r,dt,dt);var e=t.a,u=t.b,a=t.c,i=t.d,c=t.e;switch(f(gt,n,u)){case 0:return v(yt,e,u,a,o(wt,n,r,i),c);case 1:return v(mt,e,u,r,i,c);default:return v(yt,e,u,a,i,o(wt,n,r,c))}})),At=e((function(n,r,t){var e=o(wt,n,r,t);if(-1!==e.$||e.a)return e;return v(mt,1,e.b,e.c,e.d,e.e)})),jt=t((function(n,r){var t=n.a,e=n.b,u=f(pt,t,r);return o(At,t,1===u.$?y([e]):f(nr,e,u.a),r)})),kt=function(n){return K((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(I(0))}))},_t=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,a=o(n,t.b,t.c,o(_t,n,r,t.d));n=u,r=a,t=e}})),Ft=i((function(n,r,u,a,i,f){var v=o(_t,e((function(t,e,a){n:for(;;){var i=a.a,f=a.b;if(i.b){var v=i.a,s=v.a,l=v.b,h=i.b;if(0>b(s,t)){t=t,e=e,a=d(h,o(n,s,l,f));continue n}return b(s,t)>0?d(i,o(u,t,e,f)):d(h,c(r,s,l,e,f))}return d(i,o(u,t,e,f))}})),d(tr(a),f),i),s=v.a,l=v.b;return o(br,t((function(r,t){return o(n,r.a,r.b,t)})),l,s)})),Nt=en,Et=Yn,Ot=G,Lt=e((function(n,r,t){if(r.b){var e=r.a,u=r.b,a=Ot(f(Et,e,f(Nt,n,e)));return f(Pr,(function(r){return o(Lt,n,u,o(At,e,r,t))}),a)}return Dr(t)})),Tt=e((function(n,r,t){var a=t.bt,i=e((function(n,r,t){var e=t.c;return h(t.a,t.b,f(Pr,(function(){return e}),kt(r)))})),c=o(br,jt,ht,r),v=s(Ft,e((function(n,r,t){var e=t.b,u=t.c;return h(f(nr,n,t.a),e,u)})),u((function(n,r,t,e){var u=e.c;return h(e.a,o(At,n,t,e.b),u)})),i,c,a,h(g,ht,Dr(0))),b=v.a,l=v.b;return f(Pr,(function(n){return Dr(f(lt,c,n))}),f(Pr,(function(){return o(Lt,n,b,l)}),v.c))})),xt=e((function(n,r,t){var e=f(pt,r,t.bB);if(1===e.$)return Dr(t);var u=e.a;return f(Pr,(function(){return Dr(t)}),f(Pr,(function(r){return Sr(f(Rr,(function(t){return f(Mr,n,t(r))}),u))}),Xr))})),Bt=e((function(n,r,t){return n(r(t))}));Z.Time=nn($t,Tt,xt,0,t((function(n,r){return f(bt,r.a,f(Bt,n,r.b))})));var Dt,Ct,qt,zt=un("Time"),Rt=t((function(n,r){return zt(f(bt,n,r))})),Pt=Hn,It=Un,Kt=e((function(n,r,t){return f(Pt,t,f(It,1/r,n))})),St=function(n){return 30*n},Mt=function(n){return{aO:n[0],aP:n[1]}},Wt=function(n){var r=f(ct,.01,0),t=St(n.D),e=Mt(n.O),u=0>b(e.aO,t)?5:b(e.aO,600-t)>0?-5:0,a=0>b(e.aP,t)?5:b(e.aP,600-t)>0?-5:0,i=f(ct,u,a),c=f(Pt,n.ap,o(Kt,r,n.D,o(Kt,f(ct,0,.1),n.D,o(Kt,i,n.D,f(ct,0,0)))));return $(n,{O:f(Pt,n.O,c),ap:c})},Gt=function(n){return{$:2,m:n}}(g),Jt=t((function(n,r){return d($(r,n.$?{K:n.a}:{K:f(Rr,Wt,r.K)}),Gt)})),Yt=c(u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),0,0,0,1),Qt=hn(function(n){return"script"==n?"p":n}("http://www.w3.org/2000/svg")),Ht=Qt("circle"),Ut=t((function(n,r){return f(gn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))})),Vt=L,Xt=function(n){switch(n.$){case 0:return Vt(n.a)+"cm";case 1:return Vt(n.a)+"em";case 2:return Vt(n.a)+"ex";case 3:return Vt(n.a)+"in";case 4:return Vt(n.a)+"mm";case 5:return Vt(n.a);case 6:return Vt(n.a)+"pc";case 7:return Vt(n.a)+"%";case 8:return Vt(n.a)+"pt";default:return Vt(n.a)+"px"}},Zt=function(n){return f(Ut,"cy",Xt(n))},ne=N,re=function(n){var r,t,e=n.b,u=n.c,a=n.d,i=function(n){return ne(1e4*n)/100};return r=y(["rgba(",Vt(i(n.a)),"%,",Vt(i(e)),"%,",Vt(i(u)),"%,",Vt((t=a,ne(1e3*t)/1e3)),")"]),f(sr,"",r)},te=function(n){return n.$?"none":re(n.a)},ee=f(Bt,Ut("fill"),te),ue=function(n){return{$:9,a:n}},ae=function(n){return f(Ut,"r",Xt(n))},ie=function(n){var r,t,e=n.D,u=Mt(n.O),a=u.aP;return f(Ht,y([(t=ue(u.aO),f(Ut,"cx",Xt(t))),Zt(ue(a)),ae(ue(St(e))),ee((r=Yt,{$:0,a:r}))]),g)},fe=function(n){return f(Ut,"height",Xt(n))},oe=ee({$:1}),ce=Qt("rect"),ve=function(n){return f(Ut,"width",Xt(n))},se=f(ce,y([ve(ue(600)),fe(ue(600)),oe,(Ct=Yt,f(Ut,"stroke",re(Ct))),(Dt=ue(3),f(Ut,"stroke-width",Xt(Dt)))]),g),be=Qt("svg"),le=u((function(n,r,t,e){return f(Ut,"viewBox",f(sr," ",f(Rr,Vt,y([n,r,t,e]))))})),de=Jr({b3:function(){return d({K:g},vt)},cs:function(){return f(Rt,20,st)},cy:Jt,cz:function(n){return f(be,y([ve(ue(600)),fe(ue(600)),c(le,0,0,600,600)]),f(nr,se,f(Rr,ie,n.K)))}});qt={Forces:{WallBalls:{init:de(Tr(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?k(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,qt):n.Elm=qt}(window);