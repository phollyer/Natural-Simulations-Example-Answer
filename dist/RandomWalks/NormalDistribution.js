!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function i(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(i){return n(r,t,e,u,i)}}}}}))}function f(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(i){return function(f){return n(r,t,e,u,i,f)}}}}}}))}function a(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}function s(n,r,t,e,u,i,f){return 6===n.a?n.f(r,t,e,u,i,f):n(r)(t)(e)(u)(i)(f)}function b(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=b(n.a,r.a))||(t=b(n.b,r.b))?t:b(n.c,r.c);for(;n.b&&r.b&&!(t=b(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var d=t((function(n,r){var t=b(n,r);return 0>t?Vn:t?Qn:Kn}));function l(n,r){return{a:n,b:r}}function h(n,r,t){return{a:n,b:r,c:t}}var $={$:0};function g(n,r){return{$:1,a:n,b:r}}var m=t(g);function p(n){for(var r=$,t=n.length;t--;)r=g(n[t],r);return r}function y(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var w=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),j=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,l(t,r)}));function A(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var _=Math.cos;var k=Math.ceil,N=Math.floor,E=Math.round,L=Math.sqrt,T=Math.log;var x=t((function(n,r){return r.join(n)}));function F(n){return n+""}function q(n){return{$:2,b:n}}q((function(n){return"number"!=typeof n?I("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?ir(n):!isFinite(n)||n%1?I("an INT",n):ir(n)})),q((function(n){return"boolean"==typeof n?ir(n):I("a BOOL",n)})),q((function(n){return"number"==typeof n?ir(n):I("a FLOAT",n)})),q((function(n){return ir(n)})),q((function(n){return"string"==typeof n?ir(n):n instanceof String?ir(n+""):I("a STRING",n)}));var B=t((function(n,r){return C(n,r)}));function C(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?ir(n.c):I("null",r);case 3:return O(r)?R(n.b,r,p):I("a LIST",r);case 4:return O(r)?R(n.b,r,z):I("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return I("an OBJECT with a field named `"+t+"`",r);var e=C(n.b,r[t]);return Lr(e)?e:rr(a(er,t,e.a));case 7:var u=n.e;if(!O(r))return I("an ARRAY",r);if(u>=r.length)return I("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=C(n.b,r[u]);return Lr(e)?e:rr(a(ur,u,e.a));case 8:if("object"!=typeof r||null===r||O(r))return I("an OBJECT",r);var i=$;for(var f in r)if(r.hasOwnProperty(f)){e=C(n.b,r[f]);if(!Lr(e))return rr(a(er,f,e.a));i=g(l(f,e.a),i)}return ir(sr(i));case 9:for(var o=n.f,c=n.g,v=0;c.length>v;v++){e=C(c[v],r);if(!Lr(e))return e;o=o(e.a)}return ir(o);case 10:e=C(n.b,r);return Lr(e)?C(n.h(e.a),r):e;case 11:for(var s=$,b=n.g;b.b;b=b.b){e=C(b.a,r);if(Lr(e))return e;s=g(e.a,s)}return rr(fr(sr(s)));case 1:return rr(a(tr,n.a,r));case 0:return ir(n.a)}}function R(n,r,t){for(var e=r.length,u=Array(e),i=0;e>i;i++){var f=C(n,r[i]);if(!Lr(f))return rr(a(ur,i,f.a));u[i]=f.a}return ir(t(u))}function O(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function z(n){return a(Er,n.length,(function(r){return n[r]}))}function I(n,r){return rr(a(tr,"Expecting "+n,r))}function M(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return M(n.b,r.b);case 6:return n.d===r.d&&M(n.b,r.b);case 7:return n.e===r.e&&M(n.b,r.b);case 9:return n.f===r.f&&S(n.g,r.g);case 10:return n.h===r.h&&M(n.b,r.b);case 11:return S(n.g,r.g)}}function S(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!M(n[e],r[e]))return!1;return!0}function D(n){return{$:0,a:n}}function U(n){return{$:2,b:n,c:null}}var P=t((function(n,r){return{$:3,b:n,d:r}}));var W=0;function G(n){var r={$:0,e:W++,f:n,g:null,h:[]};return V(r),r}function J(n){return U((function(r){r(D(G(n)))}))}function Y(n,r){n.h.push(r),V(n)}var H=t((function(n,r){return U((function(t){Y(n,r),t(D(0))}))}));var K=!1,Q=[];function V(n){if(Q.push(n),!K){for(K=!0;n=Q.shift();)X(n);K=!1}}function X(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,V(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function Z(n,r,t,e,u,i){var f=a(B,n,r?r.flags:void 0);Lr(f)||A(2);var o={},c=(f=t(f.a)).a,v=i(b,c),s=function(n,r){var t;for(var e in nn){var u=nn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=tn(u,r)}return t}(o,b);function b(n,r){v(c=(f=a(e,n,c)).a,r),cn(o,f.b,u(c))}return cn(o,f.b,u(c)),s?{ports:s}:{}}var nn={};function rn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function tn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,i=n.e,f=n.f;function v(n){return a(P,v,{$:5,b:function(r){var a=r.a;return 0===r.$?o(u,t,a,n):i&&f?c(e,t,a.i,a.j,n):o(e,t,i?a.i:a.j,n)}})}return t.h=G(a(P,v,n.b))}var en=t((function(n,r){return U((function(t){n.g(r),t(D(0))}))})),un=t((function(n,r){return a(H,n.h,{$:0,a:r})}));function fn(n){return function(r){return{$:1,k:n,l:r}}}var an=[],on=!1;function cn(n,r,t){if(an.push({p:n,q:r,r:t}),!on){on=!0;for(var e;e=an.shift();)vn(e.p,e.q,e.r);on=!1}}function vn(n,r,t){var e={};for(var u in sn(!0,r,e,null),sn(!1,t,e,null),n)Y(n[u],{$:"fx",a:e[u]||{i:$,j:$}})}function sn(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return a(n?nn[r].e:nn[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:$,j:$},n?t.i=g(r,t.i):t.j=g(r,t.j),t}(n,i,t[u]));case 2:for(var f=r.m;f.b;f=f.b)sn(n,f.a,t,e);return;case 3:return void sn(n,r.o,t,{s:r.n,t:e})}}var bn;var dn="undefined"!=typeof document?document:{};function ln(n,r){n.appendChild(r)}function hn(n){return{$:0,a:n}}var $n=t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var f=e.a;i+=f.b||0,u.push(f)}return i+=u.length,{$:1,c:r,d:yn(t),e:u,f:n,b:i}}))})),gn=$n(void 0);t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var f=e.a;i+=f.b.b||0,u.push(f)}return i+=u.length,{$:2,c:r,d:yn(t),e:u,f:n,b:i}}))}))(void 0);var mn=t((function(n,r){return{$:"a3",n:n,o:r}}));var pn;function yn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var f=r[e]||(r[e]={});"a3"===e&&"class"===u?wn(f,u,i):f[u]=i}else"className"===u?wn(r,u,i):r[u]=i}return r}function wn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function jn(n,r){var t=n.$;if(5===t)return jn(n.k||(n.k=n.m()),r);if(0===t)return dn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(f=jn(e,i)).elm_event_node_ref=i,f}if(3===t)return An(f=n.h(n.g),r,n.d),f;var f=n.f?dn.createElementNS(n.f,n.c):dn.createElement(n.c);bn&&"a"==n.c&&f.addEventListener("click",bn(f)),An(f,r,n.d);for(var a=n.e,o=0;a.length>o;o++)ln(f,jn(1===t?a[o]:a[o].b,r));return f}function An(n,r,t){for(var e in t){var u=t[e];"a1"===e?_n(n,u):"a0"===e?En(n,r,u):"a3"===e?kn(n,u):"a4"===e?Nn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function _n(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function kn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Nn(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function En(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],f=e[u];if(i){if(f){if(f.q.$===i.$){f.q=i;continue}n.removeEventListener(u,f)}f=Ln(r,i),n.addEventListener(u,f,pn&&{passive:2>xr(i)}),e[u]=f}else n.removeEventListener(u,f),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){pn=!0}}))}catch(n){}function Ln(n,r){function t(r){var e=t.q,u=C(e.a,r);if(Lr(u)){for(var i,f=xr(e),a=u.a,o=f?3>f?a.a:a.y:a,c=1==f?a.b:3==f&&a.aH,v=(c&&r.stopPropagation(),(2==f?a.b:3==f&&a.aB)&&r.preventDefault(),n);i=v.j;){if("function"==typeof i)o=i(o);else for(var s=i.length;s--;)o=i[s](o);v=v.p}v(o,c)}}return t.q=r,t}function Tn(n,r){return n.$==r.$&&M(n.a,r.a)}function xn(n,r){var t=[];return qn(n,r,t,0),t}function Fn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function qn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void Fn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var f=n.l,a=r.l,o=f.length,c=o===a.length;c&&o--;)c=f[o]===a[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return qn(n.k,r.k,v,0),void(v.length>0&&Fn(t,1,e,v));case 4:for(var s=n.j,b=r.j,d=!1,l=n.k;4===l.$;)d=!0,"object"!=typeof s?s=[s,l.j]:s.push(l.j),l=l.k;for(var h=r.k;4===h.$;)d=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return d&&s.length!==b.length?void Fn(t,0,e,r):((d?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Fn(t,2,e,b),void qn(l,h,t,e+1));case 0:return void(n.a!==r.a&&Fn(t,3,e,r.a));case 1:return void Bn(n,r,t,e,Rn);case 2:return void Bn(n,r,t,e,On);case 3:if(n.h!==r.h)return void Fn(t,0,e,r);var $=Cn(n.d,r.d);$&&Fn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Fn(t,5,e,g))}}}function Bn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=Cn(n.d,r.d);i&&Fn(t,4,e,i),u(n,r,t,e)}else Fn(t,0,e,r)}function Cn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],f=r[u];i===f&&"value"!==u&&"checked"!==u||"a0"===t&&Tn(i,f)||((e=e||{})[u]=f)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var a=Cn(n[u],r[u]||{},u);a&&((e=e||{})[u]=a)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Rn(n,r,t,e){var u=n.e,i=r.e,f=u.length,a=i.length;f>a?Fn(t,6,e,{v:a,i:f-a}):a>f&&Fn(t,7,e,{v:f,e:i});for(var o=a>f?f:a,c=0;o>c;c++){var v=u[c];qn(v,i[c],t,++e),e+=v.b||0}}function On(n,r,t,e){for(var u=[],i={},f=[],a=n.e,o=r.e,c=a.length,v=o.length,s=0,b=0,d=e;c>s&&v>b;){var l=(N=a[s]).a,h=(E=o[b]).a,$=N.b,g=E.b,m=void 0,p=void 0;if(l!==h){var y=a[s+1],w=o[b+1];if(y){var j=y.a,A=y.b;p=h===j}if(w){var _=w.a,k=w.b;m=l===_}if(m&&p)qn($,k,u,++d),zn(i,u,l,g,b,f),d+=$.b||0,In(i,u,l,A,++d),d+=A.b||0,s+=2,b+=2;else if(m)d++,zn(i,u,h,g,b,f),qn($,k,u,d),d+=$.b||0,s+=1,b+=2;else if(p)In(i,u,l,$,++d),d+=$.b||0,qn(A,g,u,++d),d+=A.b||0,s+=2,b+=1;else{if(!y||j!==_)break;In(i,u,l,$,++d),zn(i,u,h,g,b,f),d+=$.b||0,qn(A,k,u,++d),d+=A.b||0,s+=2,b+=2}}else qn($,g,u,++d),d+=$.b||0,s++,b++}for(;c>s;){var N;d++,In(i,u,(N=a[s]).a,$=N.b,d),d+=$.b||0,s++}for(;v>b;){var E,L=L||[];zn(i,u,(E=o[b]).a,E.b,void 0,L),b++}(u.length>0||f.length>0||L)&&Fn(t,8,e,{w:u,x:f,y:L})}function zn(n,r,t,e,u,i){var f=n[t];if(!f)return i.push({r:u,A:f={c:0,z:e,r:u,s:void 0}}),void(n[t]=f);if(1===f.c){i.push({r:u,A:f}),f.c=2;var a=[];return qn(f.z,e,a,f.r),f.r=u,void(f.s.s={w:a,A:f})}zn(n,r,t+"_elmW6BL",e,u,i)}function In(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var f=[];return qn(e,i.z,f,u),void Fn(r,9,u,{w:f,A:i})}In(n,r,t+"_elmW6BL",e,u)}else{var a=Fn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:a}}}function Mn(n,r,t,e){!function n(r,t,e,u,i,f,a){var o=e[u],c=o.r;for(;c===i;){var v=o.$;if(1===v)Mn(r,t.k,o.s,a);else if(8===v){o.t=r,o.u=a,(s=o.s.w).length>0&&n(r,t,s,0,i,f,a)}else if(9===v){o.t=r,o.u=a;var s,b=o.s;if(b)b.A.s=r,(s=b.w).length>0&&n(r,t,s,0,i,f,a)}else o.t=r,o.u=a;if(u++,!(o=e[u])||(c=o.r)>f)return u}var d=t.$;if(4===d){for(var l=t.k;4===l.$;)l=l.k;return n(r,l,e,u,i+1,f,r.elm_event_node_ref)}for(var h=t.e,$=r.childNodes,g=0;h.length>g;g++){i++;var m=1===d?h[g]:h[g].b,p=i+(m.b||0);if(c>=i&&p>=c&&(u=n($[g],m,e,u,i,p,a),!(o=e[u])||(c=o.r)>f))return u;i=p}return u}(n,r,t,0,0,r.b,e)}function Sn(n,r,t,e){return 0===t.length?n:(Mn(n,r,t,e),Dn(n,t))}function Dn(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,i=Un(u,e);u===n&&(n=i)}return n}function Un(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=jn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return An(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Dn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(jn(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var f=t.A;return void 0!==f.r&&n.parentNode.removeChild(n),f.s=Dn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=dn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;ln(t,2===u.c?u.s:jn(u.z,r.u))}return t}(t.y,r);n=Dn(n,t.w);for(var u=t.x,i=0;u.length>i;i++){var f=u[i],a=f.A,o=2===a.c?a.s:jn(a.z,r.u);n.insertBefore(o,n.childNodes[f.r])}e&&ln(n,e);return n}(n,r);case 5:return r.s(n);default:A(10)}}function Pn(n){if(3===n.nodeType)return hn(n.textContent);if(1!==n.nodeType)return hn("");for(var r=$,t=n.attributes,e=t.length;e--;){var u=t[e];r=g(a(mn,u.name,u.value),r)}var i=n.tagName.toLowerCase(),f=$,c=n.childNodes;for(e=c.length;e--;)f=g(Pn(c[e]),f);return o(gn,i,r,f)}var Wn=u((function(n,r,t,e){return Z(r,e,n.b$,n.cu,n.co,(function(r,t){var u=n.cv,i=e.node,f=Pn(i);return Jn(t,(function(n){var t=u(n),e=xn(f,t);i=Sn(i,f,e,r),f=t}))}))})),Gn=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function Jn(n,r){r(n);var t=0;function e(){t=1===t?0:(Gn(e),r(n),1)}return function(u,i){n=u,i?(r(n),2===t&&(t=1)):(0===t&&Gn(e),t=2)}}var Yn={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Hn=t((function(n,r){return U((function(){var t=setInterval((function(){G(r)}),n);return function(){clearInterval(t)}}))}));var Kn=1,Qn=2,Vn=0,Xn=m,Zn=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,i=o(n,t.b,t.c,o(Zn,n,r,t.e));n=u,r=i,t=e}})),nr=function(n){return o(Zn,e((function(n,r,t){return a(Xn,l(n,r),t)})),$,n)},rr=function(n){return{$:1,a:n}},tr=t((function(n,r){return{$:3,a:n,b:r}})),er=t((function(n,r){return{$:0,a:n,b:r}})),ur=t((function(n,r){return{$:1,a:n,b:r}})),ir=function(n){return{$:0,a:n}},fr=function(n){return{$:2,a:n}},ar=function(n){return{$:0,a:n}},or={$:1},cr=t((function(n,r){return a(x,n,y(r))})),vr=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=a(n,t.a,r);n=u,r=i,t=e}})),sr=function(n){return o(vr,Xn,$,n)},br=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),dr=[],lr=k,hr=t((function(n,r){return T(r)/T(n)})),$r=lr(a(hr,2,32)),gr=c(br,0,$r,dr,dr),mr=w,pr=N,yr=function(n){return n.length},wr=t((function(n,r){return b(n,r)>0?n:r})),jr=j,Ar=t((function(n,r){for(;;){var t=a(jr,32,n),e=t.b,u=a(Xn,{$:0,a:t.a},r);if(!e.b)return sr(u);n=e,r=u}})),_r=t((function(n,r){for(;;){var t=lr(r/32);if(1===t)return a(jr,32,n).a;n=a(Ar,n,$),r=t}})),kr=t((function(n,r){if(r.a){var t=32*r.a,e=pr(a(hr,32,t-1)),u=n?sr(r.d):r.d,i=a(_r,u,r.a);return c(br,yr(r.c)+t,a(wr,5,e*$r),i,r.c)}return c(br,yr(r.c),$r,dr,r.c)})),Nr=i((function(n,r,t,e,u){for(;;){if(0>r)return a(kr,!1,{d:e,a:t/32|0,c:u});var i={$:1,a:o(mr,32,r,n)};n=n,r=r-32,t=t,e=a(Xn,i,e),u=u}})),Er=t((function(n,r){if(n>0){var t=n%32;return v(Nr,r,n-t-32,n,$,o(mr,t,n-t,r))}return gr})),Lr=function(n){return!n.$},Tr=function(n){return{$:0,a:n}},xr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Fr=function(n){return n},qr=D,Br=qr(0),Cr=u((function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var f=i.a,v=i.b;if(v.b){var s=v.a,b=v.b;if(b.b){var d=b.b;return a(n,u,a(n,f,a(n,s,a(n,b.a,t>500?o(vr,n,r,sr(d)):c(Cr,n,r,t+1,d)))))}return a(n,u,a(n,f,a(n,s,r)))}return a(n,u,a(n,f,r))}return a(n,u,r)}return r})),Rr=e((function(n,r,t){return c(Cr,n,r,0,t)})),Or=t((function(n,r){return o(Rr,t((function(r,t){return a(Xn,n(r),t)})),$,r)})),zr=P,Ir=t((function(n,r){return a(zr,(function(r){return qr(n(r))}),r)})),Mr=e((function(n,r,t){return a(zr,(function(r){return a(zr,(function(t){return qr(a(n,r,t))}),t)}),r)})),Sr=function(n){return o(Rr,Mr(Xn),qr($),n)},Dr=en,Ur=t((function(n,r){var t=r;return J(a(zr,Dr(n),t))}));nn.Task=rn(Br,e((function(n,r){return a(Ir,(function(){return 0}),Sr(a(Or,Ur(n),r)))})),e((function(){return qr(0)})),t((function(n,r){return a(Ir,n,r)})));fn("Task");var Pr,Wr=Wn,Gr=function(n){return{$:1,a:n}},Jr=t((function(n,r){return{$:0,a:n,b:r}})),Yr=function(n){var r=n.b;return a(Jr,1664525*n.a+r>>>0,r)},Hr=(Pr=Fr,U((function(n){n(D(Pr(Date.now())))}))),Kr=a(zr,(function(n){return qr((r=n,t=Yr(a(Jr,0,1013904223)),Yr(a(Jr,t.a+r>>>0,t.b))));var r,t}),Hr),Qr=t((function(n,r){return n(r)})),Vr=e((function(n,r,t){if(r.b){var e=r.b,u=a(Qr,r.a,t),i=u.b;return a(zr,(function(){return o(Vr,n,e,i)}),a(Dr,n,u.a))}return qr(t)})),Xr=e((function(n,r,t){return qr(t)})),Zr=t((function(n,r){var t=r;return function(r){var e=t(r),u=e.b;return l(n(e.a),u)}}));nn.Random=rn(Kr,Vr,Xr,t((function(n,r){return a(Zr,n,r)})));var nt=fn("Random"),rt=t((function(n,r){return nt(a(Zr,n,r))})),tt=_,et=function(n){var r=n.a,t=277803737*(r^r>>>4+(r>>>28));return(t>>>22^t)>>>0},ut=t((function(n,r){return function(t){var e,u=Yr(t),i=0>(e=r-n)?-e:e,f=et(u);return l((134217728*(1*(67108863&et(t)))+1*(134217727&f))/9007199254740992*i+n,Yr(u))}})),it=L,ft=o(e((function(n,r,t){var e=r,u=t;return function(r){var t=e(r),i=t.a,f=u(t.b),o=f.b;return l(a(n,i,f.a),o)}})),t((function(n,r){return it(-2*a(hr,2.718281828459045,1-a(wr,0,n)))*tt(r)})),a(ut,0,1),a(ut,0,6.283185307179586)),at=a(rt,Gr,a(t((function(n,r){return a(Zr,(function(t){return t*r+n}),ft)})),300,40)),ot=function(n){return{$:0,a:n}},ct=t((function(n,r){return{$:0,a:n,b:r}})),vt=t((function(n,r){return{bn:r,bv:n}})),st={$:-2},bt=st,dt=qr(a(vt,bt,bt)),lt=d,ht=t((function(n,r){n:for(;;){if(-2===r.$)return or;var t=r.c,e=r.d,u=r.e;switch(a(lt,n,r.b)){case 0:n=n,r=e;continue n;case 1:return ar(t);default:n=n,r=u;continue n}}})),$t=i((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),gt=i((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v($t,n,r,t,e,u);var i=e.d;s=e.e;return v($t,0,e.b,e.c,v($t,1,i.b,i.c,i.d,i.e),v($t,1,r,t,s,u))}var f=u.b,a=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return v($t,n,f,a,v($t,0,r,t,e,o),c);var s;return v($t,0,r,t,v($t,1,e.b,e.c,e.d,s=e.e),v($t,1,f,a,o,c))})),mt=e((function(n,r,t){if(-2===t.$)return v($t,0,n,r,st,st);var e=t.a,u=t.b,i=t.c,f=t.d,c=t.e;switch(a(lt,n,u)){case 0:return v(gt,e,u,i,o(mt,n,r,f),c);case 1:return v($t,e,u,r,f,c);default:return v(gt,e,u,i,f,o(mt,n,r,c))}})),pt=e((function(n,r,t){var e=o(mt,n,r,t);if(-1!==e.$||e.a)return e;return v($t,1,e.b,e.c,e.d,e.e)})),yt=t((function(n,r){var t=n.a,e=n.b,u=a(ht,t,r);return o(pt,t,1===u.$?p([e]):a(Xn,e,u.a),r)})),wt=function(n){return U((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(D(0))}))},jt=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,i=o(n,t.b,t.c,o(jt,n,r,t.d));n=u,r=i,t=e}})),At=f((function(n,r,u,i,f,a){var v=o(jt,e((function(t,e,i){n:for(;;){var f=i.a,a=i.b;if(f.b){var v=f.a,s=v.a,d=v.b,h=f.b;if(0>b(s,t)){t=t,e=e,i=l(h,o(n,s,d,a));continue n}return b(s,t)>0?l(f,o(u,t,e,a)):l(h,c(r,s,d,e,a))}return l(f,o(u,t,e,a))}})),l(nr(i),a),f),s=v.a,d=v.b;return o(vr,t((function(r,t){return o(n,r.a,r.b,t)})),d,s)})),_t=un,kt=Hn,Nt=J,Et=e((function(n,r,t){if(r.b){var e=r.a,u=r.b,i=Nt(a(kt,e,a(_t,n,e)));return a(zr,(function(r){return o(Et,n,u,o(pt,e,r,t))}),i)}return qr(t)})),Lt=e((function(n,r,t){var i=t.bn,f=e((function(n,r,t){var e=t.c;return h(t.a,t.b,a(zr,(function(){return e}),wt(r)))})),c=o(vr,yt,bt,r),v=s(At,e((function(n,r,t){var e=t.b,u=t.c;return h(a(Xn,n,t.a),e,u)})),u((function(n,r,t,e){var u=e.c;return h(e.a,o(pt,n,t,e.b),u)})),f,c,i,h($,bt,qr(0))),b=v.a,d=v.b;return a(zr,(function(n){return qr(a(vt,c,n))}),a(zr,(function(){return o(Et,n,b,d)}),v.c))})),Tt=e((function(n,r,t){var e=a(ht,r,t.bv);if(1===e.$)return qr(t);var u=e.a;return a(zr,(function(){return qr(t)}),a(zr,(function(r){return Sr(a(Or,(function(t){return a(Dr,n,t(r))}),u))}),Hr))})),xt=e((function(n,r,t){return n(r(t))}));nn.Time=rn(dt,Lt,Tt,0,t((function(n,r){return a(ct,r.a,a(xt,n,r.b))})));var Ft,qt=fn("Time"),Bt=t((function(n,r){return qt(a(ct,n,r))})),Ct=function(n){return{$:2,m:n}}($),Rt=t((function(n,r){return n.$?l(function(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}(r,{U:a(Xn,n.a,r.U)}),Ct):l(r,at)})),Ot=t((function(n,r){return a(mn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))})),zt=F,It=function(n){switch(n.$){case 0:return zt(n.a)+"cm";case 1:return zt(n.a)+"em";case 2:return zt(n.a)+"ex";case 3:return zt(n.a)+"in";case 4:return zt(n.a)+"mm";case 5:return zt(n.a);case 6:return zt(n.a)+"pc";case 7:return zt(n.a)+"%";case 8:return zt(n.a)+"pt";default:return zt(n.a)+"px"}},Mt=function(n){return a(Ot,"height",It(n))},St=function(n){return{$:9,a:n}},Dt=E,Ut=function(n){var r,t,e=n.b,u=n.c,i=n.d,f=function(n){return Dt(1e4*n)/100};return r=p(["rgba(",zt(f(n.a)),"%,",zt(f(e)),"%,",zt(f(u)),"%,",zt((t=i,Dt(1e3*t)/1e3)),")"]),a(cr,"",r)},Pt=function(n){return n.$?"none":Ut(n.a)},Wt=a(xt,Ot("fill"),Pt),Gt=$n(function(n){return"script"==n?"p":n}("http://www.w3.org/2000/svg")),Jt=Gt("rect"),Yt=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),Ht=u((function(n,r,t,e){return c(Yt,n,r,t,e)})),Kt=function(n){return a(Ot,"width",It(n))},Qt=function(n){return a(Ot,"y",It(n))},Vt=function(n){return a(Jt,p([(t=St(n),a(Ot,"x",It(t))),Qt(St(0)),Kt(St(20)),Mt(St(600)),Wt((r=c(Ht,0,0,0,.1),{$:0,a:r}))]),$);var r,t},Xt=Gt("svg"),Zt=u((function(n,r,t,e){return a(Ot,"viewBox",a(cr," ",a(Or,zt,p([n,r,t,e]))))})),ne=Wr({b$:function(){return l({U:p([300])},at)},co:function(){return a(Bt,100,ot)},cu:Rt,cv:function(n){return a(Xt,p([Kt(St(600)),Mt(St(600)),c(Zt,0,0,600,600)]),a(Or,Vt,n.U))}});Ft={RandomWalks:{NormalDistribution:{init:ne(Tr(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?A(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,Ft):n.Elm=Ft}(window);