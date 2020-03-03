!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function a(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(a){return n(r,t,e,u,a)}}}}}))}function i(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(a){return function(i){return n(r,t,e,u,a,i)}}}}}}))}function f(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,a){return 5===n.a?n.f(r,t,e,u,a):n(r)(t)(e)(u)(a)}function s(n,r,t,e,u,a,i){return 6===n.a?n.f(r,t,e,u,a,i):n(r)(t)(e)(u)(a)(i)}function b(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=b(n.a,r.a))||(t=b(n.b,r.b))?t:b(n.c,r.c);for(;n.b&&r.b&&!(t=b(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var l=t((function(n,r){var t=b(n,r);return 0>t?Vn:t?Un:Qn}));function d(n,r){return{a:n,b:r}}function h(n,r,t){return{a:n,b:r,c:t}}var g={$:0};function $(n,r){return{$:1,a:n,b:r}}var m=t($);function p(n){for(var r=g,t=n.length;t--;)r=$(n[t],r);return r}function w(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var y=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),A=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,d(t,r)}));function j(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var k=Math.cos,_=Math.sin;var N=Math.ceil,T=Math.floor,F=Math.round,E=Math.log;var L=t((function(n,r){return r.join(n)}));function x(n){return n+""}function B(n){return{$:2,b:n}}B((function(n){return"number"!=typeof n?M("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?ir(n):!isFinite(n)||n%1?M("an INT",n):ir(n)})),B((function(n){return"boolean"==typeof n?ir(n):M("a BOOL",n)})),B((function(n){return"number"==typeof n?ir(n):M("a FLOAT",n)})),B((function(n){return ir(n)})),B((function(n){return"string"==typeof n?ir(n):n instanceof String?ir(n+""):M("a STRING",n)}));var S=t((function(n,r){return C(n,r)}));function C(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?ir(n.c):M("null",r);case 3:return q(r)?O(n.b,r,p):M("a LIST",r);case 4:return q(r)?O(n.b,r,z):M("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return M("an OBJECT with a field named `"+t+"`",r);var e=C(n.b,r[t]);return Er(e)?e:tr(f(ur,t,e.a));case 7:var u=n.e;if(!q(r))return M("an ARRAY",r);if(u>=r.length)return M("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=C(n.b,r[u]);return Er(e)?e:tr(f(ar,u,e.a));case 8:if("object"!=typeof r||null===r||q(r))return M("an OBJECT",r);var a=g;for(var i in r)if(r.hasOwnProperty(i)){e=C(n.b,r[i]);if(!Er(e))return tr(f(ur,i,e.a));a=$(d(i,e.a),a)}return ir(br(a));case 9:for(var o=n.f,c=n.g,v=0;c.length>v;v++){e=C(c[v],r);if(!Er(e))return e;o=o(e.a)}return ir(o);case 10:e=C(n.b,r);return Er(e)?C(n.h(e.a),r):e;case 11:for(var s=g,b=n.g;b.b;b=b.b){e=C(b.a,r);if(Er(e))return e;s=$(e.a,s)}return tr(fr(br(s)));case 1:return tr(f(er,n.a,r));case 0:return ir(n.a)}}function O(n,r,t){for(var e=r.length,u=Array(e),a=0;e>a;a++){var i=C(n,r[a]);if(!Er(i))return tr(f(ar,a,i.a));u[a]=i.a}return ir(t(u))}function q(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function z(n){return f(Fr,n.length,(function(r){return n[r]}))}function M(n,r){return tr(f(er,"Expecting "+n,r))}function I(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return I(n.b,r.b);case 6:return n.d===r.d&&I(n.b,r.b);case 7:return n.e===r.e&&I(n.b,r.b);case 9:return n.f===r.f&&R(n.g,r.g);case 10:return n.h===r.h&&I(n.b,r.b);case 11:return R(n.g,r.g)}}function R(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!I(n[e],r[e]))return!1;return!0}function W(n){return{$:0,a:n}}function D(n){return{$:2,b:n,c:null}}var P=t((function(n,r){return{$:3,b:n,d:r}}));var G=0;function Y(n){var r={$:0,e:G++,f:n,g:null,h:[]};return U(r),r}function J(n){return D((function(r){r(W(Y(n)))}))}function X(n,r){n.h.push(r),U(n)}var H=t((function(n,r){return D((function(t){X(n,r),t(W(0))}))}));var K=!1,Q=[];function U(n){if(Q.push(n),!K){for(K=!0;n=Q.shift();)V(n);K=!1}}function V(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,U(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function Z(n,r,t,e,u,a){var i=f(S,n,r?r.flags:void 0);Er(i)||j(2);var o={},c=(i=t(i.a)).a,v=a(b,c),s=function(n,r){var t;for(var e in nn){var u=nn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=tn(u,r)}return t}(o,b);function b(n,r){v(c=(i=f(e,n,c)).a,r),cn(o,i.b,u(c))}return cn(o,i.b,u(c)),s?{ports:s}:{}}var nn={};function rn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function tn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,a=n.e,i=n.f;function v(n){return f(P,v,{$:5,b:function(r){var f=r.a;return 0===r.$?o(u,t,f,n):a&&i?c(e,t,f.i,f.j,n):o(e,t,a?f.i:f.j,n)}})}return t.h=Y(f(P,v,n.b))}var en=t((function(n,r){return D((function(t){n.g(r),t(W(0))}))})),un=t((function(n,r){return f(H,n.h,{$:0,a:r})}));function an(n){return function(r){return{$:1,k:n,l:r}}}var fn=[],on=!1;function cn(n,r,t){if(fn.push({p:n,q:r,r:t}),!on){on=!0;for(var e;e=fn.shift();)vn(e.p,e.q,e.r);on=!1}}function vn(n,r,t){var e={};for(var u in sn(!0,r,e,null),sn(!1,t,e,null),n)X(n[u],{$:"fx",a:e[u]||{i:g,j:g}})}function sn(n,r,t,e){switch(r.$){case 1:var u=r.k,a=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return f(n?nn[r].e:nn[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:g,j:g},n?t.i=$(r,t.i):t.j=$(r,t.j),t}(n,a,t[u]));case 2:for(var i=r.m;i.b;i=i.b)sn(n,i.a,t,e);return;case 3:return void sn(n,r.o,t,{s:r.n,t:e})}}var bn;var ln="undefined"!=typeof document?document:{};function dn(n,r){n.appendChild(r)}function hn(n){return{$:0,a:n}}var gn=t((function(n,r){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b||0,u.push(i)}return a+=u.length,{$:1,c:r,d:wn(t),e:u,f:n,b:a}}))})),$n=gn(void 0);t((function(n,r){return t((function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b.b||0,u.push(i)}return a+=u.length,{$:2,c:r,d:wn(t),e:u,f:n,b:a}}))}))(void 0);var mn=t((function(n,r){return{$:"a3",n:n,o:r}}));var pn;function wn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=r[e]||(r[e]={});"a3"===e&&"class"===u?yn(i,u,a):i[u]=a}else"className"===u?yn(r,u,a):r[u]=a}return r}function yn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function An(n,r){var t=n.$;if(5===t)return An(n.k||(n.k=n.m()),r);if(0===t)return ln.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:r};return(i=An(e,a)).elm_event_node_ref=a,i}if(3===t)return jn(i=n.h(n.g),r,n.d),i;var i=n.f?ln.createElementNS(n.f,n.c):ln.createElement(n.c);bn&&"a"==n.c&&i.addEventListener("click",bn(i)),jn(i,r,n.d);for(var f=n.e,o=0;f.length>o;o++)dn(i,An(1===t?f[o]:f[o].b,r));return i}function jn(n,r,t){for(var e in t){var u=t[e];"a1"===e?kn(n,u):"a0"===e?Tn(n,r,u):"a3"===e?_n(n,u):"a4"===e?Nn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function kn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function _n(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Nn(n,r){for(var t in r){var e=r[t],u=e.f,a=e.o;void 0!==a?n.setAttributeNS(u,t,a):n.removeAttributeNS(u,t)}}function Tn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}n.removeEventListener(u,i)}i=Fn(r,a),n.addEventListener(u,i,pn&&{passive:2>xr(a)}),e[u]=i}else n.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){pn=!0}}))}catch(n){}function Fn(n,r){function t(r){var e=t.q,u=C(e.a,r);if(Er(u)){for(var a,i=xr(e),f=u.a,o=i?3>i?f.a:f.y:f,c=1==i?f.b:3==i&&f.aM,v=(c&&r.stopPropagation(),(2==i?f.b:3==i&&f.aG)&&r.preventDefault(),n);a=v.j;){if("function"==typeof a)o=a(o);else for(var s=a.length;s--;)o=a[s](o);v=v.p}v(o,c)}}return t.q=r,t}function En(n,r){return n.$==r.$&&I(n.a,r.a)}function Ln(n,r){var t=[];return Bn(n,r,t,0),t}function xn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Bn(n,r,t,e){if(n!==r){var u=n.$,a=r.$;if(u!==a){if(1!==u||2!==a)return void xn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),a=1}switch(a){case 5:for(var i=n.l,f=r.l,o=i.length,c=o===f.length;c&&o--;)c=i[o]===f[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Bn(n.k,r.k,v,0),void(v.length>0&&xn(t,1,e,v));case 4:for(var s=n.j,b=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return l&&s.length!==b.length?void xn(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||xn(t,2,e,b),void Bn(d,h,t,e+1));case 0:return void(n.a!==r.a&&xn(t,3,e,r.a));case 1:return void Sn(n,r,t,e,On);case 2:return void Sn(n,r,t,e,qn);case 3:if(n.h!==r.h)return void xn(t,0,e,r);var g=Cn(n.d,r.d);g&&xn(t,4,e,g);var $=r.i(n.g,r.g);return void($&&xn(t,5,e,$))}}}function Sn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var a=Cn(n.d,r.d);a&&xn(t,4,e,a),u(n,r,t,e)}else xn(t,0,e,r)}function Cn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var a=n[u],i=r[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&En(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=Cn(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function On(n,r,t,e){var u=n.e,a=r.e,i=u.length,f=a.length;i>f?xn(t,6,e,{v:f,i:i-f}):f>i&&xn(t,7,e,{v:i,e:a});for(var o=f>i?i:f,c=0;o>c;c++){var v=u[c];Bn(v,a[c],t,++e),e+=v.b||0}}function qn(n,r,t,e){for(var u=[],a={},i=[],f=n.e,o=r.e,c=f.length,v=o.length,s=0,b=0,l=e;c>s&&v>b;){var d=(N=f[s]).a,h=(T=o[b]).a,g=N.b,$=T.b,m=void 0,p=void 0;if(d!==h){var w=f[s+1],y=o[b+1];if(w){var A=w.a,j=w.b;p=h===A}if(y){var k=y.a,_=y.b;m=d===k}if(m&&p)Bn(g,_,u,++l),zn(a,u,d,$,b,i),l+=g.b||0,Mn(a,u,d,j,++l),l+=j.b||0,s+=2,b+=2;else if(m)l++,zn(a,u,h,$,b,i),Bn(g,_,u,l),l+=g.b||0,s+=1,b+=2;else if(p)Mn(a,u,d,g,++l),l+=g.b||0,Bn(j,$,u,++l),l+=j.b||0,s+=2,b+=1;else{if(!w||A!==k)break;Mn(a,u,d,g,++l),zn(a,u,h,$,b,i),l+=g.b||0,Bn(j,_,u,++l),l+=j.b||0,s+=2,b+=2}}else Bn(g,$,u,++l),l+=g.b||0,s++,b++}for(;c>s;){var N;l++,Mn(a,u,(N=f[s]).a,g=N.b,l),l+=g.b||0,s++}for(;v>b;){var T,F=F||[];zn(a,u,(T=o[b]).a,T.b,void 0,F),b++}(u.length>0||i.length>0||F)&&xn(t,8,e,{w:u,x:i,y:F})}function zn(n,r,t,e,u,a){var i=n[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(n[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var f=[];return Bn(i.z,e,f,i.r),i.r=u,void(i.s.s={w:f,A:i})}zn(n,r,t+"_elmW6BL",e,u,a)}function Mn(n,r,t,e,u){var a=n[t];if(a){if(0===a.c){a.c=2;var i=[];return Bn(e,a.z,i,u),void xn(r,9,u,{w:i,A:a})}Mn(n,r,t+"_elmW6BL",e,u)}else{var f=xn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function In(n,r,t,e){!function n(r,t,e,u,a,i,f){var o=e[u],c=o.r;for(;c===a;){var v=o.$;if(1===v)In(r,t.k,o.s,f);else if(8===v){o.t=r,o.u=f,(s=o.s.w).length>0&&n(r,t,s,0,a,i,f)}else if(9===v){o.t=r,o.u=f;var s,b=o.s;if(b)b.A.s=r,(s=b.w).length>0&&n(r,t,s,0,a,i,f)}else o.t=r,o.u=f;if(u++,!(o=e[u])||(c=o.r)>i)return u}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,a+1,i,r.elm_event_node_ref)}for(var h=t.e,g=r.childNodes,$=0;h.length>$;$++){a++;var m=1===l?h[$]:h[$].b,p=a+(m.b||0);if(c>=a&&p>=c&&(u=n(g[$],m,e,u,a,p,f),!(o=e[u])||(c=o.r)>i))return u;a=p}return u}(n,r,t,0,0,r.b,e)}function Rn(n,r,t,e){return 0===t.length?n:(In(n,r,t,e),Wn(n,t))}function Wn(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,a=Dn(u,e);u===n&&(n=a)}return n}function Dn(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=An(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return jn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Wn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,a=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(An(u[e],r.u),a);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var i=t.A;return void 0!==i.r&&n.parentNode.removeChild(n),i.s=Wn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=ln.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;dn(t,2===u.c?u.s:An(u.z,r.u))}return t}(t.y,r);n=Wn(n,t.w);for(var u=t.x,a=0;u.length>a;a++){var i=u[a],f=i.A,o=2===f.c?f.s:An(f.z,r.u);n.insertBefore(o,n.childNodes[i.r])}e&&dn(n,e);return n}(n,r);case 5:return r.s(n);default:j(10)}}function Pn(n){if(3===n.nodeType)return hn(n.textContent);if(1!==n.nodeType)return hn("");for(var r=g,t=n.attributes,e=t.length;e--;){var u=t[e];r=$(f(mn,u.name,u.value),r)}var a=n.tagName.toLowerCase(),i=g,c=n.childNodes;for(e=c.length;e--;)i=$(Pn(c[e]),i);return o($n,a,r,i)}var Gn=u((function(n,r,t,e){return Z(r,e,n.b4,n.cz,n.ct,(function(r,t){var u=n.cA,a=e.node,i=Pn(a);return Jn(t,(function(n){var t=u(n),e=Ln(i,t);a=Rn(a,i,e,r),i=t}))}))})),Yn=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function Jn(n,r){r(n);var t=0;function e(){t=1===t?0:(Yn(e),r(n),1)}return function(u,a){n=u,a?(r(n),2===t&&(t=1)):(0===t&&Yn(e),t=2)}}var Xn={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Hn=t((function(n,r){return new Float64Array([n,r])}));new Float64Array(3),new Float64Array(3),new Float64Array(3);new Float64Array(16),new Float64Array(16),new Float64Array([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]);var Kn=t((function(n,r){return D((function(){var t=setInterval((function(){Y(r)}),n);return function(){clearInterval(t)}}))}));var Qn=1,Un=2,Vn=0,Zn=m,nr=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,a=o(n,t.b,t.c,o(nr,n,r,t.e));n=u,r=a,t=e}})),rr=function(n){return o(nr,e((function(n,r,t){return f(Zn,d(n,r),t)})),g,n)},tr=function(n){return{$:1,a:n}},er=t((function(n,r){return{$:3,a:n,b:r}})),ur=t((function(n,r){return{$:0,a:n,b:r}})),ar=t((function(n,r){return{$:1,a:n,b:r}})),ir=function(n){return{$:0,a:n}},fr=function(n){return{$:2,a:n}},or=function(n){return{$:0,a:n}},cr={$:1},vr=t((function(n,r){return f(L,n,w(r))})),sr=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,a=f(n,t.a,r);n=u,r=a,t=e}})),br=function(n){return o(sr,Zn,g,n)},lr=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),dr=[],hr=N,gr=t((function(n,r){return E(r)/E(n)})),$r=hr(f(gr,2,32)),mr=c(lr,0,$r,dr,dr),pr=y,wr=T,yr=function(n){return n.length},Ar=t((function(n,r){return b(n,r)>0?n:r})),jr=A,kr=t((function(n,r){for(;;){var t=f(jr,32,n),e=t.b,u=f(Zn,{$:0,a:t.a},r);if(!e.b)return br(u);n=e,r=u}})),_r=t((function(n,r){for(;;){var t=hr(r/32);if(1===t)return f(jr,32,n).a;n=f(kr,n,g),r=t}})),Nr=t((function(n,r){if(r.a){var t=32*r.a,e=wr(f(gr,32,t-1)),u=n?br(r.d):r.d,a=f(_r,u,r.a);return c(lr,yr(r.c)+t,f(Ar,5,e*$r),a,r.c)}return c(lr,yr(r.c),$r,dr,r.c)})),Tr=a((function(n,r,t,e,u){for(;;){if(0>r)return f(Nr,!1,{d:e,a:t/32|0,c:u});var a={$:1,a:o(pr,32,r,n)};n=n,r=r-32,t=t,e=f(Zn,a,e),u=u}})),Fr=t((function(n,r){if(n>0){var t=n%32;return v(Tr,r,n-t-32,n,g,o(pr,t,n-t,r))}return mr})),Er=function(n){return!n.$},Lr=function(n){return{$:0,a:n}},xr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Br=function(n){return n},Sr=W,Cr=Sr(0),Or=u((function(n,r,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,v=a.b;if(v.b){var s=v.a,b=v.b;if(b.b){var l=b.b;return f(n,u,f(n,i,f(n,s,f(n,b.a,t>500?o(sr,n,r,br(l)):c(Or,n,r,t+1,l)))))}return f(n,u,f(n,i,f(n,s,r)))}return f(n,u,f(n,i,r))}return f(n,u,r)}return r})),qr=e((function(n,r,t){return c(Or,n,r,0,t)})),zr=t((function(n,r){return o(qr,t((function(r,t){return f(Zn,n(r),t)})),g,r)})),Mr=P,Ir=t((function(n,r){return f(Mr,(function(r){return Sr(n(r))}),r)})),Rr=e((function(n,r,t){return f(Mr,(function(r){return f(Mr,(function(t){return Sr(f(n,r,t))}),t)}),r)})),Wr=function(n){return o(qr,Rr(Zn),Sr(g),n)},Dr=en,Pr=t((function(n,r){var t=r;return J(f(Mr,Dr(n),t))}));nn.Task=rn(Cr,e((function(n,r){return f(Ir,(function(){return 0}),Wr(f(zr,Pr(n),r)))})),e((function(){return Sr(0)})),t((function(n,r){return f(Ir,n,r)})));an("Task");var Gr,Yr=Gn,Jr=function(n){return{$:2,m:n}}(g),Xr=Hn,Hr=t((function(n,r){return{$:0,a:n,b:r}})),Kr=t((function(n,r){return{bs:r,bA:n}})),Qr={$:-2},Ur=Qr,Vr=Sr(f(Kr,Ur,Ur)),Zr=l,nt=t((function(n,r){n:for(;;){if(-2===r.$)return cr;var t=r.c,e=r.d,u=r.e;switch(f(Zr,n,r.b)){case 0:n=n,r=e;continue n;case 1:return or(t);default:n=n,r=u;continue n}}})),rt=a((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),tt=a((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v(rt,n,r,t,e,u);var a=e.d;s=e.e;return v(rt,0,e.b,e.c,v(rt,1,a.b,a.c,a.d,a.e),v(rt,1,r,t,s,u))}var i=u.b,f=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return v(rt,n,i,f,v(rt,0,r,t,e,o),c);var s;return v(rt,0,r,t,v(rt,1,e.b,e.c,e.d,s=e.e),v(rt,1,i,f,o,c))})),et=e((function(n,r,t){if(-2===t.$)return v(rt,0,n,r,Qr,Qr);var e=t.a,u=t.b,a=t.c,i=t.d,c=t.e;switch(f(Zr,n,u)){case 0:return v(tt,e,u,a,o(et,n,r,i),c);case 1:return v(rt,e,u,r,i,c);default:return v(tt,e,u,a,i,o(et,n,r,c))}})),ut=e((function(n,r,t){var e=o(et,n,r,t);if(-1!==e.$||e.a)return e;return v(rt,1,e.b,e.c,e.d,e.e)})),at=t((function(n,r){var t=n.a,e=n.b,u=f(nt,t,r);return o(ut,t,1===u.$?p([e]):f(Zn,e,u.a),r)})),it=function(n){return D((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(W(0))}))},ft=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,a=o(n,t.b,t.c,o(ft,n,r,t.d));n=u,r=a,t=e}})),ot=i((function(n,r,u,a,i,f){var v=o(ft,e((function(t,e,a){n:for(;;){var i=a.a,f=a.b;if(i.b){var v=i.a,s=v.a,l=v.b,h=i.b;if(0>b(s,t)){t=t,e=e,a=d(h,o(n,s,l,f));continue n}return b(s,t)>0?d(i,o(u,t,e,f)):d(h,c(r,s,l,e,f))}return d(i,o(u,t,e,f))}})),d(rr(a),f),i),s=v.a,l=v.b;return o(sr,t((function(r,t){return o(n,r.a,r.b,t)})),l,s)})),ct=un,vt=Kn,st=J,bt=e((function(n,r,t){if(r.b){var e=r.a,u=r.b,a=st(f(vt,e,f(ct,n,e)));return f(Mr,(function(r){return o(bt,n,u,o(ut,e,r,t))}),a)}return Sr(t)})),lt=e((function(n,r,t){var a=t.bs,i=e((function(n,r,t){var e=t.c;return h(t.a,t.b,f(Mr,(function(){return e}),it(r)))})),c=o(sr,at,Ur,r),v=s(ot,e((function(n,r,t){var e=t.b,u=t.c;return h(f(Zn,n,t.a),e,u)})),u((function(n,r,t,e){var u=e.c;return h(e.a,o(ut,n,t,e.b),u)})),i,c,a,h(g,Ur,Sr(0))),b=v.a,l=v.b;return f(Mr,(function(n){return Sr(f(Kr,c,n))}),f(Mr,(function(){return o(bt,n,b,l)}),v.c))})),dt=(Gr=Br,D((function(n){n(W(Gr(Date.now())))}))),ht=e((function(n,r,t){var e=f(nt,r,t.bA);if(1===e.$)return Sr(t);var u=e.a;return f(Mr,(function(){return Sr(t)}),f(Mr,(function(r){return Wr(f(zr,(function(t){return f(Dr,n,t(r))}),u))}),dt))})),gt=e((function(n,r,t){return n(r(t))}));nn.Time=rn(Vr,lt,ht,0,t((function(n,r){return f(Hr,r.a,f(gt,n,r.b))})));var $t,mt=an("Time"),pt=t((function(n,r){return mt(f(Hr,n,r))})),wt=k,yt=_,At=t((function(n,r){var t=-.25*yt(r.S)/r.T,e=(r.ag+t)*r.aB,u=r.S+e;return d(function(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}(r,{S:u,ag:e,W:f(Xr,r.T*yt(u),r.T*wt(u))}),Jr)})),jt=c(u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),0,0,0,1),kt=t((function(n,r){return f(mn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))})),_t=x,Nt=function(n){switch(n.$){case 0:return _t(n.a)+"cm";case 1:return _t(n.a)+"em";case 2:return _t(n.a)+"ex";case 3:return _t(n.a)+"in";case 4:return _t(n.a)+"mm";case 5:return _t(n.a);case 6:return _t(n.a)+"pc";case 7:return _t(n.a)+"%";case 8:return _t(n.a)+"pt";default:return _t(n.a)+"px"}},Tt=function(n){return f(kt,"height",Nt(n))},Ft=function(n){return f(vr,"",n)},Et=F,Lt=function(n){var r,t=n.b,e=n.c,u=n.d,a=function(n){return Et(1e4*n)/100};return Ft(p(["rgba(",_t(a(n.a)),"%,",_t(a(t)),"%,",_t(a(e)),"%,",_t((r=u,Et(1e3*r)/1e3)),")"]))},xt=function(n){return n.$?"none":Lt(n.a)},Bt=f(gt,kt("fill"),xt),St=Bt({$:1}),Ct=function(n){return{$:9,a:n}},Ot=gn(function(n){return"script"==n?"p":n}("http://www.w3.org/2000/svg")),qt=Ot("rect"),zt=function(n){return f(kt,"stroke",Lt(n))},Mt=function(n){return f(kt,"stroke-width",Nt(n))},It=function(n){return f(kt,"width",Nt(n))},Rt=f(qt,p([It(Ct(600)),Tt(Ct(600)),St,zt(jt),Mt(Ct(3))]),g),Wt=Ot("svg"),Dt=u((function(n,r,t,e){return f(kt,"viewBox",f(vr," ",f(zr,_t,p([n,r,t,e]))))})),Pt=t((function(n,r){return{$:5,a:n,b:r}})),Gt=Ot("g"),Yt=function(n){var r=t((function(n,r){return Ft(p([n,"(",f(vr," ",f(zr,_t,r)),")"]))}));switch(n.$){case 0:return f(r,"matrix",p([n.a,n.b,n.c,n.d,n.e,n.f]));case 1:return f(r,"rotate",p([n.a,n.b,n.c]));case 2:return f(r,"scale",p([n.a,n.b]));case 3:return f(r,"skewX",p([n.a]));case 4:return f(r,"skewY",p([n.a]));default:return f(r,"translate",p([n.a,n.b]))}},Jt=Ot("circle"),Xt=function(n){return f(kt,"cy",Nt(n))},Ht=function(n){return n[0]},Kt=function(n){return n[1]},Qt=function(n){return f(kt,"r",Nt(n))},Ut=t((function(n,r){return f(Jt,p([(e=Ct(Ht(r)),f(kt,"cx",Nt(e))),Xt(Ct(Kt(r))),Qt(Ct(n)),Bt((t=jt,{$:0,a:t}))]),g);var t,e})),Vt=Ot("line"),Zt=function(n){return f(kt,"x2",Nt(n))},ne=function(n){return f(kt,"y1",Nt(n))},re=function(n){return f(kt,"y2",Nt(n))},te=function(n){return f(Gt,p([(e=p([f(Pt,300,0)]),f(kt,"transform",f(vr," ",f(zr,Yt,e))))]),p([(r=n.W,f(Vt,p([(t=Ct(0),f(kt,"x1",Nt(t))),ne(Ct(0)),Zt(Ct(Ht(r))),re(Ct(Kt(r))),zt(jt),Mt(Ct(2))]),g)),f(Ut,n.au,n.W)]));var r,t,e},ee=Yr({b4:function(){return d({S:1.0471975511965976,T:200,au:20,aB:.999,ag:0,W:f(Xr,0,0)},Jr)},ct:function(){return f(pt,10,Br)},cz:At,cA:function(n){return f(Wt,p([It(Ct(600)),Tt(Ct(600)),c(Dt,0,0,600,600)]),p([Rt,te(n)]))}});$t={Oscillations:{Pendulum:{init:ee(Lr(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?j(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,$t):n.Elm=$t}(window);