!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function i(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(i){return n(r,t,e,u,i)}}}}}))}function a(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(i){return function(a){return n(r,t,e,u,i,a)}}}}}}))}function f(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}function s(n,r,t,e,u,i,a){return 6===n.a?n.f(r,t,e,u,i,a):n(r)(t)(e)(u)(i)(a)}function b(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=b(n.a,r.a))||(t=b(n.b,r.b))?t:b(n.c,r.c);for(;n.b&&r.b&&!(t=b(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var l=t((function(n,r){var t=b(n,r);return 0>t?Qn:t?Kn:Hn}));function d(n,r){return{a:n,b:r}}function h(n,r,t){return{a:n,b:r,c:t}}var $={$:0};function g(n,r){return{$:1,a:n,b:r}}var m=t(g);function p(n){for(var r=$,t=n.length;t--;)r=g(n[t],r);return r}function w(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var y=e((function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e})),A=t((function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,d(t,r)}));function j(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var k=Math.cos,_=Math.sin;var x=Math.ceil,N=Math.floor,E=Math.round,L=Math.log;var T=t((function(n,r){return r.join(n)}));function M(n){return n+""}function F(n){return{$:2,b:n}}F((function(n){return"number"!=typeof n?z("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?ir(n):!isFinite(n)||n%1?z("an INT",n):ir(n)})),F((function(n){return"boolean"==typeof n?ir(n):z("a BOOL",n)})),F((function(n){return"number"==typeof n?ir(n):z("a FLOAT",n)})),F((function(n){return ir(n)})),F((function(n){return"string"==typeof n?ir(n):n instanceof String?ir(n+""):z("a STRING",n)}));var q=t((function(n,r){return C(n,r)}));function C(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?ir(n.c):z("null",r);case 3:return O(r)?B(n.b,r,p):z("a LIST",r);case 4:return O(r)?B(n.b,r,S):z("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return z("an OBJECT with a field named `"+t+"`",r);var e=C(n.b,r[t]);return Er(e)?e:rr(f(er,t,e.a));case 7:var u=n.e;if(!O(r))return z("an ARRAY",r);if(u>=r.length)return z("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=C(n.b,r[u]);return Er(e)?e:rr(f(ur,u,e.a));case 8:if("object"!=typeof r||null===r||O(r))return z("an OBJECT",r);var i=$;for(var a in r)if(r.hasOwnProperty(a)){e=C(n.b,r[a]);if(!Er(e))return rr(f(er,a,e.a));i=g(d(a,e.a),i)}return ir(sr(i));case 9:for(var o=n.f,c=n.g,v=0;c.length>v;v++){e=C(c[v],r);if(!Er(e))return e;o=o(e.a)}return ir(o);case 10:e=C(n.b,r);return Er(e)?C(n.h(e.a),r):e;case 11:for(var s=$,b=n.g;b.b;b=b.b){e=C(b.a,r);if(Er(e))return e;s=g(e.a,s)}return rr(ar(sr(s)));case 1:return rr(f(tr,n.a,r));case 0:return ir(n.a)}}function B(n,r,t){for(var e=r.length,u=Array(e),i=0;e>i;i++){var a=C(n,r[i]);if(!Er(a))return rr(f(ur,i,a.a));u[i]=a.a}return ir(t(u))}function O(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function S(n){return f(Nr,n.length,(function(r){return n[r]}))}function z(n,r){return rr(f(tr,"Expecting "+n,r))}function D(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return D(n.b,r.b);case 6:return n.d===r.d&&D(n.b,r.b);case 7:return n.e===r.e&&D(n.b,r.b);case 9:return n.f===r.f&&I(n.g,r.g);case 10:return n.h===r.h&&D(n.b,r.b);case 11:return I(n.g,r.g)}}function I(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!D(n[e],r[e]))return!1;return!0}function R(n){return{$:0,a:n}}function V(n){return{$:2,b:n,c:null}}var J=t((function(n,r){return{$:3,b:n,d:r}}));var P=0;function Y(n){var r={$:0,e:P++,f:n,g:null,h:[]};return Q(r),r}function G(n){return V((function(r){r(R(Y(n)))}))}function W(n,r){n.h.push(r),Q(n)}var X=t((function(n,r){return V((function(t){W(n,r),t(R(0))}))}));var H=!1,K=[];function Q(n){if(K.push(n),!H){for(H=!0;n=K.shift();)U(n);H=!1}}function U(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,Q(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function Z(n,r,t,e,u,i){var a=f(q,n,r?r.flags:void 0);Er(a)||j(2);var o={},c=(a=t(a.a)).a,v=i(b,c),s=function(n,r){var t;for(var e in nn){var u=nn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=tn(u,r)}return t}(o,b);function b(n,r){v(c=(a=f(e,n,c)).a,r),cn(o,a.b,u(c))}return cn(o,a.b,u(c)),s?{ports:s}:{}}var nn={};function rn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function tn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,i=n.e,a=n.f;function v(n){return f(J,v,{$:5,b:function(r){var f=r.a;return 0===r.$?o(u,t,f,n):i&&a?c(e,t,f.i,f.j,n):o(e,t,i?f.i:f.j,n)}})}return t.h=Y(f(J,v,n.b))}var en=t((function(n,r){return V((function(t){n.g(r),t(R(0))}))})),un=t((function(n,r){return f(X,n.h,{$:0,a:r})}));function an(n){return function(r){return{$:1,k:n,l:r}}}var fn=[],on=!1;function cn(n,r,t){if(fn.push({p:n,q:r,r:t}),!on){on=!0;for(var e;e=fn.shift();)vn(e.p,e.q,e.r);on=!1}}function vn(n,r,t){var e={};for(var u in sn(!0,r,e,null),sn(!1,t,e,null),n)W(n[u],{$:"fx",a:e[u]||{i:$,j:$}})}function sn(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return f(n?nn[r].e:nn[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:$,j:$},n?t.i=g(r,t.i):t.j=g(r,t.j),t}(n,i,t[u]));case 2:for(var a=r.m;a.b;a=a.b)sn(n,a.a,t,e);return;case 3:return void sn(n,r.o,t,{s:r.n,t:e})}}var bn;var ln="undefined"!=typeof document?document:{};function dn(n,r){n.appendChild(r)}function hn(n){return{$:0,a:n}}var $n=t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var a=e.a;i+=a.b||0,u.push(a)}return i+=u.length,{$:1,c:r,d:wn(t),e:u,f:n,b:i}}))})),gn=$n(void 0);t((function(n,r){return t((function(t,e){for(var u=[],i=0;e.b;e=e.b){var a=e.a;i+=a.b.b||0,u.push(a)}return i+=u.length,{$:2,c:r,d:wn(t),e:u,f:n,b:i}}))}))(void 0);var mn=t((function(n,r){return{$:"a3",n:n,o:r}}));var pn;function wn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var a=r[e]||(r[e]={});"a3"===e&&"class"===u?yn(a,u,i):a[u]=i}else"className"===u?yn(r,u,i):r[u]=i}return r}function yn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function An(n,r){var t=n.$;if(5===t)return An(n.k||(n.k=n.m()),r);if(0===t)return ln.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(a=An(e,i)).elm_event_node_ref=i,a}if(3===t)return jn(a=n.h(n.g),r,n.d),a;var a=n.f?ln.createElementNS(n.f,n.c):ln.createElement(n.c);bn&&"a"==n.c&&a.addEventListener("click",bn(a)),jn(a,r,n.d);for(var f=n.e,o=0;f.length>o;o++)dn(a,An(1===t?f[o]:f[o].b,r));return a}function jn(n,r,t){for(var e in t){var u=t[e];"a1"===e?kn(n,u):"a0"===e?Nn(n,r,u):"a3"===e?_n(n,u):"a4"===e?xn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function kn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function _n(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function xn(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function Nn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],a=e[u];if(i){if(a){if(a.q.$===i.$){a.q=i;continue}n.removeEventListener(u,a)}a=En(r,i),n.addEventListener(u,a,pn&&{passive:2>Tr(i)}),e[u]=a}else n.removeEventListener(u,a),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){pn=!0}}))}catch(n){}function En(n,r){function t(r){var e=t.q,u=C(e.a,r);if(Er(u)){for(var i,a=Tr(e),f=u.a,o=a?3>a?f.a:f.y:f,c=1==a?f.b:3==a&&f.aJ,v=(c&&r.stopPropagation(),(2==a?f.b:3==a&&f.aD)&&r.preventDefault(),n);i=v.j;){if("function"==typeof i)o=i(o);else for(var s=i.length;s--;)o=i[s](o);v=v.p}v(o,c)}}return t.q=r,t}function Ln(n,r){return n.$==r.$&&D(n.a,r.a)}function Tn(n,r){var t=[];return Fn(n,r,t,0),t}function Mn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Fn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void Mn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var a=n.l,f=r.l,o=a.length,c=o===f.length;c&&o--;)c=a[o]===f[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Fn(n.k,r.k,v,0),void(v.length>0&&Mn(t,1,e,v));case 4:for(var s=n.j,b=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return l&&s.length!==b.length?void Mn(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Mn(t,2,e,b),void Fn(d,h,t,e+1));case 0:return void(n.a!==r.a&&Mn(t,3,e,r.a));case 1:return void qn(n,r,t,e,Bn);case 2:return void qn(n,r,t,e,On);case 3:if(n.h!==r.h)return void Mn(t,0,e,r);var $=Cn(n.d,r.d);$&&Mn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Mn(t,5,e,g))}}}function qn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=Cn(n.d,r.d);i&&Mn(t,4,e,i),u(n,r,t,e)}else Mn(t,0,e,r)}function Cn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],a=r[u];i===a&&"value"!==u&&"checked"!==u||"a0"===t&&Ln(i,a)||((e=e||{})[u]=a)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=Cn(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Bn(n,r,t,e){var u=n.e,i=r.e,a=u.length,f=i.length;a>f?Mn(t,6,e,{v:f,i:a-f}):f>a&&Mn(t,7,e,{v:a,e:i});for(var o=f>a?a:f,c=0;o>c;c++){var v=u[c];Fn(v,i[c],t,++e),e+=v.b||0}}function On(n,r,t,e){for(var u=[],i={},a=[],f=n.e,o=r.e,c=f.length,v=o.length,s=0,b=0,l=e;c>s&&v>b;){var d=(x=f[s]).a,h=(N=o[b]).a,$=x.b,g=N.b,m=void 0,p=void 0;if(d!==h){var w=f[s+1],y=o[b+1];if(w){var A=w.a,j=w.b;p=h===A}if(y){var k=y.a,_=y.b;m=d===k}if(m&&p)Fn($,_,u,++l),Sn(i,u,d,g,b,a),l+=$.b||0,zn(i,u,d,j,++l),l+=j.b||0,s+=2,b+=2;else if(m)l++,Sn(i,u,h,g,b,a),Fn($,_,u,l),l+=$.b||0,s+=1,b+=2;else if(p)zn(i,u,d,$,++l),l+=$.b||0,Fn(j,g,u,++l),l+=j.b||0,s+=2,b+=1;else{if(!w||A!==k)break;zn(i,u,d,$,++l),Sn(i,u,h,g,b,a),l+=$.b||0,Fn(j,_,u,++l),l+=j.b||0,s+=2,b+=2}}else Fn($,g,u,++l),l+=$.b||0,s++,b++}for(;c>s;){var x;l++,zn(i,u,(x=f[s]).a,$=x.b,l),l+=$.b||0,s++}for(;v>b;){var N,E=E||[];Sn(i,u,(N=o[b]).a,N.b,void 0,E),b++}(u.length>0||a.length>0||E)&&Mn(t,8,e,{w:u,x:a,y:E})}function Sn(n,r,t,e,u,i){var a=n[t];if(!a)return i.push({r:u,A:a={c:0,z:e,r:u,s:void 0}}),void(n[t]=a);if(1===a.c){i.push({r:u,A:a}),a.c=2;var f=[];return Fn(a.z,e,f,a.r),a.r=u,void(a.s.s={w:f,A:a})}Sn(n,r,t+"_elmW6BL",e,u,i)}function zn(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var a=[];return Fn(e,i.z,a,u),void Mn(r,9,u,{w:a,A:i})}zn(n,r,t+"_elmW6BL",e,u)}else{var f=Mn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function Dn(n,r,t,e){!function n(r,t,e,u,i,a,f){var o=e[u],c=o.r;for(;c===i;){var v=o.$;if(1===v)Dn(r,t.k,o.s,f);else if(8===v){o.t=r,o.u=f,(s=o.s.w).length>0&&n(r,t,s,0,i,a,f)}else if(9===v){o.t=r,o.u=f;var s,b=o.s;if(b)b.A.s=r,(s=b.w).length>0&&n(r,t,s,0,i,a,f)}else o.t=r,o.u=f;if(u++,!(o=e[u])||(c=o.r)>a)return u}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,i+1,a,r.elm_event_node_ref)}for(var h=t.e,$=r.childNodes,g=0;h.length>g;g++){i++;var m=1===l?h[g]:h[g].b,p=i+(m.b||0);if(c>=i&&p>=c&&(u=n($[g],m,e,u,i,p,f),!(o=e[u])||(c=o.r)>a))return u;i=p}return u}(n,r,t,0,0,r.b,e)}function In(n,r,t,e){return 0===t.length?n:(Dn(n,r,t,e),Rn(n,t))}function Rn(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,i=Vn(u,e);u===n&&(n=i)}return n}function Vn(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=An(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return jn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Rn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(An(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var a=t.A;return void 0!==a.r&&n.parentNode.removeChild(n),a.s=Rn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=ln.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;dn(t,2===u.c?u.s:An(u.z,r.u))}return t}(t.y,r);n=Rn(n,t.w);for(var u=t.x,i=0;u.length>i;i++){var a=u[i],f=a.A,o=2===f.c?f.s:An(f.z,r.u);n.insertBefore(o,n.childNodes[a.r])}e&&dn(n,e);return n}(n,r);case 5:return r.s(n);default:j(10)}}function Jn(n){if(3===n.nodeType)return hn(n.textContent);if(1!==n.nodeType)return hn("");for(var r=$,t=n.attributes,e=t.length;e--;){var u=t[e];r=g(f(mn,u.name,u.value),r)}var i=n.tagName.toLowerCase(),a=$,c=n.childNodes;for(e=c.length;e--;)a=g(Jn(c[e]),a);return o(gn,i,r,a)}var Pn=u((function(n,r,t,e){return Z(r,e,n.b1,n.cw,n.cq,(function(r,t){var u=n.cx,i=e.node,a=Jn(i);return Gn(t,(function(n){var t=u(n),e=Tn(a,t);i=In(i,a,e,r),a=t}))}))})),Yn=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function Gn(n,r){r(n);var t=0;function e(){t=1===t?0:(Yn(e),r(n),1)}return function(u,i){n=u,i?(r(n),2===t&&(t=1)):(0===t&&Yn(e),t=2)}}var Wn={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Xn=t((function(n,r){return V((function(){var t=setInterval((function(){Y(r)}),n);return function(){clearInterval(t)}}))}));var Hn=1,Kn=2,Qn=0,Un=m,Zn=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,i=o(n,t.b,t.c,o(Zn,n,r,t.e));n=u,r=i,t=e}})),nr=function(n){return o(Zn,e((function(n,r,t){return f(Un,d(n,r),t)})),$,n)},rr=function(n){return{$:1,a:n}},tr=t((function(n,r){return{$:3,a:n,b:r}})),er=t((function(n,r){return{$:0,a:n,b:r}})),ur=t((function(n,r){return{$:1,a:n,b:r}})),ir=function(n){return{$:0,a:n}},ar=function(n){return{$:2,a:n}},fr=function(n){return{$:0,a:n}},or={$:1},cr=t((function(n,r){return f(T,n,w(r))})),vr=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=f(n,t.a,r);n=u,r=i,t=e}})),sr=function(n){return o(vr,Un,$,n)},br=u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),lr=[],dr=x,hr=t((function(n,r){return L(r)/L(n)})),$r=dr(f(hr,2,32)),gr=c(br,0,$r,lr,lr),mr=y,pr=N,wr=function(n){return n.length},yr=t((function(n,r){return b(n,r)>0?n:r})),Ar=A,jr=t((function(n,r){for(;;){var t=f(Ar,32,n),e=t.b,u=f(Un,{$:0,a:t.a},r);if(!e.b)return sr(u);n=e,r=u}})),kr=t((function(n,r){for(;;){var t=dr(r/32);if(1===t)return f(Ar,32,n).a;n=f(jr,n,$),r=t}})),_r=t((function(n,r){if(r.a){var t=32*r.a,e=pr(f(hr,32,t-1)),u=n?sr(r.d):r.d,i=f(kr,u,r.a);return c(br,wr(r.c)+t,f(yr,5,e*$r),i,r.c)}return c(br,wr(r.c),$r,lr,r.c)})),xr=i((function(n,r,t,e,u){for(;;){if(0>r)return f(_r,!1,{d:e,a:t/32|0,c:u});var i={$:1,a:o(mr,32,r,n)};n=n,r=r-32,t=t,e=f(Un,i,e),u=u}})),Nr=t((function(n,r){if(n>0){var t=n%32;return v(xr,r,n-t-32,n,$,o(mr,t,n-t,r))}return gr})),Er=function(n){return!n.$},Lr=function(n){return{$:0,a:n}},Tr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Mr=function(n){return n},Fr=R,qr=Fr(0),Cr=u((function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var a=i.a,v=i.b;if(v.b){var s=v.a,b=v.b;if(b.b){var l=b.b;return f(n,u,f(n,a,f(n,s,f(n,b.a,t>500?o(vr,n,r,sr(l)):c(Cr,n,r,t+1,l)))))}return f(n,u,f(n,a,f(n,s,r)))}return f(n,u,f(n,a,r))}return f(n,u,r)}return r})),Br=e((function(n,r,t){return c(Cr,n,r,0,t)})),Or=t((function(n,r){return o(Br,t((function(r,t){return f(Un,n(r),t)})),$,r)})),Sr=J,zr=t((function(n,r){return f(Sr,(function(r){return Fr(n(r))}),r)})),Dr=e((function(n,r,t){return f(Sr,(function(r){return f(Sr,(function(t){return Fr(f(n,r,t))}),t)}),r)})),Ir=function(n){return o(Br,Dr(Un),Fr($),n)},Rr=en,Vr=t((function(n,r){var t=r;return G(f(Sr,Rr(n),t))}));nn.Task=rn(qr,e((function(n,r){return f(zr,(function(){return 0}),Ir(f(Or,Vr(n),r)))})),e((function(){return Fr(0)})),t((function(n,r){return f(zr,n,r)})));an("Task");var Jr,Pr=Pn,Yr=d(0,0),Gr=function(n){return{$:2,m:n}}($),Wr=t((function(n,r){return{$:0,a:n,b:r}})),Xr=t((function(n,r){return{bo:r,bx:n}})),Hr={$:-2},Kr=Hr,Qr=Fr(f(Xr,Kr,Kr)),Ur=l,Zr=t((function(n,r){n:for(;;){if(-2===r.$)return or;var t=r.c,e=r.d,u=r.e;switch(f(Ur,n,r.b)){case 0:n=n,r=e;continue n;case 1:return fr(t);default:n=n,r=u;continue n}}})),nt=i((function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}})),rt=i((function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v(nt,n,r,t,e,u);var i=e.d;s=e.e;return v(nt,0,e.b,e.c,v(nt,1,i.b,i.c,i.d,i.e),v(nt,1,r,t,s,u))}var a=u.b,f=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return v(nt,n,a,f,v(nt,0,r,t,e,o),c);var s;return v(nt,0,r,t,v(nt,1,e.b,e.c,e.d,s=e.e),v(nt,1,a,f,o,c))})),tt=e((function(n,r,t){if(-2===t.$)return v(nt,0,n,r,Hr,Hr);var e=t.a,u=t.b,i=t.c,a=t.d,c=t.e;switch(f(Ur,n,u)){case 0:return v(rt,e,u,i,o(tt,n,r,a),c);case 1:return v(nt,e,u,r,a,c);default:return v(rt,e,u,i,a,o(tt,n,r,c))}})),et=e((function(n,r,t){var e=o(tt,n,r,t);if(-1!==e.$||e.a)return e;return v(nt,1,e.b,e.c,e.d,e.e)})),ut=t((function(n,r){var t=n.a,e=n.b,u=f(Zr,t,r);return o(et,t,1===u.$?p([e]):f(Un,e,u.a),r)})),it=function(n){return V((function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(R(0))}))},at=e((function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,i=o(n,t.b,t.c,o(at,n,r,t.d));n=u,r=i,t=e}})),ft=a((function(n,r,u,i,a,f){var v=o(at,e((function(t,e,i){n:for(;;){var a=i.a,f=i.b;if(a.b){var v=a.a,s=v.a,l=v.b,h=a.b;if(0>b(s,t)){t=t,e=e,i=d(h,o(n,s,l,f));continue n}return b(s,t)>0?d(a,o(u,t,e,f)):d(h,c(r,s,l,e,f))}return d(a,o(u,t,e,f))}})),d(nr(i),f),a),s=v.a,l=v.b;return o(vr,t((function(r,t){return o(n,r.a,r.b,t)})),l,s)})),ot=un,ct=Xn,vt=G,st=e((function(n,r,t){if(r.b){var e=r.a,u=r.b,i=vt(f(ct,e,f(ot,n,e)));return f(Sr,(function(r){return o(st,n,u,o(et,e,r,t))}),i)}return Fr(t)})),bt=e((function(n,r,t){var i=t.bo,a=e((function(n,r,t){var e=t.c;return h(t.a,t.b,f(Sr,(function(){return e}),it(r)))})),c=o(vr,ut,Kr,r),v=s(ft,e((function(n,r,t){var e=t.b,u=t.c;return h(f(Un,n,t.a),e,u)})),u((function(n,r,t,e){var u=e.c;return h(e.a,o(et,n,t,e.b),u)})),a,c,i,h($,Kr,Fr(0))),b=v.a,l=v.b;return f(Sr,(function(n){return Fr(f(Xr,c,n))}),f(Sr,(function(){return o(st,n,b,l)}),v.c))})),lt=(Jr=Mr,V((function(n){n(R(Jr(Date.now())))}))),dt=e((function(n,r,t){var e=f(Zr,r,t.bx);if(1===e.$)return Fr(t);var u=e.a;return f(Sr,(function(){return Fr(t)}),f(Sr,(function(r){return Ir(f(Or,(function(t){return f(Rr,n,t(r))}),u))}),lt))})),ht=e((function(n,r,t){return n(r(t))}));nn.Time=rn(Qr,bt,dt,0,t((function(n,r){return f(Wr,r.a,f(ht,n,r.b))})));var $t,gt,mt=an("Time"),pt=t((function(n,r){return mt(f(Wr,n,r))})),wt=t((function(n,r){var t=r.M,e=d(t.a+.3,t.b+.03);return d(function(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}(r,{V:f(Un,e,r.V),M:e}),Gr)})),yt=c(u((function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}})),0,0,0,1),At=t((function(n,r){return f(mn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))})),jt=M,kt=function(n){switch(n.$){case 0:return jt(n.a)+"cm";case 1:return jt(n.a)+"em";case 2:return jt(n.a)+"ex";case 3:return jt(n.a)+"in";case 4:return jt(n.a)+"mm";case 5:return jt(n.a);case 6:return jt(n.a)+"pc";case 7:return jt(n.a)+"%";case 8:return jt(n.a)+"pt";default:return jt(n.a)+"px"}},_t=function(n){return f(At,"height",kt(n))},xt={$:1},Nt=function(n){return f(cr,"",n)},Et=E,Lt=function(n){var r,t=n.b,e=n.c,u=n.d,i=function(n){return Et(1e4*n)/100};return Nt(p(["rgba(",jt(i(n.a)),"%,",jt(i(t)),"%,",jt(i(e)),"%,",jt((r=u,Et(1e3*r)/1e3)),")"]))},Tt=function(n){return n.$?"none":Lt(n.a)},Mt=f(ht,At("fill"),Tt),Ft=Mt(xt),qt=function(n){return{$:9,a:n}},Ct=$n(function(n){return"script"==n?"p":n}("http://www.w3.org/2000/svg")),Bt=Ct("rect"),Ot=function(n){return f(At,"stroke",Lt(n))},St=function(n){return f(At,"stroke-width",kt(n))},zt=function(n){return f(At,"width",kt(n))},Dt=f(Bt,p([zt(qt(600)),_t(qt(600)),Ft,Ot(yt),St(qt(3))]),$),It=Ct("svg"),Rt=Ct("circle"),Vt=k,Jt=function(n){return f(At,"cy",kt(n))},Pt=function(n){return f(At,"r",kt(n))},Yt=function(n){var r=t((function(n,r){return Nt(p([n,"(",f(cr," ",f(Or,jt,r)),")"]))}));switch(n.$){case 0:return f(r,"matrix",p([n.a,n.b,n.c,n.d,n.e,n.f]));case 1:return f(r,"rotate",p([n.a,n.b,n.c]));case 2:return f(r,"scale",p([n.a,n.b]));case 3:return f(r,"skewX",p([n.a]));case 4:return f(r,"skewY",p([n.a]));default:return f(r,"translate",p([n.a,n.b]))}},Gt=($t=p([f(t((function(n,r){return{$:5,a:n,b:r}})),300,300)]),f(At,"transform",f(cr," ",f(Or,Yt,$t)))),Wt=_,Xt=t((function(n,r){var t,e,u=r.a,i=r.b,a=u*Vt(i),o=u*Wt(i);return f(Rt,p([Gt,(e=qt(a),f(At,"cx",kt(e))),Jt(qt(o)),Pt(qt(n)),Mt((t=yt,{$:0,a:t}))]),$)})),Ht=u((function(n,r,t,e){return f(At,"viewBox",f(cr," ",f(Or,jt,p([n,r,t,e]))))})),Kt=Ct("polyline"),Qt=Ct("line"),Ut=function(n){return f(At,"x2",kt(n))},Zt=function(n){return f(At,"y1",kt(n))},ne=function(n){return f(At,"y2",kt(n))},re=Pr({b1:function(){return d({ar:20,V:$,M:Yr,bw:100},Gr)},cq:function(){return f(pt,10,Mr)},cw:wt,cx:function(n){return f(It,p([zt(qt(600)),_t(qt(600)),c(Ht,0,0,600,600)]),p([Dt,(o=n.V,s=f(Or,(function(n){var r=n.a,t=n.b;return d(r*Vt(t),r*Wt(t))}),o),f(Kt,p([Gt,Mt(xt),Ot(yt),(v=s,f(At,"points",f(cr," ",f(Or,(function(n){var r=n.b;return jt(n.a)+", "+jt(r)}),v))))]),$)),(r=n.M,e=r.a,u=r.b,i=e*Vt(u),a=e*Wt(u),f(Qt,p([Gt,(t=qt(0),f(At,"x1",kt(t))),Zt(qt(0)),Ut(qt(i)),ne(qt(a)),Ot(yt),St(qt(2))]),$)),f(Xt,n.ar,n.M)]));var r,t,e,u,i,a,o,v,s}});gt={AngularMovement:{SpiralDrawer:{init:re(Lr(0))(0)}}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?j(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,gt):n.Elm=gt}(window);