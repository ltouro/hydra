"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[8550],{3905:(e,t,n)=>{n.d(t,{Zo:()=>u,kt:()=>f});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var l=a.createContext({}),c=function(e){var t=a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},u=function(e){var t=c(e.components);return a.createElement(l.Provider,{value:t},e.children)},p="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},h=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,l=e.parentName,u=i(e,["components","mdxType","originalType","parentName"]),p=c(n),h=r,f=p["".concat(l,".").concat(h)]||p[h]||d[h]||o;return n?a.createElement(f,s(s({ref:t},u),{},{components:n})):a.createElement(f,s({ref:t},u))}));function f(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,s=new Array(o);s[0]=h;var i={};for(var l in t)hasOwnProperty.call(t,l)&&(i[l]=t[l]);i.originalType=e,i[p]="string"==typeof e?e:r,s[1]=i;for(var c=2;c<o;c++)s[c]=n[c];return a.createElement.apply(null,s)}return a.createElement.apply(null,n)}h.displayName="MDXCreateElement"},39901:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>s,default:()=>d,frontMatter:()=>o,metadata:()=>i,toc:()=>c});var a=n(87462),r=(n(67294),n(3905));const o={slug:5,title:"5. Use io-classes\n",authors:[],tags:["Accepted"]},s=void 0,i={permalink:"/head-protocol/unstable/fr/adr/5",source:"@site/adr/2021-06-09_005-use-io-sim-classes.md",title:"5. Use io-classes\n",description:"Status",date:"2021-06-09T00:00:00.000Z",formattedDate:"9 juin 2021",tags:[{label:"Accepted",permalink:"/head-protocol/unstable/fr/adr/tags/accepted"}],readingTime:1.055,hasTruncateMarker:!1,authors:[],frontMatter:{slug:"5",title:"5. Use io-classes\n",authors:[],tags:["Accepted"]},prevItem:{title:"4. Use Handle to model Effects\n",permalink:"/head-protocol/unstable/fr/adr/4"},nextItem:{title:"6. Network Broadcasts all messages\n",permalink:"/head-protocol/unstable/fr/adr/6"}},l={authorsImageUrls:[]},c=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}],u={toc:c},p="wrapper";function d(e){let{components:t,...n}=e;return(0,r.kt)(p,(0,a.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"status"},"Status"),(0,r.kt)("p",null,"Accepted"),(0,r.kt)("h2",{id:"context"},"Context"),(0,r.kt)("p",null,"Although we try to contain the use of IO at the outskirt of the Hydra node using ",(0,r.kt)("a",{parentName:"p",href:"/adr/4"},"Handle pattern")," and ",(0,r.kt)("a",{parentName:"p",href:"/adr/2"},"Reactive core"),", low-level effects are still needed in various places, notably to define concurrently executing actions, and thus need to be tested"),(0,r.kt)("p",null,"Testing asynchronous and concurrent code is notoriously painful"),(0,r.kt)("p",null,"The ouroboros consensus test suite and ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra-sim"},"hydra-sim")," simulation have demonstrated the effectiveness of abstracting concurrent primitives through the use of typeclasses (MTL-style pattern) and being able to run these as pure code, harvesting and analysing produced execution traces."),(0,r.kt)("p",null,"There are other such libraries, e.g. ",(0,r.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/concurrency"},"concurrency")," and ",(0,r.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/dejafu"},"dejafu"),", as well as the venerable ",(0,r.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/exceptions"},"exceptions")," (for abstracting exception throwing)."),(0,r.kt)("h2",{id:"decision"},"Decision"),(0,r.kt)("p",null,"For all IO effects covered by the library, use functions from typeclasses exposed by ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/ouroboros-network/tree/e338f2cf8e1078fbda9555dd2b169c6737ef6774/io-classes"},"io-classes"),". As of this writing, this covers:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"All STM operations through ",(0,r.kt)("inlineCode",{parentName:"li"},"MonadSTM")),(0,r.kt)("li",{parentName:"ul"},"Time and timers through ",(0,r.kt)("inlineCode",{parentName:"li"},"MonadTime")," and ",(0,r.kt)("inlineCode",{parentName:"li"},"MonadTimer")),(0,r.kt)("li",{parentName:"ul"},"Concurrency through ",(0,r.kt)("inlineCode",{parentName:"li"},"MonadAsync"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"MonadFork")),(0,r.kt)("li",{parentName:"ul"},"Exceptions through ",(0,r.kt)("inlineCode",{parentName:"li"},"MonadThrow"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"MonadCatch")," and ",(0,r.kt)("inlineCode",{parentName:"li"},"MonadMask"))),(0,r.kt)("h2",{id:"consequences"},"Consequences"),(0,r.kt)("p",null,"We can use ",(0,r.kt)("inlineCode",{parentName:"p"},"io-sim")," to evaluate IO-ish functions easily"),(0,r.kt)("p",null,"Instantiation to concrete IO is pushed at the outermost layer, eg. in the ",(0,r.kt)("inlineCode",{parentName:"p"},"Main")," or tests."),(0,r.kt)("p",null,"As some of these functions and typeclasses clash with the\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/cardano-prelude"},"cardano-prelude")," we might\nwant to define a custom prelude (candidate for another ADR)"))}d.isMDXComponent=!0}}]);