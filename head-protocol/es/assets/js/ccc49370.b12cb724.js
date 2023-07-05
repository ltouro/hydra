(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[6103],{65203:(e,t,a)=>{"use strict";a.r(t),a.d(t,{default:()=>j});var s=a(67294),c=a(86010),d=a(10833),o=a(35281),n=a(9460),r=a(39058),l=a(74050),m=a(87462),i=a(95999),f=a(32244);function b(e){const{nextItem:t,prevItem:a}=e;return s.createElement("nav",{className:"pagination-nav docusaurus-mt-lg","aria-label":(0,i.I)({id:"theme.blog.post.paginator.navAriaLabel",message:"Blog post page navigation",description:"The ARIA label for the blog posts pagination"})},a&&s.createElement(f.Z,(0,m.Z)({},a,{subLabel:s.createElement(i.Z,{id:"theme.blog.post.paginator.newerPost",description:"The blog post button label to navigate to the newer/previous post"},"Newer Post")})),t&&s.createElement(f.Z,(0,m.Z)({},t,{subLabel:s.createElement(i.Z,{id:"theme.blog.post.paginator.olderPost",description:"The blog post button label to navigate to the older/next post"},"Older Post"),isNext:!0})))}function u(){const{assets:e,metadata:t}=(0,n.C)(),{title:a,description:c,date:o,tags:r,authors:l,frontMatter:m}=t,{keywords:i}=m,f=e.image??m.image;return s.createElement(d.d,{title:a,description:c,keywords:i,image:f},s.createElement("meta",{property:"og:type",content:"article"}),s.createElement("meta",{property:"article:published_time",content:o}),l.some((e=>e.url))&&s.createElement("meta",{property:"article:author",content:l.map((e=>e.url)).filter(Boolean).join(",")}),r.length>0&&s.createElement("meta",{property:"article:tag",content:r.map((e=>e.label)).join(",")}))}var h=a(39407);function p(e){let{sidebar:t,children:a}=e;const{metadata:c,toc:d}=(0,n.C)(),{nextItem:o,prevItem:m,frontMatter:i}=c,{hide_table_of_contents:f,toc_min_heading_level:u,toc_max_heading_level:p}=i;return s.createElement(r.Z,{sidebar:t,toc:!f&&d.length>0?s.createElement(h.Z,{toc:d,minHeadingLevel:u,maxHeadingLevel:p}):void 0},s.createElement(l.Z,null,a),(o||m)&&s.createElement(b,{nextItem:o,prevItem:m}))}function j(e){const t=e.content;return s.createElement(n.n,{content:e.content,isBlogPostPage:!0},s.createElement(d.FG,{className:(0,c.Z)(o.k.wrapper.blogPages,o.k.page.blogPostPage)},s.createElement(u,null),s.createElement(p,{sidebar:e.sidebar},s.createElement(t,null))))}},39407:(e,t,a)=>{"use strict";a.d(t,{Z:()=>m});var s=a(87462),c=a(67294),d=a(86010),o=a(93743);const n={tableOfContents:"tableOfContents_bqdL",docItemContainer:"docItemContainer_F8PC"},r="table-of-contents__link toc-highlight",l="table-of-contents__link--active";function m(e){let{className:t,...a}=e;return c.createElement("div",{className:(0,d.Z)(n.tableOfContents,"thin-scrollbar",t)},c.createElement(o.Z,(0,s.Z)({},a,{linkClassName:r,linkActiveClassName:l})))}},93743:(e,t,a)=>{"use strict";a.d(t,{Z:()=>u});var s=a(87462),c=a(67294),d=a(86668);function o(e){const t=e.map((e=>({...e,parentIndex:-1,children:[]}))),a=Array(7).fill(-1);t.forEach(((e,t)=>{const s=a.slice(2,e.level);e.parentIndex=Math.max(...s),a[e.level]=t}));const s=[];return t.forEach((e=>{const{parentIndex:a,...c}=e;a>=0?t[a].children.push(c):s.push(c)})),s}function n(e){let{toc:t,minHeadingLevel:a,maxHeadingLevel:s}=e;return t.flatMap((e=>{const t=n({toc:e.children,minHeadingLevel:a,maxHeadingLevel:s});return function(e){return e.level>=a&&e.level<=s}(e)?[{...e,children:t}]:t}))}function r(e){const t=e.getBoundingClientRect();return t.top===t.bottom?r(e.parentNode):t}function l(e,t){let{anchorTopOffset:a}=t;const s=e.find((e=>r(e).top>=a));if(s){return function(e){return e.top>0&&e.bottom<window.innerHeight/2}(r(s))?s:e[e.indexOf(s)-1]??null}return e[e.length-1]??null}function m(){const e=(0,c.useRef)(0),{navbar:{hideOnScroll:t}}=(0,d.L)();return(0,c.useEffect)((()=>{e.current=t?0:document.querySelector(".navbar").clientHeight}),[t]),e}function i(e){const t=(0,c.useRef)(void 0),a=m();(0,c.useEffect)((()=>{if(!e)return()=>{};const{linkClassName:s,linkActiveClassName:c,minHeadingLevel:d,maxHeadingLevel:o}=e;function n(){const e=function(e){return Array.from(document.getElementsByClassName(e))}(s),n=function(e){let{minHeadingLevel:t,maxHeadingLevel:a}=e;const s=[];for(let c=t;c<=a;c+=1)s.push(`h${c}.anchor`);return Array.from(document.querySelectorAll(s.join()))}({minHeadingLevel:d,maxHeadingLevel:o}),r=l(n,{anchorTopOffset:a.current}),m=e.find((e=>r&&r.id===function(e){return decodeURIComponent(e.href.substring(e.href.indexOf("#")+1))}(e)));e.forEach((e=>{!function(e,a){a?(t.current&&t.current!==e&&t.current.classList.remove(c),e.classList.add(c),t.current=e):e.classList.remove(c)}(e,e===m)}))}return document.addEventListener("scroll",n),document.addEventListener("resize",n),n(),()=>{document.removeEventListener("scroll",n),document.removeEventListener("resize",n)}}),[e,a])}function f(e){let{toc:t,className:a,linkClassName:s,isChild:d}=e;return t.length?c.createElement("ul",{className:d?void 0:a},t.map((e=>c.createElement("li",{key:e.id},c.createElement("a",{href:`#${e.id}`,className:s??void 0,dangerouslySetInnerHTML:{__html:e.value}}),c.createElement(f,{isChild:!0,toc:e.children,className:a,linkClassName:s}))))):null}const b=c.memo(f);function u(e){let{toc:t,className:a="table-of-contents table-of-contents__left-border",linkClassName:r="table-of-contents__link",linkActiveClassName:l,minHeadingLevel:m,maxHeadingLevel:f,...u}=e;const h=(0,d.L)(),p=m??h.tableOfContents.minHeadingLevel,j=f??h.tableOfContents.maxHeadingLevel,g=function(e){let{toc:t,minHeadingLevel:a,maxHeadingLevel:s}=e;return(0,c.useMemo)((()=>n({toc:o(t),minHeadingLevel:a,maxHeadingLevel:s})),[t,a,s])}({toc:t,minHeadingLevel:p,maxHeadingLevel:j});return i((0,c.useMemo)((()=>{if(r&&l)return{linkClassName:r,linkActiveClassName:l,minHeadingLevel:p,maxHeadingLevel:j}}),[r,l,p,j])),c.createElement(b,(0,s.Z)({toc:g,className:a,linkClassName:r},u))}},74050:(e,t,a)=>{"use strict";a.d(t,{Z:()=>u});var s=a(67294),c=a(30390);const d=JSON.parse('{"site":{"lastUpdatedAt":"2023-06-30T11:17:32+02:00"},"adr/1":{"source":{"lastUpdatedAt":"2022-07-22T09:16:51+02:00","commitHash":"64a94e277cea82c0302eb15b1fae7278c93e75f9"}},"adr/2":{"source":{"lastUpdatedAt":"2022-03-09T18:19:50+01:00","commitHash":"477d1cbab7a54793edc4c91ce3ca1f579db5c07c"}},"adr/3":{"source":{"lastUpdatedAt":"2022-03-09T18:19:50+01:00","commitHash":"477d1cbab7a54793edc4c91ce3ca1f579db5c07c"}},"adr/4":{"source":{"lastUpdatedAt":"2022-03-09T18:19:50+01:00","commitHash":"477d1cbab7a54793edc4c91ce3ca1f579db5c07c"}},"adr/5":{"source":{"lastUpdatedAt":"2022-03-09T18:19:50+01:00","commitHash":"477d1cbab7a54793edc4c91ce3ca1f579db5c07c"}},"adr/6":{"source":{"lastUpdatedAt":"2022-03-09T18:19:50+01:00","commitHash":"477d1cbab7a54793edc4c91ce3ca1f579db5c07c"}},"adr/7":{"source":{"lastUpdatedAt":"2022-04-19T11:02:00+02:00","commitHash":"6e6d3635017291f8cadb9f6c033aa1dad8e78f90"}},"adr/8":{"source":{"lastUpdatedAt":"2022-03-09T18:19:50+01:00","commitHash":"477d1cbab7a54793edc4c91ce3ca1f579db5c07c"}},"adr/9":{"source":{"lastUpdatedAt":"2022-04-21T16:12:57+02:00","commitHash":"dc52442bd5967196db5a8003f923b6977437fd7f"}},"adr/10":{"source":{"lastUpdatedAt":"2023-05-09T16:56:10+02:00","commitHash":"a87dd1648da0c766e5456de52b73b3cccfd5cfbd"}},"adr/11":{"source":{"lastUpdatedAt":"2022-03-09T18:19:50+01:00","commitHash":"477d1cbab7a54793edc4c91ce3ca1f579db5c07c"}},"adr/12":{"source":{"lastUpdatedAt":"2022-11-18T09:23:26+01:00","commitHash":"cb8657e254422d0c1bc4878af9a359163c48ce0a"}},"adr/13":{"source":{"lastUpdatedAt":"2022-04-19T11:02:28+02:00","commitHash":"d15d4d3a19df8496d3841c2d4bbdf1317886fc62"}},"adr/14":{"source":{"lastUpdatedAt":"2022-11-18T09:23:26+01:00","commitHash":"cb8657e254422d0c1bc4878af9a359163c48ce0a"}},"adr/15":{"source":{"lastUpdatedAt":"2022-11-18T09:23:26+01:00","commitHash":"cb8657e254422d0c1bc4878af9a359163c48ce0a"}},"adr/16":{"source":{"lastUpdatedAt":"2022-11-18T09:23:26+01:00","commitHash":"cb8657e254422d0c1bc4878af9a359163c48ce0a"}},"adr/17":{"source":{"lastUpdatedAt":"2023-05-03T14:08:25+02:00","commitHash":"40d8229ba2d0bf169ccbcb5de7d8660806829903"}},"adr/18":{"source":{"lastUpdatedAt":"2023-05-03T10:27:38+02:00","commitHash":"8b509f01da73feae5ec5ee14b35e4ffe0dddf499"}},"adr/19":{"source":{"lastUpdatedAt":"2022-08-01T16:40:59+02:00","commitHash":"985df773ef673ac4fcc4b790b4428beb617aa842"}},"adr/20":{"source":{"lastUpdatedAt":"2022-09-13T13:23:04+02:00","commitHash":"a97f8b3652ff15cc2a92cb4eac381b186663f1aa"}},"adr/21":{"source":{"lastUpdatedAt":"2023-04-27T09:29:20+02:00","commitHash":"d45a6e5b0cf74ac5c7f4a88ddca543916be58211"}},"adr/22":{"source":{"lastUpdatedAt":"2022-12-13T09:50:56+00:00","commitHash":"48da70c7c2b29331328fb06fe47c0ab98fd72069"}},"adr/23":{"source":{"lastUpdatedAt":"2023-05-08T08:53:07+02:00","commitHash":"ba54f484ce65429bdc3c8baaa1651a1072d9c4aa"}},"adr/24":{"source":{"lastUpdatedAt":"2023-06-23T10:13:57+02:00","commitHash":"677d0dafb6627f7487d7c54a5fe8f1353e1d2bc6"}},"benchmarks/end-to-end-benchmarks":{"source":{"lastUpdatedAt":"","commitHash":""}},"benchmarks":{"source":{"lastUpdatedAt":"2023-06-26T12:41:56+02:00","commitHash":"a8af2e6026d60ab1990a661e40efce27b5805afc"}},"benchmarks/profiling":{"source":{"lastUpdatedAt":"2023-03-31T10:05:15+02:00","commitHash":"811f124f85e300ec9725e6284d9e4d0984c6253b"}},"benchmarks/tests/hydra-cluster/hspec-results":{"source":{"lastUpdatedAt":"","commitHash":""}},"benchmarks/tests/hydra-node/hspec-results":{"source":{"lastUpdatedAt":"","commitHash":""}},"benchmarks/tests/hydra-plutus/hspec-results":{"source":{"lastUpdatedAt":"","commitHash":""}},"benchmarks/tests/hydra-tui/hspec-results":{"source":{"lastUpdatedAt":"","commitHash":""}},"benchmarks/tests/plutus-cbor/hspec-results":{"source":{"lastUpdatedAt":"","commitHash":""}},"benchmarks/tests/plutus-merkle-tree/hspec-results":{"source":{"lastUpdatedAt":"","commitHash":""}},"benchmarks/transaction-cost":{"source":{"lastUpdatedAt":"","commitHash":""}},"core-concepts/architecture":{"source":{"lastUpdatedAt":"2023-05-10T17:12:16+02:00","commitHash":"ffa2371bdd7a3949d9d03c59c643a33bd19450da"}},"core-concepts/architecture/networking":{"source":{"lastUpdatedAt":"2023-05-03T10:09:26+02:00","commitHash":"bb27e01012de87c21f72c3379b7fe3e8380ef180"}},"core-concepts/behavior":{"source":{"lastUpdatedAt":"2023-04-20T13:21:07+02:00","commitHash":"4f967d92d967d259df918d9f2cffd01ad1de1377"}},"core-concepts/faq":{"source":{"lastUpdatedAt":"2022-11-18T09:23:26+01:00","commitHash":"cb8657e254422d0c1bc4878af9a359163c48ce0a"}},"core-concepts":{"source":{"lastUpdatedAt":"2023-06-21T13:09:28+02:00","commitHash":"717c29b718a5dc80693c89beecc7e671fd13c48f"}},"core-concepts/layer-two":{"source":{"lastUpdatedAt":"2023-04-20T13:21:07+02:00","commitHash":"4f967d92d967d259df918d9f2cffd01ad1de1377"}},"core-concepts/rollbacks":{"source":{"lastUpdatedAt":"2023-04-26T16:57:44+02:00","commitHash":"38c699cac3c1f2b60e109d3e7773b096c88240d3"}},"core-concepts/specification":{"source":{"lastUpdatedAt":"2023-04-25T11:04:43+02:00","commitHash":"b24f1d11946149e7aaa964b77e0ce56de8093770"}},"docs/getting-started/demo":{"source":{"lastUpdatedAt":"2022-08-25T19:24:56+02:00","commitHash":"40dc912488f0023e14cf519d953738b20b67d146"},"fr":{"lastUpdatedAt":"2022-08-26T08:51:11+02:00","commitHash":"480f2eb7c8777bae119ac551bbc614bc8b8a5066"},"ja":{"lastUpdatedAt":"2022-08-25T19:31:47+02:00","commitHash":"a144c4d5694732205a775c57b0794bcd764d54b4"}},"docs/getting-started/demo/with-docker":{"source":{"lastUpdatedAt":"2023-06-20T13:24:14+02:00","commitHash":"241389ae368f19421d2b7ca29bae938720b4a1a6"},"fr":{"lastUpdatedAt":"2023-06-20T13:24:14+02:00","commitHash":"241389ae368f19421d2b7ca29bae938720b4a1a6"},"ja":{"lastUpdatedAt":"2023-06-20T13:24:14+02:00","commitHash":"241389ae368f19421d2b7ca29bae938720b4a1a6"}},"docs/getting-started/demo/without-docker":{"source":{"lastUpdatedAt":"2023-06-29T15:08:04+02:00","commitHash":"3d3da1f6bd2c859b6e6854352a211c2f5d281459"},"ja":{"lastUpdatedAt":"2023-05-11T12:38:12+02:00","commitHash":"0171bc9cde2c22c34cd18cc596644fd17514cb7f"}},"docs/getting-started/developing-on-hydra":{"source":{"lastUpdatedAt":"2023-03-31T10:05:15+02:00","commitHash":"811f124f85e300ec9725e6284d9e4d0984c6253b"}},"docs/getting-started/glossary":{"source":{"lastUpdatedAt":"2023-03-31T10:05:15+02:00","commitHash":"811f124f85e300ec9725e6284d9e4d0984c6253b"}},"docs/getting-started":{"source":{"lastUpdatedAt":"2023-06-30T11:17:32+02:00","commitHash":"d905e7db871e179c4640a8af545de3df719f853e"},"fr":{"lastUpdatedAt":"2022-05-21T09:28:11+02:00","commitHash":"7ebee872dbf4563035a1fbb394afbd4ae85b487f"},"ja":{"lastUpdatedAt":"2022-07-07T22:33:21+09:00","commitHash":"ff12ae270baadd05eecf1752f5357663db2083b2"}},"docs/getting-started/installation":{"source":{"lastUpdatedAt":"2023-03-02T15:51:13+01:00","commitHash":"ac42f523245900ff5d0f95271626603d81cfad2c"},"ja":{"lastUpdatedAt":"2023-03-02T15:51:13+01:00","commitHash":"ac42f523245900ff5d0f95271626603d81cfad2c"}},"docs/getting-started/operating-hydra":{"source":{"lastUpdatedAt":"2023-05-17T08:59:48+02:00","commitHash":"8006fc1e69df4b30b9b5f4aa414f3791dcfe4afa"}},"docs/getting-started/quickstart":{"source":{"lastUpdatedAt":"2023-06-20T17:48:22+02:00","commitHash":"93201bf07fe1fcb6d13f4fae0f06580f8dc267b8"},"ja":{"lastUpdatedAt":"2023-06-20T17:48:22+02:00","commitHash":"93201bf07fe1fcb6d13f4fae0f06580f8dc267b8"}},"docs/haskell_packages":{"source":{"lastUpdatedAt":"2023-05-04T22:32:28+02:00","commitHash":"3805cdc672d4b3c6106e72629adea4ae133bcccb"},"fr":{"lastUpdatedAt":"2023-03-31T10:05:15+02:00","commitHash":"811f124f85e300ec9725e6284d9e4d0984c6253b"},"ja":{"lastUpdatedAt":"2023-03-31T10:05:15+02:00","commitHash":"811f124f85e300ec9725e6284d9e4d0984c6253b"}},"docs/known-issues":{"source":{"lastUpdatedAt":"2023-06-15T15:55:01+02:00","commitHash":"08e0b71606aef49d380e2c574a9cca8aaf519690"}},"docs/tutorial":{"source":{"lastUpdatedAt":"2023-02-22T08:37:04+01:00","commitHash":"c78bf8782d70aa2d0032a5db4784a63a8c7290c0"}},"docs/tutorial/intro":{"source":{"lastUpdatedAt":"2023-02-22T08:37:04+01:00","commitHash":"c78bf8782d70aa2d0032a5db4784a63a8c7290c0"}},"docs/tutorial/protocol-outline":{"source":{"lastUpdatedAt":"2023-02-22T08:37:04+01:00","commitHash":"c78bf8782d70aa2d0032a5db4784a63a8c7290c0"}},"docs/tutorial/using_hydra/using-hydra-part-1":{"source":{"lastUpdatedAt":"2023-02-22T08:37:04+01:00","commitHash":"c78bf8782d70aa2d0032a5db4784a63a8c7290c0"}},"docs/tutorial/using_hydra/using-hydra-part-2":{"source":{"lastUpdatedAt":"2023-06-20T17:48:22+02:00","commitHash":"93201bf07fe1fcb6d13f4fae0f06580f8dc267b8"}},"docs/tutorial/using_hydra/using-hydra-part-3":{"source":{"lastUpdatedAt":"2023-06-20T17:48:22+02:00","commitHash":"93201bf07fe1fcb6d13f4fae0f06580f8dc267b8"}},"docs-benchmarksindex":{"ja":{"lastUpdatedAt":"2023-06-26T12:41:56+02:00","commitHash":"a8af2e6026d60ab1990a661e40efce27b5805afc"}},"docs-core-conceptsfaq":{"ja":{"lastUpdatedAt":"2022-07-08T16:15:21+09:00","commitHash":"c0b2f2dca525b58ea9da94b2ade7225106733102"}},"docs-core-conceptsindex":{"ja":{"lastUpdatedAt":"2023-05-04T09:31:55+02:00","commitHash":"1e9d8eb15cd6468d0c3ffa0894bb1d48f1303363"}},"docs-core-conceptslayer-two":{"ja":{"lastUpdatedAt":"2022-07-07T22:33:21+09:00","commitHash":"ff12ae270baadd05eecf1752f5357663db2083b2"}},"docs-core-conceptsnetworking":{"ja":{"lastUpdatedAt":"2022-11-18T09:23:26+01:00","commitHash":"cb8657e254422d0c1bc4878af9a359163c48ce0a"}},"docs-core-conceptsrollbacks":{"ja":{"lastUpdatedAt":"2022-07-07T22:33:21+09:00","commitHash":"ff12ae270baadd05eecf1752f5357663db2083b2"}},"docs-topologiesbasic":{"ja":{"lastUpdatedAt":"2022-07-07T22:33:21+09:00","commitHash":"ff12ae270baadd05eecf1752f5357663db2083b2"}},"docs-topologiesindex":{"ja":{"lastUpdatedAt":"2023-05-04T09:31:55+02:00","commitHash":"1e9d8eb15cd6468d0c3ffa0894bb1d48f1303363"}},"docs-topologiesstar-shaped":{"ja":{"lastUpdatedAt":"2022-07-07T22:33:21+09:00","commitHash":"ff12ae270baadd05eecf1752f5357663db2083b2"}},"monthly/2022-11-monthly":{"source":{"lastUpdatedAt":"2022-12-12T09:56:09+01:00","commitHash":"3ef369dcd97e14f3b004bc8dca9d425219df2253"}},"monthly/2022-12-monthly":{"source":{"lastUpdatedAt":"2023-01-09T09:53:11+01:00","commitHash":"45cb2b34d67f0d5b88385ea2ea633b65c2012cb4"}},"monthly/2023-01-monthly":{"source":{"lastUpdatedAt":"2023-01-30T10:50:20+01:00","commitHash":"6152d368b84961b8713a882b506cccd054fdad5f"}},"monthly/2023-02-monthly":{"source":{"lastUpdatedAt":"2023-03-02T08:25:48+01:00","commitHash":"dd95bfe60c446a9b3793d7d162afa6716b08ddae"}},"monthly/2023-03-monthly":{"source":{"lastUpdatedAt":"2023-04-27T09:29:20+02:00","commitHash":"d45a6e5b0cf74ac5c7f4a88ddca543916be58211"}},"monthly/2023-04-monthly":{"source":{"lastUpdatedAt":"2023-05-03T10:14:55+02:00","commitHash":"e2ebbcf40068d51fc234c000e34437f5ad70a2cf"}},"monthly/2023-05-monthly":{"source":{"lastUpdatedAt":"2023-06-26T12:41:56+02:00","commitHash":"a8af2e6026d60ab1990a661e40efce27b5805afc"}},"monthly/2023-06-monthly":{"source":{"lastUpdatedAt":"2023-06-26T12:54:34+02:00","commitHash":"4e996771a19c1647b883e4e9c24f167de0390f04"}},"standalone/audit-guidelines":{"source":{"lastUpdatedAt":"2023-06-15T18:10:57+02:00","commitHash":"adc05e87f39e8c06c036d87db1c2eda3d31ae88d"}},"standalone/contribute":{"source":{"lastUpdatedAt":"2022-11-30T09:08:20+01:00","commitHash":"fe7fab9f3af4b37ab529c0ec4cbfe0d7ccb71611"}},"topologies/basic":{"source":{"lastUpdatedAt":"2022-03-23T10:27:47+01:00","commitHash":"1ae660159a325caf355a6ae0a09d10b6a1a35acf"}},"topologies/delegated-head":{"source":{"lastUpdatedAt":"2023-03-31T10:05:15+02:00","commitHash":"811f124f85e300ec9725e6284d9e4d0984c6253b"}},"topologies":{"source":{"lastUpdatedAt":"2023-04-28T16:40:09+02:00","commitHash":"b9c7badc8a1989ee86c664e0c3e988dfbaac894e"}},"topologies/star-shaped":{"source":{"lastUpdatedAt":"2022-09-22T17:23:22+02:00","commitHash":"1700139001f6edf64bb6f0f78207280cb24f5d14"}},"use-cases":{"source":{"lastUpdatedAt":"2023-04-28T16:40:09+02:00","commitHash":"b9c7badc8a1989ee86c664e0c3e988dfbaac894e"}},"use-cases/inter-wallet-payments":{"source":{"lastUpdatedAt":"2022-07-06T17:08:14+02:00","commitHash":"79e491ec53d89504ef00be25e9d77c9e58e2e888"}},"use-cases/nft-auction":{"source":{"lastUpdatedAt":"2022-11-18T09:23:26+01:00","commitHash":"cb8657e254422d0c1bc4878af9a359163c48ce0a"}},"use-cases/pay-per-use-api":{"source":{"lastUpdatedAt":"2022-07-07T09:12:34+02:00","commitHash":"8c24ca271ff0aa2019e82a4fe8e5292b4ef8ea2a"}},"use-cases/poker-game":{"source":{"lastUpdatedAt":"2022-07-07T17:53:28+02:00","commitHash":"f3ae37f2dbd2972f76bc553757dff5cc8328de5f"}}}');var o=a(30381),n=a.n(o),r=a(72389),l=a(52263);const m={marginBottom:"1em"},i=e=>{let{lastUpdatedAt:t,commitHash:a}=e,c=`https://github.com/input-output-hk/hydra/commit/${a}`;return s.createElement("div",{style:m},"Last updated:\xa0",s.createElement("a",{href:c},n()(t).fromNow()))},f=e=>{let{sourceUpdatedAt:t,translationUpdatedAt:a,commitHash:c}=e,d=`https://github.com/input-output-hk/hydra/commit/${c}`;const o=n()(a).diff(t),r=o<0&&s.createElement("b",null,"(\u26a0\ufe0f Warning:\xa0 ",n().duration(o).humanize()," behind default language)");return s.createElement("div",{style:m},"Translation updated:\xa0",s.createElement("a",{href:d},n()(a).fromNow(),r))};function b(e){let{}=e;const t=(0,l.Z)();if(!(0,r.Z)())return s.createElement(s.Fragment,null);const a=t.siteConfig.baseUrl,c=new URL(window.location.href).pathname.replace(a,"").replace(/\/$/,""),o=t.i18n.defaultLocale,n=t.i18n.currentLocale,m=o!==n;if(void 0===d[c])return s.createElement(s.Fragment,null);const b=d[c],u=b.source,h=b[n];if(void 0===u)return s.createElement(s.Fragment,null);if(m&&h){const e={sourceUpdatedAt:u.lastUpdatedAt,translationUpdatedAt:h.lastUpdatedAt,commitHash:h.commitHash};return f(e)}return i(u)}function u(e){return s.createElement(s.Fragment,null,s.createElement(c.Z,e),s.createElement(b,null))}},46700:(e,t,a)=>{var s={"./af":42786,"./af.js":42786,"./ar":30867,"./ar-dz":14130,"./ar-dz.js":14130,"./ar-kw":96135,"./ar-kw.js":96135,"./ar-ly":56440,"./ar-ly.js":56440,"./ar-ma":47702,"./ar-ma.js":47702,"./ar-sa":16040,"./ar-sa.js":16040,"./ar-tn":37100,"./ar-tn.js":37100,"./ar.js":30867,"./az":31083,"./az.js":31083,"./be":9808,"./be.js":9808,"./bg":68338,"./bg.js":68338,"./bm":67438,"./bm.js":67438,"./bn":8905,"./bn-bd":76225,"./bn-bd.js":76225,"./bn.js":8905,"./bo":11560,"./bo.js":11560,"./br":1278,"./br.js":1278,"./bs":80622,"./bs.js":80622,"./ca":2468,"./ca.js":2468,"./cs":5822,"./cs.js":5822,"./cv":50877,"./cv.js":50877,"./cy":47373,"./cy.js":47373,"./da":24780,"./da.js":24780,"./de":59740,"./de-at":60217,"./de-at.js":60217,"./de-ch":60894,"./de-ch.js":60894,"./de.js":59740,"./dv":5300,"./dv.js":5300,"./el":50837,"./el.js":50837,"./en-au":78348,"./en-au.js":78348,"./en-ca":77925,"./en-ca.js":77925,"./en-gb":22243,"./en-gb.js":22243,"./en-ie":46436,"./en-ie.js":46436,"./en-il":47207,"./en-il.js":47207,"./en-in":44175,"./en-in.js":44175,"./en-nz":76319,"./en-nz.js":76319,"./en-sg":31662,"./en-sg.js":31662,"./eo":92915,"./eo.js":92915,"./es":55655,"./es-do":55251,"./es-do.js":55251,"./es-mx":96112,"./es-mx.js":96112,"./es-us":71146,"./es-us.js":71146,"./es.js":55655,"./et":5603,"./et.js":5603,"./eu":77763,"./eu.js":77763,"./fa":76959,"./fa.js":76959,"./fi":11897,"./fi.js":11897,"./fil":42549,"./fil.js":42549,"./fo":94694,"./fo.js":94694,"./fr":94470,"./fr-ca":63049,"./fr-ca.js":63049,"./fr-ch":52330,"./fr-ch.js":52330,"./fr.js":94470,"./fy":5044,"./fy.js":5044,"./ga":29295,"./ga.js":29295,"./gd":2101,"./gd.js":2101,"./gl":38794,"./gl.js":38794,"./gom-deva":27884,"./gom-deva.js":27884,"./gom-latn":23168,"./gom-latn.js":23168,"./gu":95349,"./gu.js":95349,"./he":24206,"./he.js":24206,"./hi":30094,"./hi.js":30094,"./hr":30316,"./hr.js":30316,"./hu":22138,"./hu.js":22138,"./hy-am":11423,"./hy-am.js":11423,"./id":29218,"./id.js":29218,"./is":90135,"./is.js":90135,"./it":90626,"./it-ch":10150,"./it-ch.js":10150,"./it.js":90626,"./ja":39183,"./ja.js":39183,"./jv":24286,"./jv.js":24286,"./ka":12105,"./ka.js":12105,"./kk":47772,"./kk.js":47772,"./km":18758,"./km.js":18758,"./kn":79282,"./kn.js":79282,"./ko":33730,"./ko.js":33730,"./ku":1408,"./ku.js":1408,"./ky":33291,"./ky.js":33291,"./lb":36841,"./lb.js":36841,"./lo":55466,"./lo.js":55466,"./lt":57010,"./lt.js":57010,"./lv":37595,"./lv.js":37595,"./me":39861,"./me.js":39861,"./mi":35493,"./mi.js":35493,"./mk":95966,"./mk.js":95966,"./ml":87341,"./ml.js":87341,"./mn":5115,"./mn.js":5115,"./mr":10370,"./mr.js":10370,"./ms":9847,"./ms-my":41237,"./ms-my.js":41237,"./ms.js":9847,"./mt":72126,"./mt.js":72126,"./my":56165,"./my.js":56165,"./nb":64924,"./nb.js":64924,"./ne":16744,"./ne.js":16744,"./nl":93901,"./nl-be":59814,"./nl-be.js":59814,"./nl.js":93901,"./nn":83877,"./nn.js":83877,"./oc-lnc":92135,"./oc-lnc.js":92135,"./pa-in":15858,"./pa-in.js":15858,"./pl":64495,"./pl.js":64495,"./pt":89520,"./pt-br":57971,"./pt-br.js":57971,"./pt.js":89520,"./ro":96459,"./ro.js":96459,"./ru":21793,"./ru.js":21793,"./sd":40950,"./sd.js":40950,"./se":10490,"./se.js":10490,"./si":90124,"./si.js":90124,"./sk":64249,"./sk.js":64249,"./sl":14985,"./sl.js":14985,"./sq":51104,"./sq.js":51104,"./sr":49131,"./sr-cyrl":79915,"./sr-cyrl.js":79915,"./sr.js":49131,"./ss":85893,"./ss.js":85893,"./sv":98760,"./sv.js":98760,"./sw":91172,"./sw.js":91172,"./ta":27333,"./ta.js":27333,"./te":23110,"./te.js":23110,"./tet":52095,"./tet.js":52095,"./tg":27321,"./tg.js":27321,"./th":9041,"./th.js":9041,"./tk":19005,"./tk.js":19005,"./tl-ph":75768,"./tl-ph.js":75768,"./tlh":89444,"./tlh.js":89444,"./tr":72397,"./tr.js":72397,"./tzl":28254,"./tzl.js":28254,"./tzm":51106,"./tzm-latn":30699,"./tzm-latn.js":30699,"./tzm.js":51106,"./ug-cn":9288,"./ug-cn.js":9288,"./uk":67691,"./uk.js":67691,"./ur":13795,"./ur.js":13795,"./uz":6791,"./uz-latn":60588,"./uz-latn.js":60588,"./uz.js":6791,"./vi":65666,"./vi.js":65666,"./x-pseudo":14378,"./x-pseudo.js":14378,"./yo":75805,"./yo.js":75805,"./zh-cn":83839,"./zh-cn.js":83839,"./zh-hk":55726,"./zh-hk.js":55726,"./zh-mo":99807,"./zh-mo.js":99807,"./zh-tw":74152,"./zh-tw.js":74152};function c(e){var t=d(e);return a(t)}function d(e){if(!a.o(s,e)){var t=new Error("Cannot find module '"+e+"'");throw t.code="MODULE_NOT_FOUND",t}return s[e]}c.keys=function(){return Object.keys(s)},c.resolve=d,e.exports=c,c.id=46700}}]);