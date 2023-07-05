"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[8609],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>m});var a=n(67294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,o=function(e,t){if(null==e)return{};var n,a,o={},r=Object.keys(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var l=a.createContext({}),h=function(e){var t=a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},d=function(e){var t=h(e.components);return a.createElement(l.Provider,{value:t},e.children)},p="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},u=a.forwardRef((function(e,t){var n=e.components,o=e.mdxType,r=e.originalType,l=e.parentName,d=s(e,["components","mdxType","originalType","parentName"]),p=h(n),u=o,m=p["".concat(l,".").concat(u)]||p[u]||c[u]||r;return n?a.createElement(m,i(i({ref:t},d),{},{components:n})):a.createElement(m,i({ref:t},d))}));function m(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=n.length,i=new Array(r);i[0]=u;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[p]="string"==typeof e?e:o,i[1]=s;for(var h=2;h<r;h++)i[h]=n[h];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}u.displayName="MDXCreateElement"},32986:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>c,frontMatter:()=>r,metadata:()=>s,toc:()=>h});var a=n(87462),o=(n(67294),n(3905));const r={title:"June 2023",slug:"2023-06",authors:["v0d1ch","ch1bo"],tags:["monthly"]},i=void 0,s={permalink:"/head-protocol/monthly/2023-06",editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/monthly/2023-06-monthly.md",source:"@site/monthly/2023-06-monthly.md",title:"June 2023",description:"This report summarizes the work on Hydra since May 2023. It serves as",date:"2023-06-27T07:53:28.000Z",formattedDate:"June 27, 2023",tags:[{label:"monthly",permalink:"/head-protocol/monthly/tags/monthly"}],readingTime:6.975,hasTruncateMarker:!1,authors:[{name:"Sasha Bogicevic",title:"Senior software engineer - Hydra @ IOG",url:"https://github.com/v0d1ch",imageURL:"https://github.com/v0d1ch.png",key:"v0d1ch"},{name:"Sebastian Nagel",title:"Software Engineering Lead - Hydra @ IOG",url:"https://github.com/ch1bo",imageURL:"https://github.com/ch1bo.png",key:"ch1bo"}],frontMatter:{title:"June 2023",slug:"2023-06",authors:["v0d1ch","ch1bo"],tags:["monthly"]},nextItem:{title:"May 2023",permalink:"/head-protocol/monthly/2023-05"}},l={authorsImageUrls:[void 0,void 0]},h=[{value:"Roadmap",id:"roadmap",level:2},{value:"Notable roadmap updates",id:"notable-roadmap-updates",level:4},{value:"Development",id:"development",level:2},{value:"Commits with multiple UTXO #774",id:"commits-with-multiple-utxo-774",level:4},{value:"Commits from external wallet #215",id:"commits-from-external-wallet-215",level:4},{value:"Benchmark performance of Hydra Head #186",id:"benchmark-performance-of-hydra-head-186",level:4},{value:"Operating hydra nodes",id:"operating-hydra-nodes",level:4},{value:"Community",id:"community",level:2},{value:"Hydra hackathon / workshop",id:"hydra-hackathon--workshop",level:4},{value:"Hydra for Auctions contributions and closing of project",id:"hydra-for-auctions-contributions-and-closing-of-project",level:4},{value:"Conclusion",id:"conclusion",level:2}],d={toc:h},p="wrapper";function c(e){let{components:t,...r}=e;return(0,o.kt)(p,(0,a.Z)({},d,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("p",null,"This report summarizes the work on Hydra since May 2023. It serves as\npreparation for the monthly review meeting (see ",(0,o.kt)("a",{parentName:"p",href:"https://docs.google.com/presentation/d/1TVzjaFKXBi9DAugSd2L8MSUSZGIU9EjTmwf6yccckPI"},"slides")," and\n",(0,o.kt)("a",{parentName:"p",href:"https://drive.google.com/file/d/1_N6b4RDe579TgLawiJzbE0NLofD3ljE6/view"},"recording"),"), where the team updates project stakeholders on recent\ndevelopments to gather their feedback on proposed plans."),(0,o.kt)("h2",{id:"roadmap"},"Roadmap"),(0,o.kt)("p",null,"While there was no release this month, the team implemented several notable\nfeatures, which will be released soon as version 0.11.0:"),(0,o.kt)("p",null,(0,o.kt)("img",{alt:"The roadmap without idea items",src:n(943).Z,width:"2372",height:"813"})," ",(0,o.kt)("small",null,(0,o.kt)("center",null,"The roadmap without idea items"))),(0,o.kt)("h4",{id:"notable-roadmap-updates"},"Notable roadmap updates"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Realized ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/774"},"allowing commit txs with multiple UTxO\n#774")," as a dedicated roadmap\nitem separate from the related ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/215"},"commit from external wallet\n#215")," (which grew too\nbig).")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Revisited ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/186"},"off-chain performance by doing benchmarks\n#186"),". Identified a\nbottleneck, groomed and planned a follow-up performance improvement feature\nfor ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/913"},'Event sourced persistence"\n#913'))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Plan to release 0.11.0 without ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/727"},"Authenticate network messages\n#727")," to deliver enabling\nfeatues earlier.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"API improvements and exploring batched transactions on L2 showed that ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/728"},"ReqSN\nonly sends transaction IDs\n#728")," is in demand,\nfront-loads further API changes and is estimated to be a low-hanging fruit.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Reprioritized items in ",(0,o.kt)("inlineCode",{parentName:"p"},"<= 1.0.0")," column, to do items with on-chain protocol\nimpact earlier and not much feedback has been received on snapshotting items. In\nfact, if ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/190"},"Support larger # of UTXO via split-fanout\n#190")," is done slightly\ndifferently, that should avoid some of the problems of impossible to finalize\nsnapshots."))),(0,o.kt)("p",null,(0,o.kt)("img",{alt:"The latest roadmap with features and ideas",src:n(55439).Z,width:"3122",height:"1082"})," ",(0,o.kt)("small",null,(0,o.kt)("center",null,"The latest roadmap with featuresand ideas"))),(0,o.kt)("h2",{id:"development"},"Development"),(0,o.kt)("p",null,(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-05-24..2023-06-22"},"Issues and pull requests closed since last\nreport")),(0,o.kt)("p",null,"This month, the team worked on the following:"),(0,o.kt)("h4",{id:"commits-with-multiple-utxo-774"},"Commits with multiple UTXO ",(0,o.kt)("a",{parentName:"h4",href:"https://github.com/input-output-hk/hydra/pull/774"},"#774")),(0,o.kt)("p",null,'One of the early adopting projects is exploring how to move scripts from layer 1\nto layer 2. For that purpose, it was necessary to not only commit the actual\nscript UTXO, but also a "regular" UTXO holding only ada to be used as collateral\n(the ',(0,o.kt)("inlineCode",{parentName:"p"},"cardano-ledger")," always requires collateral although it would not be\nnecessary on a layer 2)."),(0,o.kt)("p",null,"To enable this, the specification and on-chain protocol needed updating. Before\na protocol participant could commit zero or one UTXO, which changed now to a\nlist of UTXO. As the specification is now ",(0,o.kt)("a",{parentName:"p",href:"/monthly/2023-04#versioned-docs-and-specification"},"part of the\nrepository"),", it could be kept\nconsistent within the same pull request."),(0,o.kt)("p",null,"Despite being a ",(0,o.kt)("strong",{parentName:"p"},"breaking change"),", leading to new Hydra script hashes to be\npublished and used starting with version 0.11.0, this change was suprisingly\neasy to do and demonstrated the amenability of the Head protocol and the system\narchitecture."),(0,o.kt)("h4",{id:"commits-from-external-wallet-215"},"Commits from external wallet ",(0,o.kt)("a",{parentName:"h4",href:"https://github.com/input-output-hk/hydra/issues/215"},"#215")),(0,o.kt)("p",null,"The team started to mark ",(0,o.kt)("em",{parentName:"p"},"fuel")," some time ago as it was an easy workaround to\ndistinguish UTXOs that can be committed into a head apart from regular outputs\nholding ada to pay for fees - the so-called ",(0,o.kt)("em",{parentName:"p"},"fuel"),'. However, this required users\nto "send funds" they want to commit first to the ',(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node"),"s internal wallet\nand involved additional steps in tagging such outputs with a special datum hash"),(0,o.kt)("p",null,"To commit from external wallets, a new API endpoint was introduced for the\npurpose of ",(0,o.kt)("em",{parentName:"p"},"drafting")," a commit transaction. The clients would request such draft\ntransaction by sending a POST request to ",(0,o.kt)("inlineCode",{parentName:"p"},"/commit")," and the ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node")," would\nrespond with a transaction already authorized by the internal wallet. If the\ncommit involved user funds (empty commits are still possible), then the client\napplication would need to sign the transaction using the corresponding signing\nkey. Also, submitting this transaction has shifted from ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node")," to the\nclient."),(0,o.kt)("p",null,"This removes direct custody of ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node")," over user funds since clients can\nnow use whatever key they own, not known to the ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node"),", to do a commit\nstep and no single ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node")," has access to user funds used in the Head\nprotocol."),(0,o.kt)("p",null,"Within this work package, ",(0,o.kt)("em",{parentName:"p"},"marking fuel")," became deprecated and all UTXOs owned\nby the internal wallet are considered fuel. Fuel marking wil be completely\nremoved in the future. Furthermore, a good old HTTP-based API is now used\nfor the new query (so far it was only WebSocket-based), which prompts a\npotential shift to using OpenAPI as API specification since AsyncAPI does not\ndescribe synchronous requests well."),(0,o.kt)("h4",{id:"benchmark-performance-of-hydra-head-186"},"Benchmark performance of Hydra Head ",(0,o.kt)("a",{parentName:"h4",href:"https://github.com/input-output-hk/hydra/issues/215"},"#186")),(0,o.kt)("p",null,"Low confirmation time is a key feature of Hydra Head protocol and is\nneeded for some use cases. The existing end-to-end\nbenchmarks have been revived and tailored for a ",(0,o.kt)("em",{parentName:"p"},"baseline scenario"),", one with a\nsingle hydra-node and a single client. While this scenario is not\nrepresentative of target deployments, it's interesting as it gives us\nan upper bound on the performance of a Hydra Head network."),(0,o.kt)("p",null,"The results have been somewhat disappointing, and further investigations lead to a few insights:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"There was a misconfiguration in the RTS of the hydra-node executable\nwhich was therefore not taking advantage of parallelism"),(0,o.kt)("li",{parentName:"ul"},"We identified the main bottleneck to be persistence of the node's\nstate, which is currently done by overwriting a single file with the\nJSON content of the ",(0,o.kt)("em",{parentName:"li"},"full")," state on ",(0,o.kt)("em",{parentName:"li"},"state change")," which is pretty\ninefficient. As a consequence, the team will work on improving the\npersistence strategy as described in ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/pull/940"},"this\nADR")),(0,o.kt)("li",{parentName:"ul"},"More benchmarks:",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"The aforementioned ",(0,o.kt)("a",{parentName:"li",href:"/benchmarks/end-to-end-benchmarks"},"End-to-end benchmarks"),","),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"/benchmarks/ledger"},"Ledger Micro-benchmarks")," as a comparison basis.")))),(0,o.kt)("h4",{id:"operating-hydra-nodes"},"Operating hydra nodes"),(0,o.kt)("p",null,'As a "dogfooding" exercise, the Hydra team is operating a Hydra Head\non mainnet on top of which is running our ',(0,o.kt)("inlineCode",{parentName:"p"},"hydraw")," application.\nThe Head protocol got stuck a couple of times and\npost-mortem investigations lead to a few improvements in how to\noperate a hydra-node and network:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Better reporting on the version of executable ",(0,o.kt)("inlineCode",{parentName:"li"},"hydra-node")," and\n",(0,o.kt)("inlineCode",{parentName:"li"},"hydra-tui")," which now report the Git SHA1 at which they were built"),(0,o.kt)("li",{parentName:"ul"},"Reduction in the volume of logs emitted by hydra-node by removing\nsome chatty network-related logs and using ids to link ",(0,o.kt)("inlineCode",{parentName:"li"},"Begin/End"),"\npairs"),(0,o.kt)("li",{parentName:"ul"},"Rework the\n",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/blob/35f2964ba6d4a780a5f8e669f1afce565a492cec/hydra-cluster/exe/log-filter/Main.hs#L34"},"log-filter"),"\ntool to provide timings for various events and effects keyed by\ntransactions")),(0,o.kt)("h2",{id:"community"},"Community"),(0,o.kt)("h4",{id:"hydra-hackathon--workshop"},"Hydra hackathon / workshop"),(0,o.kt)("p",null,"The Hydra team is considering holding a workshop at/around Rare Evo at the end\nof August and is in contact with the event teams at IOG and the Rare Evo\norganizers. The concrete format, scope and agenda is still a bit unclear as we\nare contemplating whether to do a workshop/tutorial style or rather a\nintroduction + challenge setting event. If it does not work out for Rare Evo, we\nwill find another event or do it ourselves."),(0,o.kt)("p",null,"If you are reading this and would be interested in joining such an event please\ndrop us a line on ",(0,o.kt)("a",{parentName:"p",href:"https://discord.gg/Qq5vNTg9PT"},"discord")," or DM\n",(0,o.kt)("a",{parentName:"p",href:"https://twitter.com/ch1bo_"},"@ch1bo"),"! Ideally along with some thoughts on\npreferred format or what you would be interested in."),(0,o.kt)("h4",{id:"hydra-for-auctions-contributions-and-closing-of-project"},"Hydra for Auctions contributions and closing of project"),(0,o.kt)("p",null,"One of the Hydra lighthouse projects is slowly coming to an end. The\ncollaboration project between IOG and MLabs on using Hydra for auctions is\ncurrently finalizing documentation and creating demonstration running the whole\nthing on a public testnet. Although the demo video was not available at the copy\ndeadline of this report, watch this space for more about this next\nmonth."),(0,o.kt)("p",null,"The project yielded multiple github issues containing ideas and sparked great\ndiscussions on improving Hydra like ",(0,o.kt)("a",{parentName:"p",href:"https://hydra.family/head-protocol/monthly/2023-05#hydrozoa-850"},"reported last\nmonth"),". It is\nalso the first project which demonstrates how to move smart contracts from the\nlayer 1 (L1) to the layer 2 (L2)! Overall it is a great example of establishing\ncrucial state on L1 and achieving scalability through Hydra as L2."),(0,o.kt)("p",null,"The code is fully open source and available on Github\n",(0,o.kt)("a",{parentName:"p",href:"https://github.com/mlabs-haskell/hydra-auction/"},"hydra-auction"),"."),(0,o.kt)("h2",{id:"conclusion"},"Conclusion"),(0,o.kt)("p",null,"The monthly review meeting for May was held on 2023-06-21 via Google Meet with\nthese ",(0,o.kt)("a",{parentName:"p",href:"https://docs.google.com/presentation/d/1TVzjaFKXBi9DAugSd2L8MSUSZGIU9EjTmwf6yccckPI"},"slides")," and here is the ",(0,o.kt)("a",{parentName:"p",href:"https://drive.google.com/file/d/1_N6b4RDe579TgLawiJzbE0NLofD3ljE6/view"},"recording"),"."),(0,o.kt)("p",null,"It was a fairly straight-forward month and consequently unexciting review meeting\nthis month. Unfortunately, we could not release 0.11.0 yet, but we wanted to get\nthe quite significant change of supporting commits from external wallets done\nfirst. This feature in particular was more involved than expected, but as the\ndemonstration in the meeting showed, we are in the final stages of getting this\nover the line."),(0,o.kt)("p",null,"Besides some nice findings to potential performance improvements for layer 2\ntransaction processing, there was not much to announce this time around. Behind\nthe scenes, however, there have been great progress on the Hydra for Payments\nproject and the next updates are going to be more interesting again."),(0,o.kt)("p",null,"Despite holiday season approaching, we will march on, steadily adding features\nand enabling more and more use cases to scale through Hydra."))}c.isMDXComponent=!0},943:(e,t,n)=>{n.d(t,{Z:()=>a});const a=n.p+"assets/images/2023-06-roadmap-ex-ideas-9d500c027b6594ce2a53d6158c2146c7.png"},55439:(e,t,n)=>{n.d(t,{Z:()=>a});const a=n.p+"assets/images/2023-06-roadmap-6dde45fc909764d6d33f6f1d88b19b23.png"}}]);