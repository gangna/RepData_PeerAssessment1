<!DOCTYPE html>
<!-- saved from url=(0014)about:internet -->
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<meta http-equiv="x-ua-compatible" content="IE=9" >

<title>Reproducible Research: Peer Assessment 1</title>

<style type="text/css">
body, td {
   font-family: sans-serif;
   background-color: white;
   font-size: 12px;
   margin: 8px;
}

tt, code, pre {
   font-family: 'DejaVu Sans Mono', 'Droid Sans Mono', 'Lucida Console', Consolas, Monaco, monospace;
}

h1 { 
   font-size:2.2em; 
}

h2 { 
   font-size:1.8em; 
}

h3 { 
   font-size:1.4em; 
}

h4 { 
   font-size:1.0em; 
}

h5 { 
   font-size:0.9em; 
}

h6 { 
   font-size:0.8em; 
}

a:visited {
   color: rgb(50%, 0%, 50%);
}

pre {	
   margin-top: 0;
   max-width: 95%;
   border: 1px solid #ccc;
   white-space: pre-wrap;
}

pre code {
   display: block; padding: 0.5em;
}

code.r, code.cpp {
   background-color: #F8F8F8;
}

table, td, th {
  border: none;
}

blockquote {
   color:#666666;
   margin:0;
   padding-left: 1em;
   border-left: 0.5em #EEE solid;
}

hr {
   height: 0px;
   border-bottom: none;
   border-top-width: thin;
   border-top-style: dotted;
   border-top-color: #999999;
}

@media print {
   * { 
      background: transparent !important; 
      color: black !important; 
      filter:none !important; 
      -ms-filter: none !important; 
   }

   body { 
      font-size:12pt; 
      max-width:100%; 
   }
       
   a, a:visited { 
      text-decoration: underline; 
   }

   hr { 
      visibility: hidden;
      page-break-before: always;
   }

   pre, blockquote { 
      padding-right: 1em; 
      page-break-inside: avoid; 
   }

   tr, img { 
      page-break-inside: avoid; 
   }

   img { 
      max-width: 100% !important; 
   }

   @page :left { 
      margin: 15mm 20mm 15mm 10mm; 
   }
     
   @page :right { 
      margin: 15mm 10mm 15mm 20mm; 
   }

   p, h2, h3 { 
      orphans: 3; widows: 3; 
   }

   h2, h3 { 
      page-break-after: avoid; 
   }
}

</style>

<!-- Styles for R syntax highlighter -->
<style type="text/css">
   pre .operator,
   pre .paren {
     color: rgb(104, 118, 135)
   }

   pre .literal {
     color: rgb(88, 72, 246)
   }

   pre .number {
     color: rgb(0, 0, 205);
   }

   pre .comment {
     color: rgb(76, 136, 107);
   }

   pre .keyword {
     color: rgb(0, 0, 255);
   }

   pre .identifier {
     color: rgb(0, 0, 0);
   }

   pre .string {
     color: rgb(3, 106, 7);
   }
</style>

<!-- R syntax highlighter -->
<script type="text/javascript">
var hljs=new function(){function m(p){return p.replace(/&/gm,"&amp;").replace(/</gm,"&lt;")}function f(r,q,p){return RegExp(q,"m"+(r.cI?"i":"")+(p?"g":""))}function b(r){for(var p=0;p<r.childNodes.length;p++){var q=r.childNodes[p];if(q.nodeName=="CODE"){return q}if(!(q.nodeType==3&&q.nodeValue.match(/\s+/))){break}}}function h(t,s){var p="";for(var r=0;r<t.childNodes.length;r++){if(t.childNodes[r].nodeType==3){var q=t.childNodes[r].nodeValue;if(s){q=q.replace(/\n/g,"")}p+=q}else{if(t.childNodes[r].nodeName=="BR"){p+="\n"}else{p+=h(t.childNodes[r])}}}if(/MSIE [678]/.test(navigator.userAgent)){p=p.replace(/\r/g,"\n")}return p}function a(s){var r=s.className.split(/\s+/);r=r.concat(s.parentNode.className.split(/\s+/));for(var q=0;q<r.length;q++){var p=r[q].replace(/^language-/,"");if(e[p]){return p}}}function c(q){var p=[];(function(s,t){for(var r=0;r<s.childNodes.length;r++){if(s.childNodes[r].nodeType==3){t+=s.childNodes[r].nodeValue.length}else{if(s.childNodes[r].nodeName=="BR"){t+=1}else{if(s.childNodes[r].nodeType==1){p.push({event:"start",offset:t,node:s.childNodes[r]});t=arguments.callee(s.childNodes[r],t);p.push({event:"stop",offset:t,node:s.childNodes[r]})}}}}return t})(q,0);return p}function k(y,w,x){var q=0;var z="";var s=[];function u(){if(y.length&&w.length){if(y[0].offset!=w[0].offset){return(y[0].offset<w[0].offset)?y:w}else{return w[0].event=="start"?y:w}}else{return y.length?y:w}}function t(D){var A="<"+D.nodeName.toLowerCase();for(var B=0;B<D.attributes.length;B++){var C=D.attributes[B];A+=" "+C.nodeName.toLowerCase();if(C.value!==undefined&&C.value!==false&&C.value!==null){A+='="'+m(C.value)+'"'}}return A+">"}while(y.length||w.length){var v=u().splice(0,1)[0];z+=m(x.substr(q,v.offset-q));q=v.offset;if(v.event=="start"){z+=t(v.node);s.push(v.node)}else{if(v.event=="stop"){var p,r=s.length;do{r--;p=s[r];z+=("</"+p.nodeName.toLowerCase()+">")}while(p!=v.node);s.splice(r,1);while(r<s.length){z+=t(s[r]);r++}}}}return z+m(x.substr(q))}function j(){function q(x,y,v){if(x.compiled){return}var u;var s=[];if(x.k){x.lR=f(y,x.l||hljs.IR,true);for(var w in x.k){if(!x.k.hasOwnProperty(w)){continue}if(x.k[w] instanceof Object){u=x.k[w]}else{u=x.k;w="keyword"}for(var r in u){if(!u.hasOwnProperty(r)){continue}x.k[r]=[w,u[r]];s.push(r)}}}if(!v){if(x.bWK){x.b="\\b("+s.join("|")+")\\s"}x.bR=f(y,x.b?x.b:"\\B|\\b");if(!x.e&&!x.eW){x.e="\\B|\\b"}if(x.e){x.eR=f(y,x.e)}}if(x.i){x.iR=f(y,x.i)}if(x.r===undefined){x.r=1}if(!x.c){x.c=[]}x.compiled=true;for(var t=0;t<x.c.length;t++){if(x.c[t]=="self"){x.c[t]=x}q(x.c[t],y,false)}if(x.starts){q(x.starts,y,false)}}for(var p in e){if(!e.hasOwnProperty(p)){continue}q(e[p].dM,e[p],true)}}function d(B,C){if(!j.called){j();j.called=true}function q(r,M){for(var L=0;L<M.c.length;L++){if((M.c[L].bR.exec(r)||[null])[0]==r){return M.c[L]}}}function v(L,r){if(D[L].e&&D[L].eR.test(r)){return 1}if(D[L].eW){var M=v(L-1,r);return M?M+1:0}return 0}function w(r,L){return L.i&&L.iR.test(r)}function K(N,O){var M=[];for(var L=0;L<N.c.length;L++){M.push(N.c[L].b)}var r=D.length-1;do{if(D[r].e){M.push(D[r].e)}r--}while(D[r+1].eW);if(N.i){M.push(N.i)}return f(O,M.join("|"),true)}function p(M,L){var N=D[D.length-1];if(!N.t){N.t=K(N,E)}N.t.lastIndex=L;var r=N.t.exec(M);return r?[M.substr(L,r.index-L),r[0],false]:[M.substr(L),"",true]}function z(N,r){var L=E.cI?r[0].toLowerCase():r[0];var M=N.k[L];if(M&&M instanceof Array){return M}return false}function F(L,P){L=m(L);if(!P.k){return L}var r="";var O=0;P.lR.lastIndex=0;var M=P.lR.exec(L);while(M){r+=L.substr(O,M.index-O);var N=z(P,M);if(N){x+=N[1];r+='<span class="'+N[0]+'">'+M[0]+"</span>"}else{r+=M[0]}O=P.lR.lastIndex;M=P.lR.exec(L)}return r+L.substr(O,L.length-O)}function J(L,M){if(M.sL&&e[M.sL]){var r=d(M.sL,L);x+=r.keyword_count;return r.value}else{return F(L,M)}}function I(M,r){var L=M.cN?'<span class="'+M.cN+'">':"";if(M.rB){y+=L;M.buffer=""}else{if(M.eB){y+=m(r)+L;M.buffer=""}else{y+=L;M.buffer=r}}D.push(M);A+=M.r}function G(N,M,Q){var R=D[D.length-1];if(Q){y+=J(R.buffer+N,R);return false}var P=q(M,R);if(P){y+=J(R.buffer+N,R);I(P,M);return P.rB}var L=v(D.length-1,M);if(L){var O=R.cN?"</span>":"";if(R.rE){y+=J(R.buffer+N,R)+O}else{if(R.eE){y+=J(R.buffer+N,R)+O+m(M)}else{y+=J(R.buffer+N+M,R)+O}}while(L>1){O=D[D.length-2].cN?"</span>":"";y+=O;L--;D.length--}var r=D[D.length-1];D.length--;D[D.length-1].buffer="";if(r.starts){I(r.starts,"")}return R.rE}if(w(M,R)){throw"Illegal"}}var E=e[B];var D=[E.dM];var A=0;var x=0;var y="";try{var s,u=0;E.dM.buffer="";do{s=p(C,u);var t=G(s[0],s[1],s[2]);u+=s[0].length;if(!t){u+=s[1].length}}while(!s[2]);if(D.length>1){throw"Illegal"}return{r:A,keyword_count:x,value:y}}catch(H){if(H=="Illegal"){return{r:0,keyword_count:0,value:m(C)}}else{throw H}}}function g(t){var p={keyword_count:0,r:0,value:m(t)};var r=p;for(var q in e){if(!e.hasOwnProperty(q)){continue}var s=d(q,t);s.language=q;if(s.keyword_count+s.r>r.keyword_count+r.r){r=s}if(s.keyword_count+s.r>p.keyword_count+p.r){r=p;p=s}}if(r.language){p.second_best=r}return p}function i(r,q,p){if(q){r=r.replace(/^((<[^>]+>|\t)+)/gm,function(t,w,v,u){return w.replace(/\t/g,q)})}if(p){r=r.replace(/\n/g,"<br>")}return r}function n(t,w,r){var x=h(t,r);var v=a(t);var y,s;if(v){y=d(v,x)}else{return}var q=c(t);if(q.length){s=document.createElement("pre");s.innerHTML=y.value;y.value=k(q,c(s),x)}y.value=i(y.value,w,r);var u=t.className;if(!u.match("(\\s|^)(language-)?"+v+"(\\s|$)")){u=u?(u+" "+v):v}if(/MSIE [678]/.test(navigator.userAgent)&&t.tagName=="CODE"&&t.parentNode.tagName=="PRE"){s=t.parentNode;var p=document.createElement("div");p.innerHTML="<pre><code>"+y.value+"</code></pre>";t=p.firstChild.firstChild;p.firstChild.cN=s.cN;s.parentNode.replaceChild(p.firstChild,s)}else{t.innerHTML=y.value}t.className=u;t.result={language:v,kw:y.keyword_count,re:y.r};if(y.second_best){t.second_best={language:y.second_best.language,kw:y.second_best.keyword_count,re:y.second_best.r}}}function o(){if(o.called){return}o.called=true;var r=document.getElementsByTagName("pre");for(var p=0;p<r.length;p++){var q=b(r[p]);if(q){n(q,hljs.tabReplace)}}}function l(){if(window.addEventListener){window.addEventListener("DOMContentLoaded",o,false);window.addEventListener("load",o,false)}else{if(window.attachEvent){window.attachEvent("onload",o)}else{window.onload=o}}}var e={};this.LANGUAGES=e;this.highlight=d;this.highlightAuto=g;this.fixMarkup=i;this.highlightBlock=n;this.initHighlighting=o;this.initHighlightingOnLoad=l;this.IR="[a-zA-Z][a-zA-Z0-9_]*";this.UIR="[a-zA-Z_][a-zA-Z0-9_]*";this.NR="\\b\\d+(\\.\\d+)?";this.CNR="\\b(0[xX][a-fA-F0-9]+|(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)";this.BNR="\\b(0b[01]+)";this.RSR="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|\\.|-|-=|/|/=|:|;|<|<<|<<=|<=|=|==|===|>|>=|>>|>>=|>>>|>>>=|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~";this.ER="(?![\\s\\S])";this.BE={b:"\\\\.",r:0};this.ASM={cN:"string",b:"'",e:"'",i:"\\n",c:[this.BE],r:0};this.QSM={cN:"string",b:'"',e:'"',i:"\\n",c:[this.BE],r:0};this.CLCM={cN:"comment",b:"//",e:"$"};this.CBLCLM={cN:"comment",b:"/\\*",e:"\\*/"};this.HCM={cN:"comment",b:"#",e:"$"};this.NM={cN:"number",b:this.NR,r:0};this.CNM={cN:"number",b:this.CNR,r:0};this.BNM={cN:"number",b:this.BNR,r:0};this.inherit=function(r,s){var p={};for(var q in r){p[q]=r[q]}if(s){for(var q in s){p[q]=s[q]}}return p}}();hljs.LANGUAGES.cpp=function(){var a={keyword:{"false":1,"int":1,"float":1,"while":1,"private":1,"char":1,"catch":1,"export":1,virtual:1,operator:2,sizeof:2,dynamic_cast:2,typedef:2,const_cast:2,"const":1,struct:1,"for":1,static_cast:2,union:1,namespace:1,unsigned:1,"long":1,"throw":1,"volatile":2,"static":1,"protected":1,bool:1,template:1,mutable:1,"if":1,"public":1,friend:2,"do":1,"return":1,"goto":1,auto:1,"void":2,"enum":1,"else":1,"break":1,"new":1,extern:1,using:1,"true":1,"class":1,asm:1,"case":1,typeid:1,"short":1,reinterpret_cast:2,"default":1,"double":1,register:1,explicit:1,signed:1,typename:1,"try":1,"this":1,"switch":1,"continue":1,wchar_t:1,inline:1,"delete":1,alignof:1,char16_t:1,char32_t:1,constexpr:1,decltype:1,noexcept:1,nullptr:1,static_assert:1,thread_local:1,restrict:1,_Bool:1,complex:1},built_in:{std:1,string:1,cin:1,cout:1,cerr:1,clog:1,stringstream:1,istringstream:1,ostringstream:1,auto_ptr:1,deque:1,list:1,queue:1,stack:1,vector:1,map:1,set:1,bitset:1,multiset:1,multimap:1,unordered_set:1,unordered_map:1,unordered_multiset:1,unordered_multimap:1,array:1,shared_ptr:1}};return{dM:{k:a,i:"</",c:[hljs.CLCM,hljs.CBLCLM,hljs.QSM,{cN:"string",b:"'\\\\?.",e:"'",i:"."},{cN:"number",b:"\\b(\\d+(\\.\\d*)?|\\.\\d+)(u|U|l|L|ul|UL|f|F)"},hljs.CNM,{cN:"preprocessor",b:"#",e:"$"},{cN:"stl_container",b:"\\b(deque|list|queue|stack|vector|map|set|bitset|multiset|multimap|unordered_map|unordered_set|unordered_multiset|unordered_multimap|array)\\s*<",e:">",k:a,r:10,c:["self"]}]}}}();hljs.LANGUAGES.r={dM:{c:[hljs.HCM,{cN:"number",b:"\\b0[xX][0-9a-fA-F]+[Li]?\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\b\\d+(?:[eE][+\\-]?\\d*)?L\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\b\\d+\\.(?!\\d)(?:i\\b)?",e:hljs.IMMEDIATE_RE,r:1},{cN:"number",b:"\\b\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d*)?i?\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\.\\d+(?:[eE][+\\-]?\\d*)?i?\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"keyword",b:"(?:tryCatch|library|setGeneric|setGroupGeneric)\\b",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\.\\.\\.",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\.\\.\\d+(?![\\w.])",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\b(?:function)",e:hljs.IMMEDIATE_RE,r:2},{cN:"keyword",b:"(?:if|in|break|next|repeat|else|for|return|switch|while|try|stop|warning|require|attach|detach|source|setMethod|setClass)\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"literal",b:"(?:NA|NA_integer_|NA_real_|NA_character_|NA_complex_)\\b",e:hljs.IMMEDIATE_RE,r:10},{cN:"literal",b:"(?:NULL|TRUE|FALSE|T|F|Inf|NaN)\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"identifier",b:"[a-zA-Z.][a-zA-Z0-9._]*\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"operator",b:"<\\-(?!\\s*\\d)",e:hljs.IMMEDIATE_RE,r:2},{cN:"operator",b:"\\->|<\\-",e:hljs.IMMEDIATE_RE,r:1},{cN:"operator",b:"%%|~",e:hljs.IMMEDIATE_RE},{cN:"operator",b:">=|<=|==|!=|\\|\\||&&|=|\\+|\\-|\\*|/|\\^|>|<|!|&|\\||\\$|:",e:hljs.IMMEDIATE_RE,r:0},{cN:"operator",b:"%",e:"%",i:"\\n",r:1},{cN:"identifier",b:"`",e:"`",r:0},{cN:"string",b:'"',e:'"',c:[hljs.BE],r:0},{cN:"string",b:"'",e:"'",c:[hljs.BE],r:0},{cN:"paren",b:"[[({\\])}]",e:hljs.IMMEDIATE_RE,r:0}]}};
hljs.initHighlightingOnLoad();
</script>




</head>

<body>
<h1>Reproducible Research: Peer Assessment 1</h1>

<h2>Loading and preprocessing the data</h2>

<pre><code class="r">activity = read.csv(&quot;activity.csv&quot;)
</code></pre>

<h2>What is mean total number of steps taken per day?</h2>

<pre><code class="r">totalstepsPD &lt;- tapply(activity$steps, activity$date, sum)
</code></pre>

<p>1.Make a histogram of the total number of steps taken each day</p>

<pre><code class="r">hist(totalstepsPD, xlab = &quot;Steps per Day&quot;, main = &quot;Total Number of Steps Per Day&quot;)
</code></pre>

<p><img src="data:image/png;base64,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" alt="plot of chunk unnamed-chunk-3"/> </p>

<p>2.Calculate the mean and median total number of steps taken per day
Mean:</p>

<pre><code class="r">mean(totalstepsPD, na.rm = TRUE)
</code></pre>

<pre><code>## [1] 10766
</code></pre>

<p>Median:</p>

<pre><code class="r">median(totalstepsPD, na.rm = TRUE)
</code></pre>

<pre><code>## [1] 10765
</code></pre>

<h2>What is the average daily activity pattern?</h2>

<p>1.Make a time series plot(i.e. type = &ldquo;l&rdquo;) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)</p>

<pre><code class="r">steps_interval &lt;- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(steps_interval, xlab = &quot;5-minute Time Interval&quot;, ylab = &quot;Mean number of steps&quot;, 
    main = &quot;Average Steps at 5-minute Intervals&quot;, type = &quot;l&quot;)
</code></pre>

<p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfgAAAH4CAMAAACR9g9NAAAAmVBMVEX9/v0AAAAAADkAAGUAOTkAOWUAOY8AZo8AZrU5AAA5ADk5AGU5OQA5OWU5OY85ZrU5j485j9plAABlADllAGVlOQBlZjllZmVltbVltf2POQCPOTmPOWWPZgCPj2WPtY+P29qP2/21ZgC1tWW124+1/rW1/tq1/v3ajznatWXa24/a/rXa/tra/v39tWX924/9/rX9/tr9/v0JBnMCAAAAM3RSTlP//////////////////////////////////////////////////////////////////wBxnr2OAAAACXBIWXMAAAsSAAALEgHS3X78AAAUI0lEQVR4nO2djXqkthmFK2/q2t3Wqb2btuOkSTxp6mkTzw/3f3FFAoQACQRIIDjnfXbtmQF9+kbvICQMzB8yAskf1k6ArAPFg0LxoFA8KBQPCsWDQvGgUDwoFA8KxYNC8aBQPCgUDwrFg0LxoFA8KBQPCsWDQvGgUDwoFA8KxYNC8aBQPCgUD0pi4m+v4mFG8euLEPcf+YPjwbtMterlUQjx6d0S0/KipYrWeh4ZuCIvQmLi89af0RjSu5CfnKPwFq9XPQu7+KFydeVmYZ8MKL7mJP4hW+yotnv185TrOMiu4O7v4u5N2rl7Uz3Dp19f5MZdLldcHu8/8tb8z2uhXxf99EuhVG7UVVsXkW7lqrJqGTerFv4pr+D9JMtLPTpE/uA9f+X+92YVEnO9WzsDmfxf9Jsq34USb+a0JGmJzxvu348PsuFzp9eXvHGOcjsUz5lqyvvf1CZ9/6GefZNr1ssVeSMeVBTV7I2istS1LC1XLR//rsWrtcxPRcGn90KoKPqSlnizdnM9Wwb3/63eVJWHLGHmtChpib88PqimzTeRN2VfbsOqteudv2yuy2O+0slcrhZpebKjNYs+S9EH9YqB2uKqPrkwUAY6C1mHOBSayhBn9YkrxH80q6ii6fU6GTxk9Zuq1i7eyQrSJWmJP+XNJf/nP55V25WbnuyTi55Y+vn0fpYNKdtfLy8DHAt5raLSTB5RudXdahFJi5cGckEH2fXmG+dDKbISr3rmtvhG7Y31rMlXb6qqW2/x8F297lFl4/0mt8dW26mRd5/4sru3iy+KF+tWkZqjsHwlQ7z0XIkvfQ+K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" alt="plot of chunk unnamed-chunk-6"/> </p>

<p>2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?</p>

<pre><code class="r">steps_interval[which.max(steps_interval$steps), ]$interval
</code></pre>

<pre><code>## [1] 835
</code></pre>

<h2>Imputing missing values</h2>

<p>Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.</p>

<p>1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)</p>

<pre><code class="r">sum(is.na(activity$steps))
</code></pre>

<pre><code>## [1] 2304
</code></pre>

<p>2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.</p>

<pre><code class="r"># use the mean interval to substitute NAs
fillinNA &lt;- function(x) {
    steps_interval[steps_interval$interval == x, ]$steps
}
</code></pre>

<p>3.Create a new dataset that is equal to the original dataset but with the missing data filled in.</p>

<pre><code class="r"># create new dataset
activity_new &lt;- activity
# fill in the NAs in new dataset
for (i in 1:nrow(activity_new)) {
    if (is.na(activity_new[i, ]$steps)) {
        activity_new[i, ]$steps &lt;- fillinNA(activity_new[i, ]$interval)
    }
}
# check if NAs in new dataset equals 0.
sum(is.na(activity_new))
</code></pre>

<pre><code>## [1] 0
</code></pre>

<p>4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?</p>

<pre><code class="r">totalstepsPD_new &lt;- tapply(activity_new$steps, activity_new$date, sum)
hist(totalstepsPD_new, xlab = &quot;Steps per Day&quot;, main = &quot;Imputed Total Number of Steps Per Day&quot;)
</code></pre>

<p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfgAAAH4CAMAAACR9g9NAAAAllBMVEX9/v0AAAAAADkAAGUAOTkAOWUAOY8AZo8AZrU5AAA5ADk5AGU5OWU5OY85ZrU5j485j9plAABlADllAGVlOQBlOY9lZgBlZjllZmVltbVltf2POQCPOTmPOWWPjzmPtY+P27WP29qP2/21ZgC1Zjm1tWW124+1/tq1/v3ajzna/rXa/tra/v39tWX924/9/rX9/tr9/v1kp8NVAAAAMnRSTlP/////////////////////////////////////////////////////////////////AA1QmO8AAAAJcEhZcwAACxIAAAsSAdLdfvwAAA/nSURBVHic7Z0Le+LGGUYj3Dq4qTewSZoWdpus6cWk5vb//1w1MxIW1mA0WOKV9J3zPAlewIdZHXSBRcN3BzDJd+oBgAbCG4XwRiG8UQhvFMIbhfBGIbxRCG8UwhuF8EYhvFEIbxTCG4XwRiG8UQhvFMIbhfBGIbxRCG8UwhuF8EYhvFEIbxR9+N387vnSfVaL2i/dv7jL/TLzTKP3LdW7ebZw9z3zQGdvOGH7kE2eCm2W+YevDest7p450bu9d9sNGET4VW3xnA9fvW8lfH73D4Y/ikOxaWxYbyniFk+Y6G3Z7PJDd0E/wm+y75fZ3fParQB5hn9n+R9Cj7zw/5ZhOa+L9cPF/kcR/uDWRP/j2i/DfbjvJiztSnh/291z6XypP+LR4jcOk1/LXBVxeEz3gLn5v6fDOg7bbRqy4pkUnp8rF7dmDoPbuLuG4fqxbW71ROhL+BLXxv9w//Im/KpYP463F78fwodbpyF8WJl8niL83d9C9Er4yCOePkZ4hKq4DH98/sWGVT54eOAyfN1cDC7fbJS/sc69q+jWoQP6Ev7u2S3P3dw98fPl4/bKx0h+m+r7uqu2D/nV6zfhtw9+Bc//V25/nbYS/j8Pszfha484eTo+Rn7NtLS/FYd2TnwyrOOwiy1QMYhiUx8xF4NbB6370/Zh2myv0wZ9CT/1l/mGMGzx8uUxOw1frKKTJ3ff4z7+UIT3V/qFGPq4JX4S/nk1+XYa/u0j5r94fAx3bbC/FXtWofzJsI7D9rGPm/piFx8xV8OH4e6X93/MK4epndKn8G7r3kp4t5d9G343/74evvqICeGLzX08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" alt="plot of chunk unnamed-chunk-11"/> </p>

<p>Calculate and report the mean and median total number steps taken per day.
Mean:</p>

<pre><code class="r">mean(totalstepsPD_new, na.rm = TRUE)
</code></pre>

<pre><code>## [1] 10766
</code></pre>

<p>(the same as previous one.)</p>

<p>Median:</p>

<pre><code class="r">median(totalstepsPD_new, na.rm = TRUE)
</code></pre>

<pre><code>## [1] 10766
</code></pre>

<p>(slightly differs from before-imputation.)</p>

<h2>Are there differences in activity patterns between weekdays and weekends?</h2>

<p>For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.</p>

<p>1.Create a new factor variable in the dataset with two levels &ndash; &ldquo;weekday&rdquo; and &ldquo;weekend&rdquo; indicating whether a given date is a weekday or weekend day.</p>

<pre><code class="r">Sys.setlocale(&quot;LC_TIME&quot;, &quot;English&quot;)
</code></pre>

<pre><code>## [1] &quot;English_United States.1252&quot;
</code></pre>

<pre><code class="r">activity_new$date &lt;- as.Date(activity_new$date, &quot;%Y-%m-%d&quot;)
day &lt;- weekdays(activity_new$date)
activity_new$day_type &lt;- ifelse(day == &quot;Saturday&quot; | day == &quot;Sunday&quot;, &quot;Weekend&quot;, 
    &quot;Weekday&quot;)
mean_new &lt;- aggregate(activity_new$steps, by = list(activity_new$interval, activity_new$day_type), 
    mean)
names(mean_new) &lt;- c(&quot;interval&quot;, &quot;day_type&quot;, &quot;steps&quot;)
</code></pre>

<p>2.Make a panel plot containing a time series plot (i.e. type = &ldquo;l&rdquo;) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).</p>

<pre><code class="r"># make panel plot
library(&quot;lattice&quot;)
panel &lt;- xyplot(steps ~ interval | day_type, data = mean_new, layout = c(1, 
    2), xlab = &quot;Interval&quot;, ylab = &quot;Number of steps&quot;, type = &quot;l&quot;)

# show and save plot
show(panel)
</code></pre>

<p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfgAAAH4CAMAAACR9g9NAAAA5FBMVEUAAAAAAC4AADoAAFIAAGYAM1IAM3MAOmYAOpAAXJEAZrYAgP86AAA6AC46ADo6AFI6AGY6MwA6M1I6M3M6OmY6OpA6XJE6gHM6gK86kNtmAABmAC5mADpmAFJmAGZmMwBmMy5mM3NmOgBmOjpmXFJmo8xmtv+QMwCQMy6QM1KQOgCQOjqQOmaQXACQZgCQkLaQo3OQtpCQxZGQxcyQ2/+2XAC2XC62ZgC25cy2/7a2///bgC7bkDrb5a/b5czb/7bb/9vb////o1L/tmb/xXP/25D/5ZH/5a//5cz//7b//9v///8lZfTBAAAACXBIWXMAAAsSAAALEgHS3X78AAAY50lEQVR4nO2dCWPcxnmG147jynzVOolrR1Scw3FEyVJbp6VkqhKjVCJFUsT//z/FjTmxOGYwAL73sQVgrm9m91kAs+Au9pARkRxSD4CkgeKFQvFCoXihULxQKF4oFC8UihcKxQuF4oVC8UKheKFQvFAoXigULxSKFwrFC4XihULxQqF4oVC8UCheKBQvFIoXCsULheKFQvFCoXihhBcfMGK4UBxUxM7DR9zJcxw/EsXHj7SXQVF8ulAUHz8UBxWx8/ARd/Icx4+0qPgDWRXLib8jK4LihULxQqF4N6+e3N19OBSLr/SC22fnzga3T58sMKxwULybt4/yf397VG2oUPy+xV9/8+722d+LxfndzePDr17f1as84/bpl++qxPU3/3koDgt56td/pvg9iL/54+ubH/7x0+t8fffq0d3b/IBfrXLx+UaduH746O7Dl++KVHle2BAU7yb3e/1dfqbPFzd/OC9fB9Xq9tlvc+914vrrfK//unx18FC/D/F3b5+8fXL34VF+is+P44fDZ+f16vbp73941+TV4otVOR3cEBTv4cNXv5zfXX/3S7lnFxn1Kj8UFC+GKsE9fn/ib/7yzbvc8u9el+fz5kT+ZTHbKyZ8VaIWz3P8jsTfPi3ewL8qFvlx/bPzZlVYL3byMtGIv33KWf1exO+dBcWTVbGc+MktSQQoXigULxSKFwrFC4XihULxQqF4oVC8UCheKBQvFIoXCsULheKFQvFCoXihULxQKF4oFC8UihcKxQuF4oVC8UKheKFQvFAoXigULxSKFwrFC4XihULxQqF4oag67p8fDg/eZxeHz19m9bK3/vSeSHJUHVcnue+zqwfv2//76w/uY9ItGEhUTCmXZ5en2ac/vamWjvrTe5rckkTA0JHv9Jdn2f1/vKmWR+tP74mkRddxkR/suceLQJ/cneXL8Of4uS1JBFQdF8XZ/pSzehHwfbxQKF4oFC8UihcKxQuF4oVC8UKheKFQvFAoXigULxSKFwrFC4XihULxQqF4oVC8UCheKBQvFIpvQOoBLAvFNyD1AJaF4huQegDLIl08HFsiEC8e7Va6QaSA4tutdINIAcW3W+kGkQKKb7fSDSIFFN9upRtECii+3Uo3iBRQfLuVbhAp0HWU90K4OBTfkBbyNWmKL7g6fPEmu39RCJdy8yO54hUp9z8Xt7359OPDw0km5VYocsVrqUL8Vb7XX55JufkRxZc0rq9Ohezx6HzDX2uPOPb40+Jud0JufpRE/HI99eDY4y+KOyAJmdWnEN+dXVIi/H38VPGjKptt5zQOBsV3m6PazehzTuNgUHy3OardjD7nNA6GbPGg+Fj1Q7SMB8VHqx+iZTwU8eN0jKpsd7oCxItvHGKMzFnuKD49KcSP6igeFI96k+KD1g/RMh7IQoofFoDiVwAyio9UP0TLeCCj+Ej1Q7SMB7Kg4vsioNsY0VE8KL72QPFh64doGQ+0i5HinZX7xTeFWMcf5Cl+YfHIKD49aBfxxbf9UHx60C4CiEevUYpfE2gXC4jvehvRUzQovhWCMe0cldFnFBnFrwh0ywXEt9JH9BQNiqf4KPVDtIwHuuUs8WgW3hhtT3qtEX0GhuKXFY8so/j0oFsuIr5+qw8zOwEUP0E8TGNoFt4YdYEhHlakxaD4rFGGMc1gZgwTbxzqKT4R6FYY8XkqjBbfxm5rttmjzjHhoPhsvnjlAO6JoceGmk3xCUC3woiDrll3qnhUkQb3GxLnzY+KL0iL+Jo0uhWSiR9+pAmJffOj6pYIMm5+hG6FueKhRrRa6PXbNdKJt25+VN0ERcatUNCtsKx4tOs17PHlHTGq2x7JuPkR2jWyCeLb5kprdxBDPDrxYyaVIXGIl7PHo9uAlj7erqpfO5soHpn9Tn7oEGbjuPlR8HP83JbRQLcBLZ3ZKaOkqV85RzZKfLZK8YJm9eg2oKUzO2WUdPXL3ReBxC932Bf9Ph7dBrR0ZqeMErU+5ojPKH550G1AS2d2yihR62vi3c1M8e2Ojozil0Z9H1VtQi/3t6T4RVpGYrZ4aEkl5erL7rrtEt56jo5DQfH1drnUy/0te8W72lnim7OL0RHFL8Bk8cjGireuznnE44ja/tJRUHy9XS6Ncl/DrE+8y45LvNK5mu3p0xt6KhRfb5dLo9zTrlmgy8AR8XbmNPH9xWOQLF59xlHlaOV6UmlXL9FlwH3KdnVlR0dfvUFDmgDFN4lsoHi0K3Q50F5F/V1Z0eGtZ7ai+BAcEe/b+9Cu1Objxbs2jXqWZ4oPwTTx6NZq817xCCjeOagJUHyTyNKL74npDzQR0eJ7c9zPMZQNaPmw67QZ/b6cMcu93WxF8QHAkazlxKtHkTpQfZA3W1F8AHAky/kcQ92CVuAtmiAeRsHQQCOgeF/WKsWHm9dTvC8LrmO2tziEeLSBoBeovfQGGgHF+7IwS7weapx4UHxM0J9VPMNWFWibWnW9jOJDtIwD+rMofl79EC3jgP6sWeIN0a5QjrCoA/WJ56x+NujPgqsKtE29ulo2SXy9hNoaVsXeQCOgeE8WXFWgberV1TJYoQYMpV5CPZ7Dqng02EAo3pMFJd2KgFauV1fLoBU7enL1Wy8pPjLoz0LzTz30QivXq1spKFWHDKVeUnxk0J+FTJlEw66A4+IdrfqGUi97xDsnnBOheE8WMsUm7AowqhvhEEt8KPMU78lCNlO81fjIUJrafeKHhRuEWPE4kodstnhtTz46FqWPxcVfHIrvRnu+IO2oP72n9KA/D9UCTQpmGxghjHjTxWe2eHjqzUHVcf+iEO67JYJdf3Afa7z5EXrz6q3uNA3n/M0fb5x4ZNarSt1ohgFtTPNQpXz68eHhJPPdBKWuP72nyS2jgN68emu6eGUG7urJrru4eGX76ovi1ke+2x7Z9af3tALQl9dszBQ/9NjcvUicA4EWKIL4gqtT7vErFN/+GxRvCNoef5rle3z4c/zcljFAX2Zb2Imv/1Pr6iHMgK14Z092XXc1NAsl0oCAAzBn9afe2x456k/vKTnoy20L54hvlu6e9LrealAX5uhmIfR9PPqyu0Jom9rZNaR4O5oWBXqVQQGPQvF2dle4nHhfLXShtbn9bCjeylbKZov37spmbV8tZBQfEPTlK4V94o0YZkg0K19feu2eEUHZdnY1CYo389Uy1Udc8T0FVtmgiMcQKR59BWphn/gjMdG2Gj4u94isAPMi1lC8VnDszO0v8aW9B/FhYIXiLx+8vzwczmL1FBF4C4bvoGY9X7v9if/0/cv8/4+/8V7dndlTJHrdYromX8Mdiv/Tm3yfp/ijHc5tbQUoMjB3qHMO9YfPX15t61CPI7OtjYivhjkvsKDJHWqx6KkSQfw8el6n3oJB7Fs81G0cFzv3zVcE/CP2Fgxihvj754fD4SRaTyGAugmKV5gu/v75ab68HGw+rXhkw8SvjvWJrz6m0/NhnZk9hQDaBjYpfkLJEObM6k+yrezx1frIzC5bpfgeRlxuspmxxz+uP6H7xbB9PqH4ZgWKb5Ewq0eTwv7EY2rjXYuvnxYoSbhrbpJU4vO3cw/++b3/c5kzewpA9dbdmOLth0Ti87dzH7/t+yT2zJ4CUD4zynMDb81Ngjln+Vlv53Lxq3w7h2Y966lZPe5HB8+2wdw9/nKFe3z7bIT4Y8aKgXPWoh79zTKFuZdsB3tfVLwxnd8pHvH69Uofe5zVI2sv1e0ct3hlNmsWdmz/ki3cGRDgvQSOJJpto1Bhqvj2ul3yczzMx16nVven9UjAlWyWRqHC3D1+SksbjBxF06yc1WoPD+YF+v0DVwrVQi9TWcc5Hp7t3srNmzX1Osae3735QLeG/rIfLL73C9JW/UAfr0Y1QDSfJIPrahvU+m09ZV5btq4u2Awd0F6ATpN7ZC/QbozQe0sEs/7wj1cfuflRoyzrHoP5YIxH1j4+/ZqlPOcO0G50+4cLVUr/TVDq+u3WRj9eTSpUHf23PbLqb/Dj1aRFEz9qj5/TE0nO9HP8nJ5IcmbM6uf0RFIz+crdyMs3FL8ypov/34BX7sjiTL5ydzHuM7YUvzLWca2eLM46rtWTxVnwEzhkVUwWX39pcvjf4+/IiljwEzipHypR4R4vlAXP8akfKlFZcFaf+qGO49WTu7sPh2LxlV5w++y82Xr6ZPFhBYPiPbx9lP/726NqQ4Xi9y3++pt3t8/+XizO724eH371+q5e5Rm3T798lyd+/ecnd9cPD/lhIT885PVTD3kUFO/h5o+vb374x0+v8/Xdq0d3b/MDfrXKxecbRSI/E9z84fzu+uvXH/LEo+Mx18SCl2xTP9Rx5H6vv8t35XxR2C1eB9Xq9tlvH5Wvi+ZQX75E3v1yfiziupjxdu7F4K/Gb1H83dsnb5/k+3F+ir8pvjzy2Xm9un36+x/eFbt5NQF8dciP/7fP/ueHbR3p5+zx1Xdphv91LvVDHcmHr/Kd+Pq7X8rdvMioV/mhoHgxVHv8zeMn5WugnAZuCp7jfdz8JZ+u3T773evqfP7lu3pVTO7yCV91ji+kX//reflvW1C8j9unxRv4V8UiP8h/dt6sCuu579un5az+7aFc3f70OvV4R7LgPXBSP9SY5FPAjbHgPXDIqpgsfsX3wCHH2eM9cMgA9ngPHDIAfuZOKBQvlLmH+nX/QgXxsuBHr0b2RKKy/dudkUnMONRfrP83aYiX2fe543fntgln9UKZIf5q+3s8Ug8gHTMmd8P/MDeppyVA6gGkQ/bXpJF6AOmYcai/PI3a0xIg9QDSMUc8z/EbRvg5HqlHkAzh53ikHkEyhJ/jkXoEyVjwc/Uje1oCio9WP0TLeFB8tPohWsaD4ifU56F+08zd44fP8Ch+VcwVv+0PYlD85PpXPNRvk9nn+E3/NAnFR6sfomU8KD5a/RAt40Hxo+vv4zN3FD+1/iXP8RtllvhPj7X9vfhqzYP39U8aWT9sRPGrYo74y4N+9ebqJPd9Vv2Imf1TZqsUL/ejGHPezrlO75dn1c8Wqj9eOO0WDPGBaPEjpbQ1rw6ui7X5Tl/9UKn9c6UUvyrCzuovTpofKrV/rpTiV0XI9/H3z4sp/nbO8RQfqH75U3Sn25nVU3zE+iFaxoLiI9YP0TIWFB+xfoiWkSiti72EQ/Gph5EIik89jERQfOphJILiUw8jEdLFi53WixbfLeVB8UnHkA6KTzqGdFB80jGkg+KTjiEd4sVLNU/xQs1TvFDzFJ/JNE/xxqYUKN7algHFOxISoHhHQgIU70hIgOIdCQlQvCMhAYp3JCRA8Y6EBCjekZAAxTsSEqB4R0ICFO9ISCCs+PJeCBeH4hvS6/+aNLwJCQQVf1XcH+P+RSF8AzdGgDchgaB3xPi5uO3Npx8fHk6yDdz8CN6EBCbf/MhJIb64n/Xl2QZufgRvQgJhz/GN66vTDdz8CN6EBMKLvzot7na3tXO8OPMR9viL4g5IW5vVU3zg+iFaRgI9qf1D8c7U/qF4Z2r/ULwztX8o3pnaPxTvTO0finem9g/FO1P7h+Kdqf1D8c7U/qF4Z2r/ULwztX8o3pnaPxTvTO0finem9g/FO1P7h+Kdqf1D8c7U/qF4Z2r/ULwztX8o3plKCxbog+KdqbRggT4o3plKCxbog+KdqbRggT4o3plKCxbog+I9yZRggT7kiseRdEKW+BVEivelE0LxMcGRdEIoPiYw02ZGMrDEizDKzY+KL0iv/mvSMNNmRjK2J768+VF1S4T13xgBZtrMSMbmxFc3P6pugrL+mx/BTJsZyVhGfPCbH1W3PVr/zY9gps2MZGxuj6/FW3v8tJ5iAzO9GvMbFb/Zc3zUp3tM7I2K3+ysPq74EcE3KD5OyzjASps5QXsbEZziowIrbeYE7W1EcIqPCqy0mRO0txHBscQ7DIrv0mZO0N5GBEdZP7J8iu/SZk7Q3kYEb6pGVU/xfTkBexseHI6t8FB8X07A3oYHh2MrPOLEw9qwi4YHGd5iSvDx3QxHrHiHCTvnWJAR3fY1gbYJV35wpIp3TZrtnCNBRnXb0wbaJpwFoaF4q2h4kFHd9rSBtglnQWiEindeI7FzvEGGV21j9zSBtglnQWj2LR6+rBWLh7sgNDLFo6dsUNzBNbvQPU2gbsJdEBp54pFRfLZz8c7DOZS93iqcHvhYC39wUHxotiheCz26m+GIEo/qWYajalM+LfDxBj3BtYkmxYdg8+Ijmt+zeEsxPO/jlPKBkQfXVCL72hjirXZx2Lt4qOnqKYa7dpZIPCg+NB7xfQ2GRh5cU4nsaUPxwYE5VQolvnei4I/sbqKPkuIDAON5xLEPsvUWavUofomWU0HWXaer0sHEjz3Ywx9efxXBLIzFzsVX/9Ckj+yovYV6vSni3fEpPjho/0FL9zYYGJjiF2g5GigrtFdPoBT1tuupgObcoZ1DRo3IbKSJtwtjsU/xqFb1NsKJzxr5IcVnFB8G9dDezOcQTHy9hSwbcbxHtzLbaHmuwjhEEH9xKL4hnfBr0lCfzWYiP+QN2LEaSvk08e3rzyiCWW/oiKYTXvz9i0J4yhsjwCV+iKVjNZTyyeKtNsh2Iv7Tjw8PJ1nKmx8hU3atIXu62rCvVCmvz/FDY6Ndr0Z80JsfFVx9UdwAKeXNj5AFEG+3CiHecQkJWRrxUepfnaa8+RGqBdTk8IbeVrDK3eJ783Ys/uo0y/f4xOf4cXu61tCxXefoWfD0AUcu1E3oJVArmC0RzX2cWf1p0psfoVpiYkN7O3MFhLqvQsu22qInATXP0RJx3O/zfXy5xMSG9rYzxy0ezuMAehJQ8xwtQfFDgbYa1RDKtjuqUdnoC5nl0mqrJ2C1t3uxcgOwY/FTGkLZPhJV84usS9ni9YOPnoCW5+gVFD8UTG/YKXLOvo3KXTaU2XlzkRhKVW8gig8HZrRsbGGseEVQ+9cBdFW9gRrxcNVsBmLlBoDitZbjxTd7eSca7dm/DeYP1O7xcPVB8WPA9JatTNhXWsywXvHKtK+IYXmDtg11bfZB8WPA9Ja6eD2QGXaI+GobZkPnNjKKnwemt2x3PMfzbYa1xLcvFK2x7Q1m4y7f7IPix4A5TdGsBotvjunqAUJt7JwsNNWhF5hVq4pWhBBQvN4UWb3rwQxkhh0q3m6I5uRP8UHBnKaoAgBWIDPsYPFWJ1D6MUsctb3i/V0MgOKtxmiCwCwx0uhK1FO8XujoQnnJGLUcjUrvcFbydzEAircaowkC7Tm2wnaF1cEbWlnfKLppYF+tNhLFDwPzGqMNohxl4XDUZTRnbDuMp5O2sK9WW8OIPqJxDxRvNoYapJ5bOc/ZXY5jx+3dl8fM1mCddtrNWZM+ijcawxHEOs16GvVnqaUjBmSLh7kxBYo3GsMZ5MhzbJeGfAsGvQuK94AQjY0gFC9FvJndH9QuDSne6EMTP7kbih8Wsz+oXRpevHIwqoPjyJuHXih+WMyxQeOIR71udvYZV3P3Jx6rCBpBfLN/o+4AtvgRnVJ8pKDjWwwICTUyMuP6wqgDP8VvB5h/CzJcU/xOOSLedVnZD8VvCPtPtKpsZGMePMVviP4/GSCTLD7GbHo1oDcX3houti9eO83F+ZjSuqkfNLKsFa9meYgmfvDXpJF11yaULLuSp7EiG3OuaGya5klAOw/AkdleLPEjboyg/MEbqD9ppE1Z0D2cTBGt1B3wCheC8sS5rvB0xBI/6uZHaF6qVQLNjtxQP5wm1bw+qrw2iNS9XQOOLSfhb35UkfLmR2QAS+zx03oiUVnBOZ6kIP2sniRh++/jySQoXigULxSKFwrFC4XihbKgeLIqFhO/QMRwoTioiJ2Hj7iT5zh+JIqPH2kvg6L4dKEoPn4oDipi5+Ej7uQ5jh9pFeLJJqB4oVC8UCheKBQvFIoXCsULJbR46zOZ04J0v1g/J2D5gXA1zPRgdajZ47p/fjg8eB9oUHMILN7+FPYE1F+snxPw6vDFGy3M9GBlqBDjujrJTZ+FGdQsAou3v3cxAfUX62cEvP+5+O6PGmZysCpUoHEVP8oeYlDzCC3e+qbVBNRfrJ8VsBSvhJkRrGgXaFz5Th9oUHNY4x5f0Pxi/ayAwfb4rHUzf1wXJ1moQc1hjed49RfrZwUsd9Mwp9MyVIBx3T8/y7JQg5rDWmf1pyHmvOVuGmYCXYeaPa6L4uNxp/ub1ZOtQPFCoXihULxQKF4oFC8UihcKxQuF4oVC8UKheKFQvFBkif/4mzfWlr/OrqF4f51dI078x2//63A4+/T48MWbcpF9/Pe/Fh9/vH/x8uPDvITi90gh/uFp8TmIQu/FaXZ5kn18eFZ8ivLjt//8/mVZgeL3R+21Wn3KRX/6U5lxeVr8n2VNWgCSxT8+FJ+TLzO+/b8X5Wdh8mM/xe8QXXz1Ecci4/7Ff3/7/tPjMx7qd4omvjjH12f77PJwWr4CPv7bS4rfIZ34++flrP7zl9UsPhde2D/8y1/PKJ7sGYoXCsULheKFQvFCoXihULxQKF4oFC8UihcKxQuF4oXy/+n1GbWMCj+9AAAAAElFTkSuQmCC" alt="plot of chunk unnamed-chunk-15"/> </p>

<pre><code class="r">png(filename = &quot;panel_plot.png&quot;, width = 480, height = 480)
plot(panel)
dev.off()
</code></pre>

<pre><code>## pdf 
##   2
</code></pre>

</body>

</html>
