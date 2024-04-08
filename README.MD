<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="" xml:lang=""><head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <meta charset="utf-8">
  <meta name="generator" content="pandoc">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes">
  <title>README</title>
  <style type="text/css">
      code{white-space: pre-wrap;}
      span.smallcaps{font-variant: small-caps;}
      span.underline{text-decoration: underline;}
      div.column{display: inline-block; vertical-align: top; width: 50%;}
  </style>
  <style type="text/css">
a.sourceLine { display: inline-block; line-height: 1.25; }
a.sourceLine { pointer-events: none; color: inherit; text-decoration: inherit; }
a.sourceLine:empty { height: 1.2em; }
.sourceCode { overflow: visible; }
code.sourceCode { white-space: pre; position: relative; }
div.sourceCode { margin: 1em 0; }
pre.sourceCode { margin: 0; }
@media screen {
div.sourceCode { overflow: auto; }
}
@media print {
code.sourceCode { white-space: pre-wrap; }
a.sourceLine { text-indent: -1em; padding-left: 1em; }
}
pre.numberSource a.sourceLine
  { position: relative; left: -4em; }
pre.numberSource a.sourceLine::before
  { content: attr(title);
    position: relative; left: -1em; text-align: right; vertical-align: baseline;
    border: none; pointer-events: all; display: inline-block;
    -webkit-touch-callout: none; -webkit-user-select: none;
    -khtml-user-select: none; -moz-user-select: none;
    -ms-user-select: none; user-select: none;
    padding: 0 4px; width: 4em;
    color: #aaaaaa;
  }
pre.numberSource { margin-left: 3em; border-left: 1px solid #aaaaaa;  padding-left: 4px; }
div.sourceCode
  {  }
@media screen {
a.sourceLine::before { text-decoration: underline; }
}
code span.al { color: #ff0000; font-weight: bold; } /* Alert */
code span.an { color: #60a0b0; font-weight: bold; font-style: italic; } /* Annotation */
code span.at { color: #7d9029; } /* Attribute */
code span.bn { color: #40a070; } /* BaseN */
code span.bu { } /* BuiltIn */
code span.cf { color: #007020; font-weight: bold; } /* ControlFlow */
code span.ch { color: #4070a0; } /* Char */
code span.cn { color: #880000; } /* Constant */
code span.co { color: #60a0b0; font-style: italic; } /* Comment */
code span.cv { color: #60a0b0; font-weight: bold; font-style: italic; } /* CommentVar */
code span.do { color: #ba2121; font-style: italic; } /* Documentation */
code span.dt { color: #902000; } /* DataType */
code span.dv { color: #40a070; } /* DecVal */
code span.er { color: #ff0000; font-weight: bold; } /* Error */
code span.ex { } /* Extension */
code span.fl { color: #40a070; } /* Float */
code span.fu { color: #06287e; } /* Function */
code span.im { } /* Import */
code span.in { color: #60a0b0; font-weight: bold; font-style: italic; } /* Information */
code span.kw { color: #007020; font-weight: bold; } /* Keyword */
code span.op { color: #666666; } /* Operator */
code span.ot { color: #007020; } /* Other */
code span.pp { color: #bc7a00; } /* Preprocessor */
code span.sc { color: #4070a0; } /* SpecialChar */
code span.ss { color: #bb6688; } /* SpecialString */
code span.st { color: #4070a0; } /* String */
code span.va { color: #19177c; } /* Variable */
code span.vs { color: #4070a0; } /* VerbatimString */
code span.wa { color: #60a0b0; font-weight: bold; font-style: italic; } /* Warning */
  </style>
  <link rel="stylesheet" href="README_files/CRAN_web.css">
</head>
<body>
<!-- README.md is generated from README.Rmd. Please edit that file -->
<h1 id="bigalgebra">bigalgebra <img src="README_files/logo.png" align="right" width="200"></h1>
<h1 id="arithmetic-routines-for-native-r-matrices-and-big.matrix-objects">Arithmetic routines for native R matrices and big.matrix objects</h1>
<h2 id="frédéric-bertrand-michael-j.-kane-bryan-lewis-john-w.-emerson">Frédéric Bertrand, Michael J. Kane, Bryan Lewis, John W. Emerson</h2>
<!-- badges: start -->
<p><a href="https://lifecycle.r-lib.org/articles/stages.html"><img src="README_files/lifecycle-stable-green.svg" alt="Lifecycle: stable"></a> <a href="https://www.repostatus.org/#active"><img src="README_files/active.svg" alt="Project Status: Active – The project has reached a stable, usable state and is being actively developed."></a> <a href="https://github.com/fbertran/bigalgebra/actions"><img src="README_files/badge.svg" alt="R-CMD-check"></a> <a href="https://app.codecov.io/gh/fbertran/bigalgebra?branch=master"><img src="README_files/badge_002.svg" alt="Codecov test coverage"></a> <a href="https://cran.r-project.org/package=bigalgebra"><img src="README_files/bigalgebra.svg" alt="CRAN status"></a> <a href="https://cran.r-project.org/package=bigalgebra"><img src="README_files/bigalgebra_003.svg" alt="CRAN RStudio mirror downloads"></a> <a href="https://github.com/fbertran/bigalgebra"><img src="README_files/bigalgebra_002.svg" alt="GitHub Repo stars"></a> <a href="https://zenodo.org/badge/latestdoi/353292865"><img src="README_files/zenodo.4701056.svg" alt="DOI"></a></p>
<!-- badges: end -->
<p>This package provides arithmetic functions for native <code>R</code> matrices and <code>bigmemory::big.matrix</code>
 objects as well as functions for QR factorization, Cholesky 
factorization, General eigenvalue, and Singular value decomposition 
(SVD). A method matrix multiplication and an arithmetic method -for 
matrix addition, matrix difference- allows for mixed type operation -a 
matrix class object and a big.matrix class object- and pure type 
operation for two big.matrix class objects.</p>
<p>The package defines a number of global options that begin with <code>bigalgebra</code>.</p>
<p>They include:</p>
<p>Option Default value * <code>bigalgebra.temp_pattern</code> with default <code>matrix_</code> * <code>bigalgebra.tempdir</code> with default <code>tempdir</code> * <code>bigalgebra.mixed_arithmetic_returns_R_matrix</code> with default <code>TRUE</code> * <code>bigalgebra.DEBUG</code> with default <code>FALSE</code></p>
<p>The <code>bigalgebra.tempdir</code> option must be a function that 
returns a temporary directory path used to big matrix results of BLAS 
and LAPACK operations. The deault value is simply the default R <code>tempdir</code> function.</p>
<p>The <code>bigalgebra.temp_pattern</code> is a name prefix for file names of generated big matrix objects output as a result of BLAS and LAPACK operations.</p>
<p>The <code>bigalgebra.mixed_arithmetic_returns_R_matrix</code> option 
determines whether arithmetic operations involving an R matrix or vector
 and a big.matrix matrix or vector return a big matrix (when the option 
is <code>FALSE</code>), or return a normal R matrix (<code>TRUE</code>).</p>
<p>The package is built, by default, with <code>R</code>’s native BLAS 
libraries, which use 32-bit signed integer indexing. The default build 
is limited to vectors of at most 2^31 - 1 entries and matrices with at 
most 2^31 - 1 rows and 2^31 - 1 columns (note that standard R matrices 
are limtied to 2^31 - 1 total entries).</p>
<p>The package includes a reference BLAS implementation that supports 
64-bit integer indexing, relaxing the limitation on vector lengths and 
matrix row and column limits. Installation of this package with the 
64-bit reference BLAS implementation may be performed from the 
command-line install:</p>
<p><code>REFBLAS=1 R CMD INSTALL bigalgebra</code></p>
<p>where <code>bigalgebra</code> is the source package (for example, <code>bigalgebra_0.9.0.tar.gz</code>).</p>
<p>The package may also be build with user-supplied external BLAS and 
LAPACK libraries, in either 32- or 64-bit varieties. This is an advanced
 topic that requires additional Makevars modification, and may include 
adjustment of the low-level calling syntax depending on the library 
used.</p>
<p>Feel free to contact us for help installing and running the package.</p>
<p>This website and these examples were created by F. Bertrand.</p>
<p>Maintainer: Frédéric Bertrand <a href="mailto:frederic.bertrand@utt.fr" class="email">frederic.bertrand@utt.fr</a>.</p>
<h2 id="installation">Installation</h2>
<p>You can install the released version of bigalgebra from <a href="https://cran.r-project.org/">CRAN</a> with:</p>
<div class="sourceCode" id="cb1"><pre class="sourceCode r"><code class="sourceCode r"><a class="sourceLine" id="cb1-1" title="1"><span class="kw">install.packages</span>(<span class="st">"bigalgebra"</span>)</a></code></pre></div>
<p>You can install the development version of bigalgebra from <a href="https://github.com/">github</a> with:</p>
<div class="sourceCode" id="cb2"><pre class="sourceCode r"><code class="sourceCode r"><a class="sourceLine" id="cb2-1" title="1">devtools<span class="op">::</span><span class="kw">install_github</span>(<span class="st">"fbertran/bigalgebra"</span>)</a></code></pre></div>
<h2 id="examples">Examples</h2>
<div class="sourceCode" id="cb3"><pre class="sourceCode r"><code class="sourceCode r"><a class="sourceLine" id="cb3-1" title="1"><span class="kw">library</span>(<span class="st">"bigmemory"</span>)</a>
<a class="sourceLine" id="cb3-2" title="2">A &lt;-<span class="st"> </span>bigmemory<span class="op">::</span><span class="kw">big.matrix</span>(<span class="dv">5</span>,<span class="dv">4</span>,<span class="dt">init =</span> <span class="dv">1</span>)</a>
<a class="sourceLine" id="cb3-3" title="3">B &lt;-<span class="st"> </span>bigmemory<span class="op">::</span><span class="kw">big.matrix</span>(<span class="dv">4</span>,<span class="dv">4</span>,<span class="dt">init =</span> <span class="dv">2</span>)</a>
<a class="sourceLine" id="cb3-4" title="4"></a>
<a class="sourceLine" id="cb3-5" title="5">C &lt;-<span class="st"> </span>A <span class="op">%*%</span><span class="st"> </span>B       <span class="co"># Returns a new big.matrix object</span></a>
<a class="sourceLine" id="cb3-6" title="6">D &lt;-<span class="st"> </span>A[] <span class="op">%*%</span><span class="st"> </span>B[]   <span class="co"># Compute the same thing in R</span></a>
<a class="sourceLine" id="cb3-7" title="7"></a>
<a class="sourceLine" id="cb3-8" title="8"><span class="kw">print</span>(C <span class="op">-</span><span class="st"> </span>D)       <span class="co"># Compare the results (subtraction of an R matrix from a</span></a>
<a class="sourceLine" id="cb3-9" title="9"><span class="co">#&gt;      [,1] [,2] [,3] [,4]</span></a>
<a class="sourceLine" id="cb3-10" title="10"><span class="co">#&gt; [1,]    0    0    0    0</span></a>
<a class="sourceLine" id="cb3-11" title="11"><span class="co">#&gt; [2,]    0    0    0    0</span></a>
<a class="sourceLine" id="cb3-12" title="12"><span class="co">#&gt; [3,]    0    0    0    0</span></a>
<a class="sourceLine" id="cb3-13" title="13"><span class="co">#&gt; [4,]    0    0    0    0</span></a>
<a class="sourceLine" id="cb3-14" title="14"><span class="co">#&gt; [5,]    0    0    0    0</span></a>
<a class="sourceLine" id="cb3-15" title="15">                   <span class="co"># big.matrix)</span></a>
<a class="sourceLine" id="cb3-16" title="16"></a>
<a class="sourceLine" id="cb3-17" title="17"><span class="co"># The next example illustrates mixing R and big.matrix objects. It returns by</span></a>
<a class="sourceLine" id="cb3-18" title="18"><span class="co"># default (see # options("bigalgebra.mixed_arithmetic_returns_R_matrix")</span></a>
<a class="sourceLine" id="cb3-19" title="19">D &lt;-<span class="st"> </span><span class="kw">matrix</span>(<span class="kw">rnorm</span>(<span class="dv">16</span>),<span class="dv">4</span>)</a>
<a class="sourceLine" id="cb3-20" title="20">E &lt;-<span class="st"> </span>A <span class="op">%*%</span><span class="st"> </span>D</a></code></pre></div>


</body></html>