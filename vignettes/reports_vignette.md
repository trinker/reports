<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{A Markdown Vignette with knitr}
-->



# reports Package Vignette
## Tyler Rinker

The reports package (<a href="http://github.com/trinker/reports">Rinker, 2013</a> ) is designed to bring together the best R has to offer in report writing in an intuitive fashion. The user houses everything related to the writing process in one accessible location.  The reports package assists in writing reports and presentations by providing a frame work that brings together existing R, <span class="latex">L<sup>a</sup>T<sub>e</sub>X</span>/.docx and Pandoc tools.  The package is designed to be used with [RStudio](http://www.rstudio.com/), [MikTex](http://miktex.org/)/[Tex Live](http://www.tug.org/texlive/)/[LibreOffice](http://www.libreoffice.org/), [knitr](http://yihui.name/knitr/), [slidify](http://ramnathv.github.com/slidify/), [knitcitations](http://www.carlboettiger.info/2012/05/30/knitcitations.html), [Pandoc](http://johnmacfarlane.net/pandoc/) and [pander](https://github.com/rapporter/pander).  The user will want to download these free programs/packages to maximize the effectiveness of the reports package.

The reports package assumes the user has some familairity with the report writing programs/packages it depends on.  For a complete list of reports dependencies <a href="http://trinker.github.io/reports/dependencies" target="_blank">click here</a>
.  This manual will not help the user to understand these dependencies as they have their own help documentation.  Many functions in the reports package are set with defaults that assume the user is utilizing <a href="http://www.rstudio.com/" target="_blank">RStudio</a>
.  This vignette assumes that the user is also operating out of the root directory of the new_report/presentation in RStudio.

<div style="width:367.5px;margin:auto;">
    <p><img src="https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG" width="350" height="250"></p>
</div>


<hr>
<h3 id="toc">Select from sections below:</h3>

<div style="float: left; width: 50%;">
<ul>
<div>1.  <a href="#rprofile">Setting Up .Rprofile </a>
    </div> 
<div>2.  <a href="#report">Creating a New Report/Presentation </a>
  </div>  
<div>3.  <a href="#rep">What's In the new_report/presentation Directory </a>
    </div> 
<div>4.  <a href="#code chunks">Using reports Functions In-Text</a>
 </div>    
<div>5.  <a href="#dir">Directory Management </a>
   </div>  
<div>6.  <a href="#notes">Using notes.xlsx </a>
  </div>   
<div>7.  <a href="#cite">Citations </a>
    </div> 
<div>8.  <a href="#convert">Document Conversion </a>
</div>     
<div>9.  <a href="#html">HTML Short Cuts</a>
</div>    
<div>10. <a href="#temp">Custom Reports Templates</a>
</div>   

</ul>
</div>
<div style="float: right; width: 50%;">
<ul>
<div><b>Symbol Conventions:</b></div>  
<div><font size="5" color="gold">&diams;</font> = Example (R code)    </div> 
<div><b><font size="5" color="firebrick">[YT]</font></b> = Video Demo (click to watch)    </div> 
</ul>
</div>
<br style="clear:both;"/>


<hr>
<h3 id="rprofile">Setting Up .Rprofile</h3>



The user may want to set the following options in her <a href="http://stat.ethz.ch/R-manual/R-patched/library/base/html/Startup.html" target="_blank">.Rprofile</a>
.  This will add efficiency to your workflow. <a href="http://www.youtube.com/watch?v=oZrVlsWMDBc&feature=youtu.be" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
  

<div>
<TABLE border=1>
<TR> <TH>  </TH> <TH> Option          </TH> <TH> Function </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> bib.loc         </TD> <TD> The path to the users primary .bib file (I store this in DropBox) </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> name.reports    </TD> <TD> The name that will be automatically added to a report/presentation </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> sources.reports </TD> <TD>  Path(s) to additional scripts to be sourced on project startup </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> temp.reports    </TD> <TD> The primary template to use to generate reports (see template) </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> github.user     </TD> <TD> GitHub user name </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> speed.temp      </TD> <TD> A speed dial like interface that allows the template argument to take a number </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> slidify.template     </TD> <TD> Path to, or defualt, .Rmd file tempalte for use in as the .Rmd used in the slidify presentations </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> revealjs.loc     </TD> <TD> The path to the user's <a href="https://github.com/hakimel/reveal.js/#installation" target="_blank">reveal.js full install</a>
 </TD> </TR>
</TABLE>
</div>

</br></br>

The following is the code I added to my own .Rprofile:

<pre><code class="r">1. options(bib.loc = "C:/Users/trinker/Desktop/PhD Program/MASTER.bib")
2. options(name.reports = "Tyler Rinker\\\\University at Buffalo\\\\Department of Learning and Instruction")
3. options(sources.reports = path.expand("~/path_1"), path.expand("~/path_2"))
4. options(temp.reports = "apa6.mod.qual_tex")
5. options(github.user = "trinker")
6. options(speed.temp = list(`1` = "wordpress_rmd", `2` = "basic_rmd", `3` = "apa6.mod.qual_tex"))
7. options(slidify.template = "revealjs")
8. options(revealjs.loc = "C:/Users/trinker/Desktop/Copy/reveal.js")</code></pre>

***Note*** - *The full install of reveal.js enables the user to locally view presentation with slide notes.*

<hr>
<h3 id="report">Creating a New Report/Presentation</h3> 
Creating a new report or presentation directory is easy. <a href="http://youtu.be/XVdRT95k3wk" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
  </br></br>

The user can either preset the working directory or can supply a path to create the report/presentation diectory in and then use the <a href="http://trinker.github.io/reports_dev/new_report.html" target="_blank"><code>new_report</code></a>
 or <a href="http://trinker.github.io/reports_dev/presentation.html" target="_blank"><code>presentation</code></a>
 functions. </br></br>

**setwd() method**
<pre><code class="r">setwd("C:/Users/trinker/Desktop")
new_report("MY REPORT")</code></pre>

**path method**
<pre><code class="r">new_report("MY REPORT", path="C:/Users/trinker/Desktop")</code></pre>

You've just created your first report/presentation directory and you're ready to begin writing.

\****Note*** *The user can name the main directory differently from the names used within the directy by supplying a vector of length 2 to the report/presentation arguments as seen here:* 

<pre><code class="r">new_report(c("Major1", "minor"))
presentation(c("Major2", "minor"))</code></pre>

<hr>
<h3 id="rep">What's In the new_report/presentation Directory</h3>

The directory created by the <a href="http://trinker.github.io/reports_dev/new_report.html" target="_blank"><code>new_report</code></a>
 or <a href="http://trinker.github.io/reports_dev/presentation.html" target="_blank"><code>presentation</code></a>
 functions houses multiple subdirectories and files that integrate to speed up work flow.  Please see the following links for PDF descriptions of the contents of the reports/presentation directory. </br></br>

<div style="text-align: center;">
<table width="30%" style="text-align: center;margin: 0px auto;">
<colgroup>
<col width="110" />
<col width="110" />
</colgroup>
<tr>
<tr style="text-align: center;">
<td style="text-align: center;">Report<br> Workflow</td>
<td style="text-align: center;">Presentation Workflow</td>
</tr>
<tr>
<td style="text-align: center; onClick="document.location.href='https://copy.com/cSd32ZXgsZSFJE2j/PRESENTATION_WORKFLOW_GUIDE.pdf?download=1';">
<a href="https://copy.com/cSd32ZXgsZSFJE2j/PRESENTATION_WORKFLOW_GUIDE.pdf?download=1"><img src="http://drupal.org/files/project-images/Download%20Views%20PDF_2.png" width="50" height="75"><br></a>
<a href="https://copy.com/cSd32ZXgsZSFJE2j/PRESENTATION_WORKFLOW_GUIDE.pdf?download=1" target="_blank">click here</a>
<td style="text-align: center; onClick="document.location.href='https://copy.com/26FrXMSZJ2ikDJFp/PRESENTATION_WORKFLOW_GUIDE.pdf?download=1';">
<p><a href="https://copy.com/26FrXMSZJ2ikDJFp/PRESENTATION_WORKFLOW_GUIDE.pdf?download=1"  target="_blank"><img src="http://drupal.org/files/project-images/Download%20Views%20PDF_2.png" width="50" height="75"><br></a>
<a href="https://copy.com/26FrXMSZJ2ikDJFp/PRESENTATION_WORKFLOW_GUIDE.pdf?download=1" target="_blank">click here</a></p></td>
</tr>
</table>
</div>



<hr>
<h3 id="code chunks">Using reports Functions In-Text</h3>

reports has a number of HTML shortcut functions that utilize two capital letters as names.  These functions are ideally used in text with <font color="blue">&#96;<code>r XX(args)</code>&#96;</font>.  Thus <font color="blue">&#96;<code>r HR2("http://www.talkstats.com/")</code>&#96;</font> yields:

<a href="http://www.talkstats.com/" target="_blank">www.talkstats.com</a>



This use does not require wrapping with <code>cat</code> or the <code>print</code> argument.  

To better display the HTML code the following examples do not use <a href="#code chunks">in-text</a>
 (in-line) coding.  The examples use the following code chunk form (rarely used in an actual report):

<pre><code class="r">&#96;&#96;&#96;{r}</br>
R_CODE(_HERE)</br>
&#96;&#96;&#96;</code></pre>

This means that wrapping with <code>cat</code> or the <code>print</code> argument are usually required, however, this is not typical in standard usage.  

<hr>
<h3 id="dir">Directory Management</h3>

The reports package contains functions to help maintain directories. They can speed up workflow through automation of directory related tasks.

<a href="http://trinker.github.io/reports_dev/rdirs.html" target="_blank"><code>rdirs</code></a>
 is a function designed to recursivly create directories.  This is particularly useful when used in combination with <a href="http://trinker.github.io/reports_dev/new_report.html" target="_blank"><code>new_report</code></a>
/<a href="http://trinker.github.io/reports_dev/presentation.html" target="_blank"><code>presentation</code></a>
 for creating weekly lecture notes for a class or other similar repeated tasks.  The following code chunks demonstrate uses for 3 directory management functions in the reports package:

1. <a href="http://trinker.github.io/reports_dev/rdirs.html" target="_blank"><code>rdirs</code></a>
 &#8211; recursive directory creation
2. <a href="http://trinker.github.io/reports_dev/file_handling.html" target="_blank">folder</a>
 &#8211; create a directory
3. <a href="http://trinker.github.io/reports_dev/file_handling.html" target="_blank">delete</a>
 &#8211; delete a directory(s)

\**The next 5 code chunks are meant to be run sequentially*

<font size="5" color="gold">&diams;</font> **Chunk 1**- *Create directory with multiple subdirectories* <font size="5" color="gold">&diams;</font> 

<pre><code class="r">fx <- folder(delete_me)
WD <- getwd()
setwd(fx)
rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6))</code></pre>

<font size="5" color="gold">&diams;</font> **Chunk 2**- *Return just the character vector. <a href="http://trinker.github.io/reports_dev/rdirs.html" target="_blank"><code>rdirs</code></a>
 takes care of the front end padding of numbers.* <font size="5" color="gold">&diams;</font>

```r
rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6), text.only = TRUE)
```

```
##  [1] "admin_01_d_1" "admin_02_f_4" "admin_03_w_6" "admin_04_d_1"
##  [5] "admin_05_f_4" "admin_06_w_6" "admin_07_d_1" "admin_08_f_4"
##  [9] "admin_09_w_6" "admin_10_d_1" "admin_11_f_4" "admin_12_w_6"
## [13] "admin_13_d_1" "admin_14_f_4" "admin_15_w_6"
```


<font size="5" color="gold">&diams;</font> **Chunk 3**- *Create dated folders* <font size="5" color="gold">&diams;</font>
<pre><code class="r">rdirs(session, 1:12, seq(as.Date("2000/1/1"), by = "month", length.out = 12))</code></pre>

<font size="5" color="gold">&diams;</font> **Chunk 4**- *Create director of subdirectories of working reports folders* <font size="5" color="gold">&diams;</font>
<pre><code class="r">setwd(WD)
gx <- folder(delete_me2)
setwd(gx)
x <- rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6), text.only = TRUE)
lapply(x, new_report)
## View it...</code></pre>

<font size="5" color="gold">&diams;</font> **Chunk 5**- *Clean up with <a href="http://trinker.github.io/reports_dev/file_handling.html" target="_blank">delete</a>
 and reset working directory* <font size="5" color="gold">&diams;</font>
<pre><code class="r">delete(c(fx, gx))
setwd(WD)</code></pre>

**sync_all** <a href="http://youtu.be/bhArI_dp4B8" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>

The <a href="http://trinker.github.io/reports_dev/sync.html" target="_blank">sync family of functions</a>
 are also useful for syncing files netween directories.  This is particularly useful with the figure files in the REPORT and PRESENTATION directories and/or the img directory used by slidify (<a href="http://ramnathv.github.com/slidify/">Vaidyanathan, 2012</a> ).  The base version in the family, <a href="http://trinker.github.io/reports_dev/sync.html" target="_blank"><code>sync</code></a>
 is generic and syncs files between 2 directories.  The <a href="http://trinker.github.io/reports_dev/sync.html" target="_blank">sync_all</a>
 function is specifically designed to sync any files between the three image directories in a new_report/presentation directory.  
 
<hr>
To use <a href="http://trinker.github.io/reports_dev/sync.html" target="_blank">sync_all</a>
 simply type:

<pre><code class="r">sync_all()</code></pre>

There are no arguments to sync_all, just run and the files between the following directories are synced:

1. ~/REPORT/figure
2. ~/PRESENTATION/figure
3. ~/PRESENTATION/assets/img

<hr>
<h3 id="notes">Using notes.xlsx</h3>

The notes.xlsx/notes.csv is a file located in the ARTICLES directory.  It is useful to take nores from articles/websites/etc. and organize them here.  The <a href="http://trinker.github.io/reports_dev/notes.html" target="_blank"><code>notes</code></a>
 function can be used to view a truncated version of the notes.  The citation functions also enables the user to insert in-text citations into a document with the quote taken from the article or other source.  It is important that the format of notes.xlsx is respected in order for the <a href="http://trinker.github.io/reports_dev/cite.html" target="_blank">citation family</a>
 of functions to be utilized.  In order for this workflow related to citations to be most effective the user must use a .bib file to manage citations.  I use <a href="http://jabref.sourceforge.net/" target="_blank">JabRef</a>
 manage my bibliography.  Here is a screen shot of notes.xlsx file.

<div style="width:840px;margin:auto;">
    <p><img src="figure/excel.png" width="800" height="600"></p>
</div>


notes.xlsx contains the following columns:

1. **Document (bib key)** - The bib key from the .bib file (usually authorYEAR)
2. **Location (page)** - The page number of the quote/passage (use an integer or integers separated by a dash)
3. **Gist/Quote** - The quote or a jist of a passage 
4. **Q** - Is the row gist a quote?  If so use **b** if not use **n**.  Those marked **y** can be inerted as quotes into a document.
5. **Notes** - An optional column to add notes to self

The **Gist/QuoteQ** should be written in <a href="http://daringfireball.net/projects/markdown/syntax" target="_blank">markdown</a>
/html.  The notes.xlsx file also contains a quick reference for html on the right side:

![](http://i.imgur.com/bcDDgoL.png)

**Format Text for notes.xlsx** <a href="http://youtu.be/qtqpTGMKWd0" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>

To assist in copying and pasting text from documents (particularly PDFs) the <a href="http://trinker.github.io/reports_dev/QQ.html" target="_blank"><code>QQ</code></a>
 (quick quote) takes a text string (optionally from the clipboard) and formats the text for use with notes.xlsx.  Non-ascii characters (e.g. ligatures) and <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/Quotes.html" target="_blank">**&#92;n**</a>
 are removed/converted (though the string should still be checked).  By default the string is copied to the clipboard for easy pasting into notes.xlsx.


<hr>
<h3 id="cite">Citations</h3>

The reports package has an integrated system citation related functions that can be accessed in .tex, .Rnw and .Rmd documents.  

The user can view the bibkey and document title of the references in the .bib files via the <a href="http://trinker.github.io/reports_dev/BV.html" target="_blank"><code>BV</code></a>
 (bib view) function.

<font size="5" color="gold">&diams;</font> **Example** <font size="5" color="gold">&diams;</font>

```r
head(BV())
```

```
##   num bibkey           title                                   
## 1   1 Yihui20131       knitr: A general-purpose package for dyn
## 2   2 Yihui20132       Dynamic Documents with {R} and knitr    
## 3   3 Yihui20133       knitr: A Comprehensive Tool for Reproduc
## 4   4 Boettiger2013    knitcitations: Citations for knitr markd
## 5   5 Vaidyanathan2012 slidify: Generate reproducible html5 sli
## 6   6 Rinker2013a      {qdap}: {Q}uantitative Discourse Analysi
```

<hr>

**Citation Family of Functions**
The <a href="http://trinker.github.io/reports_dev/cite.html" target="_blank">citation family</a>
 of functions grab a quote and format the text taken from the notes.xlsx/notes.csv for <span class="latex">L<sup>a</sup>T<sub>e</sub>X</span>/.Rmd with optional citation included. Functions attempt to copy the output to the clipboard for easy paste inclusion.

Each of the functions in the cite family follow a pattern of (cite, parencite, textcite, posscite, poscite) prefix and (L or M) suffix (note that currently only parencite and textcite functions exist for markdown). The cite and textcite are in the form of <span class="latex">L<sup>a</sup>T<sub>e</sub>X</span> commands by the same name. posscite and poscite are user defined <span class="latex">L<sup>a</sup>T<sub>e</sub>X</span> function styles that are extensions of the textcite command to fit possessive and -s- ending possessives. They can be defined as:

<pre><code class="r">\newcommand\posscite[1]{\citeauthor{#1}'s (\citeyear{#1})} 
\newcommand\poscite[1]{\citeauthor{#1}' (\citeyear{#1})}</code></pre>

The L and M correspond to <span class="latex">L<sup>a</sup>T<sub>e</sub>X</span> or markdown outputs; markdown relies on the <a href="https://github.com/cboettig/knitcitations" target="_blank">knitcitations</a>
 (<a href="https://github.com/cboettig/knitcitations">Boettiger, 2013</a> ) package.

<font size="5" color="gold">&diams;</font> **Example 1** - *parenthesis cite* <font size="5" color="gold">&diams;</font>

```r
parenciteM(1, force.block = FALSE)
```

<pre><code>> "An **EXAMPLE**; feel "free" to *delete* it ***soon***" (<a href="http://github.com/trinker/qdap">Rinker, 2013b</a> , p. 12)</code></pre>
 
"An **EXAMPLE**; feel "free" to *delete* it ***soon***" (<a href="http://github.com/trinker/qdap">Rinker, 2013b</a> , p. 12)

 
<font size="5" color="gold">&diams;</font> **Example 2** - *textcite* <font size="5" color="gold">&diams;</font>

```r
textciteM(1, force.block = FALSE)
```

<pre><code>&#180;r  citet(x=bib[["Rinker2013a"]])&#180; "An **EXAMPLE**; feel "free" to *delete* it ***soon***"</code></pre>
<a href="http://github.com/trinker/qdap">Rinker (2013)</a>  "An **EXAMPLE**; feel "free" to *delete* it ***soon***"

The <span class="latex">L<sup>a</sup>T<sub>e</sub>X</span> (L) versions of the functions work in a similar fashion but are used in .Rnw documents.
<hr>
**Updating .bib File** <a href="http://youtu.be/XriSbJTXcZk" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>


The user may update the .bib files within the new&#95;report/presentation created directories directly or may choose to update the master .bib file housed elsewhere (set with the <code>bib.loc</code> option in the <a href="#rprofile">.Rprofile</a>
).  If the user opts for the second route the <a href="http://trinker.github.io/reports_dev/update_bib.html" target="_blank"><code>update_bib</code></a>
 function will update all the .bib files in the respective directory(s) according to the master .bib file.
<hr>

<h3 id="convert">Document Conversion <a href="http://youtu.be/OcaFub5OLSg" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h3>
A useful <a href="http://johnmacfarlane.net/pandoc/" target="_blank">Pandoc</a>
 uses preset path defaults (default is REPORT directory) to enable the user to quickly convert between document types.  The user must have Pandoc installed and on the OS path.  The function names are intuitive with the portion before the 2 being the current document type and the portion after is the desired outcome document type (**from2to**).  Here is an example, converting the doc.tex file from the reports package library directory to a .docx file (when the user is employing these functions it is not necessary to provide arguments).

<font size="5" color="gold">&diams;</font> **Example** - *file conversion* <font size="5" color="gold">&diams;</font>

```r
DOC <- system.file("extdata/doc_library/apa6.qual_tex/doc.tex", package = "reports")
BIB <- system.file("extdata/docs/example.bib", package = "reports")
tex2docx(DOC, file.path(getwd(), "test.docx"), path = NULL, bib.loc = BIB)
```


\****Note*** *that all of the conversion functions can be used to convert any legitimate Pandoc conversion but if the* ***from2to*** *format is not followed the documents and paths must be explicitally supplied as arguments to the reports conversion function.*

<hr>
<h3 id="html">HTML Shortcuts</h3>




The general format of the HTML examples is:

<pre><code class="r">(a) the display of the code used</code></pre>     
<pre><code>(b) the HTML code produced</code></pre>     
&#40;c) the way the HTML renders in a browser.  

<hr>

This <a href="#tab">table </a>
 lists the HTML functions and the HTML tag types produced.  Click on the left column function names to see a demonstation of its use.  
</br></br>

<TABLE id="tab" border=1>
<col width="60px"><col width="275px">
<TR> <TH style="text-align:left"> Code </TH> <TH style="text-align:left"> HTML Tag </TH>  </TR>
  <TR> <TD> <a href="#CN">CN</a>
 </TD> <TD> courrier new </TD> </TR>
  <TR> <TD> <a href="#EM">EM</a>
 </TD> <TD> email </TD> </TR>
  <TR> <TD> <a href="#FT">FT</a>
 </TD> <TD> font </TD> </TR>
  <TR> <TD> <a href="#HL">HL</a>
 </TD> <TD> highlight text </TD> </TR>  
  <TR> <TD> <a href="#HR">HR</a>
 </TD> <TD> hyperref </TD> </TR>
  <TR> <TD> <a href="#HR">HR2</a>
 </TD> <TD> hyperref (new window) </TD> </TR>
  <TR> <TD> <a href="#HR">BT</a>
 </TD> <TD> button </TD> </TR>
  <TR> <TD> <a href="#space">HS</a>
 </TD> <TD> horizontal space </TD> </TR>
  <TR> <TD> <a href="#space">VS</a>
 </TD> <TD> verticle space </TD> </TR>
  <TR> <TD> <a href="#iframe">IF</a>
 </TD> <TD> iframe </TD> </TR>
  <TR> <TD> <a href="#IM">IM</a>
 </TD> <TD> image </TD> </TR>
  <TR> <TD> <a href="#IM">IM2</a>
 </TD> <TD> image (short path) </TD> </TR>
  <TR> <TD> <a href="#IM">IW</a>
 </TD> <TD> image wrap </TD> </TR>
  <TR> <TD> <a href="#PN">PN</a>
 </TD> <TD> presenter notes </TD> </TR>
  <TR> <TD> <a href="#RF">RF</a>
 </TD> <TD> reveal.js fragment </TD> </TR>
  <TR> <TD> <a href="#SC">SC</a>
 </TD> <TD> sprecial character </TD> </TR>
  <TR> <TD> <a href="#TB">TB</a>
 </TD> <TD> text box </TD> </TR>
  <TR> <TD> <a href="#YT">YT</a>
 </TD> <TD> YouTube </TD> </TR>
  <TR> <TD> <a href="#YT">VM</a>
 </TD> <TD> Vimeo </TD> </TR>
  <TR> <TD> <a href="#col">col2hex</a>
 </TD> <TD> hex colors </TD> </TR>
   </TABLE>
  
<hr> 

### <p id="CN"><a href="http://trinker.github.io/reports_dev/CN.html" target="_blank">CN (courrier new)</a>
</p>

Wraps text with a courier new font tag. A specified version of <a href="#FT">FT</a>
 but more convient for constant use of courier new tags.


```r
CN("new_report()")
```

```
## [1] <font face="courier">new_report()</font>
```

<font face="courier">new_report()</font>
<hr>

### <p id="EM"><a href="http://trinker.github.io/reports_dev/EM.html" target="_blank">EM (email)</a>
</p>
Wrap an email to generate an HTML email tag.


```r
EM("tyler.rinker@gmail.com", print = TRUE)
```

```
## <a href="mailto:tyler.rinker@gmail.com" target="_blank">tyler.rinker@gmail.com</a>
```

<a href="mailto:tyler.rinker@gmail.com" target="_blank">tyler.rinker@gmail.com</a>

<hr>

### <p id="FT"><a href="http://trinker.github.io/reports_dev/FT.html" target="_blank">FT (font)</a>
</p>
Wraps text with a font tags. Conveniently detects c(<code>face</code>, <code>size</code>` and/or <code>color</code>) and creates a font tag with the supplied text.

<font size="5" color="gold">&diams;</font> **Example 1** <font size="5" color="gold">&diams;</font>

```r
FT(6, text = "guy")
```

```
## [1] <font size="6">guy</font>
```

[1] <font size="6">guy</font>


<font size="5" color="gold">&diams;</font> **Example 2** <font size="5" color="gold">&diams;</font>

```r
FT(6, blue, text = "guy")
```

```
## [1] <font size="6" color="blue">guy</font>
```

[1] <font size="6" color="blue">guy</font>


<font size="5" color="gold">&diams;</font> **Example 3** <font size="5" color="gold">&diams;</font>

```r
FT(6, red, times_new_roman, text = "guy")
```

```
## [1] <font size="6" color="red" face="times new roman">guy</font>
```

[1] <font size="6" color="red" face="times new roman">guy</font>

<hr>

### <p id="HL"><a href="http://trinker.github.io/reports_dev/HL.html" target="_blank">HL (highlight text)</a>
</p>

<font size="5" color="gold">&diams;</font> **Example 1** <font size="5" color="gold">&diams;</font>

```r
cat(HL("Do not trust robots!"), "They are bent on destruction.")
```

```
## <font style="background-color: #FFFF00;">Do not trust robots!</font> They are bent on destruction.
```

<font style="background-color: #FFFF00;">Do not trust robots!</font> They are bent on destruction.


<font size="5" color="gold">&diams;</font> **Example 2** <font size="5" color="gold">&diams;</font>

```r
cat(HL("Jake is a cookie scientist,", color = "pink"), "an honorable profession.")
```

```
## <font style="background-color: #FFC0CB;">Jake is a cookie scientist,</font> an honorable profession.
```

<font style="background-color: #FFC0CB;">Jake is a cookie scientist,</font> an honorable profession.



### <p id="HR"><a href="http://trinker.github.io/reports_dev/hyperref.html" target="_blank">HR/HR2/BT (hyperrefs and buttons)</a>
</p>
Convert path/url to HTML href tag.  

1. **HR** - Wrap a path/url to generate an HTML href tag.
2. **HR2** - Convenience version of HR that opens in a new page.
3. **BT** - Wrap a path/url to generate an HTML hyperlinked text button tag.


<font size="5" color="gold">&diams;</font> **Example 1** - *HR demo: no text given--<a href="http://stat.ethz.ch/R-manual/R-patched/library/base/html/basename.html">basename</a>
 used; opens in same window* <font size="5" color="gold">&diams;</font>

```r
HR(path = "http://dl.dropbox.com/u/61803503/Likert.pdf", print = TRUE)
```

```
## <a href="http://dl.dropbox.com/u/61803503/Likert.pdf">Likert.pdf</a>
```

<a href="http://dl.dropbox.com/u/61803503/Likert.pdf">Likert.pdf</a>


<font size="5" color="gold">&diams;</font> **Example 2** - *HR demo: no text given--<a href="http://stat.ethz.ch/R-manual/R-patched/library/base/html/basename.html">basename</a>
 used; opens in same window* <font size="5" color="gold">&diams;</font>

```r
HR("http://cran.r-project.org/", print = TRUE)
```

```
## <a href="http://cran.r-project.org/">cran.r-project.org</a>
```

<a href="http://cran.r-project.org/">cran.r-project.org</a>


<font size="5" color="gold">&diams;</font> **Example 3** - *HR demo: text given; opens in same window* <font size="5" color="gold">&diams;</font>

```r
HR("http://www.rstudio.com/ide/download/desktop", "click me", print = TRUE)
```

```
## <a href="http://www.rstudio.com/ide/download/desktop">click me</a>
```

<a href="http://www.rstudio.com/ide/download/desktop">click me</a>


<font size="5" color="gold">&diams;</font> **Example 4** - *HR2 demo: text given; opens in new window* <font size="5" color="gold">&diams;</font>

```r
HR2("https://github.com/trinker/reports", "reports", print = TRUE)
```

```
## <a href="https://github.com/trinker/reports" target="_blank">reports</a>
```

<a href="https://github.com/trinker/reports" target="_blank">reports</a>


<font size="5" color="gold">&diams;</font> **Example 5** - *BT demo* <font size="5" color="gold">&diams;</font>

```r
BT("http://trinker.github.io/reports/dependencies", "Click Here!", print = TRUE)
```

```
## <form action="http://trinker.github.io/reports/dependencies">
##     <input type="submit" value="Click Here!">
## </form>
```

<form action="http://trinker.github.io/reports/dependencies">
    <input type="submit" value="Click Here!">
</form>



<hr>

### <p id="space"><a href="http://trinker.github.io/reports_dev/space.html" target="_blank">HS/VS (spaces)</a>
</p>
Insert n iterations of HTML spacing into a document.

<font size="5" color="gold">&diams;</font> **Example 1** - *HS demo* <font size="5" color="gold">&diams;</font>

```r
cat(paste0("reports", HS(10), "end"))
```

```
## reports&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;end
```

reports&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;end</br>


<font size="5" color="gold">&diams;</font> **Example 2** - *VS demo: single space* <font size="5" color="gold">&diams;</font>

```r
cat(paste0("the", VS(), "end"))
```

```
## the</br>end
```

the</br>end</br>


<font size="5" color="gold">&diams;</font> **Example 3** - *HS demo: multiple spaces* <font size="5" color="gold">&diams;</font>

```r
cat(paste0("the", VS(3), "end"))
```

```
## the</br></br></br>end
```

the</br></br></br>end</br>


<hr>

### <p id = "iframe"><a href="http://trinker.github.io/reports_dev/IF.html" target="_blank">IF (iframe embedding)</a>
</p>
Wrap a url to generate an HTML iframe tag.

<font size="5" color="gold">&diams;</font> **Example 1** <font size="5" color="gold">&diams;</font>

```r
IF("https://dl.dropboxusercontent.com/u/61803503/MWE.html", print = TRUE, height = 250)
```

```
## <div style="text-align:center;">
##      <iframe src="https://dl.dropboxusercontent.com/u/61803503/MWE.html" width="640" height="250">Your browser does not support iframes.</iframe>
## </div>
```

<div style="text-align:center;">
     <iframe src="https://dl.dropboxusercontent.com/u/61803503/MWE.html" width="640" height="250">Your browser does not support iframes.</iframe>
</div>


<font size="5" color="gold">&diams;</font> **Example 2** <font size="5" color="gold">&diams;</font>

```r
IF("http://www.online-stopwatch.com/countdown-clock/full-screen/", height = 400, 
    center = TRUE, print = TRUE)
```

```
## <div style="text-align:center;">
##      <iframe src="http://www.online-stopwatch.com/countdown-clock/full-screen/" width="640" height="400">Your browser does not support iframes.</iframe>
## </div>
```

<div style="text-align:center;">
     <iframe src="http://www.online-stopwatch.com/countdown-clock/full-screen/" width="640" height="400">Your browser does not support iframes.</iframe>
</div>


<font size="5" color="gold">&diams;</font> **Example 3** <font size="5" color="gold">&diams;</font>

```r
IF("http://awwapp.com/draw.html", width = 700, height = 500, print = TRUE)
```

```
## <div style="text-align:center;">
##      <iframe src="http://awwapp.com/draw.html" width="700" height="500">Your browser does not support iframes.</iframe>
## </div>
```

<div style="text-align:center;">
     <iframe src="http://awwapp.com/draw.html" width="700" height="500">Your browser does not support iframes.</iframe>
</div>


<font size="5" color="gold">&diams;</font> **Example 4** <font size="5" color="gold">&diams;</font>

```r
IF("http://glimmer.rstudio.com/pssguy/TVShowRatings/", width = "100%", height = 650, 
    print = TRUE)
```

```
## <div style="text-align:center;">
##      <iframe src="http://glimmer.rstudio.com/pssguy/TVShowRatings/" width="100%" height="650">Your browser does not support iframes.</iframe>
## </div>
```

<div style="text-align:center;">
     <iframe src="http://glimmer.rstudio.com/pssguy/TVShowRatings/" width="100%" height="650">Your browser does not support iframes.</iframe>
</div>


<font size="5" color="gold">&diams;</font> **Example 5** <font size="5" color="gold">&diams;</font>

```r
IF("https://dl.dropboxusercontent.com/u/61803503/Slides/reports/index.html", 
    width = 1150, height = 750, print = TRUE)
```

```
## <div style="text-align:center;">
##      <iframe src="https://dl.dropboxusercontent.com/u/61803503/Slides/reports/index.html" width="1150" height="750">Your browser does not support iframes.</iframe>
## </div>
```

<div style="text-align:center;">
     <iframe src="https://dl.dropboxusercontent.com/u/61803503/Slides/reports/index.html" width="1150" height="750">Your browser does not support iframes.</iframe>
</div>


<font size="5" color="gold">&diams;</font> **Example 6** *Here's an example of embedding a beautiful interactive visualization compliments of the googleVis package (<a href="http://journal.r-project.org/archive/2011-2/RJournal_2011-2_Gesmann+de~Castillo.pdf">Gesmann & Castillo, 2011</a> )* <font size="5" color="gold">&diams;</font>

```r
IF("https://dl.dropboxusercontent.com/u/61803503/presentations/tmp.html", width = 770, 
    height = 680, print = TRUE)
```

```
## <div style="text-align:center;">
##      <iframe src="https://dl.dropboxusercontent.com/u/61803503/presentations/tmp.html" width="770" height="680">Your browser does not support iframes.</iframe>
## </div>
```

<div style="text-align:center;">
     <iframe src="https://dl.dropboxusercontent.com/u/61803503/presentations/tmp.html" width="770" height="680">Your browser does not support iframes.</iframe>
</div>

<hr>

### <p id="IM"><a href="http://trinker.github.io/reports_dev/image.html" target="_blank">IM/IM2/IW (images and text-wrapped images)</a>
</p>

These functions geberate an HTML image tag that embeds an image.  <code>IM2</code> not demonstrated as this function assumes local image source.

1. **IM** - Wrap a path/url to generate an HTML tag. Often markup code: &#33;[](url&#40; lacks flexibility with centering and sizing. IM enables conrol of centering via altering the sty/center commands and control of sizing via the numeric values supplied to height and width.
2. **IM2** - A wrapper for IM that sets the base path to "assets/img/". This allows the users to just specify the image name that resides in one of the following directories: 1-"<code>~/assets/img</code>" or 2-"<code>~/figure</code>".
3. **IW** - Text wrapped images.

<font size="5" color="gold">&diams;</font> **Example 1** - *IM demo: Use <code>width=NULL</code> to retain original size* <font size="5" color="gold">&diams;</font>

```r
IM("http://cran.r-project.org/Rlogo.jpg", width = NULL, print = TRUE)
```

```
## <div style="width:480px;margin:auto;">
##     <p><img src="http://cran.r-project.org/Rlogo.jpg"></p>
## </div>
```

<div style="width:480px;margin:auto;">
    <p><img src="http://cran.r-project.org/Rlogo.jpg"></p>
</div>


<font size="5" color="gold">&diams;</font> **Example 2** - *IM demo: uses default <code>width = 540, height = IE(width, 360)</code> to size* <font size="5" color="gold">&diams;</font>

```r
IM("https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG", print = TRUE)
```

```
## <div style="width:567px;margin:auto;">
##     <p><img src="https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG" width="540" height="360"></p>
## </div>
```

<div style="width:567px;margin:auto;">
    <p><img src="https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG" width="540" height="360"></p>
</div>


<font size="5" color="gold">&diams;</font> **Example 3** - *IM demo: hyperlinked image* <font size="5" color="gold">&diams;</font>

```r
IM("http://cran.r-project.org/Rlogo.jpg", NULL, print = TRUE, link = "http://cran.r-project.org")
```

```
## <div style="width:480px;margin:auto;">
##     <p><a href="http://cran.r-project.org" target="_blank"><img src="http://cran.r-project.org/Rlogo.jpg"></a></p>
## </div>
```

<div style="width:480px;margin:auto;">
    <p><a href="http://cran.r-project.org" target="_blank"><img src="http://cran.r-project.org/Rlogo.jpg"></a></p>
</div>


<font size="5" color="gold">&diams;</font> **Example 4** - *IM demo: assumed image directory* <font size="5" color="gold">&diams;</font>

```r
IM2("Rlogo.jpg", center = FALSE)
```

```
## [1] "<img src=\"assets/img/Rlogo.jpg\" width=\"540\" height=\"360\">"
```

```r
IM2("Rlogo.jpg", loc = 2, center = FALSE)
```

```
## [1] "<img src=\"figure/Rlogo.jpg\" width=\"540\" height=\"360\">"
```


<font size="5" color="gold">&diams;</font> **Example 5** - *IW demo: text wrapped image with hyperlink* <font size="5" color="gold">&diams;</font> </br></br>
<pre><code class="r">&#96;r IW("http://www.talkstats.com/images/misc/logo.png", "http://www.talkstats.com/", width=140, height=75)&#96;</code></pre>

<pre><code>## &lt;a href="http://www.talkstats.com/" target="_blank"&gt;&lt;img src="http://www.talkstats.com/images/misc/logo.png" width="140" height="75"&gt;&lt;/a&gt;</code></pre>

<div>
<div style="float:right;margin:-15px 20px 0px 20px;">
    <a href="http://www.talkstats.com/" target="_blank"><img src="http://www.talkstats.com/images/misc/logo.png" width="140" height="75"></a>
</div> So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!  So much text!
</div>
<hr>

### <p id="PN"><a href="http://trinker.github.io/reports_dev/PN.html" target="_blank">PN (presenter notes)</a>
</p>

Creates presenter notes (an aside of the class "notes") for slides (e.g., reveal.js slides).  Multiline notes can be supplied with <code>\n</code> separating the lines or on separate lines as seen below.    

<font size="5" color="gold">&diams;</font> **Example 1** - *single line* <font size="5" color="gold">&diams;</font>

```r
PN("some fancy notes", print = TRUE)
```

```
## <aside class="notes">
## some fancy notes <br>
## </aside>
```

<aside class="notes">
some fancy notes <br>
</aside>


<font size="5" color="gold">&diams;</font> **Example 2** - *multiline with \n as separator* <font size="5" color="gold">&diams;</font>

```r
PN("1) some\n2) fancy\n3) notes", print = TRUE)
```

```
## <aside class="notes">
## 1) some <br>
## 2) fancy <br>
## 3) notes <br>
## </aside>
```

<aside class="notes">
1) some <br>
2) fancy <br>
3) notes <br>
</aside>


<font size="5" color="gold">&diams;</font> **Example 3** - *multiline on separate lines* <font size="5" color="gold">&diams;</font>
<pre><code class="r">PN("1) some<br>2) fancy<br>3) notes", print = TRUE)</code></pre>    


```
## <aside class="notes">
## 1) some  <br>
## 2) fancy  <br>
## 3) notes <br>
## </aside>
```


<aside class="notes">
1) some  <br>
2) fancy  <br>
3) notes <br>
</aside>


<hr>

### <p id="RF"><a href="http://trinker.github.io/reports_dev/RF.html" target="_blank">RF (reveal.js fragments)</a>
</p>
Slidify uses to include animated fragments (see <a href="https://github.com/ramnathv/slidifyExamples/blob/gh-pages/examples/revealjs/index.Rmd" target="_blank">this example</a>
) in reveal.js slides. This is required per each slide. Using &#96;r RF()&#96; reduces the typing invloved with this action.


```r
RF()
```

```
## [1] "<script>\n$('ul.incremental li').addClass('fragment')\n</script>\n"
```



<hr>
### <p id="SC"><a href="http://trinker.github.io/reports_dev/SC.html" target="_blank">SC (special character)</a>
</p>
The general use of SCis to provide a character string of length 2. The first character is a letter and the second is one of the following symbols (left column) and HTML equivalent (right column): 




<TABLE border=1>
<TR> <TH> Key </TH> <TH> Symb </TH> <TH> HTML </TH>  </TR>
  <TR> <TD> apostrophe </TD> <TD> ' </TD> <TD> acute </TD> </TR>
  <TR> <TD> back tick </TD> <TD> &#96; </TD> <TD> grave </TD> </TR>
  <TR> <TD> colon </TD> <TD> : </TD> <TD> uml </TD> </TR>
  <TR> <TD> tilde </TD> <TD> ~ </TD> <TD> tilde </TD> </TR>
  <TR> <TD> carat </TD> <TD> ^ </TD> <TD> circ </TD> </TR>
  <TR> <TD> slash </TD> <TD> / </TD> <TD> slash </TD> </TR>
  <TR> <TD> lowercase o </TD> <TD> o </TD> <TD> ring </TD> </TR>
  <TR> <TD> comma </TD> <TD> , </TD> <TD> cedil </TD> </TR>
   </TABLE>


The user can create non-HTML characters with SC that will not be converted (i.e., **SC("b~")** would yield "**&btilde;**" and would not be converted appropriately).



```r
SC("A'")
```

[1] "&Aacute;"

```r
SC('a\'')  #can use single quotes with escape 
```

[1] "&aacute;"

```r
SC("a`")
```

[1] "&agrave;"

```r
SC("n~")
```

[1] "&ntilde;"

```r
SC("o:")
```

[1] "&ouml;"

```r
SC("(c)")
```

[1] "&copy;"

```r
SC("(r)")
```

[1] "&reg;"

```r
SC("c|")
```

[1] "&cent;"

```r
SC("o/")
```

[1] "&oslash;"

```r
SC("ao")
```

[1] "&aring;"

```r
SC("c,")
```

[1] "&ccedil;"

```r
SC("p") 
```

[1] "&para;"

```r
SC("P") 
```

[1] "&para;"

```r
SC("E")
```

[1] "&euro;"

```r
SC("Y")
```

[1] "&yen;"

```r
SC("/")
```

[1] "&divide;"

```r
SC("+-")
```

[1] "&plusmn;"

```r
SC("L")
```

[1] "&pound;"

```r
SC("tm") 
```

[1] "&trade;"

```r
SC("S") 
```

[1] "&sect;"


<hr>
### <p id="TB"><a href="http://trinker.github.io/reports_dev/TB.html" target="_blank">TB (text box)</a>
</p>
Wrap text to generate an HTML text box tag.

<font size="5" color="gold">&diams;</font> **Example 1** <font size="5" color="gold">&diams;</font>

```r
TB("I like ice cream!", print = TRUE)
```

```
## <div align="left">
##     <input style="text-align:center" name="box1" type="text" value="I like ice cream!" size="17" />
## </div>
```

<div align="left">
    <input style="text-align:center" name="box1" type="text" value="I like ice cream!" size="17" />
</div>


<font size="5" color="gold">&diams;</font> **Example 2** <font size="5" color="gold">&diams;</font>

```r
TB("Free cookies for a year!", print = TRUE)
```

```
## <div align="left">
##     <input style="text-align:center" name="box1" type="text" value="Free cookies for a year!" size="24" />
## </div>
```

<div align="left">
    <input style="text-align:center" name="box1" type="text" value="Free cookies for a year!" size="24" />
</div>


<hr>
### <p id="YT"><a href="http://trinker.github.io/reports_dev/video.html" target="_blank">YT/VM (insert video)</a>
</p>
Returns a character vector of an HTML iframe tag that embeds a YouTube or Vimeo video.

<font size="5" color="gold">&diams;</font> **Example 1**- *YouTube using url tag* <font size="5" color="gold">&diams;</font>

```r
YT("kws1PX1Dw9w", print = TRUE)
```

```
## <iframe class="youtube-player" type="text/html" width="640" height="360" src="http://www.youtube.com/embed/kws1PX1Dw9w?autoplay=0" frameborder="0"></iframe>
```

<iframe class="youtube-player" type="text/html" width="640" height="360" src="http://www.youtube.com/embed/kws1PX1Dw9w?autoplay=0" frameborder="0"></iframe>


<font size="5" color="gold">&diams;</font> **Example 2**- *YouTube using full url* <font size="5" color="gold">&diams;</font>

```r
YT("http://www.youtube.com/watch?v=kws1PX1Dw9w", print = TRUE)
```

```
## <iframe class="youtube-player" type="text/html" width="640" height="360" src="http://www.youtube.com/embed/kws1PX1Dw9w?autoplay=0" frameborder="0"></iframe>
```

<iframe class="youtube-player" type="text/html" width="640" height="360" src="http://www.youtube.com/embed/kws1PX1Dw9w?autoplay=0" frameborder="0"></iframe>


<font size="5" color="gold">&diams;</font> **Example 3**- *Vimeo video* <font size="5" color="gold">&diams;</font>

```r
VM("http://vimeo.com/54007714", print = TRUE)
```

```
## <iframe src="http://player.vimeo.com/video/54007714" width="640" height="360" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>
```

<iframe src="http://player.vimeo.com/video/54007714" width="640" height="360" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>


<hr>
### <p id="color"><a href="http://trinker.github.io/reports_dev/col2hex.html" target="_blank">col2hex (colors)</a>
</p>
Convert R <code>colors()</code> to Hexadecimal.


```r
col2hex("darkblue")
```

```
## [1] "#00008B"
```

```r
col2hex(c("darkblue", "red"))
```

```
## [1] "#00008B" "#FF0000"
```

```r
cat(paste0("<hr color=\"", col2hex("red"), "\" size=\"4\">"))
```

```
## <hr color="#FF0000" size="4">
```

<hr color="#FF0000" size="4">

<hr>
<h3 id="temp">Custom Templates <a href="http://youtu.be/CMmHOvMrEvQ" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h3> 

The user can create a document template framework for template personal use or submission with the <a href="http://trinker.github.io/reports_dev/doc_temp.html" target="_blank"><code>doc_temp</code></a>
 function.  This function generates a document template for personal use or submission to the reports package to be included as a template in the doc_library. This function is similar to <code><a href='http://www.inside-r.org/r-doc/utils/package.skeleton'>package.skeleton</a></code>.

<hr>
## Acknowledgements

The reports package is just a collection of the best R tools.  A special thanks to <a href="http://yihui.name/" target="_blank">Yihui</a>
 <a href="http://www.crcpress.com/product/isbn/9781466561595">Xie (2013)</a>  for his work with the <a href="http://yihui.name/knitr/" target="_blank">knitr package</a>
 and <a href="http://people.mcgill.ca/ramnath.vaidyanathan/" target="_blank">Ramnath Vaidyanathan</a>
 for his <a href="http://slidify.org/" target="_blank">slidify package</a>
.  These R packages are the core of the reports package.  For a complete list of reports dependencies <a href="http://trinker.github.io/reports/dependencies" target="_blank">click here</a>
.
<hr> 




## References

- Carl Boettiger,   (2013) knitcitations: Citations for knitr markdown files.  <a href="https://github.com/cboettig/knitcitations">https://github.com/cboettig/knitcitations</a>
- Markus Gesmann, Diego Castillo,   (2011) googleVis: Interface between R and the Google Visualisation API.  <em>The R Journal</em>  <strong>3</strong>  (2)   40-44  <a href="http://journal.r-project.org/archive/2011-2/RJournal_2011-2_Gesmann+de~Castillo.pdf">http://journal.r-project.org/archive/2011-2/RJournal_2011-2_Gesmann+de~Castillo.pdf</a>
- Tyler Rinker,   (2013) {qdap}: {Q}uantitative Discourse Analysis Package.  <a href="http://github.com/trinker/qdap">http://github.com/trinker/qdap</a>
- Tyler Rinker,   (2013) {reports}: {P}ackage to asssist in report writing.  <a href="http://github.com/trinker/reports">http://github.com/trinker/reports</a>
- Ramnath Vaidyanathan,   (2012) slidify: Generate reproducible html5 slides from R markdown.  <a href="http://ramnathv.github.com/slidify/">http://ramnathv.github.com/slidify/</a>
- Yihui Xie,   (2013) knitr: A Comprehensive Tool for Reproducible Research in {R}.  <a href="http://www.crcpress.com/product/isbn/9781466561595">http://www.crcpress.com/product/isbn/9781466561595</a>

