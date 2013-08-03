reports
=======
reports is a package that assists in writing reports and presentations by providing a frame work that brings together existing R, LaTeX/.docx and Pandoc tools.  The package is designed to be used with [RStudio](http://www.rstudio.com/), [MikTex](http://miktex.org/)/[Tex Live](http://www.tug.org/texlive/)/[LibreOffice](http://www.libreoffice.org/), [knitr](http://yihui.name/knitr/), [slidify](http://ramnathv.github.com/slidify/), [knitcitations](http://www.carlboettiger.info/2012/05/30/knitcitations.html), [Pandoc](http://johnmacfarlane.net/pandoc/) and [pander](https://github.com/rapporter/pander).  The user will want to download these free programs to maximize the effectiveness of the reports package.

<p><a href="http://trinker.github.io/reports/dependencies"><img src="https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG"></a></p>


## Installation

To download the [development version of reports](http://trinker.github.com/reports_dev/):

download the [zip ball](https://github.com/trinker/reports/zipball/master) or [tar ball](https://github.com/trinker/reports/tarball/master), decompress and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")

library(devtools)
#Install the development versions of slidify and its libraries
install_github('slidify', 'ramnathv', ref = 'dev')
install_github('slidifyLibraries', 'ramnathv', ref = 'dev')

install_github("reports", "trinker")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

## Help

[Web Page](http://trinker.github.com/reports/)      
[Intoductory Video](http://www.youtube.com/watch?feature=player_embedded&v=AZnVM0_ru7o)            
[Quick Start Slides](http://trinker.github.com/slides/reports.html)    
[Package Vignette](https://dl.dropboxusercontent.com/u/61803503/packages/reports_vignette.html)     
[Package PDF Help Manual](https://dl.dropbox.com/u/61803503/reports.pdf)          
[click here](http://trinker.github.io/reports/dependencies) for information on the programs/packages reports brings together     


