NEWS 
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

<major>.<minor>.<patch>

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)

* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)

* Bug fixes and misc changes bumps the patch



reports 0.2.0
----------------------------------------------------------------

BUG FIXES

* `GQ`, `CA`, `LL` and `US` threw multiple warnings if a ligature was detected.
  This behavior has been corrected.

* `tex2docx` and other document conversion functions (see: 
  https://github.com/trinker/reports/blob/master/R/tex2docx.R) generated an 
  error if the file path contained a space (see: 
  http://stackoverflow.com/questions/17897359/error-with-tex2docx-function-from-reports-r-package).
  Bug caught by MYaseen208 of stackoverflow.com and fixed by Gergely Daróczi via 
  pull request 31.
  

NEW FEATURES

* `new_report` and `presentation` utilize the slidify package to render the .Rmd 
  files associated with the presentation.

* `new_vignette` is a function that takes advantage of R (>= 3.0.0)'s inclusing
  of non-sweave vignettes.  This function utilizes knitr's capabilities to make 
  .Rnw and .Rmd vignettes (the reports Markdown/HTML vignette was created this 
  way).  Add additional vignettes to the vignettes directory with the 
  `append_vignette` function.

* `QC` quick convert between Pandoc recognized languages.  Meant for a string 
  rather than an entire file.

* `FT` a new function that detects face, color and size and generates a font tag 
  for supplied text.

* `YT` and `VM` are new functions that wraps a YouTube/Vimeo tag or url to 
  generate an HTML iframe tag.

* `IF` a new function that wraps a url to generate an HTML iframe tag.  This is 
  useful for embedding a document within another document.

* `IM`, `IM2` and `IW` new functions that wraps an image path or url to generate  
  a flexible HTML tag with optional hyperlink.  `IM2` is a wrapper for `IM` and 
  assumes the location of the local image file is in "assets/img/", meaning the 
  user only needs supply the name of the local image. `IW` allows for text 
  wrapping of an image.

* The cite family of functions has been introduced.  These functions provide 
  quick pulling of quotes with citations (in Markdown or LaTeX format).  See 
  `?citeL` for more details.

* Conversion functions: `md2docx`, `md2pdf`, `tex2html`, `html2pdf`, `tex2docx`, 
  `md2tex` have been added for a convenient way to convert between file types.

* `rdirs` reclusively creates multiple directories.

* The sync family of functions has been introduced.  These functions provide 
  quick syncing between directories.  See `?sync` for more details.

* `custom_css` function for creating the components necessary for custom css 
  file for use with RStudio + knitr.  See: 
  http://www.rstudio.com/ide/docs/authoring/markdown_custom_rendering


MINOR FEATURES

* `notes` is a function that allows the user to view a truncated version of the 
  notes located in the ARTICLES directory.

* `BV` a function, bibliography viewer, that allows the user to view their 
  bibiliographs stored in the .bib file.

* A new template `wordpress_web` has been added.

* `SC` a function to quickly generate special HTML characters.

* `PN` a function to quickly add presenter notes to an HTML slide.

* `HR`, `HR2` `BT` and `EM` functions for quick html hyperrefs, button hyperrefs 
  and email hyperrefs..

* spacing functions `HS` (horizontal space) and `VS` (vertical space) have been 
  added.

* `CN` a new function that wraps text with courier new HTML a font tag for 
  supplied text.

* `WP` a convenience function for formulating Windows paths correctly.

* `slidify_layouts` function to generate html files for extra slidify slide 
  layouts.  By defualt `new_report` and `presentation` will utilize
  `slidify_layouts` to generate the appropriate files.

* The project creating functions (`presentation`, `new_report` and 
  `new_vignette`) gain an `open` argument to open the directory in RStudio after 
  it has been created.


IMPROVEMENTS

* `GQ`, `CA`, `LL` and `US` parse out both "fi" and "fl" ligatures more 
  effectively, whereas previously "fi" was assumed if a ligature was detected.

* `new_report` now allows global options to be set that allows numeric values to 
  be passed to the template option like speed dial. See ?new_report for more.

* The reports package now has a staticdocs generated help site: 
  http://trinker.github.io/reports/
  
* The reports package now has a related programs/packages help site: 
  http://trinker.github.io/reports/dependencies

* reports gains an HTML vignette (with video explanations) to better explain 
  package use.  Use `browseVignettes(package = 'reports')` to view the vignette.

CHANGES

* `html5` and `reveal.js` are now Deprecated.  The birth of the slidify package 
  makes maintaining these functions pointless.  These functions will be removed 
  from the reports package upon the next update.

* `US` has been renamed to `UF` (underscore fill) and gets a counterpart `PF` 
  (percent fill) for URL blank spaces.  `US` is now Depracated and will be 
  removed in the next update.

* All options now have a consistent naming scheme (period separator)

    ~bib.loc-The path to the users primary .bib file (I store this in DropBox)
    ~name.reports-The name that will be automatically added to a report/presentation
    ~sources.reports-Path(s) to additional scripts to be sourced on project startup
    ~temp.reports-The primary template to use to generate reports (see template)
    ~github.user-GitHub user name
    ~speed.temp-A speed dial like interface that allows the template argument to take a number
    ~slidify.template-Path to, or defualt, .Rmd file tempalte for use in as the .Rmd used in the slidify presentations
    ~revealjs.loc~The path to the user’s reveal.js full install

* The NOTES.txt and TO_DO.txt files lose the file extensions to become NOTES and 
  TO_DO.


reports 0.1.2
----------------------------------------------------------------
This is a bug fix release

BUG FIXES

* `GQ` did not handle block quotes correctly in what printed to the console.  
  This behavior has been corrected.

* `GQ`, `CA`, `LL` and `US` now parse out ligature "fi" as "fi" rather than "?".  
  Warnings were added for cases when liigatures are detected.

* Fixed the Description file for the basic_web template (missing info).

* `reveal.js` resulted in an error if a reference page was not supplied.   This 
  behavior has been corrected.

CHANGES

* `install_pandoc` is no longer exported as this functionality can be found in 
  the installr package


reports 0.1.1
----------------------------------------------------------------

* The first CRAN installation of the reports package

* Package designed to provide efficient work flow in writing and presenting 
  academic articles and other reports
