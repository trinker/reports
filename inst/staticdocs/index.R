library(staticdocs)
list(
  index = list(
    sd_section("Template",
      "Functions to generate report/presentation templates.",  
      c(
        "new_report",
        "new_vignette",
        "presentation",
        "templates"
      )
    ),
    sd_section("Text Formatting",
      "Functions to format text.",  
      c(
        "CA",
        "CW",
        "GQ",
        "LL",
        "QC"
      )
    ),
    sd_section("Path (Text String) Formatting",
      "Functions to format character strings related to paths/urls.",  
      c(
        "pad",
         "QP",
        "UF",
        "WP"
      )
    ),    
    sd_section("Insert HTML Objects",
      "Functions to format text.",  
      c(
      	"AD",
        "CN",      	
      	"EM",
        "FT",        
        "HR",
        "HL",
        "HS",
        "IF", 
        "IM",
        "PN",
        "RF",        
        "SC",
        "TB",
        "YT",
        "col2hex"
      )
    ),    
    sd_section("Citations",
      "Function to assist in citing text.",
      c(
      	"BV",
        "citeL",
        "notes",
        "parenciteL",
        "parenciteM",
        "posciteL",
        "possciteL",
        "textciteL",
        "textciteM",
        "update_bib"
      )
    ),
    sd_section("Directory Management", 
      "Functions to aid in managing directories/folders.", 
      c(
        "delete",
        "local_host",
        "repo2github",
        "rdirs",
        "sync"
      )
    ),
    sd_section("Document Conversion",
      "Functions to convert documents.",
      c(
      	"html2pdf",
        "md2docx",
      	"md2pdf",
        "md2tex",
        "tex2docx",
        "tex2html"
      )
    ),
    sd_section("User Defined Templates", 
      "Function to create user defined template.",
      c(
        "doc_temp"
      )
    ),
    sd_section("reports Tools", 
      "Exported tools used within reports.",
      c(
        "IE",
        "is.global",
        "is.url"
      )
    ),   
    sd_section("Customization", 
      "Tools for customized css and layouts.",
      c(
        "custom_css",
      	"js_copy", 
        "slidify_layouts"
      )
    ),          
    sd_section("Decapricated",
      "Functions that will be removed from the reports package in version 2.0.0.",
      c(
      "html5",
      "reveal.js"
      )
    )

))

