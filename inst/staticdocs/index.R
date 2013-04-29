library(staticdocs)
list(
  index = list(
    sd_section("Template",
      "Functions to generate report/presentation templates.",  
      c(
        "new_report",
        "presentation",
        "templates"
      )
    ),
    sd_section("Text Formatting",
      "Functions to format text.",  
      c(
        "CA",
        "CN",
        "CW",
        "GQ",
        "FT",
        "LL",
        "QC",
        "US",
        "WP"
      )
    ),
    sd_section("Insert HTML Objects",
      "Functions to format text.",  
      c(
        "HR",
        "HS",
        "IM",
        "RF",
        "RN",
        "SC",
        "YT"
      )
    ),    
    sd_section("Citations",
      "Function to assist in citing text.",
      c(
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
        "pad",
        "rdirs",
        "sync"
      )
    ),
    sd_section("Document Conversion",
      "Functions to convert documents.",
      c(
        "md2docx",
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
        "IE"
      )
    ),    
    sd_section("Decapricated",
      "Functions that will be removed from the reports package in version 0.1.4.",
      c(
      "html5",
      "reveal.js"
      )
    )

))

