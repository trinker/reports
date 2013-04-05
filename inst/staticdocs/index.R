library(staticdocs)
list(
  index = list(
    sd_section("Template",
      "Functions to generate a report templates.",  
      c(
        "new_report",
        "presentation",
        "template"
      )
    ),
    sd_section("Text Formatting",
      "Functions to format text.",  
      c(
        "CA",
        "CN",
        "CW",
        "FT",
        "GQ",
        "IM",
        "LL",
        "QC",
        "US",
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
        "rdirs"
      )
    ),
    sd_section("Document Conversion",
      "Functions to reshape data.",
      c(
        "md2docx",
        "md2tex",
        "tex2docx",
        "tex2html"
      )
    ),
    sd_section("USer Defined Templates", 
      "Function to create user defined template",
      c(
        "doc_temp"
      )
    ),
    sd_section("Decapricated",
      "Functions that will eventually be removed from the reports package.",
      c(
      "html5",
      "reveal.js"
      )
    )

))

