#==========================
#move project directions
#==========================
outpdf <- paste0(getwd(), "/inst/extdata/docs/")
inpdf <- paste0(getwd(), "/inst/pdf_gen/REPORT_WORKFLOW_GUIDE.pdf")
file.copy(inpdf, outpdf,, TRUE)

