options(WordpressLogin = c(USERNAME = "PASSWORD"), WordpressURL = "http://INSERT_WP_URL_HERE/xmlrpc.php")
# install.packages("RWordPress", repos = "http://www.omegahat.org/R", type = "source")

library(pacman); p_load(knitr, RWordPress)
knit2wp("yourfile.Rmd", title = "TITLE")