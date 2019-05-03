######### MASTER FILE ##############

###################################
# By: D. Uriarte & B. Teruya


library(rmarkdown)
library(here)
library(RStata)

stata_route <- "C:/Stata15/StataSE-64"
options("RStata.StataPath" = stata_route)
options("RStata.StataVersion" = 15)



render(input = here("coding-files", "0_data-cleaning.Rmd"))
render(input = here("coding-files", "1_analysis_employment.Rmd"))
render(input = here("coding-files", "2_analysis_wages.Rmd"))
stata(here("coding-files", "3.1_meta_analysis.do"))
# stata(here("coding-files", "3.2_meta_analysis_conditional_work.do"))

  