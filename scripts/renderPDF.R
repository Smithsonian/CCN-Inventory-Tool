## this script renders all country-level pdfs for use in app
#cheneyr@si.edu

library(tidyverse)
library(rmarkdown)
library(RefManageR)
library(readr)
tinytex::tlmgr_install("pdfcrop")

        #test render for belize 
        #render("app/country_insights.Rmd")

## 1. create list of countries included ####
allstocks <- read_csv("app/data/all_stocks_table.csv")

countries <- allstocks %>% select(country) %>% 
  na.omit() %>% distinct()

country_list <-  as.list(unique(countries$country))


## 2. function to render pdf ####
writeCountryInsights <- function(chosen_country){

  # generate country pdf
  rmarkdown::render(input = "app/data/country_insights.Rmd",
                    output_file = paste0(chosen_country, "_Country_Insights.pdf"))
}
#for some reason it doesn't like printing to the country pdfs folder, moving around manually for now 

      #TEST writeCountryInsights(chosen_country = "United States")

## 3. loop over entire 'country_list' to render pdfs ####
for (i in seq_along(country_list)) {
  
  {writeCountryInsights(country_list[i])}
}











