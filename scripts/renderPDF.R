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

            # countries <- allstocks %>% select(country) %>% 
            #   na.omit() %>% distinct()# 148 total countries included 

#list of all countries with tier II data in at least 1 habitat 
any_tierII_data <- allstocks %>% 
  filter(TierIorII == "Tier II") %>% 
  select(country) %>% distinct()

countries_with_NO_tierII_data <- allstocks %>% 
  filter(!TierIorII == "Tier II") %>% 
  select(country) %>% distinct()

tier3countries <- allstocks %>% 
  filter(!is.na(TierIII_mean)) %>% 
  select(country) %>% distinct()

#if case exists of tier II data, remove from NO data list 
onlytierI <- anti_join(countries_with_NO_tierII_data, any_tierII_data)

IIIoverlapI <- semi_join(tier3countries, onlytierI) #no countries with tier III values that do not have tier II 

#create lists to loop over 
tierIcountrylist <- as.list(onlytierI)
tierIIcountrylist <- as.list(any_tierII_data)

#99 countries have only tier I data --> render "country_insights_NOdata.Rmd"
#49 countries have any tier II data --> render "country_insights_data.Rmd"


## 2. function to render pdfs ####

#when data inclues tier II
writeCountryInsightsDat <- function(chosen_country){

  # generate country pdf
  rmarkdown::render(input = "app/country_insights_data.Rmd",
                    output_file = paste0(chosen_country, "_Country_Insights.pdf"))
}
#for some reason it doesn't like printing to the country pdfs folder, moving around manually for now 

      #TEST writeCountryInsights(chosen_country = "United States")


#only tier I 
writeCountryInsightsNOdat <- function(chosen_country){
  
  # generate country pdf
  rmarkdown::render(input = "app/country_insights_NOdata.Rmd",
                    output_file = paste0(chosen_country, "_Country_Insights.pdf"))
}



## 3. loop over each 'country_list' to render pdfs ####

#for countries with only tier I data 
for (i in seq_along(tierIcountrylist)) {
  
  {writeCountryInsightsNOdat(tierIcountrylist[i])}
}


#for countries with available tier II data 
for (i in seq_along(tierIIcountrylist)) {
  
  {writeCountryInsightsDat(tierIIcountrylist[i])}
}



