library(tidyverse)
library(data.table)
library(xml2)
# library(parallel)

companies = fread("data/combined.csv",
                  select = "Data")

get.url = function(company) {
  paste0("https://app.companiesoffice.govt.nz/companies/app/ui/pages/companies/"
         ,company,
         "/detail?backurl=%2Fcompanies%2Fapp%2Fui%2Fpages%2Fcompanies%2F508")
}



if(!dir.exists("html")) {
  dir.create("html")
}


keeper = function(x) {
  z = read_html(get.url(x))
  write_html(z, paste0("html/",x,".html"))
  1
}



logger = sapply(companies$Data, 
                  possibly(keeper, NA))

fwrite(data.frame(logger), "html_log.csv")
