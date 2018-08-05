library(tidyverse)
library(data.table)
library(xml2)

generate.date.sequence = function(years, months) {
    retter = expand.grid(years = years,
                months = months)
    as.Date(paste(retter$years, retter$months, "01",sep="-"))
}

# every 5 years till 1960
all.dates = generate.date.sequence(c(1800, seq(1900,1960, by = 5)), 1)

# every year till 1980
all.dates = c(all.dates, 
              generate.date.sequence(1961:1980, 1))

# every 3 months till 1995
all.dates = c(all.dates, 
              generate.date.sequence(1981:1989,c(1,4,7,10)))

# every month to 2018
all.dates = c(all.dates, 
              generate.date.sequence(1990:2018, 1:12))



format.date = function(date) {
    paste(format(date,"%d"),
          format(date,"%m"),
          format(date, "%Y"),
          sep="%2F")
}


all.scrapes = expand.grid(
    letters = LETTERS,
    years = all.dates
) %>%
    arrange(letters,years) %>%
    mutate(yearsfrom = lag(years)) %>%
    na.omit() %>%
    mutate(url = paste0("https://app.companiesoffice.govt.nz/companies/app/ui/pages/companies/search?q=",
                        letters,
                        "&entityTypes=ALL&entityStatusGroups=ALL&incorpFrom=",
                        format.date(yearsfrom),
                        "&incorpTo=",
                        format.date(years),
                        "&addressTypes=ALL&addressKeyword=&start=0&limit=1000&sf=&sd=&",
                        "advancedPanel=true&mode=advanced#results")) %>%
    mutate(
        save.location = paste("data", 
                              letters,
                              paste0(yearsfrom, ".csv"),
                              sep="/")
    )


get.a.links = function(url) {
    Sys.sleep(10)
    z = read_html(url)
    urls = z %>% 
        xml_find_all("//div[@class = 'dataList']/table/tbody//a[@class='link']") %>% 
        xml_attr("href")
    gsub("^.+'([^']+)'.+$","\\1",urls)   
}

scraper = function(id) {
    scrape.row = all.scrapes[id,]
    
    save.location = scrape.row$save.location
    
    if(!file.exists(save.location)) { # make sure we're not redoing old work
        to.save = data.frame(Data =get.a.links(scrape.row$url))
        
        
        fwrite(to.save,
               save.location)
        
        Sys.sleep(10 + rnorm(1))
    }
    1
}

if(!dir.exists("data")) {
    dir.create("data")
    lapply(paste("data",LETTERS,sep="/"), dir.create)
}

sapply(1:nrow(all.scrapes)
       , possibly(scraper, NA))

