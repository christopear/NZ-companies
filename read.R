library(tidyverse)
library(data.table)
library(xml2)

# functions ---------

get.company.number = function(x) {
    retter = x[grepl("^Company number: ",x)]
    retter = gsub("^Company number: ([0-9]+)$","\\1",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.director.count = function(x) {
    retter = x[grepl("^Directors Showing",x)]
    retter = gsub("^.+([0-9]+) directors$","\\1",retter) %>%
        as.numeric()
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.NZBN = function(x) {
    retter = x[grepl("^NZBN: ",x)]
    retter = gsub("^NZBN: ([0-9]+)$","\\1",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.inc.date = function(x) {
    retter = x[grepl("^Incorporation Date:",x)]
    retter = gsub("^Incorporation Date: ","",retter) %>%
        as.Date(format="%d %b %Y")
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.close.date = function(x) {
  # Registered from 26 Jan 1973 to 27 Jul 1992
  retter = x[grepl("^Registered from.+ to ",x)]
  retter = gsub("^Registered from.+ to ","",retter) %>%
    as.Date(format="%d %b %Y")
  
  if(length(retter) == 0) {
    retter = NA
  }
  retter
}

get.company.status = function(x) {
    retter = x[grepl("^Company Status: ",x)]
    retter = gsub("Company Status: ","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.entity.type = function(x) {
    retter = x[grepl("^Entity type: ",x)]
    retter = gsub("Entity type: ","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.constitution.filed = function(x) {
    retter = x[grepl("^Constitution filed: ",x)]
    retter = gsub("Constitution filed: ","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.FRA.reporting.month = function(x) {
    retter = x[grepl("^FRA Reporting Month: ",x)]
    retter = gsub("FRA Reporting Month: ","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.GUP = function(x) {
    retter = x[grepl("^Ultimate holding company",x)]
    retter = gsub("^Ultimate holding company (.+) Edit$","\\1",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.address = function(x) {
    retter = x[grepl("^Company addresses: ", x)]
    retter = gsub("^Company addresses: Registered Office ","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.company.name = function(datum) {
    retter = datum %>% xml_find_first("//h1") %>%
        xml_text()
    retter = gsub(" ?[(][0-9]+[)].+$","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.summary = function(datum) {
    retter = datum %>% xml_find_all(paste0("//div[contains(@class, 'panelContent')]",
                                  "/div[@class='entityMaincol']",
                                  "/div[contains(@class,'companySummary')]",
                                  "/div")) %>%
        xml_text() %>%
        iconv(to="UTF-8") %>%
        iconv(to="ASCII",sub="@@")
    
    retter = gsub("\n|\r|\t|@@"," ",retter)
    retter = gsub(" +"," ",retter) %>%
        trimws()
    
    CompanyNumber = get.company.number(retter)
    Directors = get.director.count(retter)
    NZBN = get.NZBN(retter)
    IncorporationDate = get.inc.date(retter)
    ShutdownDate = get.close.date(retter)
    Status = get.company.status(retter)
    EntityType = get.entity.type(retter)
    Constitution = get.constitution.filed(retter)
    FRAMonth = get.FRA.reporting.month(retter)
    GUP = get.GUP(retter)
    Address = get.address(retter)
    
    data.frame(
      CompanyNumber,
      Directors,
      NZBN,
      IncorporationDate,
      ShutdownDate,
      Status,
      EntityType,
      Constitution,
      FRAMonth,
      GUP,
      Address
    )
}


get.trading.name = function(x) {
    retter = x[grepl("^Trading Name:",x)]
    retter = gsub("^Trading Name:\r\n","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.phone.number = function(x) {
    retter = x[grepl("^Phone Number:",x)]
    retter = gsub("^Phone Number:\r\n","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.email = function(x) {
    retter = x[grepl("^Email Address:",x)]
    retter = gsub("^Email Address:\r\n","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
}

get.website = function(x) {
    retter = x[grepl("^Website:",x)]
    retter = gsub("^Website:\r\n","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
    
}

get.industry = function(x) {
    retter = x[grepl("^Industry Classification:",x)]
    retter = gsub("^Industry Classification:\r\n","",retter)
    
    if(length(retter) == 0) {
      retter = NA
    }
    retter
    
}



get.voluntary.details = function(datum) {
    retter = datum %>% xml_find_all("//div[contains(@class, 'panelContent')]/div[@id='rightcol']/table//label[contains(@class, 'SCR')]/../..") %>% xml_text()
    
    data.frame(
        TradingName = get.trading.name(retter),
        PhoneNumber = get.phone.number(retter),
        Email = get.email(retter),
        Website = get.website(retter),
        Industry = get.industry(retter)
    )
}



get.current.directors = function(datum) {
    # https://app.companiesoffice.govt.nz/companies/app/ui/pages/companies/1830488/detail?backurl=%2Fcompanies%2Fapp%2Fui%2Fpages%2Fcompanies%2F508
}

get.former.directors = function(datum) {
    # https://app.companiesoffice.govt.nz/companies/app/ui/pages/companies/1830488/detail?backurl=%2Fcompanies%2Fapp%2Fui%2Fpages%2Fcompanies%2F508
}

get.current.shareholders = function(datum) {
    # https://app.companiesoffice.govt.nz/companies/app/ui/pages/companies/1830488/detail?backurl=%2Fcompanies%2Fapp%2Fui%2Fpages%2Fcompanies%2F508
  retter = datum %>% xml_find_all("//div[@id='allocations']/div[@class='allocationDetail']")
}

get.shares.count = function(datum) {
  retter = datum %>% xml_find_first("//div[@class='allocations']/span")
}

get.url = function(company) {
  paste0("https://app.companiesoffice.govt.nz/companies/app/ui/pages/companies/"
         ,company,
         "/detail?backurl=%2Fcompanies%2Fapp%2Fui%2Fpages%2Fcompanies%2F508")
}


get.company = function(company) {
  # System.sleep(10 + rnorm(1))
  
  url = get.url(company)
  
  z = read_html(url)
  
  cbind(CompanyName = z %>% get.company.name(),
        z %>% get.summary(),
        z %>% get.voluntary.details())
}

# run it ------------


companies = fread("combined.csv")
sample.id = sample(companies$Data, 100)

get.url(sample.id)
details = rbindlist(
  lapply(sample.id, get.company),
  fill=TRUE,
  use.names = TRUE)

