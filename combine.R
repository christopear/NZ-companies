library(tidyverse)
library(data.table)

all.files = list.files("data",recursive = TRUE, full.names = TRUE, pattern="csv")

my.fread = function(loc) {
  fread(loc) %>%
    mutate(Location = loc)
}

companies = rbindlist(lapply(all.files, my.fread),
                      fill = TRUE,
                      use.names = TRUE) %>%
  group_by(Location) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(keepers = !duplicated(Data))


assert = function(assert.n = 1000) {
  to.warn = companies %>% filter(n >= assert.n) %>% select(Location) %>% unique()
  
  if(any(companies$n >= assert.n)) {
    warning(paste("Might need to rescrape:",
                  paste(to.warn$Location, collapse=", ")))
    
  } else {
    message("Success: no missing companies")
  }
  to.warn$Location
}

# check that we got everything
assert()


companies %>% 
  filter(keepers) %>%
  select(-keepers,
         -n) %>%
  fwrite("combined.csv")

