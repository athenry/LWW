## Install needed packages
install.packages("bibliometrix")
install.packages("tidyverse")

## Load needed packages
library(bibliometrix)
library(tidyverse)

## Read in publication, citation, use and pricing data
filePathCR <- dir("./Data/scopus_citing_pubs", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
ABCR <- convert2df(filePathCR, dbsource = "scopus", format = "csv")

filePathPubs <- dir("./Data/scopus_lww_pubs", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
ABpubs <- convert2df(filePathPubs, dbsource = "scopus", format = "csv")

price <- read.csv("./Data/lww_abbreviations.csv", header = TRUE)
price$issn <- gsub("-", "", price$issn)

LWWuse2022 <- read.csv("./Data/usage/Ovid_TR_J3_2022.csv", skip = 13, header = TRUE) %>%
  filter(Metric_Type == "Total_Item_Requests") %>%
  mutate(ISSN = if_else(Online_ISSN != "", Online_ISSN, Print_ISSN), .keep = "unused", .after = Title) %>%
  group_by(ISSN) %>%
  summarise(Reporting_Period_Total = sum(Reporting_Period_Total))
LWWuse2022$ISSN <- gsub("-", "", LWWuse2022$ISSN)

LWWuse2021 <- read.csv("./Data/usage/Ovid_TR_J3_2021.csv", skip = 13, header = TRUE) %>%
  filter(Metric_Type == "Total_Item_Requests") %>%
  mutate(ISSN = if_else(Online_ISSN != "", Online_ISSN, Print_ISSN), .keep = "unused", .after = Title) %>%
  group_by(ISSN) %>%
  summarise(Reporting_Period_Total = sum(Reporting_Period_Total))
LWWuse2021$ISSN <- gsub("-", "", LWWuse2021$ISSN)

LWWuse2020 <- read.csv("./Data/usage/Ovid_TR_J3_2020.csv", skip = 13, header = TRUE) %>%
  filter(Metric_Type == "Total_Item_Requests") %>%
  mutate(ISSN = if_else(Online_ISSN != "", Online_ISSN, Print_ISSN), .keep = "unused", .after = Title) %>%
  group_by(ISSN) %>%
  summarise(Reporting_Period_Total = sum(Reporting_Period_Total))
LWWuse2020$ISSN <- gsub("-", "", LWWuse2020$ISSN)

## Determine counts by ISSN

ABauthorship <- ABpubs %>%
  group_by(ISSN) %>%
  summarise(pubs=n())


ABcitations <- ABCR %>%
  select(PY, DI, CR, UT) %>%  ##keep publication year in case we want to consider timeliness of citation behaviour
  rename(CY = PY) %>% ## rename PY (publication year) to CY (year of citation)
  separate_longer_delim(CR, delim = ";") %>% ## break up the reference field so each reference is on one line
  mutate(id_ref = 1:n()) %>% ## add an identifier for each refefence
  filter(str_detect(CR, str_c(price$abbreviation, collapse = "|"))) %>% ## keep only the rows that reference a LWW title
  mutate(title = str_extract(CR, str_c(price$abbreviation, collapse = "|"))) ## extract the matching title into a new column

CiteUse <- ABcitations %>%
  group_by(title) %>%
  summarise(cites=n())
  
PubUse <- full_join(price, ABauthorship, join_by(issn == ISSN), keep = FALSE, na_matches = "never")

InterimUse <- full_join(PubUse, CiteUse, join_by(abbreviation == title), keep = FALSE, na_matches = "never")
