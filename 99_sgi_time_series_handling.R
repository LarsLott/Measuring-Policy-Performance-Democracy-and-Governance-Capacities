#### "Measuring Policy Performance, Democracy, and Governance Capacities: 
####  A Conceptual and Methodological Assessment of the Sustainable Governance Indicators (SGI)" ####
# authors: "Aurel Croissant and Lars Pelke"
# date: 2022-03-04
# written under "R version 4.1.2 (2021-11-01)"


#### Preliminaries ####

R.version$version.string

#### Hardware Information ####

Sys.info()

#sysname        release        version           machine      
#"Windows"      #"10 x64"  "build 19042"         "x86-64"  


# clear workspace
rm(list=ls())

#libraries
library(tidyverse)
library(ggpubr)
library(countrycode)
library(DiagrammeR)

library(sjPlot)
library(stargazer)
library(readstata13)
library(readxl)
library(MCMCpack)
library(ggrepel)
library(htmlwidgets)
library(webshot)


# set working directory
# please use the working directory, where you stored the zip-file. 


#### Load SGI Excel Data ####

sgi_2020 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2020 Scores")
sgi_2019 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2019 Scores")
sgi_2018 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2018 Scores")
sgi_2017 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2017 Scores")
sgi_2016 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2016 Scores")
sgi_2015 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2015 Scores")
sgi_2014 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2014 Scores")

sgi_2020 <- sgi_2020 %>%
  dplyr::select(-c(starts_with("..."))) %>%
  mutate(year = 2020) %>%
  rename(cname = starts_with("SGI"))

sgi_2019 <- sgi_2019 %>%
  dplyr::select(-c(starts_with("..."))) %>%
  mutate(year = 2019) %>%
  rename(cname = starts_with("SGI"))

sgi_2018 <- sgi_2018 %>%
  dplyr::select(-c(starts_with("..."))) %>%
  mutate(year = 2018) %>%
  rename(cname = starts_with("SGI"))

sgi_2017 <- sgi_2017 %>%
  dplyr::select(-c(starts_with("..."))) %>%
  mutate(year = 2017) %>%
  rename(cname = starts_with("SGI"))

sgi_2016 <- sgi_2016 %>%
  dplyr::select(-c(starts_with("..."))) %>%
  mutate(year = 2016) %>%
  rename(cname = starts_with("SGI"))

sgi_2015 <- sgi_2015 %>%
  dplyr::select(-c(starts_with("..."))) %>%
  mutate(year = 2015) %>%
  rename(cname = starts_with("SGI"))

sgi_2014 <- sgi_2014 %>%
  dplyr::select(-c(starts_with("..."))) %>%
  mutate(year = 2014) %>%
  rename(cname = starts_with("SGI"))

sgi_timeseries <- rbind(sgi_2020, sgi_2019, sgi_2018, sgi_2017, sgi_2016, sgi_2015, sgi_2014)

sgi_timeseries <- sgi_timeseries %>%
  arrange(cname, year) %>%
  relocate(cname, year, .before = starts_with("Policy"))

#### Delete variables that shows the rangs #### 

sgi_timeseries <- sgi_timeseries %>%
  dplyr::select(!starts_with("Rank among 41"))

#### Save Cross-national time-series dataset in different formats ####

saveRDS(sgi_timeseries, "sgi_timeseries.rds") # RDS-File
write.csv(sgi_timeseries, "sgi_timeseries.csv") # csv-file
save.dta13(sgi_timeseries, "sgi_timeseries.dta") # dta.13 File

