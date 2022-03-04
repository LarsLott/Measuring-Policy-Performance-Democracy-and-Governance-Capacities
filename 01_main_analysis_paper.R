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
library("readxl")
library(MCMCpack)
library(ggrepel)
library(htmlwidgets)
library(webshot)


# set working directory
# please use the working directory, where you stored the zip-file. 


##################################################################################
##################################################################################
#### Data Management Tasks ####
##################################################################################
##################################################################################

#### Load different data sets ####

bti_timeseries <- read.dta13("calculations/data/bti/BTI 2006-2020.dta")
summary(bti_timeseries)

wgi_timeseries <- read.dta13("calculations/data/wgi/wgidataset.dta") 
summary(wgi_timeseries)

vdem_timeseries <- readRDS("calculations/data/vdem/V-Dem-CY-Full+Others-v11.rds")
summary(vdem_timeseries$year)

sgi_2020 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2020 Scores", na = "n/a")
sgi_2019 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2019 Scores", na = "n/a")
sgi_2018 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2018 Scores", na = "n/a")
sgi_2017 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2017 Scores", na = "n/a")
sgi_2016 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2016 Scores", na = "n/a")
sgi_2015 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2015 Scores", na = "n/a")
sgi_2014 <- read_excel("calculations/data/sgi/SGI2020_Scores.xlsx", sheet = "SGI 2014 Scores", na = "n/a")

sgi_2020 <- sgi_2020 %>%
  dplyr::select(-c("...4", "...5", "...8", "...9", "...58", "...61", "...62")) %>%
  mutate(year = 2020) %>%
  rename(cname = starts_with("SGI"))

sgi_2019 <- sgi_2019 %>%
  dplyr::select(-c("...4", "...5", "...8", "...9", "...58", "...61", "...62")) %>%
  mutate(year = 2019) %>%
  rename(cname = starts_with("SGI"))

sgi_2018 <- sgi_2018 %>%
  dplyr::select(-c("...4", "...5", "...8", "...9", "...58", "...61", "...62")) %>%
  mutate(year = 2018) %>%
  rename(cname = starts_with("SGI"))

sgi_2017 <- sgi_2017 %>%
  dplyr::select(-c("...4", "...5", "...8", "...9", "...58", "...61", "...62")) %>%
  mutate(year = 2017) %>%
  rename(cname = starts_with("SGI"))

sgi_2016 <- sgi_2016 %>%
  dplyr::select(-c("...4", "...5", "...8", "...9", "...58", "...61", "...62")) %>%
  mutate(year = 2016) %>%
  rename(cname = starts_with("SGI"))

sgi_2015 <- sgi_2015 %>%
  dplyr::select(-c("...4", "...5", "...8", "...9", "...58", "...61", "...62")) %>%
  mutate(year = 2015) %>%
  rename(cname = starts_with("SGI"))

sgi_2014 <- sgi_2014 %>%
  dplyr::select(-c("...4", "...5", "...8", "...9", "...58", "...61", "...62")) %>%
  mutate(year = 2014) %>%
  rename(cname = starts_with("SGI"))

sgi_timeseries <- rbind(sgi_2020, sgi_2019, sgi_2018, sgi_2017, sgi_2016, sgi_2015, sgi_2014)

sgi_timeseries <- sgi_timeseries %>%
  arrange(cname, year) %>%
  relocate(cname, year, .before = starts_with("Policy"))

sgi_timeseries <- sgi_timeseries %>%
  dplyr::select(!starts_with("...")) %>%
  dplyr::select(!starts_with("Rank among 41"))

##################################################################################
##################################################################################
#### Tables Main Paper ####
##################################################################################
##################################################################################


## Define Bayesian Factor Analysis Values ####

ITER <- 100
BURNIN <- 10000
MCMC <- 10000
THIN <- 200


#### Table 1: Conceptual alignment across SGI Policy Performance components (BFA Estimates). Attribute Measure Loadings (Λ) Uniqueness (Ψ) ####

##Economic Policies ##

posterier_economic_policy <- MCMCfactanal(~`Economy` + `Labor Markets` + Taxes + Budgets + `Research, Innovation and Infrastructure` + `Global Financial System`, 
                                          factors=1,
                                          verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                          data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.economic_policy <- data.frame(summary(posterier_economic_policy)$statistics[1:6])
names(loadings.economic_policy)[1] <- "Loadings"
loadings.economic_policy

## Second, Uniqueness ##
uniquenesses.economic_policy <- data.frame(summary(posterier_economic_policy)$statistics[7:12])
names(uniquenesses.economic_policy)[1] <- "uniquenesses"
uniquenesses.economic_policy

economic_policy.df <- data.frame(Attribute = "Economic Policies", 
                                 Measure = c("Economy", "Labor Markets", "Taxes", "Budgets", "Research, Innovation and Infrastruture", 
                                             "Global Financial System"), 
                                 Loadings = loadings.economic_policy, 
                                 Uniqueness = uniquenesses.economic_policy)


## Social Policies ##

posterier_social_policies <- MCMCfactanal(~`Education` + `Social Inclusion` + Health + Families + Pensions + `Integration` +
                                            `Safe Living` + `Global Inequalities`, factors=1,
                                          verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                          data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.social_policies <- data.frame(summary(posterier_social_policies)$statistics[1:8])
names(loadings.social_policies)[1] <- "Loadings"
loadings.social_policies

## Second, Uniqueness ##
uniquenesses.social_policies <- data.frame(summary(posterier_social_policies)$statistics[9:16])
names(uniquenesses.social_policies)[1] <- "uniquenesses"
uniquenesses.social_policies

social_policies.df <- data.frame(Attribute = "Social Policies", 
                                 Measure = c("Education", "Social Inclusion", "Health", "Families", "Pensions", 
                                             "Integration", "Safe Living", "Global Inequalities"), 
                                 Loadings = loadings.social_policies, 
                                 Uniqueness = uniquenesses.social_policies)

## Environmental Policies ##

posterier_environmental_policies <- MCMCfactanal(~`Environmental Policy` + `Global Environmental Protection`, factors=1,
                                                 verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                                 data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.environmental_policies <- data.frame(summary(posterier_environmental_policies)$statistics[1:2])
names(loadings.environmental_policies)[1] <- "Loadings"
loadings.environmental_policies

## Second, Uniqueness ##
uniquenesses.environmental_policies <- data.frame(summary(posterier_environmental_policies)$statistics[3:4])
names(uniquenesses.environmental_policies)[1] <- "uniquenesses"
uniquenesses.environmental_policies

environmental_policies.df <- data.frame(Attribute = "Environmental Policies", 
                                        Measure = c("Environmental Policy", "Global Environmental Protection"), 
                                        Loadings = loadings.environmental_policies, 
                                        Uniqueness = uniquenesses.environmental_policies)

## Table 1 ##

bfa.table_policy_performance <- rbind(economic_policy.df, social_policies.df, environmental_policies.df)

bfa.table_policy_performance$Loadings <- round(bfa.table_policy_performance$Loadings , digits = 3)
bfa.table_policy_performance$uniquenesses <- round(bfa.table_policy_performance$uniquenesses , digits = 3)

stargazer(bfa.table_policy_performance,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="bfa.table_policy_performance_attributes.doc")


#### Two factor Solution Policy Performance (Table A5) ####

## Economic Policies ##

posterier_economic_policy <- MCMCfactanal(~`Economy` + `Labor Markets` + Taxes + Budgets + `Research, Innovation and Infrastructure` + `Global Financial System`, 
                                          factors=2,
                                          verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                          data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.economic_policy <- data.frame(summary(posterier_economic_policy)$statistics[1:12])
names(loadings.economic_policy)[1] <- "Loadings"
loadings.economic_policy

loadings.economic_policy <- loadings.economic_policy %>%
  mutate(factor = c(1,2,1,2,1,2,1,2,1,2,1,2),
         variable = c("Economy", "Economy", "Labor Markets","Labor Markets",
                      "Taxes", "Taxes","Budgets", "Budgets",
                      "Research, Innovation and Infrastruture", "Research, Innovation and Infrastruture", 
                      "Global Financial System", "Global Financial System")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)


## Second, Uniqueness ##
uniquenesses.economic_policy <- data.frame(summary(posterier_economic_policy)$statistics[13:18])
names(uniquenesses.economic_policy)[1] <- "uniquenesses"
uniquenesses.economic_policy

economic_policy.df <- cbind(loadings.economic_policy, uniquenesses.economic_policy)


## Social Policies ##

posterier_social_policies <- MCMCfactanal(~`Education` + `Social Inclusion` + Health + Families + Pensions + `Integration` +
                                            `Safe Living` + `Global Inequalities`, factors=2,
                                          verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                          data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.social_policies <- data.frame(summary(posterier_social_policies)$statistics[1:16])
names(loadings.social_policies)[1] <- "Loadings"
loadings.social_policies

loadings.social_policies <- loadings.social_policies %>%
  mutate(factor = c(1,2,1,2,1,2,1,2,1,2,1,2, 1,2, 1,2),
         variable = c("Education", "Education", "Social Inclusion","Social Inclusion",
                      "Health", "Health", "Families", "Families", "Pensions", "Pensions", 
                      "Integration", "Integration", "Safe Living","Safe Living",
                      "Global Inequalities",  "Global Inequalities")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)

## Second, Uniqueness ##
uniquenesses.social_policies <- data.frame(summary(posterier_social_policies)$statistics[17:24])
names(uniquenesses.social_policies)[1] <- "uniquenesses"
uniquenesses.social_policies

social_policies.df <- cbind(loadings.social_policies, uniquenesses.social_policies)

## Environmental Policies ##

posterier_environmental_policies <- MCMCfactanal(~`Environmental Policy` + `Global Environmental Protection`, factors=2,
                                                 verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                                 data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.environmental_policies <- data.frame(summary(posterier_environmental_policies)$statistics[1:4])
names(loadings.environmental_policies)[1] <- "Loadings"
loadings.environmental_policies


loadings.environmental_policies <- loadings.environmental_policies %>%
  mutate(factor = c(1,2,1,2),
         variable = c("Environmental Policy", "Environmental Policy",
                      "Global Environmental Protection", "Global Environmental Protection")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)

## Second, Uniqueness ##
uniquenesses.environmental_policies <- data.frame(summary(posterier_environmental_policies)$statistics[3:4])
names(uniquenesses.environmental_policies)[1] <- "uniquenesses"
uniquenesses.environmental_policies

environmental_policies.df <- cbind(loadings.environmental_policies,uniquenesses.environmental_policies )

## Table 1 ##

bfa.table_policy_performance <- rbind(economic_policy.df, social_policies.df, environmental_policies.df)

bfa.table_policy_performance$`1` <- round(bfa.table_policy_performance$`1` , digits = 3)
bfa.table_policy_performance$`2` <- round(bfa.table_policy_performance$`2` , digits = 3)

bfa.table_policy_performance$uniquenesses <- round(bfa.table_policy_performance$uniquenesses , digits = 3)

stargazer(bfa.table_policy_performance,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="bfa.table_policy_performance_attributes_two_factor_solution.doc")


#### Table 2 Conceptual alignment across SGI Policy Performance attributes (BFA Estimates). ####

## Policy Performance ##
posterier_policy_performance <- MCMCfactanal(~`Economic Policies` + `Social Policies` + `Environmental Policies`, factors=1,
                                            verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                            data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.policy_performance <- data.frame(summary(posterier_policy_performance)$statistics[1:3])
names(loadings.policy_performance)[1] <- "Loadings"
loadings.policy_performance

## Second, Uniqueness ##
uniquenesses.policy_performance <- data.frame(summary(posterier_policy_performance)$statistics[4:6])
names(uniquenesses.policy_performance)[1] <- "uniquenesses"
uniquenesses.policy_performance

policy_performance.df <- data.frame(Measure = c("Economic Policies", "Social Policies", 
                                               "Environmental Policies"), 
                                   Loadings = loadings.policy_performance, 
                                   Uniqueness = uniquenesses.policy_performance)

#Table 2# 
stargazer(policy_performance.df,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="bfa.table_Policy_Performance_Index.doc")

## Two-Factor Solution (Table A6) ##

posterier_policy_performance_2 <- MCMCfactanal(~`Economic Policies` + `Social Policies` + `Environmental Policies`, 
                                               factors=2,
                                             verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                             data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.policy_performance <- data.frame(summary(posterier_policy_performance_2)$statistics[1:6])
names(loadings.policy_performance)[1] <- "Loadings"
loadings.policy_performance

loadings.policy_performance <- loadings.policy_performance %>%
  mutate(factor = c(1,2,1,2,1,2), 
         variable = c("Economic Policies","Economic Policies","Social Policies","Social Policies",
                      "Environmental Policies", "Environmental Policies")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)

## Second, Uniqueness ##
uniquenesses.policy_performance <- data.frame(summary(posterier_policy_performance_2)$statistics[7:9])
names(uniquenesses.policy_performance)[1] <- "uniquenesses"
uniquenesses.policy_performance

policy_performance.df <- cbind(loadings.policy_performance, uniquenesses.policy_performance)

#Table A6# 
stargazer(policy_performance.df,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="bfa.table_Policy_Performance_Index_two_factor_solution.doc")



#### Table 3 Conceptual alignment across SGI Democratic Quality indicators (BFA Estimates). ####

## Electoral Processes ##
posterier_electoral_process <- MCMCfactanal(~`Candidacy Procedures` + `Media Access` + `Voting and Registration Rights` + `Party Financing` + `Popular Decision-Making`, 
                                            factors=2,
                                      verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                      data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.electoral_process <- data.frame(summary(posterier_electoral_process)$statistics[1:10])
names(loadings.electoral_process)[1] <- "Loadings"
loadings.electoral_process

loadings.electoral_process <- loadings.electoral_process %>%
  mutate(factor = c(1,2,1,2,1,2,1,2,1,2),
         variable = c("Candidacy Procedures","Candidacy Procedures", "Media Access",  "Media Access", 
                      "Voting and Registration Rights", "Voting and Registration Rights",
                      "Party Financing", "Party Financing", "Popular Decision-Making",
                      "Popular Decision-Making")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)

## Second, Uniqueness ##
uniquenesses.electoral_process <- data.frame(summary(posterier_electoral_process)$statistics[11:15])
names(uniquenesses.electoral_process)[1] <- "uniquenesses"
uniquenesses.electoral_process

electoral_process.df <- cbind(loadings.electoral_process,uniquenesses.electoral_process)

## Access to information ##
posterier_access_information <- MCMCfactanal(~`Media Freedom` + `Media Pluralism` + `Access to Government Information`, 
                                             factors=2,
                                            verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                            data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

loadings.access_information <- data.frame(summary(posterier_access_information)$statistics[1:6])
names(loadings.access_information)[1] <- "Loadings"
loadings.access_information

loadings.access_information <- loadings.access_information %>%
  mutate(factor = c(1,2,1,2,1,2),
         variable = c("Media Freedom", "Media Freedom", "Media Pluralism", "Media Pluralism", 
                      "Access to Government Information", "Access to Government Information")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)


uniquenesses.access_information <- data.frame(summary(posterier_access_information)$statistics[7:9])
names(uniquenesses.access_information)[1] <- "uniquenesses"
uniquenesses.access_information

access_information.df <- cbind(loadings.access_information, uniquenesses.access_information)

## Civil Rights and Political Liberties ##
posterier_cr_pr <- MCMCfactanal(~`Civil Rights` + `Political Liberties` + `Non-discrimination`, factors=2,
                                             verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                             data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

loadings.civil_rights <- data.frame(summary(posterier_cr_pr )$statistics[1:6])
names(loadings.civil_rights)[1] <- "Loadings"
loadings.civil_rights

loadings.civil_rights <- loadings.civil_rights %>%
  mutate(factor = c(1,2,1,2,1,2),
         variable = c("Civil Rights", "Civil Rights", "Political Liberties", "Political Liberties", 
                      "Non-discrimination", "Non-discrimination")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)

uniquenesses.civil_rights <- data.frame(summary(posterier_cr_pr )$statistics[7:9])
names(uniquenesses.civil_rights)[1] <- "uniquenesses"
uniquenesses.civil_rights

civil_rights.df <- cbind(loadings.civil_rights,uniquenesses.civil_rights )

## Rule of Law ##
posterier_rule_of_law <- MCMCfactanal(~`Legal Certainty` + `Judicial Review` + `Appointment of Justices` + `Corruption Prevention` , 
                                      factors=2,
                                verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

loadings.rule_of_law <- data.frame(summary(posterier_rule_of_law )$statistics[1:8])
names(loadings.rule_of_law)[1] <- "Loadings"
loadings.rule_of_law

loadings.rule_of_law <- loadings.rule_of_law %>%
  mutate(factor = c(1,2,1,2,1,2, 1,2),
         variable = c("Legal Certainty", "Legal Certainty", "Judicial Review", "Judicial Review",
                      "Appointment of Justices", "Appointment of Justices", "Corruption Prevention", 
                      "Corruption Prevention")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)


uniquenesses.rule_of_law <- data.frame(summary(posterier_rule_of_law )$statistics[9:12])
names(uniquenesses.rule_of_law)[1] <- "uniquenesses"
uniquenesses.rule_of_law

rule_of_law.df <- cbind(loadings.rule_of_law, uniquenesses.rule_of_law)

bfa.table_Quality_Democracy <- rbind(electoral_process.df, access_information.df, civil_rights.df, rule_of_law.df)

bfa.table_Quality_Democracy$`1` <- round(bfa.table_Quality_Democracy$`1` , digits = 3)
bfa.table_Quality_Democracy$`2` <- round(bfa.table_Quality_Democracy$`2` , digits = 3)
bfa.table_Quality_Democracy$uniquenesses <- round(bfa.table_Quality_Democracy$uniquenesses , digits = 3)


## Table 3 ##
stargazer(bfa.table_Quality_Democracy,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="bfa.table_Quality_Democracy_two_factor_solution.doc")


## Table 4  Conceptual alignment across SGI Democratic Quality attributes (BFA Estimates) ##

## Electoral Processes ##
posterier_democratic_quality <- MCMCfactanal(~`Electoral Processes` + `Access to Information` + `Civil Rights and Political Liberties` + `Rule of Law`, 
                                             factors=2,
                                            verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                            data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.democratic_quality <- data.frame(summary(posterier_democratic_quality)$statistics[1:8])
names(loadings.democratic_quality)[1] <- "Loadings"
loadings.democratic_quality

loadings.democratic_quality <- loadings.democratic_quality %>%
  mutate(factor = c(1,2,1,2,1,2, 1,2),
         variable = c("Electoral Processes", "Electoral Processes", "Access to Information", "Access to Information",
                      "Civil Rights and Political Liberties","Civil Rights and Political Liberties",
                      "Rule of Law", "Rule of Law")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)

## Second, Uniqueness ##
uniquenesses.democratic_quality <- data.frame(summary(posterier_democratic_quality)$statistics[9:12])
names(uniquenesses.democratic_quality)[1] <- "uniquenesses"
uniquenesses.democratic_quality

democratic_quality.df <- cbind(loadings.democratic_quality, uniquenesses.democratic_quality)


democratic_quality.df$`1` <- round(democratic_quality.df$`1` , digits = 3)
democratic_quality.df$`2` <- round(democratic_quality.df$`2` , digits = 3)
democratic_quality.df$uniquenesses <- round(democratic_quality.df$uniquenesses , digits = 3)


stargazer(democratic_quality.df,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="bfa.table_Quality_Democracy_Index_two_factr_solution.doc")

####Table 5: Conceptual alignment across SGI Governance components (BFA Estimates). ####

## Executive Capacity ##
posterier_executive_capacity <- MCMCfactanal(~`Strategic Capacity` + Implementation + Adaptability, factors=1,
                                            verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                            data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.executive_capacity <- data.frame(summary(posterier_executive_capacity)$statistics[1:3])
names(loadings.executive_capacity)[1] <- "Loadings"
loadings.executive_capacity

## Second, Uniqueness ##
uniquenesses.executive_capacity <- data.frame(summary(posterier_executive_capacity)$statistics[4:6])
names(uniquenesses.executive_capacity)[1] <- "uniquenesses"
uniquenesses.executive_capacity

executive_capacity.df <- data.frame(Attribute = "Executive Capacity", 
                                   Measure = c("Strategic Capacity", "Implemenation", 
                                               "Adaptability"), 
                                   Loadings = loadings.executive_capacity, 
                                   Uniqueness = uniquenesses.executive_capacity)


## Executive Accountability ##

posterier_executive_accountability <- MCMCfactanal(~`Citizens' Participatory Competence` + `Legislative Actors' Resources` + Media +
                                                     `Parties and Interest Associations` + `Independent Supervisory Bodies`, factors=1,
                                             verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                             data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

loadings.executive_accountability<- data.frame(summary(posterier_executive_accountability)$statistics[1:5])
names(loadings.executive_accountability)[1] <- "Loadings"
loadings.executive_accountability

uniquenesses.executive_accountability <- data.frame(summary(posterier_executive_accountability)$statistics[6:10])
names(uniquenesses.executive_accountability)[1] <- "uniquenesses"
uniquenesses.executive_accountability

executive_accountability.df <- data.frame(Attribute = "Executive Accountability", 
                                    Measure = c("Citizens' Participatory Competence", "Legislative Actors' Resources", 
                                                "Media", "Parties and Interest Associations", "Independent Supervisory Bodies"), 
                                    Loadings = loadings.executive_accountability, 
                                    Uniqueness = uniquenesses.executive_accountability)


bfa.table_Governance <- rbind(executive_capacity.df, executive_accountability.df)

bfa.table_Governance$Loadings <- round(bfa.table_Governance$Loadings , digits = 3)
bfa.table_Governance$uniquenesses <- round(bfa.table_Governance$uniquenesses , digits = 3)

## Table 5 ##
stargazer(bfa.table_Governance,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="bfa.table_Governance.doc")

#### Table A9: Bayesian Factor Analysis - Governance####

## Executive Capacity ##
posterier_executive_capacity <- MCMCfactanal(~`Strategic Capacity` + Implementation + Adaptability, 
                                             factors=2,
                                             verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                             data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.executive_capacity <- data.frame(summary(posterier_executive_capacity)$statistics[1:6])
names(loadings.executive_capacity)[1] <- "Loadings"
loadings.executive_capacity


loadings.executive_capacity <- loadings.executive_capacity %>%
  mutate(factor = c(1,2,1,2,1,2),
         variable = c("Strategic Capacity","Strategic Capacity", "Implemenation", "Implemenation", 
                      "Adaptability", "Adaptability")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)


## Second, Uniqueness ##
uniquenesses.executive_capacity <- data.frame(summary(posterier_executive_capacity)$statistics[7:9])
names(uniquenesses.executive_capacity)[1] <- "uniquenesses"
uniquenesses.executive_capacity

executive_capacity.df <- cbind(loadings.executive_capacity, uniquenesses.executive_capacity)


## Executive Accountability ##

posterier_executive_accountability <- MCMCfactanal(~`Citizens' Participatory Competence` + `Legislative Actors' Resources` + Media +
                                                     `Parties and Interest Associations` + `Independent Supervisory Bodies`, 
                                                   factors=2,
                                                   verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                                   data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

loadings.executive_accountability<- data.frame(summary(posterier_executive_accountability)$statistics[1:10])
names(loadings.executive_accountability)[1] <- "Loadings"
loadings.executive_accountability

loadings.executive_accountability <- loadings.executive_accountability %>%
  mutate(factor = c(1,2,1,2,1,2, 1,2,1,2),
         variable = c("Citizens' Participatory Competence","Citizens' Participatory Competence",
                      "Legislative Actors' Resources", "Legislative Actors' Resources", 
                      "Media","Media", "Parties and Interest Associations",  "Parties and Interest Associations",
                      "Independent Supervisory Bodies", "Independent Supervisory Bodies")) %>%
  pivot_wider(names_from = factor, values_from = Loadings)

uniquenesses.executive_accountability <- data.frame(summary(posterier_executive_accountability)$statistics[11:15])
names(uniquenesses.executive_accountability)[1] <- "uniquenesses"
uniquenesses.executive_accountability

executive_accountability.df <- cbind(loadings.executive_accountability, uniquenesses.executive_accountability)


bfa.table_Governance <- rbind(executive_capacity.df, executive_accountability.df)

bfa.table_Governance$`1` <- round(bfa.table_Governance$`1` , digits = 3)
bfa.table_Governance$`2` <- round(bfa.table_Governance$`2` , digits = 3)
bfa.table_Governance$uniquenesses <- round(bfa.table_Governance$uniquenesses , digits = 3)

## Table A9 ##
stargazer(bfa.table_Governance,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="bfa.table_Governance_two_factor_solution.doc")

#########################################################################################################
######################################   Tables Appendix ################################################
#########################################################################################################

## Table A1 ##

#not generated by statistical analysis in R


## Table A2 ##

vdem_corr <- vdem_timeseries %>%
  filter(year>=2014) %>%
  dplyr::select(country_id, country_name, year, v2x_libdem)

vdem_corr$iso3n <- countrycode(vdem_corr$country_name, origin = 'country.name', destination = 'iso3n')

sgi_corr <- sgi_timeseries %>%
  dplyr::select(cname, year,`Quality of Democracy`)

sgi_corr$iso3n <- countrycode(sgi_corr$cname, origin = 'country.name', destination = 'iso3n')


vdem_sgi_merge <- sgi_corr %>%
  left_join(vdem_corr, by = c("iso3n", "year"))

corr_table_Vdem_sgi <- vdem_sgi_merge %>%
  group_by(year) %>%
  summarise(cor_coef = cor.test(`Quality of Democracy`, v2x_libdem)$estimate,
            p_val = cor.test(`Quality of Democracy`, v2x_libdem)$p.value)

corr_table_Vdem_sgi <- corr_table_Vdem_sgi %>%
  add_row(year = 0, cor_coef = mean(corr_table_Vdem_sgi$cor_coef), p_val = NA) %>%
  rename("Correlation Coefficient" = cor_coef, 
         "P-Value" = p_val, 
         "Year" = year)

corr_table_Vdem_sgi$`Correlation Coefficient` <- round(corr_table_Vdem_sgi$`Correlation Coefficient`, digits = 3)
corr_table_Vdem_sgi$`P-Value` <- round(corr_table_Vdem_sgi$`P-Value`, digits = 7)

write.csv(corr_table_Vdem_sgi, "TableA1_correlation_Dem_SGI.csv")


## Table A3 ##

wgi_corr <- wgi_timeseries %>%
  dplyr::select(countryname, year, gee)

wgi_corr$iso3n <- countrycode(wgi_corr$countryname, origin = 'country.name', destination = 'iso3n')

wgi_corr$iso3n[wgi_corr$countryname=="Korea, Dem. Rep."] <- NA

sgi_corr <- sgi_timeseries %>%
  filter(year != 2020) %>%
  dplyr::select(cname, year,`Governance`)

sgi_corr$iso3n <- countrycode(sgi_corr$cname, origin = 'country.name', destination = 'iso3n')


wgi_sgi_merge <- sgi_corr %>%
  left_join(wgi_corr, by = c("iso3n", "year"))


corr_table_wgi_sgi <- wgi_sgi_merge %>%
  group_by(year) %>%
  summarise(cor_coef = cor.test(`Governance`, gee)$estimate,
            p_val = cor.test(`Governance`, gee)$p.value)


corr_table_wgi_sgi <- corr_table_wgi_sgi %>%
  add_row(year = 0, cor_coef = mean(corr_table_wgi_sgi$cor_coef), p_val = NA) %>%
  rename("Correlation Coefficient" = cor_coef, 
         "P-Value" = p_val, 
         "Year" = year)

corr_table_wgi_sgi$`Correlation Coefficient` <- round(corr_table_wgi_sgi$`Correlation Coefficient`, digits = 3)
corr_table_wgi_sgi$`P-Value` <- round(corr_table_wgi_sgi$`P-Value`, digits = 6)


write.csv(corr_table_wgi_sgi, "TableA2_correlation_WGI_SGI.csv")


## Table A4 ##


wgi_corr <- wgi_timeseries %>%
  dplyr::select(countryname, year, rqe)

wgi_corr$iso3n <- countrycode(wgi_corr$countryname, origin = 'country.name', destination = 'iso3n')

wgi_corr$iso3n[wgi_corr$countryname=="Korea, Dem. Rep."] <- NA

sgi_corr <- sgi_timeseries %>%
  filter(year != 2020) %>%
  dplyr::select(cname, year,`Governance`)

sgi_corr$iso3n <- countrycode(sgi_corr$cname, origin = 'country.name', destination = 'iso3n')


wgi_sgi_merge <- sgi_corr %>%
  left_join(wgi_corr, by = c("iso3n", "year"))


corr_table_wgi_sgi2 <- wgi_sgi_merge %>%
  group_by(year) %>%
  summarise(cor_coef = cor.test(`Governance`, rqe)$estimate,
            p_val = cor.test(`Governance`, rqe)$p.value)


corr_table_wgi_sgi2 <- corr_table_wgi_sgi2 %>%
  add_row(year = 0, cor_coef = mean(corr_table_wgi_sgi2$cor_coef), p_val = NA) %>%
  rename("Correlation Coefficient" = cor_coef, 
         "P-Value" = p_val, 
         "Year" = year)

corr_table_wgi_sgi2$`Correlation Coefficient` <- round(corr_table_wgi_sgi2$`Correlation Coefficient`, digits = 3)
corr_table_wgi_sgi2$`P-Value` <- round(corr_table_wgi_sgi2$`P-Value`, digits = 6)

write.csv(corr_table_wgi_sgi2, "TableA3_correlation_WGI_SGI2.csv")


##################################################################################
#### Tables Appendix Factor Analysis Indicator Levels ####
##################################################################################

## Define Bayesian Factor Analysis Values ####

ITER <- 100
BURNIN <- 10000
MCMC <- 10000
THIN <- 200

## space limitations: three examples from the Policy Performance Concept ##

#### Table A10 ####

posterier_p1_economy <- MCMCfactanal(~`Economic Policy` + `GDP per Capita` + Inflation + `Gross Fixed Capital Formation`+ 
                                       `Real Interest Rates` + `Real GDP Growth Rate`, 
                                     factors=1,
                                     verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                     data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.p1_economy <- data.frame(summary(posterier_p1_economy)$statistics[1:6])
names(loadings.p1_economy)[1] <- "Loadings"
loadings.p1_economy

## Second, Uniqueness ##
uniquenesses.p1_economy <- data.frame(summary(posterier_p1_economy)$statistics[7:12])
names(uniquenesses.p1_economy)[1] <- "uniquenesses"
uniquenesses.p1_economy

economic_p1_economy <- data.frame(Attribute = "P1 Economy", 
                                  Measure = c("Economic Policy", "GDP per Capita", "Inflation ", "Gross Fixed Capital Formation",
                                              "Real Interest Rates", "Real GDP Growth Rate"), 
                                  Loadings = loadings.p1_economy, 
                                  Uniqueness = uniquenesses.p1_economy)

## Table A10 ##
stargazer(economic_p1_economy,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="Appendix_bfa.p1_economy.doc")

## Table A11##

posterier_p7_education <- MCMCfactanal(~`Education Policy` + `Upper Secondary Attainment` + `Tertiary Attainment` + 
                                         `PISA results` + `PISA Results According to Socioeconomic Background` +
                                         `Pre-primary Education Expenditure` + `PISA Low Achievers in all Subjects` +
                                         `Less Than Upper Secondary Education by Gender`, 
                                       factors=1,
                                       verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                       data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.p7_education <- data.frame(summary(posterier_p7_education)$statistics[1:8])
names(loadings.p7_education)[1] <- "Loadings"
loadings.p7_education

## Second, Uniqueness ##
uniquenesses.p7_education <- data.frame(summary(posterier_p7_education)$statistics[9:16])
names(uniquenesses.p7_education)[1] <- "uniquenesses"
uniquenesses.p7_education

economic_p7_education <- data.frame(Attribute = "P7 Education", 
                                    Measure = c("Education Policy", "Upper Secondary Attainment", "Tertiary Attainment", 
                                                "PISA results", "PISA Results According to Socioeconomic Background",
                                                "Pre-primary Education Expenditure", "PISA Low Achievers in all Subjects", 
                                                "Less Than Upper Secondary Education by Gender"), 
                                    Loadings = loadings.p7_education, 
                                    Uniqueness = uniquenesses.p7_education)

## Table A11 ##
stargazer(economic_p7_education,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="Appendix_bfa.p7_education.doc")

## Table A12 ##

posterier_p15_environmental_policy <- MCMCfactanal(~`Environmental Policy` + `Energy Productivity` + `Greenhouse Gas Emissions` + 
                                                     `Particulate Matter` + `Biocapacity` +
                                                     `Waste Generation` + `Material Recycling` +
                                                     `Biodiversity` + `Renewable Energy` + `Material Footprint`, 
                                                   factors=1,
                                                   verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                                   data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.p15_environmental_policy <- data.frame(summary(posterier_p15_environmental_policy)$statistics[1:10])
names(loadings.p15_environmental_policy)[1] <- "Loadings"
loadings.p15_environmental_policy

## Second, Uniqueness ##
uniquenesses.p15_environmental_policy <- data.frame(summary(posterier_p15_environmental_policy)$statistics[11:20])
names(uniquenesses.p15_environmental_policy)[1] <- "uniquenesses"
uniquenesses.p15_environmental_policy

economic_p15_environmental_policy <- data.frame(Attribute = "P15 Environmental Policy", 
                                                Measure = c("Environmental Policy", "Energy Productivity", "Greenhouse Gas Emissions", 
                                                            "Particulate Matter", "Biocapacity",
                                                            "Waste Generation", "Material Recycling", 
                                                            "Biodiversity", "Renewable Energy", "Material Footprint"), 
                                                Loadings = loadings.p15_environmental_policy, 
                                                Uniqueness = uniquenesses.p15_environmental_policy)

## Table A12 ##
stargazer(economic_p15_environmental_policy,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="Appendix_bfa.p15_environmental_policy.doc")



## Table A13 ##

posterier_g1_strategic_capacity <- MCMCfactanal(~`Strategic Planning` + `Expert Advice`, 
                                                factors=1,
                                                verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                                data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.g1_strategic_capacity <- data.frame(summary(posterier_g1_strategic_capacity)$statistics[1:2])
names(loadings.g1_strategic_capacity)[1] <- "Loadings"
loadings.g1_strategic_capacity

## Second, Uniqueness ##
uniquenesses.g1_strategic_capacity <- data.frame(summary(posterier_g1_strategic_capacity)$statistics[3:4])
names(uniquenesses.g1_strategic_capacity)[1] <- "uniquenesses"
uniquenesses.g1_strategic_capacity

g1_strategic_capacity <- data.frame(Attribute = "P1 Economy", 
                                    Measure = c("Strategic Planning", "Expert Advice"), 
                                    Loadings = loadings.g1_strategic_capacity, 
                                    Uniqueness = uniquenesses.g1_strategic_capacity)

## Table A13 ##
stargazer(g1_strategic_capacity,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="Appendix_bfa.g1_strategic_capacity.doc")


## Table A14 ##

posterier_g6_implementation <- MCMCfactanal(~`Government Effectiveness` + `Ministerial Compliance` + `Monitoring Ministries` +
                                              `Monitoring Agencies/Bureaucracies` + `Task Funding` + `Constitutional Discretion` +
                                              `National Standards` + `Effective Regulatory Enforcement`, 
                                            factors=1,
                                            verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                            data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.g6_implementation <- data.frame(summary(posterier_g6_implementation)$statistics[1:8])
names(loadings.g6_implementation)[1] <- "Loadings"
loadings.g6_implementation

## Second, Uniqueness ##
uniquenesses.g6_implementation <- data.frame(summary(posterier_g6_implementation)$statistics[9:16])
names(uniquenesses.g6_implementation)[1] <- "uniquenesses"
uniquenesses.g6_implementation

g6_implementation <- data.frame(Attribute = "G6 Implementation ", 
                                Measure = c("Government Effectiveness", "Ministerial Compliance",
                                            "Monitoring Ministries", 
                                            "Monitoring Agencies/Bureaucracies", "Task Funding", 
                                            "Constitutional Discretion", "National Standards", 
                                            "Effective Regulatory Enforcement"), 
                                Loadings = loadings.g6_implementation, 
                                Uniqueness = uniquenesses.g6_implementation)

## Table A14 ##
stargazer(g6_implementation,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="Appendix_bfa.g6_implementation.doc")


## Table A15 ##

posterier_g13_ISB <- MCMCfactanal(~`Audit Office` + `Ombuds Office` + `Data Protection Authority`, 
                                  factors=1,
                                  verbose=0, store.scores=FALSE, a0=1, b0=0.15,
                                  data=sgi_timeseries, burnin=BURNIN, mcmc=MCMC, thin=THIN)

## First, the loadings (Lambda). ##

loadings.g13_ISB <- data.frame(summary(posterier_g13_ISB)$statistics[1:3])
names(loadings.g13_ISB)[1] <- "Loadings"
loadings.g13_ISB

## Second, Uniqueness ##
uniquenesses.g13_ISB <- data.frame(summary(posterier_g13_ISB)$statistics[4:6])
names(uniquenesses.g13_ISB)[1] <- "uniquenesses"
uniquenesses.g13_ISB

g13_ISB <- data.frame(Attribute = "G13 Independet Supervisory Bodies", 
                      Measure = c("Audit Office", "Ombuds Office",
                                  "Data Protection Authority"), 
                      Loadings = loadings.g13_ISB, 
                      Uniqueness = uniquenesses.g13_ISB)

## Table A15 ##
stargazer(g13_ISB,
          summary = FALSE,
          type = "html",
          #note that the argument is "out" not "file"
          out="Appendix_bfa.g13_ISB.doc")


##################################################################################
##################################################################################
#### Figures Main Paper ####
##################################################################################
##################################################################################

#### Figure 1: SGI Concept and Indicators (based on SGI 2020 Codebook, and Munck and Verkuilen 2002) ####

# not generated by statistical data analysis in R

#### Figure 2: Data Generation Process SGI – Expert Coding and Country Narratives ####

sgi_data_generation <- DiagrammeR::grViz("
digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, width = 4, fillcolor = Biege]
a [label = '@@1']
aa [label = '@@6']
ab [label = '@@7']

b [label = '@@2']
c [label = '@@3']
d [label = '@@4']
e [label = '@@5']
f [label = '@@8']
g [label = '@@9']

a -> aa
a -> ab
ab -> b -> c -> f -> g -> d -> e
b -> aa -> c
}

[1]:  paste0('Data collection by 1st Expert')
[2]: paste0('Review by 2nd Expert')
[3]: paste0('Two scores + text for each indicator')
[4]: paste0('Inter-regional calibration: Regional coordinators discuss and adjust rations if necessary')
[5]: paste0('Academic Advisory Board: examines the plausability of evaluations')
[6]: paste0('67 qualitative indicators per country per expert')
[7]: paste0('written assessment')
[8]: paste0('Regional coordinator: Review of Text and Scores')
[9]: paste0('Regional coordinator: Determine actual scores in their countries')

")

sgi_data_generation

saveWidget(sgi_data_generation, "sgi_data_generation.html")
webshot("sgi_data_generation.html", c("Figure2_sgi_data_generation.png", "Figure2_sgi_data_generation.pdf"), zoom = 1.5)


#### Figure 3: Spatial and Temporal Distribution of Different Datasets, since 1990 ####

wgi_data_overview <- wgi_timeseries %>%
  group_by(year) %>%
  summarize(number_countries = n()) %>%
  mutate(data = "WGI")

sgi_data_overview <- sgi_timeseries %>%
  group_by(year) %>%
  summarize(number_countries = n()) %>%
  mutate(data = "SGI")

vdem_data_overview <- vdem_timeseries %>%
  group_by(year) %>%
  summarize(number_countries = n()) %>%
  mutate(data = "V-Dem")

bti_data_overview <- bti_timeseries %>%
  group_by(year) %>%
  summarize(number_countries = n()) %>%
  mutate(data = "BTI")

#### Heatmap ####

# color palette 
# above 210: #593d9cff
# above 200: #90548bff
# 119: #eb8055ff
# 125: #eb8055ff
# 128: #eb8055ff
# 129: #eb8055ff
# 137: #de7065ff
# V-Dem: #2D708EFF




wgi_data_overview_plot <-  ggplot(wgi_data_overview, aes(y = data, x = year, fill = as.factor(number_countries))) +
  geom_tile(color = "white", lwd = 1,
            linetype = 1) + 
  scale_fill_manual(values=c("#33738DFF")) +
  geom_text(aes(label = number_countries), size= 4, angle = 90) + 
  labs(x= "", y= "", fill = "Number of Countries") +
  xlim(c(1990, 2021)) +
  theme_pubr(legend = "top", x.text.angle = 45) +
  guides(fill=FALSE)


bti_data_overview_plot <-  ggplot(bti_data_overview, aes(y = data, x = year, fill = as.factor(number_countries))) +
  geom_tile(color = "white", lwd = 1,
            linetype = 1) + 
  scale_fill_manual(values=c("#73D055FF", "#3CBB75FF", "#3CBB75FF", "#3CBB75FF", "#29A387FF")) +
  geom_text(aes(label = number_countries), size= 4, angle = 90) + 
  labs(x= "", y= "", fill = "Number of Countries") +
  xlim(c(1990, 2021)) +
  theme_pubr(legend = "top", x.text.angle = 45) +
  guides(fill=FALSE)


vdem_data_overview_plot <-  ggplot(subset(vdem_data_overview, year>=1990), aes(y = data, x = year, fill = as.factor(number_countries))) +
  geom_tile(color = "white", lwd = 1,
            linetype = 1) + 
  xlim(c(1989, 2021)) +
  scale_fill_manual(values=c("#2D708EFF", "#2D708EFF", "#2D708EFF", "#2D708EFF", "#2D708EFF", "#2D708EFF", "#2D708EFF", "#2D708EFF" )) +
  geom_text(aes(label = number_countries), size= 4, angle = 90) + 
  labs(x= "", y= "", fill = "Number of Countries") +
  theme_pubr(legend = "top", x.text.angle = 45) +
  guides(fill=FALSE)

sgi_data_overview_plot <-  ggplot(sgi_data_overview, aes(y = data, x = year, fill = as.factor(number_countries))) +
  geom_tile(color = "white", lwd = 1,
            linetype = 1) + 
  scale_fill_manual(values=c("#FDE725FF")) +
  geom_text(aes(label = number_countries), size= 4, angle = 90) + 
  labs(x= "", y= "", fill = "Number of Countries") +
  theme_pubr(legend = "top", x.text.angle = 45) +
  xlim(c(1990, 2021)) +
  guides(fill=FALSE)


#### Plotting World Maps for different Datasets ####

require(maps)
require(viridis)
theme_set(
  theme_void()
)


## Load World Map ##

world_map <- map_data("world")
world_map <- world_map %>%
  filter(region != "Antarctica")
  
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="white", colour = "black")

title <- theme(
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5), 
  legend.position = "right"
)

# V-Dem #

vdem_map <- vdem_timeseries %>%
  filter(year == 2020) %>%
  rename(region = country_name) %>%
  mutate(region = ifelse(region == "United States of America", "USA", region)) %>%
  mutate(region = ifelse(region == "United Kingdom", "UK", region)) %>%
  mutate(region = ifelse(region == "Republic of the Congo", "Republic of Congo", region)) %>%
  group_by(region) %>%
  summarize(observed = ifelse(is.na(v2x_polyarchy), 0, 1)) %>%
  full_join(world_map, by = "region") %>%
  ggplot(data=., aes(long, lat, group = group))+
  geom_polygon(aes(fill = as.factor(observed)), color = "black") +
  scale_fill_manual(values = "#474f8b")+
  labs(fill = "",
       title = "Varieties of Democracy dataset",
       subtitle = "observed in 2020") +
  guides(fill=FALSE) +
  title 

# SGI #

sgi_map <- sgi_timeseries %>%
  filter(year == 2020) %>%
  rename(region = cname) %>%
  mutate(region = ifelse(region == "United States", "USA", region)) %>%
  mutate(region = ifelse(region == "United Kingdom", "UK", region)) %>%
  mutate(region = ifelse(region == "Czechia", "Czech Republic", region)) %>%
  group_by(region) %>%
  summarize(observed = ifelse(is.na(`Policy Performance`), 0, 1)) %>%
  full_join(world_map, by = "region") %>%
  ggplot(data=., aes(long, lat, group = group))+
  geom_polygon(aes(fill = as.factor(observed)), color = "black") +
  scale_fill_manual(values = "#474f8b")+
  labs(fill = "",
       title = "Sustainable Governance Indicators dataset",
       subtitle = "observed in 2020") +
  guides(fill=FALSE) +
  title 

# WGI #

wgi_timeseries$region <- countrycode(wgi_timeseries$code, origin = 'iso3c', destination = 'country.name')

wgi_map <- wgi_timeseries %>%
  filter(year == 2019) %>%
  mutate(region = ifelse(region == "United States", "USA", region)) %>%
  mutate(region = ifelse(region == "United Kingdom", "UK", region)) %>%
  mutate(region = ifelse(region == "Czechia", "Czech Republic", region)) %>%
  mutate(region = ifelse(countryname == "Myanmar", "Myanmar", region)) %>%
  mutate(region = ifelse(countryname == "Côte d'Ivoire", "Ivory Coast", region)) %>%
  mutate(region = ifelse(countryname == "Congo, Rep.", "Republic of Congo", region)) %>%
  mutate(region = ifelse(countryname == "Congo, Dem. Rep.", "Democratic Republic of the Congo", region)) %>%
  mutate(region = ifelse(countryname == "Swaziland", "Swaziland", region)) %>%
  mutate(region = ifelse(countryname == "Romania", "Romania", region)) %>%
  mutate(region = ifelse(countryname == "North Macedonia", "Macedonia", region)) %>%
  mutate(region = ifelse(countryname == "Kosovo", "Kosovo", region)) %>%
  mutate(region = ifelse(countryname == "Bosnia and Herzegovina", "Bosnia and Herzegovina", region)) %>%
  group_by(region) %>%
  summarize(observed = ifelse(is.na(vae), 0, 1)) %>%
  drop_na(region) %>%
  full_join(world_map, by = "region") %>%
  ggplot(data=., aes(long, lat, group = group))+
  geom_polygon(aes(fill = as.factor(observed)), color = "black") +
  scale_fill_manual(values = c("#FFFFFF", "#474f8b"))+
  labs(fill = "",
       title = "Worldwide Governance Indicators dataset",
       subtitle = "observed in 2019") +
  guides(fill=FALSE) +
  title 


# BTI #

bti_timeseries$region <- countrycode(bti_timeseries$country_code, origin = 'iso3c', destination = 'country.name')

bti_timeseries <- bti_timeseries %>%
  mutate(region = ifelse(country_code == "BHD", "Bangladesh", region)) %>%
  mutate(region = ifelse(country_code == "KSV", "Kosovo", region)) %>%
  mutate(region = ifelse(country_code == "ROM", "Romania", region)) %>%
  mutate(region = ifelse(country_code == "ZAR", "Democratic Republic of the Congo", region))

bti_map <- bti_timeseries %>%
  filter(year == 2020) %>%
  mutate(region = ifelse(region == "Myanmar (Burma)", "Myanmar", region)) %>%
  mutate(region = ifelse(region == "Congo - Brazzaville", "Republic of Congo", region)) %>%
  mutate(region = ifelse(country == "Côte d'Ivoire", "Ivory Coast", region)) %>%
  mutate(region = ifelse(region == "Bosnia & Herzegovina", "Bosnia and Herzegovina", region)) %>%
  mutate(region = ifelse(region == "North Macedonia", "Macedonia", region)) %>%
  mutate(region = ifelse(region == "Swaziland", "Swaziland", region)) %>%
  group_by(region) %>%
  summarize(observed = ifelse(is.na(stat_ind), 0, 1)) %>%
  drop_na(region) %>%
  full_join(world_map, by = "region") %>%
  ggplot(data=., aes(long, lat, group = group))+
  geom_polygon(aes(fill = as.factor(observed)), color = "black") +
  scale_fill_manual(values = c("#474f8b"))+
  labs(fill = "",
       title = "Bertelsmann Transformation Index dataset",
       subtitle = "observed in 2020") +
  guides(fill=FALSE) +
  title 

## Arrange Maps ##


ggarrange(sgi_map, bti_map, sgi_data_overview_plot, bti_data_overview_plot, 
         vdem_map, wgi_map, vdem_data_overview_plot, wgi_data_overview_plot,
          heights = c(0.7, 0.3, 0.7, 0.3),
          ncol = 2, nrow = 4)

ggsave("Figure3_WorldMapDatasets.pdf", height = 30, width = 45, units= c("cm"))
ggsave("Figure3_WorldMapDatasets.png", height = 30, width = 45, units= c("cm"), dpi = 600)


#### Figure 4: Relationship between the SGI Quality of Democracy Index and the V-Dem Liberal Democracy Index, 2014 and 2020 #####

vdem2014 <- vdem_timeseries %>%
  filter(year == 2014) %>%
  dplyr::select(country_id, country_name, year, v2x_libdem)

vdem2014$iso3n <- countrycode(vdem2014$country_name, origin = 'country.name', destination = 'iso3n')

sgi2014 <- sgi_timeseries %>%
  filter(year ==2014) %>%
  dplyr::select(cname, year,`Quality of Democracy`)

sgi2014$iso3n <- countrycode(sgi2014$cname, origin = 'country.name', destination = 'iso3n')


vdem_sgi_merge2014 <- sgi2014 %>%
  left_join(vdem2014, by = c("iso3n", "year"))

reg_form2014 <- vdem_sgi_merge2014$v2x_libdem ~ vdem_sgi_merge2014$`Quality of Democracy`


dem_plot_2014 <- ggplot(vdem_sgi_merge2014, aes(x=`Quality of Democracy`, y = v2x_libdem)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_text_repel(data=subset(vdem_sgi_merge2014, v2x_libdem < 0.5),
            aes(x=`Quality of Democracy`, y = v2x_libdem,label=country_name)) +
  stat_regline_equation(
    aes(label =  paste(..adj.rr.label..)),
    formula = reg_form2014) +
  xlim(1,10)+
  ylim(c(0,1))+
  labs(x = "SGI Quality of Democracy", 
       y = "VDem Liberal Democracy Index", 
       title = "2014") +
  theme_pubr()

# 2020 #
vdem2020 <- vdem_timeseries %>%
  filter(year == 2020) %>%
  dplyr::select(country_id, country_name, year, v2x_libdem)

vdem2020$iso3n <- countrycode(vdem2020$country_name, origin = 'country.name', destination = 'iso3n')

sgi2020 <- sgi_timeseries %>%
  filter(year ==2020) %>%
  dplyr::select(cname, year,`Quality of Democracy`)

sgi2020$iso3n <- countrycode(sgi2020$cname, origin = 'country.name', destination = 'iso3n')


vdem_sgi_merge2020 <- sgi2020 %>%
  left_join(vdem2020, by = c("iso3n", "year"))

reg_form2020 <- vdem_sgi_merge2020$v2x_libdem ~ vdem_sgi_merge2020$`Quality of Democracy`

dem_plot_2020 <- ggplot(vdem_sgi_merge2020, aes(x=`Quality of Democracy`, y = v2x_libdem)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_text_repel(data=subset(vdem_sgi_merge2020, v2x_libdem < 0.25 | country_name =="Mexico"),
                  aes(x=`Quality of Democracy`, y = v2x_libdem,label=country_name)) +
  xlim(c(1,10)) +
  stat_regline_equation(
    aes(label =  paste(..adj.rr.label..)),
    formula = reg_form2020) +
  ylim(c(0,1))+
  labs(x = "SGI Quality of Democracy", 
       y = "VDem Liberal Democracy Index", 
       title = "2020") +
  theme_pubr()

ggarrange(dem_plot_2014, dem_plot_2020, ncol = 2, nrow = 1)

ggsave("Figure4_Scatter_Dem.pdf", height = 10, width = 20, units= c("cm"))
ggsave("Figure4_Scatter_Dem.png", height = 10, width = 20, units= c("cm"), dpi = 600)

#### Figure 5: Relationship between the SGI Governance Index, the WGI Government Effectiveness Index, and the WGI Regulatory Quality Index, 2014 and 2019. ####

wgi2014 <- wgi_timeseries %>%
  filter(year == 2014) %>%
  dplyr::select(countryname, year, gee)

wgi2014$iso3n <- countrycode(wgi2014$countryname, origin = 'country.name', destination = 'iso3n')

wgi2014$iso3n[wgi2014$countryname=="Korea, Dem. Rep."] <- NA

sgi2014 <- sgi_timeseries %>%
  filter(year ==2014) %>%
  dplyr::select(cname, year,`Governance`)

sgi2014$iso3n <- countrycode(sgi2014$cname, origin = 'country.name', destination = 'iso3n')


wgi_sgi_merge2014 <- sgi2014 %>%
  left_join(wgi2014, by = c("iso3n", "year"))

reg_form2014 <- wgi_sgi_merge2014$gee ~ wgi_sgi_merge2014$`Governance`


governance_plot_2014 <- ggplot(wgi_sgi_merge2014, aes(x=`Governance`, y = gee)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_text_repel(data=subset(wgi_sgi_merge2014, gee >2 | `Governance` <4.5 | gee <0.2),
                  aes(x=`Governance`, y = gee,label=countryname)) +
  stat_regline_equation(
    aes(label =  paste(..adj.rr.label..)),
    formula = reg_form2014) +
  xlim(c(1,10)) +
  ylim(c(-2.5,2.5))+
  labs(x = "SGI Governance Index", 
       y = "WGI Government Effectiveness", 
       title = "2014") +
  theme_pubr()

# 2018 #

wgi2018 <- wgi_timeseries %>%
  filter(year == 2019) %>%
  dplyr::select(countryname, year, gee)

wgi2018$iso3n <- countrycode(wgi2018$countryname, origin = 'country.name', destination = 'iso3n')

wgi2018$iso3n[wgi2018$countryname=="Korea, Dem. Rep."] <- NA

sgi2018 <- sgi_timeseries %>%
  filter(year ==2019) %>%
  dplyr::select(cname, year,`Governance`)

sgi2018$iso3n <- countrycode(sgi2018$cname, origin = 'country.name', destination = 'iso3n')


wgi_sgi_merge2018 <- sgi2018 %>%
  left_join(wgi2018, by = c("iso3n", "year"))

reg_form2018 <- wgi_sgi_merge2018$gee ~ wgi_sgi_merge2018$`Governance`


governance_plot_2018 <- ggplot(wgi_sgi_merge2018, aes(x=`Governance`, y = gee)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_text_repel(data=subset(wgi_sgi_merge2018, gee <0.15),
                  aes(x=`Governance`, y = gee,label=countryname)) +
  stat_regline_equation(
    aes(label =  paste(..adj.rr.label..)),
    formula = reg_form2018) +
  xlim(c(1,10)) +
  ylim(c(-2.5,2.5))+
  labs(x = "SGI Governance Index", 
       y = "WGI Government Effectiveness", 
       title = "2019") +
  theme_pubr()

## WGI and SGI governance + regulatory quality ##

wgi2014_2 <- wgi_timeseries %>%
  filter(year == 2014) %>%
  dplyr::select(countryname, year, rqe)

wgi2014_2$iso3n <- countrycode(wgi2014_2$countryname, origin = 'country.name', destination = 'iso3n')

wgi2014_2$iso3n[wgi2014_2$countryname=="Korea, Dem. Rep."] <- NA

sgi2014_2 <- sgi_timeseries %>%
  filter(year ==2014) %>%
  dplyr::select(cname, year,`Governance`)

sgi2014_2$iso3n <- countrycode(sgi2014_2$cname, origin = 'country.name', destination = 'iso3n')


wgi_sgi_merge2014_2 <- sgi2014_2 %>%
  left_join(wgi2014_2, by = c("iso3n", "year"))

reg_form2014_2 <- wgi_sgi_merge2014_2$rqe ~ wgi_sgi_merge2014_2$`Governance`


governance_plot2_2014 <- ggplot(wgi_sgi_merge2014_2, aes(x=`Governance`, y = rqe)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_text_repel(data=subset(wgi_sgi_merge2014_2, `Governance` < 4),
                  aes(x=`Governance`, y = rqe,label=countryname)) +
  stat_regline_equation(
    aes(label =  paste(..adj.rr.label..)),
    formula = reg_form2014_2) +
  xlim(c(1,10)) +
  ylim(c(-2.5,2.5))+
  labs(x = "SGI Governance Index", 
       y = "WGI Regulatory Quality", 
       title = "2014") +
  theme_pubr()

# 2018 #

wgi2018_2 <- wgi_timeseries %>%
  filter(year == 2019) %>%
  dplyr::select(countryname, year, rqe)

wgi2018_2$iso3n <- countrycode(wgi2018_2$countryname, origin = 'country.name', destination = 'iso3n')

wgi2018_2$iso3n[wgi2018_2$countryname=="Korea, Dem. Rep."] <- NA

sgi2018_2 <- sgi_timeseries %>%
  filter(year ==2019) %>%
  dplyr::select(cname, year,`Governance`)

sgi2018_2$iso3n <- countrycode(sgi2018_2$cname, origin = 'country.name', destination = 'iso3n')


wgi_sgi_merge2018_2 <- sgi2018_2 %>%
  left_join(wgi2018_2, by = c("iso3n", "year"))

reg_form2018_2 <- wgi_sgi_merge2018_2$rqe ~ wgi_sgi_merge2018_2$`Governance`


governance_plot2_2018 <- ggplot(wgi_sgi_merge2018_2, aes(x=`Governance`, y = rqe)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_text_repel(data=subset(wgi_sgi_merge2018_2, rqe <0.2),
                  aes(x=`Governance`, y = rqe,label=countryname)) +
  stat_regline_equation(
    aes(label =  paste(..adj.rr.label..)),
    formula = reg_form2018_2) +
  xlim(c(1,10)) +
  ylim(c(-2.5,2.5))+
  labs(x = "SGI Governance Index", 
       y = "WGI Regulatory Quality", 
       title = "2019") +
  theme_pubr()

ggarrange(governance_plot_2014, governance_plot_2018, 
          governance_plot2_2014, governance_plot2_2018, ncol = 2, nrow = 2)

ggsave("Figure5_Scatter_Governance.pdf", height = 20, width = 20, units= c("cm"))
ggsave("Figure5_Scatter_Governance.png", height = 20, width = 20, units= c("cm"), dpi = 600)

#### end of file ####





