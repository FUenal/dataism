##                                         ##                                                  ##
##                              Exploratory Data Analysis (EDA)                                ##
#################################################################################################


###### Data preprocessing

## Load packages
library(pacman)

pacman::p_load(essurvey, readr, readxl, curl, tidyverse, broom, haven, foreign, labelled, skimr, C50, class, e1071, randomForest, rpart, rpart.plot, 
               partykit, caret, caretEnsemble, lares, GGally, ggplot2, PerformanceAnalytics, fastmatch, Metrics, ipred, mlbench, RANN, 
               highcharter, ggimage, countrycode, survey, srvyr, dplyr, questionr, sjmisc, car, data.table, ryouready)

###### Download ESS Data

# # Set email for access
set_email("mars.fatih@gmail.com")
#
# ## Download all countries round 8
df8 <- import_rounds(8)

## Data preprocessing (variable selection) done in 'variable_selection.R' script
## Load data file
df <- read.csv("data/ess8.csv")

# ## Load Eco Welfare dataset
# library(readxl)
df1 <-  read_excel("data/41558_2015_BFnclimate2728_MOESM432_ESM copy.xlsx", sheet = 6, col_names = TRUE)

# ## Load Ming's dataset
dfming <-  read_excel("data/41558_2015_BFnclimate2728_MOESM432_ESM copy.xlsx", sheet = 1, col_names = TRUE)

## Load Fossil Fuel Tracker Data
dftracker <- read.csv("data/country_overview_large.csv")

# str(df1)

#### end ####


##                                         ##                                                  ##
##                              Preparation 1: Weighted Values                                 ##
#################################################################################################

# ## First peak into raw data
# ## Identify and deal with low and high correlations and zero variance variables ('lares' package)
# ## Check Overall Data Structure
# # skim(df)
# df_str(df)
# df_str(df, return = "skimr")
# # unique(df$cntry)

###### Add wighted country level scores for dependent variables ###

####### Approach to weighted scores: http://dimiter.eu/Visualizations_files/ESS/Visualizing_ESS_data.html
# set_email("mars.fatih@gmail.com")
# df8 <- import_rounds(8)

# # Working with variables: Works only with spss data
# # library(labelled) # install from CRAN first
# # 
# # labelled::look_for(df, 'climate')
# # 
# attributes(df8$clmchng)
# 
# ## DV 1: clmchng
# # Check
# table(df$clmchng)

# sum(is.na(df$clmchng))
# 
# df <- df %>% 
#         drop_na(clmchng)

# Computing weighted statistics
# attributes(df$clmchng)$labels # only with spss data

df$clmchng.f <- to_factor(df$clmchng, drop_unused_labels=TRUE, ordered=TRUE) # , labels = c(1 = "Definitely changing", 2 = "Probably changing", 3 = "Probably not changing", 4 = "Definitely not changing"
table(df$clmchng.f, df$cntry) # distribution of responses per country (raw)


## One way to get a weighted count per country
# library(questionr)
temp.table <- questionr::wtd.table(df$clmchng.f, df$cntry, weights=df$pspwght, digits = 0, useNA = c("no"))
# temp.table

## Now let's get the relative percentages
temp.cprop.table<-questionr::cprop(temp.table, digits=0, total=FALSE, n=FALSE, percent=TRUE)
temp.cprop.table

## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
aware <- as.data.frame.matrix(t(temp.cprop.table)) 
# pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed
str(aware)

# Rename columns
names(aware)[1] <- "Definitely not changing"
names(aware)[2] <- "Probably not changing " 
names(aware)[3] <- "Probably changing"
names(aware)[4] <- "Definitely changing" 

# Add columns for 'aware' vs. 'unaware'
aware <- aware %>% 
        mutate(unaware = aware$`Probably not changing` + aware$`Definitely not changing`, aware = aware$`Probably changing` + aware$`Definitely changing`)
aware

Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
                "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
                "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

aware <- cbind(Country, aware)

# ## Order the dataset by the specified column
# aware <- aware[order(aware$`aware`, decreasing = TRUE),]
# aware


## DV 2: wrclmch /// NOTE: NOT RECODED !!!!*********
# Check
table(df$wrclmch)
attributes(df8$wrclmch)
# sum(is.na(df$wrclmch))
# 
# df <- df %>% 
#         drop_na(clmchng)

# Computing weighted statistics
# attributes(df$clmchng)$labels # only with spss data

df$wrclmch.f <- to_factor(df$wrclmch, drop_unused_labels=TRUE, ordered=TRUE) 
table(df$wrclmch.f, df$cntry) # distribution of responses per country (raw)


## One way to get a weighted count per country
#library(questionr)
temp.table1 <- questionr::wtd.table(df$wrclmch.f, df$cntry, weights=df$pspwght, digits = 0, useNA = c("no"))
# temp.table1

## Now let's get the relative percentages
temp.cprop.table1<-questionr::cprop(temp.table1, digits=0, total=FALSE, n=FALSE, percent=TRUE)
temp.cprop.table1

## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
worried <- as.data.frame.matrix(t(temp.cprop.table1)) 
# pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed
str(worried)

# Rename columns
names(worried)[1] <- "Not at all worried" 
names(worried)[2] <- "Not very worried"
names(worried)[3] <- "Somewhat worried"
names(worried)[4] <- "Very worried" 
names(worried)[5] <- "Extremely worried"

# Add columns for 'worried' vs. 'not_worried'
worried <- worried %>% 
        mutate(not_worried = worried$`Not at all worried` + worried$`Not very worried`, worried = worried$`Somewhat worried` + 
                       worried$`Very worried` + worried$`Extremely worried`)
# worried
# 
# Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
#              "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
#              "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

worried <- cbind(Country, worried)

# ## Order the dataset by the specified column
# worried <- worried[order(worried$`aware`, decreasing = TRUE),]
# worried

## DV 3: ccnthum : Climate change caused by natural processes, human activity, or both /// NOTE: NOT RECODED !!!!*********
# Check
table(df$ccnthum)
attributes(df8$ccnthum)
attributes(df8$ccnthum)$labels

# sum(is.na(df$ccnthum))
# 
# df <- df %>% 
#         drop_na(ccnthum)

# Computing weighted statistics
# attributes(df$clmchng)$labels # only with spss data

df$ccnthum.f <- to_factor(df$ccnthum, drop_unused_labels=TRUE, ordered=TRUE) 
table(df$ccnthum.f, df$cntry) # distribution of responses per country (raw)


## One way to get a weighted count per country
#library(questionr)
temp.table2 <- questionr::wtd.table(df$ccnthum.f, df$cntry, weights=df$pspwght, digits = 0, useNA = c("no"))
# temp.table1

## Now let's get the relative percentages
temp.cprop.table2<-questionr::cprop(temp.table2, digits=0, total=FALSE, n=FALSE, percent=TRUE)
temp.cprop.table2

## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
cause <- as.data.frame.matrix(t(temp.cprop.table2)) 
# pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed
str(cause)

# Rename columns
names(cause)[1] <- "Entirely by natural processes"
names(cause)[2] <- "Mainly by natural processes"
names(cause)[3] <- "About equally by natural processes and human activity"
names(cause)[4] <- "Mainly by human activity"
names(cause)[5] <- "Entirely by human activity"
names(cause)[6] <- "I do not think climate change is happening"


# Add columns for 'human' vs. 'natural'
cause <- cause %>% 
        mutate(natural = cause$`Entirely by natural processes` + cause$`Mainly by natural processes`, human = cause$`About equally by natural processes and human activity` + 
                       cause$`Mainly by human activity` + cause$`Entirely by human activity`, denial = cause$`I do not think climate change is happening`)
# cause
# 
# Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
#              "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
#              "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

cause <- cbind(Country, cause)

# ## Order the dataset by the specified column
# cause <- cause[order(cause$`aware`, decreasing = TRUE),]
# cause


## DV 4: ccrdprs # To what extent feel personal responsibility to reduce climate change /// NOTE: NOT RECODED !!!!*********
# Check
table(df$ccrdprs)
attributes(df8$ccrdprs)
attributes(df8$ccrdprs)$labels

# sum(is.na(df$ccrdprs))
# 
# df <- df %>% 
#         drop_na(ccrdprs)

# Computing weighted statistics
# attributes(df$clmchng)$labels # only with spss data

df$ccrdprs.f <- to_factor(df$ccrdprs, drop_unused_labels=TRUE, ordered=TRUE) 
table(df$ccrdprs.f, df$cntry) # distribution of responses per country (raw)


## One way to get a weighted count per country
# library(questionr)
temp.table3 <- questionr::wtd.table(df$ccrdprs.f, df$cntry, weights=df$pspwght, digits = 0, useNA = c("no"))
# temp.table1

## Now let's get the relative percentages
temp.cprop.table3<-questionr::cprop(temp.table3, digits=0, total=FALSE, n=FALSE, percent=TRUE)
temp.cprop.table3

## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
responsible <- as.data.frame.matrix(t(temp.cprop.table3)) 
# pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed
str(responsible)

# Rename columns
names(responsible)[1] <- "Not at all"
names(responsible)[11] <- "A great deal"


# Add columns for 'responsible' vs. 'not_responsible'
responsible <- responsible %>% 
        mutate(NotResponsible = responsible$`Not at all` + responsible$`2` + responsible$`3` + responsible$`4` + responsible$`5`,
               responsible = responsible$`6` + responsible$`7`+ responsible$`8`+ responsible$`9` + responsible$`10` + responsible$`A great deal`)
# responsible
# 
# Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
#              "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
#              "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

responsible <- cbind(Country, responsible)

# ## Order the dataset by the specified column
# responsible <- responsible[order(responsible$`aware`, decreasing = TRUE),]
# responsible

## DV 5: ccgdbd # Climate change good or bad impact across world
# Check
table(df$ccgdbd)
attributes(df8$ccgdbd)
attributes(df8$ccgdbd)$labels

# sum(is.na(df$ccgdbd))
# 
# df <- df %>% 
#         drop_na(ccgdbd)

# Computing weighted statistics
# attributes(df$ccgdbd)$labels # only with spss data

df$ccgdbd.f <- to_factor(df$ccgdbd, drop_unused_labels=TRUE, ordered=TRUE) 
table(df$ccgdbd.f, df$cntry) # distribution of responses per country (raw)


## One way to get a weighted count per country
# library(questionr)
temp.table4 <- questionr::wtd.table(df$ccgdbd.f, df$cntry, weights=df$pspwght, digits = 0, useNA = c("no"))
# temp.table1

## Now let's get the relative percentages
temp.cprop.table4<-questionr::cprop(temp.table4, digits=0, total=F, n=FALSE, percent=TRUE)
temp.cprop.table4

## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
impact <- as.data.frame.matrix(t(temp.cprop.table4)) 
# pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed
str(impact)

# Rename columns
names(impact)[1] <- "Extremely good"
names(impact)[11] <- "Extremely bad"


# Add columns for 'good' vs. 'bad'
impact <- impact %>% 
        mutate(good_impact = impact$`Extremely good` + impact$`2` + impact$`3`+ impact$`4`+ impact$`5`,
               bad_impact =  impact$`6` + impact$`7` + impact$`8` + impact$`9` + impact$`10` + impact$`Extremely bad`)
# impact
# 
# Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
#              "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
#              "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

impact <- cbind(Country, impact)

# ## Order the dataset by the specified column
# impact <- impact[order(impact$`aware`, decreasing = TRUE),]
# impact


## DV 6: inctxff # Favour increase taxes on fossil fuels to reduce climate change
# Check
table(df$inctxff)

# sum(is.na(df$inctxff))
# 
# df <- df %>%inctxff
#         drop_na(ccgdbd)

# Computing weighted statistics
# attributes(df$inctxff)$labels # only with spss data

df$inctxff.f <- to_factor(df$inctxff, drop_unused_labels=TRUE, ordered=TRUE) 
table(df$inctxff.f, df$cntry) # distribution of responses per country (raw)


## One way to get a weighted count per country
# library(questionr)
temp.table5 <- questionr::wtd.table(df$inctxff.f, df$cntry, weights=df$pspwght, digits = 0, useNA = c("no"))
# temp.table1

## Now let's get the relative percentages
temp.cprop.table5<-questionr::cprop(temp.table5, digits=0, total=FALSE, n=FALSE, percent=TRUE)
temp.cprop.table5

## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
pol_tax <- as.data.frame.matrix(t(temp.cprop.table5)) 
# pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed
str(pol_tax)

# Rename columns
names(pol_tax)[1] <- "Strongly against"
names(pol_tax)[2] <- "Somewhat against"
names(pol_tax)[3] <- "Neither in favour nor against"
names(pol_tax)[4] <- "Somewhat in favour" 
names(pol_tax)[5] <- "Strongly in favour" 


# Add columns for 'support' vs. 'oppostion' vs 'undecided'
pol_tax <- pol_tax %>% 
        mutate(support = pol_tax$`Strongly in favour` + pol_tax$`Somewhat in favour`, 
               undecided = + pol_tax$`Neither in favour nor against`,
               opposition = pol_tax$`Somewhat against` + pol_tax$`Strongly against`)
# pol_tax
# 
# Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
#              "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
#              "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

pol_tax <- cbind(Country, pol_tax)

# ## Order the dataset by the specified column
# pol_tax <- pol_tax[order(pol_tax$`aware`, decreasing = TRUE),]
# pol_tax

## DV 7: sbsrnen # Favour subsidise renewable energy to reduce climate change
# Check
table(df$sbsrnen)
# labelled::look_for(df8, 'climate')
# attributes(df8$sbsrnen)
# attributes(df8$sbsrnen)$labels

# sum(is.na(df$sbsrnen))
# 
# df <- df %>%sbsrnen
# drop_na(sbsrnen)

# Computing weighted statistics
# attributes(df$inctxff)$labels # only with spss data

df$sbsrnen.f <- to_factor(df$sbsrnen, drop_unused_labels=TRUE, ordered=TRUE) 
table(df$sbsrnen.f, df$cntry) # distribution of responses per country (raw)


## One way to get a weighted count per country
# library(questionr)
temp.table6 <- questionr::wtd.table(df$sbsrnen.f, df$cntry, weights=df$pspwght, digits = 0, useNA = c("no"))
# temp.table1

## Now let's get the relative percentages
temp.cprop.table6<-questionr::cprop(temp.table6, digits=0, total=FALSE, n=FALSE, percent=TRUE)
temp.cprop.table6

## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
pol_subsidy <- as.data.frame.matrix(t(temp.cprop.table6)) 
# pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed
str(pol_subsidy)

# Rename columns
names(pol_subsidy)[1] <- "Strongly against"
names(pol_subsidy)[2] <- "Somewhat against"
names(pol_subsidy)[3] <- "Neither in favour nor against"
names(pol_subsidy)[4] <- "Somewhat in favour" 
names(pol_subsidy)[5] <- "Strongly in favour" 


# Add columns for 'support' vs. 'oppostion' vs 'undecided'
pol_subsidy <- pol_subsidy %>% 
        mutate(support = pol_subsidy$`Strongly in favour` + pol_subsidy$`Somewhat in favour`, 
               undecided = + pol_subsidy$`Neither in favour nor against`,
               opposition = pol_subsidy$`Somewhat against` + pol_subsidy$`Strongly against`)
# pol_subsidy
# 
# Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
#              "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
#              "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

pol_subsidy <- cbind(Country, pol_subsidy)

# ## Order the dataset by the specified column
# pol_subsidy <- pol_subsidy[order(pol_subsidy$`aware`, decreasing = TRUE),]
# pol_subsidy

## DV 8: banhhap # Favour ban sale of least energy efficient household appliances to reduce climate change
# Check
table(df$banhhap)

# sum(is.na(df$banhhap))
# 
# df <- df %>%banhhap
# drop_na(banhhap)

# Computing weighted statistics
# attributes(df$banhhap)$labels # only with spss data

df$banhhap.f <- to_factor(df$banhhap, drop_unused_labels=TRUE, ordered=TRUE) 
table(df$banhhap.f, df$cntry) # distribution of responses per country (raw)


## One way to get a weighted count per country
# library(questionr)
temp.table7 <- questionr::wtd.table(df$banhhap.f, df$cntry, weights=df$pspwght, digits = 0, useNA = c("no"))
# temp.table1

## Now let's get the relative percentages
temp.cprop.table7<-questionr::cprop(temp.table7, digits=0, total=FALSE, n=FALSE, percent=TRUE)
temp.cprop.table7

## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
pol_ban <- as.data.frame.matrix(t(temp.cprop.table7)) 
# pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed
str(pol_ban)

# Rename columns
names(pol_ban)[1] <- "Strongly against"
names(pol_ban)[2] <- "Somewhat against"
names(pol_ban)[3] <- "Neither in favour nor against"
names(pol_ban)[4] <- "Somewhat in favour" 
names(pol_ban)[5] <- "Strongly in favour" 


# Add columns for 'support' vs. 'oppostion' vs 'undecided'
pol_ban <- pol_ban %>% 
        mutate(support = pol_ban$`Strongly in favour` + pol_ban$`Somewhat in favour`, 
               undecided = + pol_ban$`Neither in favour nor against`,
               opposition = pol_ban$`Somewhat against` + pol_ban$`Strongly against`)
# pol_ban
# 
# Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
#              "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
#              "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

pol_ban <- cbind(Country, pol_ban)

# ## Order the dataset by the specified column
# pol_ban <- pol_ban[order(pol_ban$`aware`, decreasing = TRUE),]
# pol_ban


#### end ####

##                                         ##                                                  ##
##                                    Visualizations 1                                         ##
#################################################################################################

### Europe Maps Descriptives
## Variables visualized using highcharter
# Load required R packages
# library(tidyverse)
# library(highcharter) 
# library(dplyr)

# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

# Load the world Map data
data(worldgeojson, package = "highcharter")
data(map = custom/europe, package = "highcharter")
europe <- get_data_from_map(download_map_data("custom/europe"))

# # Retrieve climate change aware data 
# aware
# worried
# cause
# responsible
# impact
# pol_tax
# pol_subsidy
# pol_ban

## ESS Europe Aware
map_aware <- hcmap(
        "custom/europe",
        data = aware, 
        value = "aware",
        joinBy = c("name","Country"),
        name = "Climate Change Aware",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Aware") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_aware

## ESS Europe Aware
map_unaware <- hcmap(
        "custom/europe",
        data = aware, 
        value = "unaware",
        joinBy = c("name","Country"),
        name = "Climate Change Unware",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Unware") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_unaware

## ESS Europe Worried
map_worried <- hcmap(
        "custom/europe",
        data = worried, 
        value = "worried",
        joinBy = c("name","Country"),
        name = "Climate Change Worried",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Worried") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_worried

## ESS Europe Not Not Worried
map_not_worried <- hcmap(
        "custom/europe",
        data = worried, 
        value = "not_worried",
        joinBy = c("name","Country"),
        name = "Climate Change Unworried",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Unworried") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_not_worried

## ESS Europe Cause: Humans
map_cause_humans <- hcmap(
        "custom/europe",
        data = cause, 
        value = "human",
        joinBy = c("name","Country"),
        name = "Climate Change Cause: Humans",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Cause: Humans") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_cause_humans

## ESS Europe Cause: Natural
map_cause_nature <- hcmap(
        "custom/europe",
        data = cause, 
        value = "natural",
        joinBy = c("name","Country"),
        name = "Climate Change Cause: Natural",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Cause: Natural") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_cause_nature

## ESS Europe Cause: Denial
map_cause_denial <- hcmap(
        "custom/europe",
        data = cause, 
        value = "denial",
        joinBy = c("name","Country"),
        name = "Climate Change Denial",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Denial") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_cause_denial

## ESS Europe: Responsible
map_responsible <- hcmap(
        "custom/europe",
        data = responsible, 
        value = "responsible",
        joinBy = c("name","Country"),
        name = "Climate Change Responsibility",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Responsibility") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_responsible

## ESS Europe: Non-Responsible
map_not_responsible <- hcmap(
        "custom/europe",
        data = responsible, 
        value = "NotResponsible",
        joinBy = c("name","Country"),
        name = "Climate Change Non-Responsibility",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Non-Responsibility") %>% 
        hc_subtitle(text = "CNationally representative (weighted %)")

map_not_responsible

## ESS Europe: Bad Impact
map_bad_impact <- hcmap(
        "custom/europe",
        data = impact, 
        value = "bad_impact",
        joinBy = c("name","Country"),
        name = "Climate Change: Bad Impact",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change: Bad Impact") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_bad_impact

## ESS Europe: Good Impact
map_good_impact <- hcmap(
        "custom/europe",
        data = impact, 
        value = "good_impact",
        joinBy = c("name","Country"),
        name = "Climate Change: Good Impact",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change: Good Impact") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_good_impact

## ESS Europe: Policy Support Tax
map_tax_support <- hcmap(
        "custom/europe",
        data = pol_tax, 
        value = "support",
        joinBy = c("name","Country"),
        name = "Climate Change Support for Taxation Policy",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Support for Taxation Policy") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_tax_support

## ESS Europe: Policy Opposition Tax
map_tax_opposition <- hcmap(
        "custom/europe",
        data = pol_tax, 
        value = "opposition",
        joinBy = c("name","Country"),
        name = "Climate Change Opposition to Taxation Policy",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Opposition to Taxation Policy") %>% 
        hc_subtitle(text = "CNationally representative (weighted %)")

map_tax_opposition

## ESS Europe: Policy Undecided Tax
map_tax_undecided <- hcmap(
        "custom/europe",
        data = pol_tax, 
        value = "undecided",
        joinBy = c("name","Country"),
        name = "Climate Change Undecided on Taxation Policy",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Undecided on Taxation Policy") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_tax_undecided


## ESS Europe: Policy Support Subsidy
map_subsidy_support <- hcmap(
        "custom/europe",
        data = pol_subsidy, 
        value = "support",
        joinBy = c("name","Country"),
        name = "Climate Change Support for Subsidy",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Support for Subsidy") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_subsidy_support

## ESS Europe: Policy Opposition Subsidy
map_subsidy_opposition <- hcmap(
        "custom/europe",
        data = pol_subsidy, 
        value = "opposition",
        joinBy = c("name","Country"),
        name = "Climate Change Opposition to Subsidy Support",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Opposition to Subsidy Support") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_subsidy_opposition


## ESS Europe: Policy Subsidy Undecided
map_subsidy_undecided <- hcmap(
        "custom/europe",
        data = pol_subsidy, 
        value = "undecided",
        joinBy = c("name","Country"),
        name = "Climate Change Undecided on Subsidy Support",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Undecided on Subsidy Support") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_subsidy_undecided


## ESS Europe: Policy Support Bans
map_ban_support <- hcmap(
        "custom/europe",
        data = pol_ban, 
        value = "support",
        joinBy = c("name","Country"),
        name = "Climate Change Support for Bans",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Support for Bans") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_ban_support

## ESS Europe: Policy Opposition Bans
map_ban_opposition <- hcmap(
        "custom/europe",
        data = pol_ban, 
        value = "opposition",
        joinBy = c("name","Country"),
        name = "Climate Change Opposition to Bans",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Opposition to Bans") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_ban_opposition


## ESS Europe: Policy Bans Undecided
map_ban_undecided <- hcmap(
        "custom/europe",
        data = pol_ban, 
        value = "undecided",
        joinBy = c("name","Country"),
        name = "Climate Change Undecided on Bans",) %>%
        #hc_colors(c("#FFFFFF", "#434348")) %>%
        #hc_colorAxis(hc, stops = color_stops(), minColor = "#FFFFFF", maxColor = "#434348") %>% 
        hc_title(text = "Climate Change Undecided on Bans") %>% 
        hc_subtitle(text = "Nationally representative (weighted %)")

map_ban_undecided

## Worldmap
hc_world <- highchart() %>%
        hc_add_series_map(
                worldgeojson, dfming, value = "Aware", joinBy = c('name','Country'),
                name = "Climate Change Aware"
        )  %>% 
        # hc_colors(c("darkorange", "darkgray")) %>%
        # hc_colorAxis(stops = color_stops()) %>% 
        hc_title(text = "Climate Change Awareness Around the Globe") %>% 
        hc_subtitle(text = "Nationally representative estimates (weighted %)") %>% 
    hc_credits(
        enabled = TRUE, 
        text = "Source: Lee et. al, 2015, Nature.",
        style = list(fontSize = "13px")
    )

hc_world


# ### Correlation Plots
# # Pull and merge country mean scores, percentages and macro-level indices
# # Climate change weighted proportions data 
# aware
# worried
# cause
# responsible
# impact
# pol_tax
# pol_subsidy
# pol_ban

# attributes(df8$clmchng)

## DV 1: clmchng
## DV 2: wrclmch /// NOTE: NOT RECODED !!!!*********
## DV 3: ccnthum : Climate change caused by natural processes, human activity, or both /// NOTE: NOT RECODED !!!!*********
## DV 4: ccrdprs # To what extent feel personal responsibility to reduce climate change /// NOTE: NOT RECODED !!!!*********
## DV 5: ccgdbd # Climate change good or bad impact across world
## DV 6: inctxff # Favour increase taxes on fossil fuels to reduce climate change
## DV 7: sbsrnen # Favour subsidise renewable energy to reduce climate change
## DV 8: banhhap # Favour ban sale of least energy efficient household appliances to reduce climate change

describe(df8$wrclmch)

### Average scores per country with population and post-stratification weights: 
### Approach to weighted scores: https://federicovegetti.github.io/teaching/heidelberg_2018/lab/sst_lab_day2.html 

## Climate change worry - Political Orientation: Template
model_pol_ori_worry <- df %>%
        as_survey(weights = c(pweight, pspwght)) %>%
        dplyr:: mutate(weights = pspwght * pweight) %>% 
        group_by(cntry) %>%
        dplyr::summarize(wrclmch = survey_mean(wrclmch, na.rm = T), lrscale = survey_mean(lrscale, na.rm = T), weights = survey_mean(weights, na.rm = T))

model_pol_ori_worry

## Add country names
Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
             "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
             "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

model_pol_ori_worry <- cbind(Country, model_pol_ori_worry)

# Fit regression model
# library(dplyr)
# library(broom)
ds <- lm(wrclmch ~ lrscale, data = model_pol_ori_worry)
fit <- augment(ds) %>% arrange(lrscale)

# Visualization

hc_plot_1 <- model_pol_ori_worry %>% 
        hchart(
                'scatter', hcaes(x = lrscale, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = lrscale, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Political Orientation & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50", 
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Political Orientation: <b>{point.lrscale:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "Political Orientation Left (1) to Right (10)"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_1


#### end ####


##                                         ##                                                  ##
##                                   Feature Selection                                         ##
#################################################################################################
 
## Politics

# polintr : How interested in politics
# psppsgva : Political system allows people to have a say in what government does
# actrolga : Able to take active role in political group
# psppipla : Political system allows people to have influence on politics
# cptppola : Confident in own ability to participate in politics
# trstprl : Trust in country's parliament
# trstplt : Trust in politicians
# trstprt : Trust in political parties
# trstep : Trust in the European Parliament
# trstun : Trust in the United Nations
# stfeco : How satisfied with present state of economy in country
# stfgov : How satisfied with the national government
# stfdem : How satisfied with the way democracy works in country
# stfhlth : State of health services in country nowadays
# gincdif : Government should reduce differences in income levels
# imbgeco : Immigration bad or good for country's economy

## Demographics

# gndr
# agea
# eisced


# ## Human Values
# "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff", "ipfrule", "ipudrst", "ipmodst", 
# "ipgdtim", "impfree", "iphlppl", "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr", 
# "impenv", "imptrad", "impfun"

# ## Welfare
# "dfincac", "smdfslv", "uemplwk", "slvpens", "slvuemp", "gvslvol", "gvslvue", "gvcldcr", 
# "sbstrec", "sbprvpv", "sbeqsoc", "sbbsntx", "sblazy", "sblwcoa", "imsclbn", "uentrjb", "lbenent", "bennent", 
# "bnlwinc", "eduunmp", "wrkprbf", "basinc", "eusclbf", "eudcnbf", "lkuemp", "lknemny", 
# "vteurmmb", "vteumbgb", "vteubcmb"

# ## Media & Social trust
# "nwspol", "netusoft", "netustm", "ppltrst", "pplfair", "pplhlp",

# load("ess8.RData")

## Climate change features and weights
model <- df %>% 
        as_survey(weights = c(pweight, pspwght)) %>% 
        dplyr:: mutate(weights = pspwght * pweight) %>% 
        group_by(cntry) %>% 
        dplyr::summarize(clmchng = survey_mean(clmchng, na.rm = T), wrclmch = survey_mean(wrclmch, na.rm = T), ccnthum = survey_mean(ccnthum, na.rm = T),
                         ccrdprs = survey_mean(ccrdprs, na.rm = T), ccgdbd = survey_mean(ccgdbd, na.rm = T), inctxff = survey_mean(inctxff, na.rm = T),
                         sbsrnen = survey_mean(sbsrnen, na.rm = T), banhhap = survey_mean(banhhap, na.rm = T), lrscale = survey_mean(lrscale, na.rm = T),
                         polintr = survey_mean(polintr, na.rm = T), trstprl = survey_mean(trstprl, na.rm = T), trstep = survey_mean(trstep, na.rm = T),
                         stfeco = survey_mean(stfeco, na.rm = T), gincdif = survey_mean(gincdif, na.rm = T), imbgeco = survey_mean(imbgeco, na.rm = T),
                         imprich = survey_mean(imprich, na.rm = T), iphlppl = survey_mean(iphlppl, na.rm = T), impenv = survey_mean(impenv, na.rm = T),
                         dfincac = survey_mean(dfincac, na.rm = T), basinc = survey_mean(basinc, na.rm = T), lkuemp = survey_mean(lkuemp, na.rm = T),
                         gndrM = survey_mean(gndr, na.rm = T), ageaM = survey_mean(agea, na.rm = T), eiscedM = survey_mean(eisced, na.rm = T),
                         ppltrst = survey_mean(ppltrst, na.rm = T), pplfair = survey_mean(pplfair, na.rm = T), pplhlp = survey_mean(pplhlp, na.rm = T),
                         nwspol = survey_mean(nwspol, na.rm = T), netusoft = survey_mean(netusoft, na.rm = T), netustm = survey_mean(netustm, na.rm = T),
                         weights = survey_mean(weights, na.rm = T))

 

## Add country names
Country <- c("Austria", "Belgium", "Switzerland", "Czech Republic", "Germany", "Estonia", "Spain", "Finland", 
             "France", "United Kingdom", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", 
             "Netherlands", "Norway", "Poland", "Portugal", "Russia", "Sweden", "Slovenia")

model <- cbind(Country, model)

## Load Socio-Eco-Welfare Data
# ## Load Eco Welfare dataset
# library(readxl)
# df1 <-  read_excel("data/41558_2015_BFnclimate2728_MOESM432_ESM copy.xlsx", sheet = 6, col_names = TRUE)
# 
# str(df1)

## Merge data sets ESS8 and Socio-Eco-Welfare
model['Sub_region'] <- df1$Sub_region[match(model$cntry, df1$ISO2)]
model['cri'] <- df1$cri[match(model$cntry, df1$ISO2)]
model['MTCO2e'] <- df1$MTCO2e[match(model$cntry, df1$ISO2)]
model['CAT_rating'] <- df1$CAT_rating[match(model$cntry, df1$ISO2)]
model['EPI'] <- df1$EPI[match(model$cntry, df1$ISO2)]
model['Environmental_Health'] <- df1$`Environmental Health`[match(model$cntry, df1$ISO2)]
model['Ecosystem_Vitality'] <- df1$`Ecosystem Vitality`[match(model$cntry, df1$ISO2)]
model['DMC'] <- df1$DMC[match(model$cntry, df1$ISO2)]
model['Gini'] <- df1$Gini[match(model$cntry, df1$ISO2)]
model['Unemployment_Rate'] <- df1$`Unemployment Rate`[match(model$cntry, df1$ISO2)]
model['Long_Term_Unemployment'] <- df1$`Long-Term Unemployment`[match(model$cntry, df1$ISO2)]
model['Fossil_Renewable_Ratio'] <- df1$`Fossil/Renewable Ratio`[match(model$cntry, df1$ISO2)]
model['GDP'] <- df1$`Gross Domestic Product`[match(model$cntry, df1$ISO2)]
model['Industry_Service_Ratio'] <- df1$`Industry/Service-Ratio`[match(model$cntry, df1$ISO2)]
model['Workers_Protection'] <- df1$`Workers’ Protection`[match(model$cntry, df1$ISO2)]
model['Union_Density'] <- df1$`Union Density`[match(model$cntry, df1$ISO2)]
model['EnvironMental_Policy_Stringency'] <- df1$`Environ-Mental Policy Stringency`[match(model$cntry, df1$ISO2)]
model['Green_Party_Seats_Parliament'] <- df1$`Green Parties’ Seats in National Parliaments (in %)`[match(model$cntry, df1$ISO2)]
model['palma'] <- df1$Palma[match(model$cntry, df1$ISO2)]


# # Recheck model
# model
# summary(model)
view(model)
# ## Load Fossil Fuel Tracker Data
# dftracker <- read.csv("data/country_overview_large.csv")


## Merge data sets ESS8 and Fossil Fuel Tracker
model['global_emissions_percent'] <- dftracker$global_emissions_percent[match(model$cntry, dftracker$ISO2)]
model['MTCO2e_cat'] <- dftracker$MTCO2e_cat[match(model$cntry, dftracker$ISO2)]
model['fossil_fuel_share_energy_2019'] <- dftracker$fossil_fuel_share_energy_2019[match(model$cntry, dftracker$ISO2)]
model['Policy_total'] <- dftracker$Policy_total[match(model$cntry, dftracker$ISO2)]


# # ## Load Ming's Data
# dfming <-  read_excel("data/41558_2015_BFnclimate2728_MOESM432_ESM copy.xlsx", sheet = 1, col_names = TRUE)
# 
# 
# ## Merge data sets ESS8 and Ming`s data
# model['WPCIAS'] <- dfming$WPCIAS[match(model$cntry, dfming$Country)]
# model['SPCIAS'] <- dfming$SPCIAS[match(model$cntry, dfming$Country)]
# model['APCIAS'] <- dfming$APCIAS[match(model$cntry, dfming$Country)]
# model['HDI'] <- dfming$HDI[match(model$cntry, dfming$Country)]
# model['glo_tot'] <- dfming$glo_tot[match(model$cntry, dfming$Country)]
# model['Efcon'] <- dfming$Efcon[match(model$cntry, dfming$Country)]
# model['TotBioCap'] <- dfming$TotBioCap[match(model$cntry, dfming$Country)]
# model['WGI'] <- dfming$WGI[match(model$cntry, dfming$Country)]
# model['VA'] <- dfming$VA[match(model$cntry, dfming$Country)]
# model['PS'] <- dfming$PS[match(model$cntry, dfming$Country)]
# model['GovE'] <- dfming$GovE[match(model$cntry, dfming$Country)]
# model['RQ'] <- dfming$RQ[match(model$cntry, dfming$Country)]
# model['RL'] <- dfming$RL[match(model$cntry, dfming$Country)]
# model['CC'] <- dfming$CC[match(model$cntry, dfming$Country)]



## Load SDO/RWA Data IMPORTANT NOTE: NO WEIGHTING VARIABLES PRESENT IN DATA SET THUS NOT REPRESENTATIVE IN THE CURRENT FORMAT!!!
df2 <- read_dta("data/Replication_data.dta")
# str(df2)

## Select cntry, rwa, sdo

df2a <- df2 %>% 
        select(cntry, rwa6, rwa7, rwa8, rwa9, sdo1, sdo2, sdo3, sdo4)  
    
# # Replace NAs    
# df2a <- df2a %>% replace(is.na(.), 0)

# # Reverse coding
# attributes(df2a$rwa9)
# attributes(df2a$sdo4)
# table(df2a$rwa6)
# table(df2a$sdo2)
# table(df2b$rwa6)
# table(df2b$sdo2)

df2b <- df2a %>% 
        mutate(rwa6 = car::recode(rwa6, recodes = "1=5; 2=4; 3=3; 4=2; 5=1; else=NA"),
               rwa8 = car::recode(rwa8, recodes = "1=5; 2=4; 3=3; 4=2; 5=1; else=NA"),
               rwa9 = car::recode(rwa9, recodes = "1=5; 2=4; 3=3; 4=2; 5=1; else=NA"),
               sdo2 = car::recode(sdo2, recodes = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA"),
               sdo4 = car::recode(sdo4, recodes = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA"))

# Calculate mean scores
scale1_vars <- c("sdo1","sdo2","sdo3","sdo4")
scale2_vars <- c("rwa6","rwa7","rwa8","rwa9")

df2b <- df2b %>% dplyr::mutate(sdoM = rowMeans(df2b[ ,scale1_vars], na.rm = TRUE),
                                   rwaM = rowMeans(df2b[ ,scale2_vars], na.rm = TRUE))

# df2b

# Claculate means score sdo/rwa groubed by country
SdoRwa <- df2b %>%
        group_by(cntry) %>%
        dplyr::summarize(SdoM = mean(sdoM, na.rm = TRUE), RwaM = mean(rwaM, na.rm = TRUE))

# Check data
# SdoRwa


# Merge model with SDO/RWA
model['RWA'] <- SdoRwa$RwaM[match(model$cntry, SdoRwa$cntry)]
model['SDO'] <- SdoRwa$SdoM[match(model$cntry, SdoRwa$cntry)]

# # Check data
# df_str(model, return = "skimr")

# Replace NAs
model <- model %>% replace(is.na(.), 0)

### Convert character to numeric
#display classes of each column
# df_str(model, return = "skimr")
summary(model)
# str(model)
# vars <- c("Environmental_Health", "Ecosystem_Vitality", "Gini", "Long_Term_Unemployment", "Fossil_Renewable_Ratio", 
#           "GDP", "Industry_Service_Ratio", "Union_Density", "Green_Party_Seats_Parliament")

model[69:71] <- lapply(model[69:71], function(x) as.numeric(as.character(x)))
model[76] <- lapply(model[76], function(x) as.numeric(as.character(x)))
model[78] <- lapply(model[78], function(x) as.numeric(as.character(x)))
model[80] <- lapply(model[80], function(x) as.numeric(as.character(x)))
model[82] <- lapply(model[82], function(x) as.numeric(as.character(x)))
sapply(model, class)


#### end ####


##                                         ##                                                  ##
##                                   Visualizations 2                                          ##
#################################################################################################

## Model 1: Climate Risk Index and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
ds <- lm(wrclmch ~ cri, data = model)
fit <- augment(ds) %>% arrange(cri)

# Visualization

hc_plot_cri <- model %>% 
        hchart(
                'scatter', hcaes(x = cri, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = cri, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Climate Risk Index & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Climate Risk Index: <b>{point.cri:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "Climate Risk Index Low (0) to High (100)"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_cri


## Model 2: MTCO2e and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_z <- model %>% 
    filter(cntry != "RU")

ds <- lm(wrclmch ~ MTCO2e, data = model_z)
fit <- augment(ds) %>% arrange(MTCO2e)

# Visualization

hc_plot_MTCO2e <- model_z %>% 
        hchart(
                'scatter', hcaes(x = MTCO2e, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = MTCO2e, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "MTCO2e & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                MTCO2e: <b>{point.MTCO2e:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "MTCO2e: Low to High"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_MTCO2e

## Model 3: Environmental_Health and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
ds <- lm(wrclmch ~ Environmental_Health, data = model)
fit <- augment(ds) %>% arrange(Environmental_Health)

# Visualization

hc_plot_Environmental_Health <- model %>% 
        hchart(
                'scatter', hcaes(x = Environmental_Health, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = Environmental_Health, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Environmental Health & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Environmental Health Index: <b>{point.Environmental_Health:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "Environmental Health Index: Low (0) to High (100)"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_Environmental_Health

## Model 4: Ecosystem_Vitality and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
ds <- lm(wrclmch ~ Ecosystem_Vitality, data = model)
fit <- augment(ds) %>% arrange(Ecosystem_Vitality)

# Visualization

hc_plot_Ecosystem_Vitality <- model %>% 
        hchart(
                'scatter', hcaes(x = Ecosystem_Vitality, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = Ecosystem_Vitality, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Ecosystem Vitality & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Ecosystem Vitality Index: <b>{point.Ecosystem_Vitality:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "Ecosystem Vitality Index: Low (0) to High (100)"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_Ecosystem_Vitality

## Model 4a: EPI and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
ds <- lm(wrclmch ~ EPI, data = model)
fit <- augment(ds) %>% arrange(EPI)

# Visualization

hc_plot_EPI <- model %>% 
    hchart(
        'scatter', hcaes(x = EPI, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = EPI, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Environmental Performance Index & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Environmental Performance Index: <b>{point.EPI:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Environmental Performance Index: Low (0) to High (100)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_EPI

## Model 5: DMC and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_a <- model %>% 
    filter(DMC != 0)

ds <- lm(wrclmch ~ DMC, data = model_a)
fit <- augment(ds) %>% arrange(DMC)

# Visualization

hc_plot_DMC <- model_a %>% 
        hchart(
                'scatter', hcaes(x = DMC, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = DMC, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Domestic Material Consumption & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                DMC: <b>{point.DMC:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "Domestic Material Consumption: Tons per country"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_DMC

## Model 6: Gini and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_b <- model %>% 
    filter(Gini != 0)

ds <- lm(wrclmch ~ Gini, data = model_b)
fit <- augment(ds) %>% arrange(Gini)

# Visualization

hc_plot_Gini <- model_b %>% 
        hchart(
                'scatter', hcaes(x = Gini, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = Gini, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Gini & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Gini: <b>{point.Gini:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "Gini Low (0) to High (100)"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_Gini

## Model 7: GDP and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_c <- model %>% 
    filter(GDP != 0)

ds <- lm(wrclmch ~ GDP, data = model_c)
fit <- augment(ds) %>% arrange(GDP)

# Visualization

hc_plot_GDP <- model_c %>% 
        hchart(
                'scatter', hcaes(x = GDP, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = GDP, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "GDP & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                GDP: <b>{point.GDP:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "GDP"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_GDP

## Model 8: EnvironMental_Policy_Stringency and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_d <- model %>% 
    filter(EnvironMental_Policy_Stringency != 0)

ds <- lm(wrclmch ~ EnvironMental_Policy_Stringency, data = model_d)
fit <- augment(ds) %>% arrange(EnvironMental_Policy_Stringency)

# Visualization

hc_plot_EnvironMental_Policy_Stringency <- model_d %>% 
        hchart(
                'scatter', hcaes(x = EnvironMental_Policy_Stringency, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = EnvironMental_Policy_Stringency, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Environmental Policy Stringency & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                EnvironMental Policy Stringency: <b>{point.EnvironMental_Policy_Stringency:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "Environmental Policy Stringency Low to High"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_EnvironMental_Policy_Stringency


## Model 9: Green_Party_Seats_Parliament and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_y <- model %>% 
    filter(cntry != "LT")

model_e <- model_y %>% 
    filter(Green_Party_Seats_Parliament != 0)

ds <- lm(wrclmch ~ Green_Party_Seats_Parliament, data = model_e)
fit <- augment(ds) %>% arrange(Green_Party_Seats_Parliament)

# Visualization

hc_plot_Green_Party_Seats_Parliament <- model_e %>% 
        hchart(
                'scatter', hcaes(x = Green_Party_Seats_Parliament, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = Green_Party_Seats_Parliament, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Green Party Seats Parliament & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = " Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Green Party Seats Parliament: <b>{point.Green_Party_Seats_Parliament:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "% of Green Party Seats in Parliament"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_Green_Party_Seats_Parliament

## Model 10: SDO and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_f <- model %>% 
        filter(SDO != 0)

# # Mean Centering & Factorizing SDO
# summary(model_f$SDO)
# 
# model_f <- model_f %>%
#     dplyr::mutate(SDO = scale(model_f$SDO, center = TRUE, scale = TRUE))
# 
# ## Factor response variable
# model_f <- model_f %>%
#     dplyr::mutate(SDO = case_when(SDO < 0 ~ "Low SDO",
#                            SDO > 0 ~ "High SDO"),
#            SDO = factor(SDO, levels = c("Low SDO",  "High SDO")))
# 
# describe(model_f$SDO)

ds <- lm(wrclmch ~ SDO, data = model_f)
fit <- augment(ds) %>% arrange(SDO)

# Visualization
hc_plot_SDO <- model_f %>% 
        hchart(
                'scatter', hcaes(x = SDO, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = SDO, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Social Dominance Orientation & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                SDO: <b>{point.SDO:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "SDO Low (0) to High (7)"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_SDO

# # frq() – printing frequency tables
# frq(model_f$cntry)
# 
# # find_var() – finding variables in a data frame
# # find all variables with "climate" in name or label
# # find_var(df8clean, "climate")
# flat_table(model_f, cntry, SDO)



## Model 10: SDO and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_g <- model %>% 
        filter(RWA != 0)

ds <- lm(wrclmch ~ RWA, data = model_g)
fit <- augment(ds) %>% arrange(RWA)

# Visualization
hc_plot_RWA <- model_g %>% 
        hchart(
                'scatter', hcaes(x = RWA, y = wrclmch, size = weights, group = Country),
                maxSize = "10%"
        ) %>% 
        hc_add_series(
                fit, type = "line", hcaes(x = RWA, y = .fitted),
                name = "Fit", id = "fit"
        ) %>%
        hc_title(text = "Right-Wing Auhtoritarianism & Climate Change Worry Across Countries",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                    style = list(fontWeight = "bold", fontSize = "13px"),
                    align = "center") %>% 
        hc_plotOptions(
                series = list(
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.Country}",
                                color = "#2c3e50",
                                style = list(textOutline = FALSE)
                        )
                )
        ) %>% 
        hc_tooltip(
                headerFormat = "",
                pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                RWA: <b>{point.RWA:.2f}</b>"
        ) %>% 
        hc_xAxis(visible = TRUE, title = list(text = "RWA Low (0) to High (5)"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
                 style = list(fontWeight = "bold", fontSize = "15px")) %>% 
        hc_size(height = 800) %>% 
        hc_credits(
                enabled = TRUE, 
                text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
                style = list(fontSize = "13px")
        )

hc_plot_RWA


## Model 11: global_emissions_percent and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_x <- model %>%
    filter(Country != "Russia")

ds <- lm(wrclmch ~ global_emissions_percent, data = model_x)
fit <- augment(ds) %>% arrange(global_emissions_percent)

# Visualization
hc_plot_global_emissions_percent <- model_x %>% 
    hchart(
        'scatter', hcaes(x = global_emissions_percent, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = global_emissions_percent, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Global Greenhouse Gas Emissions % & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = " Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                % Global Greenhouse Gas Emissions: <b>{point.global_emissions_percent:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "% Global Greenhouse Gas Emissions"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_global_emissions_percent


## Model 12: fossil_fuel_share_energy_2019 and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
# model_x <- model %>%
#     filter(Country != "Russia")

ds <- lm(wrclmch ~ fossil_fuel_share_energy_2019, data = model)
fit <- augment(ds) %>% arrange(fossil_fuel_share_energy_2019)

# Visualization
hc_plot_fossil_fuel_share_energy_2019 <- model %>% 
    hchart(
        'scatter', hcaes(x = fossil_fuel_share_energy_2019, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = fossil_fuel_share_energy_2019, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Fossil-Fuel Share Energy 2019 % & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                % Fossil-Fuel Share Energy 2019: <b>{point.fossil_fuel_share_energy_2019:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Fossil-Fuel Share Energy 2019 %"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_fossil_fuel_share_energy_2019


## Model 13: Policy_total and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
model_w <- model %>%
    filter(Country != "United Kingdom")

ds <- lm(wrclmch ~ Policy_total, data = model_w)
fit <- augment(ds) %>% arrange(Policy_total)

# Visualization
hc_plot_Policy_total <- model_w %>% 
    hchart(
        'scatter', hcaes(x = Policy_total, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = Policy_total, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Number of Supply Side Restrcition Fossil-Fuel Policies & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                No. of Supply Side Fossil-Fuel Policies: <b>{point.Policy_total}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "No. of Supply Side Restriction Fossil-Fuel Policies"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_Policy_total


## 4.3 Individual Level Predictors of Climate Change Perceptions and Attitudes

## Model 14: Age and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
# model_w <- model %>%
#     filter(Country != "United Kingdom")

ds <- lm(wrclmch ~ agea, data = model)
fit <- augment(ds) %>% arrange(agea)

# Visualization
hc_plot_age <- model %>% 
    hchart(
        'scatter', hcaes(x = agea, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = agea, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Age & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Age: <b>{point.agea:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Age"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_age

## Model 15: gender and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
# model_w <- model %>%
#     filter(Country != "United Kingdom")

# # Check Missing Values in Response Variables
# sum(is.na(model$gndr))
# 
# # Drop NAs in response variable
# df <- df %>%
#     drop_na(gndr)

## Factor Gender
model_v <- model %>% 
    mutate(gndr = case_when(gndr <= 1 ~ "Male",
                            gndr >= 2 ~ "Female"),
           gndr = factor(gndr, levels = c("Male", "Female")))

table(model_v$gndr)

ds <- lm(wrclmch ~ gndr, data = model)
fit <- augment(ds) %>% arrange(gndr)

# Visualization
hc_plot_gender <- model %>% 
    hchart(
        'scatter', hcaes(x = gndr, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = gndr, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Gender & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Gender: <b>{point.gndr:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Gender"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_gender

## Model 16: education and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
# model_w <- model %>%
#     filter(Country != "United Kingdom")

attributes(df8$eisced)
sum(is.na(df8$eisced))
frq(df, eisced)
summary(df8$eisced)

ds <- lm(wrclmch ~ eisced, data = model)
fit <- augment(ds) %>% arrange(eisced)

# Visualization
hc_plot_educ <- model %>% 
    hchart(
        'scatter', hcaes(x = eisced, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = eisced, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Education & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Education: <b>{point.eisced:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Education"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_educ

## Model 17: Support for equality and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
# model_w <- model %>%
#     filter(Country != "United Kingdom")
# 
# attributes(df8$gincdif)
# sum(is.na(df8$eisced))
# frq(df, gincdif)
# summary(df$gincdif)

ds <- lm(wrclmch ~ gincdif, data = model)
fit <- augment(ds) %>% arrange(gincdif)

# Visualization
hc_plot_equal <- model %>% 
    hchart(
        'scatter', hcaes(x = gincdif, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = gincdif, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Support for Equality & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Support for Equality: <b>{point.gincdif:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Support for Equality"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_equal

## Model 18: Trust in the European Parliament and Climate DVs
# Fit regression model
# library(dplyr)
# library(broom)
# model_w <- model %>%
#     filter(Country != "United Kingdom")
# 
# attributes(df8$gincdif)
# sum(is.na(df8$eisced))
# frq(df, gincdif)
# summary(df$gincdif)

ds <- lm(wrclmch ~ trstep, data = model)
fit <- augment(ds) %>% arrange(trstep)

# Visualization
hc_plot_trsteuparl <- model %>% 
    hchart(
        'scatter', hcaes(x = trstep, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = trstep, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Trust in the European Parliament & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Trust in the European Parliament: <b>{point.trstep:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Trust in the European Parliament"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_trsteuparl

## Model 19: Immigration bad or good for country's economy and Climate DVs  
# Fit regression model
# library(dplyr)
# library(broom)
# model_w <- model %>%
#     filter(Country != "United Kingdom")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ imbgeco, data = model)
fit <- augment(ds) %>% arrange(imbgeco)

# Visualization
hc_plot_immigr <- model %>% 
    hchart(
        'scatter', hcaes(x = imbgeco, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = imbgeco, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Immigration bad or good for country's economy & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Immigration bad or good for country's economy : <b>{point.imbgeco:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Immigration bad or good for country's economy 1 (Bad) to 10 (Good)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_immigr

## Model 19: Internet use, how often and Climate DVs  
# Fit regression model
# library(dplyr)
# library(broom)
# model_w <- model %>%
#     filter(Country != "United Kingdom")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ netusoft, data = model)
fit <- augment(ds) %>% arrange(netusoft)

# Visualization
hc_plot_intuse <- model %>% 
    hchart(
        'scatter', hcaes(x = netusoft, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = netusoft, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Internet Usage & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Internet Usage: <b>{point.netusoft:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Internet Usage"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_intuse


## Model 20: News Consumption, how often and Climate DVs  # News about politics and current affairs, watching, reading or listening, in minutes
# Fit regression model
# library(dplyr)
# library(broom)
model_q <- model %>%
    filter(Country != "Italy")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ nwspol, data = model_q)
fit <- augment(ds) %>% arrange(nwspol)

# Visualization
hc_plot_nwspol <- model_q %>% 
    hchart(
        'scatter', hcaes(x = nwspol, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = nwspol, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "News Consumption & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                News Consumption: <b>{point.nwspol:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "News Consumption (in minutes per day)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_nwspol

## Model 21: General Trust in People, how often and Climate DVs  # Most people can be trusted or you can't be too careful
# Fit regression model
# library(dplyr)
# library(broom)
# model_q <- model %>%
#     filter(Country != "Italy")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ ppltrst, data = model)
fit <- augment(ds) %>% arrange(ppltrst)

# Visualization
hc_plot_ppltrst <- model %>% 
    hchart(
        'scatter', hcaes(x = ppltrst, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = ppltrst, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "General Trust in People & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                General Trust in People: <b>{point.ppltrst:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "General Trust in People (higher score = more trust)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_ppltrst

## Model 23: Against or In favour of a basic income scheme and Climate DVs  # Against or In favour of a basic income scheme
# Fit regression model
# library(dplyr)
# library(broom)
# model_q <- model %>%
#     filter(Country != "Italy")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ basinc, data = model)
fit <- augment(ds) %>% arrange(basinc)

# Visualization
hc_plot_basinc <- model %>% 
    hchart(
        'scatter', hcaes(x = basinc, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = basinc, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Attitudes toward a basic income scheme & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Attitudes toward a basic income scheme: <b>{point.basinc:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Attitudes toward a basic income scheme  (higher score = more support)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_basinc

## Model 24: Unemployment in the next 12 months and Climate DVs  # How likely unemployed and looking for work next 12 months
# Fit regression model
# library(dplyr)
# library(broom)
# model_q <- model %>%
#     filter(Country != "Italy")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ lkuemp, data = model)
fit <- augment(ds) %>% arrange(lkuemp)

# Visualization
hc_plot_lkuemp <- model %>% 
    hchart(
        'scatter', hcaes(x = lkuemp, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = lkuemp, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Risk of Unemployment in the Next 12 Months & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Risk of Unemployment: <b>{point.lkuemp:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Risk of Unemployment in the Next 12 Months (higher score = higher risk)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_lkuemp

## Model 25: Environmental Values and Climate DVs  # Important to care for nature and environment
# Fit regression model
# library(dplyr)
# library(broom)
# model_q <- model %>%
#     filter(Country != "Italy")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ impenv, data = model)
fit <- augment(ds) %>% arrange(impenv)

# Visualization
hc_plot_impenv <- model %>% 
    hchart(
        'scatter', hcaes(x = impenv, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = impenv, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Environmental Values & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Environmental Values: <b>{point.impenv:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Environmental Values (higher score = more environmentality)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_impenv


## Model 25: Environmental Values and Climate DVs  # Important to care for nature and environment
# Fit regression model
# library(dplyr)
# library(broom)
# model_q <- model %>%
#     filter(Country != "Italy")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ imprich, data = model)
fit <- augment(ds) %>% arrange(imprich)

# Visualization
hc_plot_imprich <- model %>% 
    hchart(
        'scatter', hcaes(x = imprich, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = imprich, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Materialist Values & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Materialist Values: <b>{point.imprich:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Environmental Values (higher score = more materialist values)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_imprich


## Model 26: Environmental Values and Climate DVs  # Important to help people and care for others well-being
# Fit regression model
# library(dplyr)
# library(broom)
# model_q <- model %>%
#     filter(Country != "Italy")
# 
# attributes(df8$imbgeco)
# sum(is.na(df8$eisced))
# frq(df, imbgeco)
# summary(df$gincdif)

ds <- lm(wrclmch ~ iphlppl, data = model)
fit <- augment(ds) %>% arrange(iphlppl)

# Visualization
hc_plot_iphlppl <- model %>% 
    hchart(
        'scatter', hcaes(x = iphlppl, y = wrclmch, size = weights, group = Country),
        maxSize = "10%"
    ) %>% 
    hc_add_series(
        fit, type = "line", hcaes(x = iphlppl, y = .fitted),
        name = "Fit", id = "fit"
    ) %>%
    hc_title(text = "Empathy and Helping Values & Climate Change Worry Across Countries",
             style = list(fontWeight = "bold", fontSize = "30px"),
             align = "center") %>% 
    hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
                style = list(fontWeight = "bold", fontSize = "13px"),
                align = "center") %>% 
    hc_plotOptions(
        series = list(
            dataLabels = list(
                enabled = TRUE,
                format = "{point.Country}",
                color = "#2c3e50",
                style = list(textOutline = FALSE)
            )
        )
    ) %>% 
    hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.Country}</b> <br> 
                Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
                Empathy and Helping Values: <b>{point.iphlppl:.2f}</b>"
    ) %>% 
    hc_xAxis(visible = TRUE, title = list(text = "Empathy and Helping Values (higher score = more humane values)"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_size(height = 800) %>% 
    hc_credits(
        enabled = TRUE, 
        text = "X and Y values are averaged by country. 
                Regression line is line of best fit through observations.",
        style = list(fontSize = "13px")
    )

hc_plot_iphlppl


# ## Model 26: Happiness and Climate DVs  # How happy are you
# # Fit regression model
# # library(dplyr)
# # library(broom)
# # model_q <- model %>%
# #     filter(Country != "Italy")
# # 
# # attributes(df8$imbgeco)
# # sum(is.na(df8$eisced))
# # frq(df, imbgeco)
# # summary(df$gincdif)
# 
# ds <- lm(wrclmch ~ happy, data = model)
# fit <- augment(ds) %>% arrange(happy)
# 
# # Visualization
# hc_plot_happy <- model %>% 
#     hchart(
#         'scatter', hcaes(x = happy, y = wrclmch, size = weights, group = Country),
#         maxSize = "10%"
#     ) %>% 
#     hc_add_series(
#         fit, type = "line", hcaes(x = happy, y = .fitted),
#         name = "Fit", id = "fit"
#     ) %>%
#     hc_title(text = "Happiness & Climate Change Worry Across Countries",
#              style = list(fontWeight = "bold", fontSize = "30px"),
#              align = "center") %>% 
#     hc_subtitle(text = "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error)", 
#                 style = list(fontWeight = "bold", fontSize = "13px"),
#                 align = "center") %>% 
#     hc_plotOptions(
#         series = list(
#             dataLabels = list(
#                 enabled = TRUE,
#                 format = "{point.Country}",
#                 color = "#2c3e50",
#                 style = list(textOutline = FALSE)
#             )
#         )
#     ) %>% 
#     hc_tooltip(
#         headerFormat = "",
#         pointFormat = "<b>{point.Country}</b> <br> 
#                 Climate Change Worry: <b>{point.wrclmch:.2f}</b>  <br> 
#                 Happiness: <b>{point.happy:.2f}</b>"
#     ) %>% 
#     hc_xAxis(visible = TRUE, title = list(text = "Happiness (higher score = more happiness)"),
#              style = list(fontWeight = "bold", fontSize = "15px")) %>% 
#     hc_yAxis(visible = TRUE, title = list(text = "Climate Change Worry"),
#              style = list(fontWeight = "bold", fontSize = "15px")) %>% 
#     hc_size(height = 800) %>% 
#     hc_credits(
#         enabled = TRUE, 
#         text = "X and Y values are averaged by country. 
#                 Regression line is line of best fit through observations.",
#         style = list(fontSize = "13px")
#     )
# 
# hc_plot_happy
# 
# model
### Feature Selection 
## Politics

# polintr : How interested in politics
# psppsgva : Political system allows people to have a say in what government does
# actrolga : Able to take active role in political group
# psppipla : Political system allows people to have influence on politics
# cptppola : Confident in own ability to participate in politics
# trstprl : Trust in country's parliament
# trstplt : Trust in politicians
# trstprt : Trust in political parties
# trstep : Trust in the European Parliament
# trstun : Trust in the United Nations
# stfeco : How satisfied with present state of economy in country
# stfgov : How satisfied with the national government
# stfdem : How satisfied with the way democracy works in country
# stfhlth : State of health services in country nowadays
# gincdif : Government should reduce differences in income levels
# imbgeco : Immigration bad or good for country's economy

## Demographics

# gndr
# agea
# eisced


# ## Human Values
# "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff", "ipfrule", "ipudrst", "ipmodst", 
# "ipgdtim", "impfree", "iphlppl", "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr", 
# "impenv", "imptrad", "impfun"

# ## Welfare
# "dfincac", "smdfslv", "uemplwk", "slvpens", "slvuemp", "gvslvol", "gvslvue", "gvcldcr", 
# "sbstrec", "sbprvpv", "sbeqsoc", "sbbsntx", "sblazy", "sblwcoa", "imsclbn", "uentrjb", "lbenent", "bennent", 
# "bnlwinc", "eduunmp", "wrkprbf", "basinc", "eusclbf", "eudcnbf", "lkuemp", "lknemny", 
# "vteurmmb", "vteumbgb", "vteubcmb"

# ## Media & Social trust
# "nwspol", "netusoft", "netustm", "ppltrst", "pplfair", "pplhlp",

# ## Group Subjective well-being, social exclusion, religion, national and ethnic identity
# happy
# inprdsc
# aesfdrk
# atchctr

### Split Stacked Bubble Chart with model data 
# library(tidyverse)

hc <- hchart(model, "packedbubble", hcaes(name = Country, value = Policy_total, group = MTCO2e_cat))

q95 <- as.numeric(quantile(model$Policy_total, .95))

hc %>% 
        hc_tooltip(
                useHTML = TRUE,
                pointFormat = "<b>{point.name}:</b> {point.value}"
        ) %>% 
        hc_plotOptions(
                packedbubble = list(
                        maxSize = "150%",
                        zMin = 0,
                        layoutAlgorithm = list(
                                gravitationalConstant =  0.05,
                                splitSeries =  TRUE, # TRUE to group points
                                seriesInteraction = TRUE,
                                dragBetweenSeries = TRUE,
                                parentNodeLimit = TRUE
                        ),
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.name}",
                                filter = list(
                                        property = "y",
                                        operator = ">",
                                        value = q95
                                ),
                                style = list(
                                        color = "black",
                                        textOutline = "none",
                                        fontWeight = "normal"
                                )
                        )
                )
        )





#### end ####

save.image(file = "ess8.RData")

### Split Stacked Bubble Chart with Gapminder Data Template
# install.packages("gapminder")
library(tidyverse)
library(gapminder)

data(gapminder, package = "gapminder")

gapminder <- gapminder %>% 
        filter(year == max(year)) %>% 
        select(country, pop, continent)

hc <- hchart(gapminder, "packedbubble", hcaes(name = country, value = pop, group = continent))

q95 <- as.numeric(quantile(gapminder$pop, .95))

hc %>% 
        hc_tooltip(
                useHTML = TRUE,
                pointFormat = "<b>{point.name}:</b> {point.value}"
        ) %>% 
        hc_plotOptions(
                packedbubble = list(
                        maxSize = "150%",
                        zMin = 0,
                        layoutAlgorithm = list(
                                gravitationalConstant =  0.05,
                                splitSeries =  TRUE, # TRUE to group points
                                seriesInteraction = TRUE,
                                dragBetweenSeries = TRUE,
                                parentNodeLimit = TRUE
                        ),
                        dataLabels = list(
                                enabled = TRUE,
                                format = "{point.name}",
                                filter = list(
                                        property = "y",
                                        operator = ">",
                                        value = q95
                                ),
                                style = list(
                                        color = "black",
                                        textOutline = "none",
                                        fontWeight = "normal"
                                )
                        )
                )
        )



### ggplot alternative plotting
# Template
model <- df %>% 
        as_survey(weights = c(pweight, pspwght)) %>% 
        group_by(cntry) %>% 
        dplyr::summarize(clmchng = survey_mean(clmchng, na.rm = T))

model

# Cntry Sex
model <- df %>% 
        mutate(sex = ifelse(gndr == 1, "Male", "Female")) %>%
        as_survey(weights = c(pweight, pspwght)) %>% 
        group_by(cntry, sex) %>% 
        dplyr::summarize(n = survey_mean(clmchng, na.rm = T, vartype = "ci"))

# plot it
ggplot(model, aes(x = sex, y = n)) +
        geom_bar(aes(fill = sex), stat = "identity") +
        geom_errorbar(aes(ymin = n_low, max = n_upp), width = 0.2) +
        facet_wrap(~cntry) +
        theme_bw()


ggplot(model, aes(lrscale, wrclmch)) + 
        geom_point(aes(size = weights), alpha = 0.79, color = "steelblue") +
        geom_text(aes(label = cntry), size = 10, alpha = 0.8, check_overlap = TRUE, color = "steelblue") + 
        geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
        labs(
                title = "X is positively correlated with Y",
                subtitle = paste(
                        "X and Y values are averaged by country.",
                        "Size of the bubble represents the weighted importance of the country (weighted by population and to reduce sampling error).",
                        "Blue lines are lines of best fit through observations.",
                        sep = "\n"
                ),
                x = "X value", 
                y = paste(
                        "Y value (scale 0-10) is the answer to the question,",
                        '"Taking all things together, how happy would you say you are?"',
                        sep = "\n"
                )
        ) + 
        theme_bw() + 
        theme(legend.position = "none")

# Survey Country analyses considering weights (https://rforpoliticalscience.com/2021/01/09/add-weights-to-survey-data-with-survey-and-svyr-package-in-r/)
library(ggimage) # to add flags
library(countrycode) # to add ISO country codes
library(survey)
library(srvyr)

r_agg <-df %>% 
        as_survey(weights = c(pweight, pspwght)) %>% 
        group_by(cntry) %>% 
        dplyr::summarize(clmchng_cwm = survey_mean(clmchng, na.rm = T))

# r_agg %>% 
#         dplyr::mutate(cntry, EU_member = ifelse(cntry == "BE" | cntry == "CZ" | cntry == "DE" | cntry == "EE" | cntry == "IE" | 
#                                                         cntry == "ES" | cntry == "FR" | cntry == "IT" | cntry == "NO" | cntry == "IS" |
#                                                         cntry == "LT" | cntry == "HU" | cntry == "NL" | cntry == "AT" | cntry == "Il" |
#                                                         cntry == "PL" | cntry == "PT" | cntry == "SI" | cntry == "RU" | cntry == "FI" | 
#                                                         cntry == "SE","EU member", "Non EU member")) -> r_agg

r_agg %>% 
        dplyr::summarize(eu_average = mean(clmchng_cwm)) 

r_agg$country_name <- countrycode(r_agg$cntry, "iso2c", "country.name")

eu_average <- r_agg %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)

eu_avg <- data.frame(cntry = "EU average",
                     clmchng_cwm = 3.49,
                     clmchng_cwm_se =  "EU average",
                     country_name = "EU average")

r_agg <- rbind(r_agg, eu_avg)


my_palette <- c("EU average" = "#ef476f", 
                "Non EU member" = "#06d6a0", 
                "EU member" = "#118ab2")

r_agg <- r_agg %>%          
        dplyr::mutate(ordered_country = fct_reorder(cntry, clmchng_cwm))


r_graph <- r_agg %>% 
        ggplot(aes(x = ordered_country, y = clmchng_cwm, group = cntry)) +
        geom_col() +
        ggimage::geom_flag(aes(y = -0.4, image = cntry), size = 0.04) +
        geom_text(aes(y = -0.15 , label = clmchng_cwm)) +
        ggtitle("Average Belief in Climate Change across Countries") + 
        labs(x= "Country", y= "Belief in Climate Change") +
        ggeasy::easy_center_title() +
        scale_fill_manual(values = my_palette) + coord_flip()

r_graph 

#### end ####


## Remove confounders in the climate change group: 24 (except: "clmchng", "wrclmch")
# Policy variables: "inctxff", "sbsrnen", "banhhap"

confounders <- df %>% 
        select("eneffap", "rdcenr", "cflsenr", "elgcoal", "elgngas", "elghydr", "elgnuc", "elgsun", "elgwind", "elgbio", 
               "wrpwrct", "wrenexp", "wrdpimp", "wrdpfos", "wrntdis", "wrinspw", "wrtcfl", "wrtratc", "ccnthum", "clmthgt1", "ccrdprs", "ccgdbd", 
               "clmthgt2",  "lkredcc", "lklmten", "gvsrdcc", "ownrdcc","inctxff", "sbsrnen", "banhhap") 

df <- select(df, -c("eneffap", "rdcenr", "cflsenr", "elgcoal", "elgngas", "elghydr", "elgnuc", "elgsun", "elgwind", "elgbio", 
                    "wrpwrct", "wrenexp", "wrdpimp", "wrdpfos", "wrntdis", "wrinspw", "wrtcfl", "wrtratc", "ccnthum", "clmthgt1",  "ccrdprs", "ccgdbd",
                    "clmthgt2",  "lkredcc", "lklmten", "gvsrdcc", "ownrdcc", "inctxff", "sbsrnen", "banhhap"))

## Check missing values
highmiss <- missingness(df)
head(highmiss)

# Store columns with NaNs > 50%
highmiss50 <- highmiss %>%
        filter(missingness >= 50)

dropmiss50 <- highmiss50[["variable"]]

# Removing variables with NaNs >50% 
df <- df %>%
        select(-one_of(dropmiss50))


# Recheck missing values
df_str(df)
head(missingness(df))


# Check zero variance columns (same value in each column)
zerovar(df)

## Analyze clmchng : Aware of climate change (1: Definitely changing / 2: Probably changing / 3: Probably not changing / 4: Definitely not changing)
table(df$clmchng)
sum(is.na(df$clmchng))

# Drop NAs in response variable
df <- df %>%
        drop_na(clmchng)


df <- df %>% 
        mutate(clmchng = case_when(clmchng <= 2 ~ "Aware",
                                   clmchng >= 3 ~ "Unaware"),
               clmchng = factor(clmchng, levels = c("Aware", "Unaware")))


# Table response variable counts 
table(df$clmchng)
levels(df$clmchng)


# Check Missing Values in Response Variables
sum(is.na(df$wrclmch))

# Drop NAs in response variable
df <- df %>%
        drop_na(wrclmch)

# Binarize response variable
df <- df %>%
        mutate(wrclmch = case_when(wrclmch <= 2 ~ "Not_worried",
                                   wrclmch >= 3 ~ "Worried"),
               wrclmch = factor(wrclmch, levels = c("Not_worried", "Worried")))

# Table response variable counts
table(df$wrclmch)


# Check Missing Values in Response Variables
sum(is.na(df$gndr))

# Drop NAs in response variable
df <- df %>%
        drop_na(gndr)

## Factor Gender
df <- df %>% 
        mutate(gndr = case_when(gndr <= 1 ~ "Male",
                                gndr >= 2 ~ "Female"),
               gndr = factor(gndr, levels = c("Male", "Female")))

table(df$gndr)

## Check str
str(df)

# drop X 
df <- df[,-1]

# drop idno 
df <- df[,-1]


## Split data into aware vs. non-aware
df_aware <- df %>%
        filter(clmchng == "Aware")

df_unaware <- df %>%
        filter(clmchng == "Unaware")

table(df_aware$clmchng)
table(df_unaware$clmchng)

df_str(df_aware)
df_str(df_unaware)

## Remove clmchng from df_aware

df_aware <- select(df_aware, -"clmchng")



