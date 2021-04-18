### NFL Shiny project
### March 2021
### Create Data files

# Set Working Directory to save data files
setwd("~/NFL/Shiny_Project")

library(psych)
library(foreign)
library(ggplot2)
library(Hmisc)
library(ggthemes)
library(devtools)
library(dplyr)
library(tidyverse)
library(XML)
library(RCurl)
library(ggpubr)

### Scrap data from web (Pro Football Reference.com)
## For simplicity, I use the same generic code to create separate data files for each season
pfr.dat <- getURL("https://www.pro-football-reference.com/years/2020/passing.htm")    #2020
pfr.dat <- getURL("https://www.pro-football-reference.com/years/2019/passing.htm")    #2019
pfr.dat <- getURL("https://www.pro-football-reference.com/years/2018/passing.htm")  #2018
pfr.dat <- getURL("https://www.pro-football-reference.com/years/2017/passing.htm")  #2017
pfr.dat <- getURL("https://www.pro-football-reference.com/years/2016/passing.htm")  #2016
tab1 <- readHTMLTable(pfr.dat, header = TRUE, which=1)

# rename ANY/A var
names(tab1)[names(tab1) =="ANY/A"] <- "ANY_A"

# remove duplicate row
tab1_ed<-tab1[!(tab1$Rk=="Rk"),]
tab1_name <- str_split_fixed(tab1_ed$Player, " ", 3)
t1 <- cbind(tab1_name, tab1_ed)
names(t1)[names(t1) == "1"] <- "First"
names(t1)[names(t1) == "2"] <- "Last"
names(t1)[names(t1) == "Cmp%"] <- "CompPercent"
names(t1)[names(t1) == "Y/G"] <- "YardsGame"

# Final general data file
metric.tab <- select(t1, First, Last, CompPercent, TD, Rate, QBR, YardsGame, ANY_A, Att)

# Convert data to numeric
metric.tab$ANY_A <- as.numeric(metric.tab$ANY_A)
metric.tab$YardsGame <- as.numeric(metric.tab$YardsGame)
metric.tab$QBR <- as.numeric(metric.tab$QBR)
metric.tab$Rate <- as.numeric(metric.tab$Rate)
metric.tab$TD <- as.numeric(metric.tab$TD)
metric.tab$CompPercent <- as.numeric(metric.tab$CompPercent)
metric.tab$Att <- as.numeric(metric.tab$Att)

### Each year is saved separately using the general code  #####
### Season: 2020

### Add ranking vars (to create the tables and figures in app)
## Reverse code the min_rank to rank the values from largest tp smallest
metric.tab20 <- metric.tab %>%
  filter(Att > 250) %>%
  mutate(ANYA_rank = min_rank(-ANY_A),
         Yards_rank = min_rank(-YardsGame),
         QBR_rank = min_rank(-QBR),
         Rate_rank = min_rank(-Rate),
         TD_rank = min_rank(-TD),
         Comp_rank = min_rank(-CompPercent)) %>%
  arrange(QBR_rank)

## join with EPA data
tab_EPA20 <- read.csv("~/Dropbox/TAMU/New_Projects/Git_edits/NFL/Shiny_Project/epa2020.csv") %>%
  select(Last, EPA, PLAYS, EPA_play, EPA_rank)
met.tab20 <- left_join(metric.tab20, tab_EPA20, by = "Last") 
met.tab20 <- met.tab20 %>%
  mutate(season = 2020)

### Save season data file
write.csv(met.tab20, "data2020.csv")


### Season: 2019
### Add ranking vars (to create the tables and figures in app)
## Reverse code the min_rank to rank the values from largest tp smallest
metric.tab19 <- metric.tab %>%
  filter(Att > 250) %>%
  mutate(ANYA_rank = min_rank(-ANY_A),
         Yards_rank = min_rank(-YardsGame),
         QBR_rank = min_rank(-QBR),
         Rate_rank = min_rank(-Rate),
         TD_rank = min_rank(-TD),
         Comp_rank = min_rank(-CompPercent)) %>%
  arrange(QBR_rank)

metric.tab19$Last[metric.tab19$Last == "Jackson*+"] <- "Jackson"
metric.tab19$Last[metric.tab19$Last == "Brees*"] <- "Brees"
metric.tab19$Last[metric.tab19$Last == "Tannehill*"] <- "Tannehill"

## join with EPA data
tab_EPA19 <- read.csv("~/Dropbox/TAMU/New_Projects/Git_edits/NFL/Shiny_Project/epa2019.csv") %>%
  select(Last, EPA, PLAYS, EPA_play, EPA_rank)
met.tab19 <- left_join(metric.tab19, tab_EPA19, by = "Last") 
met.tab19 <- met.tab19 %>%
  mutate(season = 2019)

### Save season data file
write.csv(met.tab19, "data2019.csv")


### Season: 2018
### Add ranking vars (to create the tables and figures in app)
## Reverse code the min_rank to rank the values from largest tp smallest
metric.tab18 <- metric.tab %>%
  filter(Att > 250) %>%
  mutate(ANYA_rank = min_rank(-ANY_A),
         Yards_rank = min_rank(-YardsGame),
         QBR_rank = min_rank(-QBR),
         Rate_rank = min_rank(-Rate),
         TD_rank = min_rank(-TD),
         Comp_rank = min_rank(-CompPercent)) %>%
  arrange(QBR_rank)

metric.tab18$Last[metric.tab18$Last == "Brees*"] <- "Brees"
metric.tab18$Last[metric.tab18$Last == "Goff*"] <- "Goff"
metric.tab18$Last[metric.tab18$Last == "Rivers*"] <- "Rivers"

## join with EPA data
tab_EPA18 <- read.csv("~/Dropbox/TAMU/New_Projects/Git_edits/NFL/Shiny_Project/epa2018.csv") %>%
  select(Last, EPA, PLAYS, EPA_play, EPA_rank)
met.tab18 <- left_join(metric.tab18, tab_EPA18, by = "Last")
met.tab18 <- met.tab18 %>%
  mutate(season = 2018)

### Save season data file
write.csv(met.tab18, "data2018.csv")


### Season: 2017
### Add ranking vars (to create the tables and figures in app)
## Reverse code the min_rank to rank the values from largest tp smallest
metric.tab17 <- metric.tab %>%
  filter(Att > 250) %>%
  mutate(ANYA_rank = min_rank(-ANY_A),
         Yards_rank = min_rank(-YardsGame),
         QBR_rank = min_rank(-QBR),
         Rate_rank = min_rank(-Rate),
         TD_rank = min_rank(-TD),
         Comp_rank = min_rank(-CompPercent)) %>%
  arrange(QBR_rank)

metric.tab17$Last[metric.tab17$Last == "Roethlisberger*"] <- "Roethlisberger"
metric.tab17$Last[metric.tab17$Last == "Smith*"] <- "Smith"
metric.tab17$Last[metric.tab17$Last == "Brees*"] <- "Brees"
metric.tab17$Last[metric.tab17$Last == "Goff*"] <- "Goff"
metric.tab17$Last[metric.tab17$Last == "Rivers*"] <- "Rivers"

## join with EPA data
tab_EPA17 <- read.csv("~/Dropbox/TAMU/New_Projects/Git_edits/NFL/Shiny_Project/epa2017.csv") %>%
  select(Last, EPA, PLAYS, EPA_play, EPA_rank)
met.tab17 <- left_join(metric.tab17, tab_EPA17, by = "Last") 
met.tab17 <- met.tab17 %>%
  mutate(season = 2017)

### Save season data file
write.csv(met.tab17, "data2017.csv")


### Season: 2016
### Add ranking vars (to create the tables and figures in app)
## Reverse code the min_rank to rank the values from largest tp smallest
metric.tab16 <- metric.tab %>%
  filter(Att > 250) %>%
  mutate(ANYA_rank = min_rank(-ANY_A),
         Yards_rank = min_rank(-YardsGame),
         QBR_rank = min_rank(-QBR),
         Rate_rank = min_rank(-Rate),
         TD_rank = min_rank(-TD),
         Comp_rank = min_rank(-CompPercent)) %>%
  arrange(QBR_rank)

metric.tab16$Last[metric.tab16$Last == "Ryan*+"] <- "Ryan"
metric.tab16$Last[metric.tab16$Last == "Roethlisberger*"] <- "Roethlisberger"
metric.tab16$Last[metric.tab16$Last == "Smith*"] <- "Smith"
metric.tab16$Last[metric.tab16$Last == "Brees*"] <- "Brees"
metric.tab16$Last[metric.tab16$Last == "Dalton*"] <- "Dalton"
metric.tab16$Last[metric.tab16$Last == "Rivers*"] <- "Rivers"

## join with EPA data
tab_EPA16 <- read.csv("~/Dropbox/TAMU/New_Projects/Git_edits/NFL/Shiny_Project/epa2016.csv") %>%
  select(Last, EPA, PLAYS, EPA_play, EPA_rank)
met.tab16 <- left_join(metric.tab16, tab_EPA16, by = "Last") 
met.tab16 <- met.tab16 %>%
  mutate(season = 2016)

### Save season data file
write.csv(met.tab16, "data2016.csv")

###################################################
### Save all season datasets to one large data file
dataFull <- bind_rows(met.tab16,met.tab17,met.tab18,met.tab19,met.tab20)
write.csv(dataFull, "dataFull.csv")








