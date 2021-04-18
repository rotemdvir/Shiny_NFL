### NFL Shiny project
### March 2021
### Create Data files: EPA (ESPN.COM)

# Set Working Directory to save data files
setwd("~/NFL/Shiny_Project")

library(foreign)
library(Hmisc)
library(devtools)
library(dplyr)
library(tidyverse)
library(XML)
library(RCurl)
library(xlsx)

### Scrap data: EPA on ESPN stats website
## For simplicity, I use the same generic code to create separate data files for each season
epa.dat <- getURL("https://www.espn.com/nfl/qbr/_/sort/cwepaTotal/dir/desc")    #2020
epa.dat <- getURL("https://www.espn.com/nfl/qbr/_/season/2019/seasontype/2/sort/cwepaTotal/dir/desc")    #2019
epa.dat <- getURL("https://www.espn.com/nfl/qbr/_/season/2018/seasontype/2/sort/cwepaTotal/dir/desc")    #2018
epa.dat <- getURL("https://www.espn.com/nfl/qbr/_/season/2017/seasontype/2/sort/cwepaTotal/dir/desc")    #2017
epa.dat <- getURL("https://www.espn.com/nfl/qbr/_/season/2016/seasontype/2/sort/cwepaTotal/dir/desc")    #2016

epa1a <- readHTMLTable(epa.dat, header = TRUE, which=1)
epa1b <- readHTMLTable(epa.dat, header = TRUE, which=2)
tab2 <- cbind(epa1a, epa1b)

## Remove and arrange QB name
tab2_ed <- tab2 %>% separate(Name, c("First", "Last", "Team"), " ")
tab2_ed$Last <- gsub('([[:upper:]])', ' \\1', tab2_ed$Last)
tab2_ed <- tab2_ed %>% separate(Last, c("A1", "A", "Last", "C", "D"), " ")
tab2_ed <- select(tab2_ed, -(A1))
tab2_ed <- select(tab2_ed, -(Last:Team))
names(tab2_ed)[names(tab2_ed) =="A"] <- "Last"

# convert to numeric
tab2_ed$EPA <- as.numeric(tab2_ed$EPA)
tab2_ed$PLAYS <- as.numeric(tab2_ed$PLAYS)

# Final EPA data per seasons (2016-2020) 
tab_EPA20 <- select(tab2_ed, First, Last, EPA, PLAYS) %>%
  mutate(EPA_play = EPA/PLAYS,
         EPA_rank = min_rank(-EPA_play)) %>%
  arrange(EPA_rank)

tab_EPA19 <- select(tab2_ed, First, Last, EPA, PLAYS) %>%
  mutate(EPA_play = EPA/PLAYS,
         EPA_rank = min_rank(-EPA_play)) %>%
  arrange(EPA_rank)

tab_EPA18 <- select(tab2_ed, First, Last, EPA, PLAYS) %>%
  mutate(EPA_play = EPA/PLAYS,
         EPA_rank = min_rank(-EPA_play)) %>%
  arrange(EPA_rank)

tab_EPA17 <- select(tab2_ed, First, Last, EPA, PLAYS) %>%
  mutate(EPA_play = EPA/PLAYS,
         EPA_rank = min_rank(-EPA_play)) %>%
  arrange(EPA_rank)

tab_EPA16 <- select(tab2_ed, First, Last, EPA, PLAYS) %>%
  mutate(EPA_play = EPA/PLAYS,
         EPA_rank = min_rank(-EPA_play)) %>%
  arrange(EPA_rank)

###################################################
### Save seasons to separate data files
write.csv(tab_EPA16, "epa2016.csv")
write.csv(tab_EPA17, "epa2017.csv")
write.csv(tab_EPA18, "epa2018.csv")
write.csv(tab_EPA19, "epa2019.csv")
write.csv(tab_EPA20, "epa2020.csv")
