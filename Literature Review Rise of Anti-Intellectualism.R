setwd("~/Downloads/QMSS Statistics II")
require(data.table)
require(tidycensus)
library(data.table)
library(ggplot2)

educ_data <- fread("socioeconomic_voting.csv", 
                   stringsAsFactors = FALSE, 
                   data.table = FALSE)

View(educ_data)

educ_datafilt <- subset(educ_data, State == "NEW YORK")
View(educ_datafilt)
names(educ_datafilt)

plot(educ_datafilt$`Unemployment Rate 2020`)
plot(educ_datafilt$`Vote Percentage`)
plot(educ_datafilt$`County Income Percentile Within State (2021)`)

prop.table(table(educ_datafilt$Party))

boxplot(`Bachelor's Degree or Higher Percentage (2018-2022)` ~ Party,
        data = educ_datafilt,
        main = "Education Level by Party",
        ylab = "Bachelor's Degree or Higher (%)",
        col = c("blue", "red"))


#######################################################################

library(dplyr)
educ_datafilt$anti_intellectual_flag <- ifelse(
        grepl("education|elit|professor|college|science|expert", 
            educ_datafilt$`V242165 - POST: Mention 1 most important problems facing the country [text]`, 
            ignore.case = TRUE), 1, 0)
 table(educ_datafilt$anti_intellectual_flag, educ_datafilt$Party)

##Out of these issues, 3 Democrats mentioned these problems. While 18 Democrats 
##did not, and 41 Republicans did not.
 
educ_datafilt %>%
  filter(grepl("education|political divisiveness|the 1% not paying their share of taxes|
               excessive government spending and inflation|Climate change and other environmental problems.|
               America becoming an Oligarchy", 
               `V242165 - POST: Mention 1 most important problems facing the country [text]`, 
               ignore.case = TRUE)) %>%
select(`County Name`, Party, `V242165 - POST: Mention 1 most important problems facing the country [text]`)

##According to these results, 18 Democrats and 41 Republican's 
###don't believe in these issues, while 5 Democrats do and 1 Republican.

educ_datafilt$anti_intellectual_flag <- ifelse(
  grepl("education|political divisiveness|the 1% not paying their share of taxes|
          excessive government spending and inflation|Climate change and other environmental problems.|
          America becoming an Oligarchy",
        educ_datafilt$`V242165 - POST: Mention 1 most important problems facing the country [text]`, 
        ignore.case = TRUE), 1, 0)

table(educ_datafilt$anti_intellectual_flag, educ_datafilt$Party)


###Multiple Optional Flags 
educ_datafilt$climate_concern <- ifelse(
  grepl("Just climate change|education", 
        educ_datafilt$`V242167 - POST: Mention 2 most important problems facing the country [text]`, 
        ignore.case = TRUE), 1, 0)
table(educ_datafilt$climate_concern, educ_datafilt$Party)


educ_datafilt$abort_concern <- ifelse(
  grepl("reproductive health for women | public health funding",
        educ_datafilt$`V242167 - POST: Mention 2 most important problems facing the country [text]`, 
        ignore.case = TRUE), 1, 0)

table(educ_datafilt$abort_concern, educ_datafilt$Party)

##21 Democrats and 41 Republicans haven't mentioned abortion healthcare


educ_datafilt$anti_elite_flag <- ifelse(
  grepl("too much govt control|the 1%|corrupt politicans|elites", 
        educ_datafilt$`V242167 - POST: Mention 2 most important problems facing the country [text]`,
        ignore.case = TRUE), 1, 0)

table(educ_datafilt$anti_elite_flag, educ_datafilt$Party)

##Again, the results are 21 Democrats and 40 Republicans did not mention these issues.
###Only 1 republican mentioned these problems.
