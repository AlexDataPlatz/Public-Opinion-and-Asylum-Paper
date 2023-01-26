library(eurostat)
library(tidyverse)
library(tseries)
library(aTSA)
library(urca)
library(lmtest)
library(sandwich)
library(dynlm)
library(broom)
library(gtsummary)
library(writexl)
library(readxl)
library(sjmisc)
library(sjlabelled)
library(dLagM)
library(lmtest)
library(stargazer)
library(zoo)
library(lubridate)

# download and clean application data

# first 2008 to present data
# data id available from:
# https://ec.europa.eu/eurostat/web/main/data/database
# download takes a while (is > 4.5GB, can take ~ 30 minutes)
app_all <- get_eurostat("migr_asyappctza",
                        time_format = "raw")
head(app_all)

# export/ import to drive as excel file (to save time!)
write.csv(app_all, "app_all0821.csv")

app_all0821 <- read.csv("/Users/AlexHartland2/app_all0821.csv")

# list all unique decision and country values 
unique(app_all0821$asyl_app)
unique(app_all0821$citizen)

# filter out duplicate applications

dupl <- c("EU27_2020", "EXT_EU27_2020", "TOTAL",
          "EU28", "EXT_EU28")

app0821 <- app_all0821 %>%
  filter(asyl_app == "ASY_APP") %>%
  filter(!citizen %in% dupl) %>%
  filter(age == "TOTAL") %>%
  filter(sex == "T")

head(app0821)


# download and clean applications for 2002 to 2007
app_all0207 <- get_eurostat("migr_asyctz",
                            time_format = "raw")

head(app_all0207)

# save this db to wd (then can import next time)
write.csv(app_all0207, "app_all0207.csv")

app_all0207 <- read.csv("/~/app_all0207.csv")

# filter out duplicate values
# first, list all unique country values 
unique(app_all0207$citizen)

dupl2 <- c("TOTAL", "AFR", "AME", "ASI", "EFTA", "EU15",
           "EUR", "EUR_C_E", "EUR_OTH", "OCE", "EX_YU")

head(app_all0207) 

app0207 <- app_all0207 %>%
  filter(!citizen %in% dupl2)



# merge 2002-2007 data with 2008-2019 data
head(app0207)
asel0207 <- app0207 %>%
  filter(time > 2001) %>%
  select(citizen, geo, time, values)


head(deapp0821)
asel0819 <- app0821 %>%
  filter(time < 2020) %>%
  select(citizen, geo, time, values)

apps <- rbind(asel0207, asel0819)

head(apps)

# rename to match other dbs
apps2 <- rename(apps, c("Nationality" = "citizen",
                          "Year" = "time",
                          "ctrapp" = "values"))


# calculate total applications per year
apps3 <- apps2 %>% 
  group_by(geo) %>% 
  summarise(totapp = sum(ctrapp))

head(deapp3)

# filter out non-geo entities and nonEU28
unique(apps3$geo)

dupl3 <- c("EU27_2007", "EU27_2020", "EU28", "TOTAL",
           "CH", "IS", "NO", "LI")

apps4 <- apps3 %>%
  filter(!geo %in% dupl3)

# order by application number, select top 10 for 2002-2019
appstop10 <- apps4 %>% 
  arrange(desc(totapp)) %>%
  slice(1:10)

appstop10$geo

# go back and select most applications top 10 for 2015-16
apps0219 <- apps2 %>% 
  group_by(Year, geo) %>% 
  summarise(totapp = sum(ctrapp))

sub1516 <- apps0219 %>%
  filter(Year > 2014, Year < 2017) %>% 
  group_by(geo) %>% 
  summarise(apps = sum(totapp))

head(sub1516)

# filter out non-geo entities and nonEU28
unique(sub1516$geo)

dupl3 <- c("EU27_2007", "EU27_2020", "EU28", "TOTAL",
           "CH", "IS", "NO", "LI")

apps1516 <- sub1516 %>%
  filter(!geo %in% dupl3)

# order by application number, select top 10 for 2002-2019
top1516 <- apps1516 %>% 
  arrange(desc(apps)) %>%
  slice(1:10)

# list top 10 EU28 countries with most applications 2015-16
unique(top1516$geo)


########################################################################
# calculate ARR for top 10 countries 2015-16
dec_annual <- get_eurostat("migr_asydcfsta",
                        time_format = "raw")


# save/ import existing file for all decisions
write.csv(dec_annual, "dec_annual0820.csv")

dec_annual <- read.csv("/~/dec_ctry0819.csv")

head(dec_annual)

# list all unique decision and country values 
unique(dec_annual$decision)
unique(dec_annual$citizen)

# filter out duplicate applications

dupl <- c("EU27_2020", "EXT_EU27_2020", "TOTAL",
          "EU28", "EXT_EU28")

all_dec <- dec_annual %>%
  filter(!citizen %in% dupl) %>%
  filter(age == "TOTAL") %>%
  filter(sex == "T")

head(all_dec)

 # filter decisions by value
  # first select all total decision values
  dectot <- all_dec %>%
  filter(decision == "TOTAL")

# group total decisions by country and date
dectot1 <- dectot %>% 
  group_by(geo, time, decision) %>% 
  summarise(decy = sum(values))

# next do the same with all positive decisions
decpos <- all_dec %>%
  filter(decision == "TOTAL_POS")

decpos1 <- decpos %>% 
  group_by(geo, time, decision) %>% 
  summarise(posy = sum(values))

# create new db to calculate quarterly asylum recognition rate (pospq) 
dec_ctry <- dectot1
dec_ctry$posy <- decpos1$posy

# select top 10 countries for further analysis
top10 <- c("DE", "HU", "IT", "SE", "FR", 
        "AT", "UK", "NL", "EL", "BE")
dec10 <- dec_ctry %>%
  filter(geo %in% top10) %>%
  select("geo", "time", "decy", "posy")

dec1516 <- dec10 %>%
  filter(time > 2014, time < 2017) %>% 
  group_by(geo) %>% 
  summarise(dec = sum(decy), pos = sum(posy))

dec1516$arr <- dec1516$pos/ dec1516$dec * 100  

dec1516$arr


