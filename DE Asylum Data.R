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

# download and clean decisions data
# first 2008 to present data
# data id available from:
# https://ec.europa.eu/eurostat/web/main/data/database
# download takes a while (is > 4.5GB, can take ~ 30 minutes)
dec_all <- get_eurostat("migr_asydcfstq",
                        time_format = "raw")
head(dec_all)

write.csv(dec_all, "dec_all0821.csv")

# list all unique decision and country values 
unique(dec_all$decision)
unique(dec_all$citizen)

# filter out duplicates

dupl <- c("EU27_2020", "EXT_EU27_2020", "TOTAL",
          "EU28", "EXT_EU28")

dec_filtered <- dec_all %>%
  filter(!citizen %in% dupl) %>%
  filter(!age %in% "TOTAL") %>%
  filter(!sex %in% "T")

write.csv(dec_filtered, "dec_filtered0821.csv")
dec_0821 <- read.csv("dec_filtered0821.csv")

getwd()

all_dec <- dec_all %>%
  filter(!citizen %in% dupl) %>%
  filter(age == "TOTAL") %>%
  filter(sex == "T")

head(all_dec)


decde <- dec_0821 %>%
  filter(geo %in% de) %>%
  select("citizen", "decision", "geo", "time", "values")

write.csv(decde, "decde0821.csv")

decde0821 <- read.csv("decde0821.csv")


# filter decisions by value
# first select all total decision values
dectot <- decde0821 %>%
  filter(decision == "TOTAL")

# group total decisions by country and date
dectot1 <- dectot %>% 
  group_by(geo, time, decision) %>% 
  summarise(decq = sum(values))


# next do the same with all positive decisions
decpos <- decde0821 %>%
  filter(decision == "TOTAL_POS")

decpos1 <- decpos %>% 
  group_by(geo, time, decision) %>% 
  summarise(posq = sum(values))

# create new db to calculate quarterly asylum recognition rate (pospq) 
dec_ctry <- dectot1
dec_ctry$posq <- decpos1$posq


# for clarity, rename time variable --> date
dec_ctry <- dec_ctry %>% rename(date = time)

# reformat date variable from quarters to year-month-day
dec_ctry$date <- as.Date(as.yearqtr(dec_ctry$date, format = "%YQ%q"),
                         frac = 1)


# filter to exclude more recent data
# (excludes possibly incomplete data and pandemic effects)
dec_ctry <- dec_ctry %>%
  filter( date < "2020-03-31")

# select DE for further analysis
de <- c("DE")
decde0819 <- dec_ctry %>%
  filter(geo %in% de) %>%
  select("geo", "date", "decision", "decq", "posq")


head(decde0819)


#####################################################################
# download and clean decisions data
# code for 2002 to 2007
dec_all0207 <- get_eurostat("migr_asydctzm",
                            time_format = "raw")

head(dec_all0207)

# list all unique decision and country values 
unique(dec_all0207$decision)
unique(dec_all0207$geo)

# reformat time into date variable, from months to year-month-day
dec_all0207$date <- as.Date(as.yearqtr(dec_all0207$time, 
                                       format = "%YM%m"),
                            frac = 1)

# save this db to wd (can start from next step next time)
write.csv(dec_all0207, "dec_all0207.csv")

##############################################################
dec_all0207 <- read.csv("/Users/AlexHartland2/dec_all0207.csv")
head(dec_all0207)
unique(dec_all0207$citizen)

dec_filtered0207 <- dec_all0207 %>%
  filter(!citizen %in% "TOTAL")


# filter decisions by value
# first select all total decision values
dectot0207 <- dec_filtered0207 %>%
  filter(decision == "TOTAL")

# group total decisions by country and date
dectot0207a <- dectot0207 %>% 
  group_by(geo, date, decision) %>% 
  summarise(decq = sum(values))

# next do the same with all positive decisions
decpos0207 <- dec_filtered0207 %>%
  filter(decision == "TOTAL_POS")

decpos0207a <- decpos0207 %>% 
  group_by(geo, date, decision) %>% 
  summarise(posq = sum(values))

# create new db to calculate quarterly asylum recognition rate (pospq) 
dec_ctry0207 <- dectot0207a
dec_ctry0207$posq <- decpos0207a$posq

dec_ctry0207 <- as_tibble(dec_ctry0207)


# convert to date format
dec_ctry0207$date <- as.Date(dec_ctry0207$date)


# select countries for further analysis
decde0207 <- dec_ctry0207 %>%
  filter(geo == "DE")

# merge 2002-2007 data with 2008-2019 data
head(decde0207)
head(decde0819)

decde0207 <- as_tibble(decde0207)
decde0819 <- as_tibble(decde0819)


deall <- rbind(decde0207, decde0819)


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deallh <- deall %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


deallh$dech <- deallh$dech / 2
deallh$posh <- deallh$posh / 2

# calculate asylum recongition rate rolling average
deallh$posph <- deallh$posh / deallh$dech * 100

write_xlsx(deallh, "deallh.xlsx")

# plot posph for DE
ggplot(deallh) +
  geom_line(aes(date, posph))
