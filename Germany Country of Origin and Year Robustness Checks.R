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

setwd("/Volumes/ALEXPHD3/Data/Immigration: Asylum")

dec_all <- read.csv("dec_all0821.csv")

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



decde0821 <- dec_filtered %>%
  select("citizen", "decision", "geo", "time", "values") %>%
  filter(geo == "DE")

write.csv(decde, "decde0821.csv")

decde0821 <- read.csv("decde0821.csv")


# filter decisions by value
# first select all total decision values
dectot <- decde0821 %>%
  filter(decision == "TOTAL")

# group total decisions by country and date
dectot1 <- dectot %>% 
  group_by(geo, time, citizen, decision) %>% 
  summarise(decq = sum(values))


# next do the same with all positive decisions
decpos <- decde0821 %>%
  filter(decision == "TOTAL_POS")

decpos1 <- decpos %>% 
  group_by(geo, time, citizen, decision) %>% 
  summarise(posq = sum(values))

# combine quarterly decisions and quarterly positive decisions 
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
  select("geo", "date", "citizen", "decision", "decq", "posq")


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
dec_all0207 <- read.csv("dec_all0207.csv")
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
  group_by(geo, date, citizen, decision) %>% 
  summarise(decq = sum(values))

# filter for DE only data
totde0207 <- dectot0207a %>%
  filter(geo == "DE")

# next do the same with all positive decisions
decpos0207 <- dec_filtered0207 %>%
  filter(decision == "TOTAL_POS")

decpos0207a <- decpos0207 %>% 
  group_by(geo, date, citizen, decision) %>% 
  summarise(posq = sum(values))


# filter for DE only data
posde0207 <- decpos0207a %>%
  filter(geo == "DE")

# combine decisions and positive decisions in same db 
dec_ctry0207 <- totde0207
dec_ctry0207$posq <- posde0207$posq

dec_ctry0207 <- as_tibble(dec_ctry0207)

# convert to date format
dec_ctry0207$date <- as.Date(dec_ctry0207$date)

# merge 2002-2007 data with 2008-2019 data
head(dec_ctry0207)
head(decde0819)

dec_ctry0207 <- as_tibble(dec_ctry0207)
decde0819 <- as_tibble(decde0819)


deall <- rbind(dec_ctry0207, decde0819)

write.csv(deall, "deall.csv")

setwd("~/Desktop/Data")

deall <- read.csv("deall.csv")

########################################################################
# remove Syrian data
deosy1 <- deall %>%
  filter(!citizen %in% "SY")

# calculate quarterly decisions and positive decisions w/o Syria
deosy <- deosy1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deosyh <- deosy %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))

deosyh$dech <- deosyh$dech / 2
deosyh$posh <- deosyh$posh / 2


# calculate asylum recongition rate rolling average
deosyh$posph <- deosyh$posh / deosyh$dech * 100

write_xlsx(deosyh, "deexsyh.xlsx")

# plot posph for DE
ggplot(deosyh) +
  geom_line(aes(date, posph))


ggplot(deosyh) +
  geom_line(aes(date, posph))


#####################################################################
# run ARDL models for ARR w/o Syria
# Import database (available from Dataverse)
deukdb <- read_excel("DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
deosyh1 <- na.omit(deosyh)

deosyh1$posph <- as.numeric(deosyh1$posph)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

dedb2$POSPH <- as.numeric(deosyh1$posph)
dedb2$TIMM2 <- as.Date(dedb2$TIMM2)

head(dedb2)

ggplot(dedb2) +
  geom_line(aes(TIMM2, POSPH)) +
  geom_line(aes(TIMM2, POLSALH))


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)



# Estimate model with Interaction Variable
M15 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(dpinteract, 1) +
              L(darr, 1:3))

# rename regressors for better readability
names(M15$coefficients) <- c("Intercept", "Salience (t-1)",
                            "Open Preference Class (t-1)",
                            "Salience*Preference Interaction (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M15)



########################################################################
# remove Iraq data
deoiq1 <- deall %>%
  filter(!citizen %in% "IQ")

# calculate quarterly decisions and positive decisions w/o Iraq
deoiq <- deoiq1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deoiqh <- deoiq %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right")/2,
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right")/2)


# calculate asylum recongition rate rolling average
deoiqh$posph <- deoiqh$posh / deoiqh$dech * 100

write_xlsx(deoiqh, "deoiqh.xlsx")

# plot posph for DE
ggplot(deoiqh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Iraq
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
deoiqh1 <- na.omit(deoiqh)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

deoiqh1$posph <- as.numeric(deoiqh1$posph)
dedb2$POSPH <- as.numeric(deoiqh1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)


# Estimate model with Interaction Variable
M16 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(dpinteract, 1) +
              L(darr, 1:2))

# rename regressors for better readability
names(M16$coefficients) <- c("Intercept", "Salience (t-1)",
                            "Open Preference Class (t-1)",
                            "Salience*Preference Interaction (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M16)


########################################################################
# remove Afghanistan data
deoaf1 <- deall %>%
  filter(citizen %in% "AF")

# calculate quarterly decisions and positive decisions w/o Afghanistan
deoaf <- deoaf1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deoafh <- deoaf %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right")/2,
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right")/2)


# calculate asylum recongition rate rolling average
deoafh$posph <- deoafh$posh / deoafh$dech * 100

write_xlsx(deoafh, "deoafh.xlsx")

# plot posph for DE
ggplot(deoafh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Afghanistan
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
deoafh1 <- na.omit(deoafh)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

deoafh1$posph <- as.numeric(deoafh1$posph)
dedb2$POSPH <- as.numeric(deoafh1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)



# Estimate model with Interaction Variable
M17 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(dpinteract, 1) +
              L(darr, 1:2))

# rename regressors for better readability
names(M17$coefficients) <- c("Intercept", "Salience (t-1)",
                            "Open Preference Class (t-1)",
                            "Salience*Preference Interaction (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M17)



########################################################################
# remove Serbia data
dears1 <- deall %>%
  filter(!citizen %in% "RS")

# calculate quarterly decisions and positive decisions w/o Serbia
dears <- dears1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
dearsh <- dears %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
dearsh$posph <- dearsh$posh / dearsh$dech * 100

write_xlsx(dearsh, "dearsh.xlsx")

# plot posph for DE
ggplot(dearsh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Serbia
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
dearsh1 <- na.omit(dearsh)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

dearsh1$posph <- as.numeric(dearsh1$posph)
dedb2$POSPH <- as.numeric(dearsh1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)




# Estimate model with Interaction Variable
M18 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M18$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M18)


########################################################################
# remove Albania data
deaal1 <- deall %>%
  filter(!citizen %in% "AL")

# calculate quarterly decisions and positive decisions w/o Albania
deaal <- deaal1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deaalh <- deaal %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
deaalh$posph <- deaalh$posh / deaalh$dech * 100

write_xlsx(deaalh, "deaalh.xlsx")

# plot posph for DE
ggplot(deaalh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Albania
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
deaalh1 <- na.omit(deaalh)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

deaalh1$posph <- as.numeric(deaalh1$posph)
dedb2$POSPH <- as.numeric(deaalh1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)





# Estimate model with Interaction Variable
M19 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M19$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M19)



########################################################################
# remove Iran data
deir1 <- deall %>%
  filter(!citizen %in% "IR")

# calculate quarterly decisions and positive decisions w/o Iran
deir <- deir1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deirh <- deir %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
deirh$posph <- deirh$posh / deirh$dech * 100

write_xlsx(deirh, "deirh.xlsx")

# plot posph for DE
ggplot(deirh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Iran
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
deirh1 <- na.omit(deirh)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

deirh1$posph <- as.numeric(deirh1$posph)
dedb2$POSPH <- as.numeric(deirh1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)





# Estimate model with Interaction Variable
M20 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M20$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M20)


########################################################################
# remove Russia data
deru1 <- deall %>%
  filter(!citizen %in% "RU")

# calculate quarterly decisions and positive decisions w/o Russia
deru <- deru1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deruh <- deru %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
deruh$posph <- deruh$posh / deruh$dech * 100

write_xlsx(deruh, "deruh.xlsx")

# plot posph for DE
ggplot(deruh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Russia
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
deruh1 <- na.omit(deruh)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

deruh1$posph <- as.numeric(deruh1$posph)
dedb2$POSPH <- as.numeric(deruh1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)



# Estimate model with Interaction Variable
M21 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M21$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M21)





########################################################################
# remove Turkey data
detr1 <- deall %>%
  filter(!citizen %in% "TR")

# calculate quarterly decisions and positive decisions w/o Turkey
detr <- detr1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
detrh <- detr %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
detrh$posph <- detrh$posh / detrh$dech * 100

write_xlsx(detrh, "detrh.xlsx")

# plot posph for DE
ggplot(detrh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Turkey
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
detrh1 <- na.omit(detrh)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

detrh1$posph <- as.numeric(detrh1$posph)
dedb2$POSPH <- as.numeric(detrh1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)


# Estimate model with Interaction Variable
M22 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M22$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M22)





########################################################################
# remove Eritrea data
deeri1 <- deall %>%
  filter(!citizen %in% "ER")

# calculate quarterly decisions and positive decisions w/o Eritrea
deeri <- deeri1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deerih <- deeri %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
deerih$posph <- deerih$posh / deerih$dech * 100

write_xlsx(deerih, "deerih.xlsx")

# plot posph for DE
ggplot(deerih) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Eritrea
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
deerih1 <- na.omit(deerih)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

deerih1$posph <- as.numeric(deerih1$posph)
dedb2$POSPH <- as.numeric(deerih1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)



# Estimate model with Interaction Variable
M23 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M23$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M23)





########################################################################
# remove Kosovo data
deko1 <- deall %>%
  filter(!citizen %in% "XK")

# calculate quarterly decisions and positive decisions w/o Kosovo
deko <- deko1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
dekoh <- deko %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
dekoh$posph <- dekoh$posh / dekoh$dech * 100

write_xlsx(dekoh, "dekoh.xlsx")

# plot posph for DE
ggplot(dekoh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Kosovo
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
dekoh1 <- na.omit(dekoh)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

dekoh1$posph <- as.numeric(dekoh1$posph)
dedb2$POSPH <- as.numeric(dekoh1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)


# Estimate model with Interaction Variable
M24 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M24$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M24)


# output regression tables
stargazer(M15, M16, M17, M18, M19,
          M20, M21, M22, M23, M24,
          type="html",
          out="DE Citizenship Robustness Checks (main effects).docx",
          model.names = T,
          single.row = F, 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          star.char = c("+", "*", "**", "***"),
          notes = c("+p<0.1; *p<0.05; **p<0.01;", "***p<0.001"),
          notes.append= FALSE,
          notes.align = "l",
          intercept.bottom = F,
          intercept.top = T,
          align = TRUE,
          omit.stat=c("LL","f"))







########################################################################
# next check top 10 countries vs rest of data
# first: results for aggregate of top 10 sending countries

top <- c("SY", "IQ", "AF", "RS", "AL",
         "IR", "RU", "TR", "ER", "XK")

detop1 <- deall %>%
  filter(citizen %in% top)

# calculate quarterly decisions and positive decisions w/o non top 10
detop <- detop1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
detoph <- detop %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
detoph$posph <- detoph$posh / detoph$dech * 100

write_xlsx(detoph, "detoph.xlsx")

# plot posph for DE
ggplot(detoph) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR of top 10 only
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
detoph1 <- na.omit(detoph)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

detoph1$posph <- as.numeric(detoph1$posph)
dedb2$POSPH <- as.numeric(detoph1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)




# estimate model with salience
M1 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:2))


# rename regressors for better readability
names(M1$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M1)


# Estimate model with 'open' immigration preference variable
M2 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(darr, 1:2))

# rename regressors for better readability
names(M2$coefficients) <- c("Intercept", "Salience (t-1)", 
                            "Open Preference Class (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M2)

# Estimate model with Interaction Variable
M25 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M25$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M25)






########################################################################
# remove top 10 data and test rest of data
derest1 <- deall %>%
  filter(!citizen %in% top)

# calculate quarterly decisions and positive decisions w/o top 10
derest <- derest1 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deresth <- derest %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
deresth$posph <- deresth$posh / deresth$dech * 100

write_xlsx(deresth, "deresth.xlsx")

# plot posph for DE
ggplot(deresth) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o top 10 sending countries
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
deresth1 <- na.omit(deresth)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:72)

deresth1$posph <- as.numeric(deresth1$posph)
dedb2$POSPH <- as.numeric(deresth1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)




# estimate model with salience
M1 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:4))


# rename regressors for better readability
names(M1$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M1)


# Estimate model with 'open' immigration preference variable
M2 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(darr, 1:2))

# rename regressors for better readability
names(M2$coefficients) <- c("Intercept", "Salience (t-1)", 
                            "Open Preference Class (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M2)

# Estimate model with Interaction Variable
M26 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M26$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M26)



# output regression tables
stargazer(M25, M26,
          type="html",
          out="DE Top 10 CoO vs Rest.docx",
          model.names = T,
          single.row = F, 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          star.char = c("+", "*", "**", "***"),
          notes = c("+p<0.1; *p<0.05; **p<0.01;", "***p<0.001"),
          notes.append= FALSE,
          notes.align = "l",
          intercept.bottom = F,
          intercept.top = T,
          align = TRUE,
          omit.stat=c("LL","f"))


#############################################################
# select post 2015 data
deall$date <- as.Date(deall$date)
de15191 <- deall %>%
  filter(date > "2014-12-31")

# calculate quarterly decisions and positive decisions 2015-19
de1519 <- de15191 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
de1519h <- de1519 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
de1519h$posph <- de1519h$posh / de1519h$dech * 100

write_xlsx(de1519h, "de1519h.xlsx")

# plot posph for DE
ggplot(de1519h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR for 2015 onwards
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
de1519h1 <- na.omit(de1519h)

de1519h1$posph <- as.numeric(de1519h1$posph)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(54:72)

dedb2$POSPH <- as.numeric(de1519h1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)




# estimate model with salience
M1 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:2))


# rename regressors for better readability
names(M1$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M1)


# Estimate model with 'open' immigration preference variable
M2 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(darr, 1:2))

# rename regressors for better readability
names(M2$coefficients) <- c("Intercept", "Salience (t-1)", 
                            "Open Preference Class (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M2)

# Estimate model with Interaction Variable
M35 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M35$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M35)




#############################################################
# select pre 2015 data
deall$date <- as.Date(deall$date)
de02141 <- deall %>%
  filter(date < "2015-01-31")

# calculate quarterly decisions and positive decisions 2002-2014
de0214 <- de02141 %>%
  group_by(geo, date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
de0214h <- de0214 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
de0214h$posph <- de0214h$posh / de0214h$dech * 100

write_xlsx(de0214h, "de0214h.xlsx")

# plot posph for DE
ggplot(de0214h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR pre 2015
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)

# omit missing values
dedb1 <- na.omit(dedb)
de0214h1 <- na.omit(de0214h)

de0214h1$posph <- as.numeric(de0214h1$posph)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(2:52)

dedb2$POSPH <- as.numeric(de0214h1$posph)


# convert variables to time series objects
sal <- as.ts(dedb2$POLSALH)
arr <- as.ts(dedb2$POSPH)
ocl <- as.ts(dedb2$OCLY)


# test variables for stationarity
adf.test(sal)
adf.test(arr)
adf.test(ocl)


# test salience and preference for cointegration with arr
coint.test(arr, sal, d = 0, nlag = NULL, output = TRUE)
coint.test(arr, ocl, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary or cointegrated

# next, take first differences to produce stationary variables
# preference variable only changes every 2 years, so don't take the difference!
dsal <- diff(sal)
darr <- diff(arr)

# test again for stationarity
adf.test(dsal)
adf.test(darr)

# variables are now stationary

# next create interaction variable
pinteract <- sal*ocl

# take first difference of interaction variable to include in models
dpinteract <- diff(pinteract)

# check ts plots of variables
ts.plot(sal)
ts.plot(arr)
ts.plot(pinteract)
ts.plot(dsal)
ts.plot(darr)
ts.plot(dpinteract)


# use AIC to select appropriate number of lags for darr variable
AR1_darr = arima(darr, order = c(1, 1, 0))
AR2_darr = arima(darr, order = c(2, 1, 0))
AR3_darr = arima(darr, order = c(3, 1, 0))
AR4_darr = arima(darr, order = c(4, 1, 0))
AR5_darr = arima(darr, order = c(5, 1, 0))
AR6_darr = arima(darr, order = c(6, 1, 0))
AR7_darr = arima(darr, order = c(7, 1, 0))
AR8_darr = arima(darr, order = c(8, 1, 0))
AR9_darr = arima(darr, order = c(9, 1, 0))

AIC(AR1_darr)
AIC(AR2_darr)
AIC(AR3_darr)
AIC(AR4_darr)
AIC(AR5_darr)
AIC(AR6_darr)
AIC(AR7_darr)
AIC(AR8_darr)
AIC(AR9_darr)




# estimate model with salience
M1 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:2))


# rename regressors for better readability
names(M1$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M1)


# Estimate model with 'open' immigration preference variable
M2 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(darr, 1:2))

# rename regressors for better readability
names(M2$coefficients) <- c("Intercept", "Salience (t-1)", 
                            "Open Preference Class (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M2)

# Estimate model with Interaction Variable
M36 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M36$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M36)



# output regression tables
stargazer(M35, M36,
          type="html",
          out="DE Pre v Post 2015 Robustness Checks.docx",
          model.names = T,
          single.row = F, 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          star.char = c("+", "*", "**", "***"),
          notes = c("+p<0.1; *p<0.05; **p<0.01;", "***p<0.001"),
          notes.append= FALSE,
          notes.align = "l",
          intercept.bottom = F,
          intercept.top = T,
          align = TRUE,
          omit.stat=c("LL","f"))





