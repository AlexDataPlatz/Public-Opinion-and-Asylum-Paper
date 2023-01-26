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

dec_all <- read.csv("dec_all0821.csv")


# list all unique decision and country values 
unique(dec_all$decision)
unique(dec_all$citizen)


de19 <- dec_all %>%
  filter(geo == "DE")

# filter out duplicates
dupl <- c("EU27_2020", "EXT_EU27_2020", "TOTAL",
          "EU28", "EXT_EU28")

aged <- c("TOTAL", "UNK", "Y14-17", "Y_LT14")

decde0821 <- de19 %>%
  filter(!sex %in% c("T","UNK")) %>%
  filter(!age %in% aged) %>%
  filter(!citizen %in% dupl)

ls(decde0821)
unique(decde0821$citizen)

# filter decisions by value
# first select all total decision values
dectot <- decde0821 %>%
  filter(decision == "TOTAL")

# group total decisions by country and date
dectot1 <- dectot %>% 
  group_by(time, citizen, decision, age, sex) %>% 
  summarise(decq = sum(values))


# next do the same with all positive decisions
decpos <- decde0821 %>%
  filter(decision == "TOTAL_POS")

decpos1 <- decpos %>% 
  group_by(time, citizen, decision, age, sex) %>% 
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
decde0819 <- dec_ctry %>%
  filter( date < "2020-03-31")


########################################################################
# remove under 18s
unique(decde0819)
delt181 <- decde0819 %>%
  filter(!age %in% "Y_LT18")

# calculate quarterly decisions and positive decisions w/o Syria
delt18 <- delt181 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
delt18h <- delt18 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
delt18h$posph <- delt18h$posh / delt18h$dech * 100

write_xlsx(delt18h, "delt18h.xlsx")

# plot posph for DE
ggplot(delt18h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o under 18s
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
delt18h1 <- na.omit(delt18h)

delt18h1$posph <- as.numeric(delt18h1$posph)

# select 2008-2019 data
dedb2 <- dedb1 %>%
  slice(26:72)

dedb2$POSPH <- as.numeric(delt18h1$posph)


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
M27 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M27$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M27)




########################################################################
# remove 18-34 yos

de18341 <- decde0819 %>%
  filter(!age %in% "Y18-34")

# calculate quarterly decisions and positive decisions w/o Syria
de1834 <- de18341 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
de1834h <- de1834 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
de1834h$posph <- de1834h$posh / de1834h$dech * 100

write_xlsx(de1834h, "de1834h.xlsx")

# plot posph for DE
ggplot(de1834h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o 18-34 yos
# Import database
deukdb <- read_excel("/~/DEUKDB4.xlsx")

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
de1834h1 <- na.omit(de1834h)

de1834h1$posph <- as.numeric(de1834h1$posph)

# select 2008-2019 data
dedb2 <- dedb1 %>%
  slice(26:72)

dedb2$POSPH <- as.numeric(de1834h1$posph)


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
M28 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M28$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M28)




########################################################################
# remove 35 to 64 year olds

de35641 <- decde0819 %>%
  filter(!age %in% "Y35-64")

# calculate quarterly decisions and positive decisions w/o Syria
de3564 <- de35641 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
de3564h <- de3564 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
de3564h$posph <- de3564h$posh / de3564h$dech * 100

write_xlsx(de3564h, "de3564h.xlsx")

# plot posph for DE
ggplot(de3564h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o 35064 yos
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
de3564h1 <- na.omit(de3564h)

de3564h1$posph <- as.numeric(de3564h1$posph)

# select 2008-2019 data
dedb2 <- dedb1 %>%
  slice(26:72)

dedb2$POSPH <- as.numeric(de3564h1$posph)


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
M29 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M29$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M29)




########################################################################
# remove 65+ year olds
unique(decde0819)
dege651 <- decde0819 %>%
  filter(!age %in% "Y_GE65")

# calculate quarterly decisions and positive decisions w/o Syria
dege65 <- dege651 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
dege65h <- dege65 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
dege65h$posph <- dege65h$posh / dege65h$dech * 100

write_xlsx(dege65h, "dege65h.xlsx")

# plot posph for DE
ggplot(dege65h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o 65+ yos
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
dege65h1 <- na.omit(dege65h)

dege65h1$posph <- as.numeric(dege65h1$posph)

# select 2008-2019 data
dedb2 <- dedb1 %>%
  slice(26:72)

dedb2$POSPH <- as.numeric(dege65h1$posph)


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
M30 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M30$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M30)



########################################################################
# remove fem, test for male only

demal1 <- decde0819 %>%
  filter(sex %in% "M")

# calculate quarterly decisions and positive decisions w/o Syria
demal <- demal1 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
demalh <- demal %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
demalh$posph <- demalh$posh / demalh$dech * 100

write_xlsx(demalh, "demalh.xlsx")

# plot posph for DE
ggplot(demalh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o female data
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
demalh1 <- na.omit(demalh)

demalh1$posph <- as.numeric(demalh1$posph)

# select 2008-2019 data
dedb2 <- dedb1 %>%
  slice(26:72)

dedb2$POSPH <- as.numeric(demalh1$posph)


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
M31 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M31$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M31)



########################################################################
# remove male, test for female only

defem1 <- decde0819 %>%
  filter(sex %in% "F")

# calculate quarterly decisions and positive decisions w/o Syria
defem <- defem1 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
defemh <- defem %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
defemh$posph <- defemh$posh / defemh$dech * 100

write_xlsx(defemh, "defemh.xlsx")

# plot posph for DE
ggplot(defemh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o men
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
defemh1 <- na.omit(defemh)

defemh1$posph <- as.numeric(defemh1$posph)

# select 2008-2019 data
dedb2 <- dedb1 %>%
  slice(26:72)

dedb2$POSPH <- as.numeric(defemh1$posph)


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
M32 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M32$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M32)





# output regression tables
stargazer(M27, M28, M29, M30, M31, M32,
          type="html",
          out="DE Age, Sex Robustness Check.docx",
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



##############################################################

# Next bring in 2002-2007 data to test by decision type
dec_all0207 <- read.csv("dec_all0207.csv")
head(dec_all0207)
unique(dec_all0207$citizen)

dec_filtered0207 <- dec_all0207 %>%
  filter(citizen %in% "TOTAL") %>%
  filter(geo == "DE")


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

unique(dec_filtered0207$decision)

# next do the same with all Geneva convention decisions
degen0207 <- dec_filtered0207 %>%
  filter(decision == "GENCONV")

decgen0207a <- degen0207 %>% 
  group_by(geo, date, citizen) %>% 
  summarise(genq = sum(values))


# filter for DE only data
gende0207 <- decgen0207a %>%
  filter(geo == "DE")

# combine decisions and positive decisions in same db 
dec_ctry0207 <- totde0207
gende0207$decq <- totde0207$decq

gende0207 <- as_tibble(gende0207)

# convert to date format
gende0207$date <- as.Date(gende0207$date)


# add non-Geneva positive decision data
unique(dec_filtered0207$decision)


depos0207 <- dec_filtered0207 %>%
  filter(decision == "TOTAL_POS")

decpos0207a <- depos0207 %>% 
  group_by(date) %>%
  summarise(posq = sum(values))


# combine decisions and positive decisions in same db 
gende0207$posq <- decpos0207a$posq
gende0207$othq <- gende0207$posq - gende0207$genq


# calculate geneva/ other totals for 2008-19
setwd("~/Desktop/Data")

de19 <- read.csv("de19.csv")

# filter out duplicates
dupl <- c("EU27_2020", "EXT_EU27_2020", "TOTAL",
          "EU28", "EXT_EU28")

aged <- c("TOTAL", "UNK", "Y14-17", "Y_LT14")

decde0821 <- de19 %>%
  filter(sex == "T") %>%
  filter(age == "TOTAL") %>%
  filter(!citizen %in% dupl)

ls(decde0821)
unique(decde0821$citizen)

# filter decisions by value
# first select all total decision values
dectot <- decde0821 %>%
  filter(decision == "TOTAL")

# group total decisions by country and date
dectot1 <- dectot %>% 
  group_by(time) %>% 
  summarise(decq = sum(values))


# next do the same with all positive decisions
decpos <- decde0821 %>%
  filter(decision == "TOTAL_POS")

decpos1 <- decpos %>% 
  group_by(time) %>% 
  summarise(posq = sum(values))

# combine quarterly decisions and quarterly positive decisions 
dec_ctry <- dectot1
dec_ctry$posq <- decpos1$posq

# next do the same with all GENEVA decisions
decgen <- decde0821 %>%
  filter(decision == "GENCONV")

decgen1 <- decgen %>% 
  group_by(time) %>% 
  summarise(genq = sum(values))

# combine quarterly decisions and quarterly positive decisions 
degen0821 <- dectot1
degen0821$posq <- decpos1$posq
degen0821$genq <- decgen1$genq
degen0821$othq <- degen0821$posq - degen0821$genq

# for clarity, rename time variable --> date
degen0821 <- degen0821 %>% rename(date = time)

# reformat date variable from quarters to year-month-day
degen0821$date <- as.Date(as.yearqtr(degen0821$date, format = "%YQ%q"),
                         frac = 1)



# merge 2002-2007 data with 2008-2019 data
head(gende0207)
head(degen0821)

gende0207a <- gende0207 %>%
  select(date, decq, posq, genq, othq)


degen_all <- rbind(gende0207a, degen0821)

write.csv(degen_all, "degen_all.csv")

setwd("~/Desktop/Data")

degen_all <- read.csv("degen_all.csv")


########################################################################
# analyse Geneva convention decisions
# calculate quarterly decisions and positive decisions
dewgen <- degen_all %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(genq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
dewgenh <- dewgen %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
dewgenh$posph <- dewgenh$posh / dewgenh$dech * 100

write_xlsx(dewgenh, "dewgenh.xlsx")

# plot posph for DE
ggplot(dewgenh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR Geneva decisions only
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
dewgenh1 <- na.omit(dewgenh)

dewgenh1$posph <- as.numeric(dewgenh1$posph)

# select 2002-2019 data
dewgenh2 <- dewgenh1 %>%
  slice(1:71)

dedb2 <- dedb1 %>%
  slice(2:72)

dedb2$POSPH <- as.numeric(dewgenh2$posph)


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
M33 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M33$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M33)



########################################################################
# analyse Non-Geneva convention decisions
# calculate quarterly decisions and positive decisions
deoth <- degen_all %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(othq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for DE
deothh <- deoth %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
deothh$posph <- deothh$posh / deothh$dech * 100

write_xlsx(deothh, "deothh.xlsx")

# plot posph for DE
ggplot(deothh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o Syria
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
deothh1 <- na.omit(deothh)

deothh1$posph <- as.numeric(deothh1$posph)

# select 2002-2019 data
deothh2 <- deothh1 %>%
  slice(1:71)

dedb2 <- dedb1 %>%
  slice(2:72)

dedb2$POSPH <- as.numeric(deothh2$posph)


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
M34 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M34$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M34)

# output regression tables
stargazer(M33, M34,
          type="html",
          out="DE By Decision Robustness Check.docx",
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

