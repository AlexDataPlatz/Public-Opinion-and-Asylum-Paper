# load required packages
library(tidyverse)
library(rio)
library(janitor)
library(zoo)
library(tseries)
library(aTSA)
library(urca)
library(lmtest)
library(sandwich)
library(dynlm)
library(broom)
library(gtsummary)
library(readxl)
library(xlsx)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(dLagM)
library(lmtest)
library(stargazer)
library(spdep)
library(httr)
library(dataverse)


# Import asylum dataset from Home Office website
url <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1011720/asylum-applications-datasets-jun-2021.xlsx'
ukall <- rio::import(file = url,which = 9)

# Use correct variable names (from row 1)
ukall2 <- ukall %>%
  row_to_names(row_number = 1)


# filter db to remove resettlements and withdrawn cases
gf <- c("Grant of Other Leave", "Grant of Protection",  "Refused")

afil <- c("Total (pre-2009)", "Unknown")

sfil <- c("Total (pre-2009)", "Unknown Sex")


ukall3 <- ukall2 %>%
  filter(`Case type` == "Asylum Case") %>%
  filter(`Case outcome group` %in% gf) %>%
  filter(!`Age` %in% afil) %>%
  filter(!`Sex` %in% sfil)


ukall3$Decisions <- as.numeric(ukall3$Decisions)


# filter decisions by value
# first select all total decision values
dectot <- ukall3 %>%
  filter(Decisions == "TOTAL")

# group total decisions by country and date
dectot1 <- ukall3 %>% 
  group_by(Quarter, Age, Sex) %>% 
  summarise(decq = sum(Decisions))


# next do the same with all positive decisions
decpos <- ukall3 %>%
  filter(!`Case outcome group` %in% "Refused")

decpos1 <- decpos %>% 
  group_by(Quarter, Age, Sex) %>% 
  summarise(posq = sum(Decisions))

# combine quarterly decisions and quarterly positive decisions 
dec_ctry <- dectot1
dec_ctry$posq <- decpos1$posq


# for clarity, rename time variable --> date
dec_ctry <- dec_ctry %>% rename(date = Quarter)

# reformat date variable from quarters to year-month-day
dec_ctry$date <- as.Date(as.yearqtr(dec_ctry$date, format = "%Y Q%q"),
                         frac = 1)


# filter to exclude more recent data
# (excludes possibly incomplete data and pandemic effects)
uk0819 <- dec_ctry %>%
  filter( date < "2020-03-31")


########################################################################
# remove under 18s
unique(decde0819)
uklt181 <- uk0819 %>%
  filter(!Age %in% "Under 18")

# calculate quarterly decisions and positive decisions w/o U18s
uklt18 <- uklt181 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for ukcisions,
# positive ukcisions, and asylum recognition rates (posph)
# first get sums for uk
uklt18h <- uklt18 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))

uklt18h$posh <- uklt18h$posh / 2
uklt18h$dech <- uklt18h$dech / 2


# calculate asylum recongition rate rolling average
uklt18h$posph <- uklt18h$posh / uklt18h$dech * 100


write_xlsx(uklt18h, "uklt18h.xlsx")

# plot posph for uk
ggplot(uklt18h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o under 18s
# Import database for UK public opinion data
deukdb <- get_dataframe_by_name(
  filename = "DEUKDB_JPP.xlsx",
  dataset = "10.7910/DVN/FSRPMX", 
  server = "dataverse.harvard.edu")
# select data for UK
ukdb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "UK")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
ukdb$POLSALH <- as.numeric(ukdb$POLSALH)
ukdb$OCLY <- as.numeric(ukdb$OCLY)

# omit missing values
ukdb1 <- na.omit(ukdb)
uklt18h1 <- na.omit(uklt18h)

uklt18h1$posph <- as.numeric(uklt18h1$posph)

# select 2009-2019 data
ukdb2 <- ukdb1 %>%
  slice(33:75)

ukdb2$POSPH <- as.numeric(uklt18h1$posph)


# convert variables to time series objects
sal <- as.ts(ukdb2$POLSALH)
arr <- as.ts(ukdb2$POSPH)
ocl <- as.ts(ukdb2$OCLY)


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

# take first difference of interaction variable to incluuk in moukls
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
M27 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:2))


# rename regressors for better readability
names(M27$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M27)


########################################################################
# remove 18-29 yos
uk18291 <- uk0819 %>%
  filter(!Age %in% "18-29")

# calculate quarterly decisions and positive decisions w/o 18-29s
uk1829 <- uk18291 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for ukcisions,
# positive ukcisions, and asylum recognition rates (posph)
# first get sums for uk
uk1829h <- uk1829 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))

uk1829h$posh <- uk1829h$posh / 2
uk1829h$dech <- uk1829h$dech / 2


# calculate asylum recongition rate rolling average
uk1829h$posph <- uk1829h$posh / uk1829h$dech * 100

write_xlsx(uk1829h, "uk1829h.xlsx")

# plot posph for uk
ggplot(uk1829h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o 18-29 yos
# select data for UK
ukdb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "UK")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
ukdb$POLSALH <- as.numeric(ukdb$POLSALH)
ukdb$OCLY <- as.numeric(ukdb$OCLY)

# omit missing values
ukdb1 <- na.omit(ukdb)
uk1829h1 <- na.omit(uk1829h)

uk1829h1$posph <- as.numeric(uk1829h1$posph)

# select 2009-2019 data
ukdb2 <- ukdb1 %>%
  slice(33:75)

ukdb2$POSPH <- as.numeric(uk1829h1$posph)


# convert variables to time series objects
sal <- as.ts(ukdb2$POLSALH)
arr <- as.ts(ukdb2$POSPH)
ocl <- as.ts(ukdb2$OCLY)


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

# take first difference of interaction variable to incluuk in moukls
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
M28 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:2))


# rename regressors for better readability
names(M28$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M28)



########################################################################
# remove 30-49 yos
uk30491 <- uk0819 %>%
  filter(!Age %in% "30-49")

# calculate quarterly decisions and positive decisions w/o 30-49 yos
uk3049 <- uk30491 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for ukcisions,
# positive ukcisions, and asylum recognition rates (posph)
# first get sums for uk
uk3049h <- uk3049 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))

uk3049h$posh <- uk3049h$posh / 2
uk3049h$dech <- uk3049h$dech / 2


# calculate asylum recongition rate rolling average
uk3049h$posph <- uk3049h$posh / uk3049h$dech * 100

write_xlsx(uk3049h, "uk3049h.xlsx")

# plot posph for uk
ggplot(uk3049h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o 30-49 yos

# select data for UK
ukdb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "UK")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
ukdb$POLSALH <- as.numeric(ukdb$POLSALH)
ukdb$OCLY <- as.numeric(ukdb$OCLY)

# omit missing values
ukdb1 <- na.omit(ukdb)
uk3049h1 <- na.omit(uk3049h)

uk3049h1$posph <- as.numeric(uk3049h1$posph)

# select 2009-2019 data
ukdb2 <- ukdb1 %>%
  slice(33:75)

ukdb2$POSPH <- as.numeric(uk3049h1$posph)


# convert variables to time series objects
sal <- as.ts(ukdb2$POLSALH)
arr <- as.ts(ukdb2$POSPH)
ocl <- as.ts(ukdb2$OCLY)


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

# take first difference of interaction variable to incluuk in moukls
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
M29 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:2))


# rename regressors for better readability
names(M29$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M29)



########################################################################
# remove 50+ yos
ukov501 <- uk0819 %>%
  filter(!Age %in% c("50-69","70+"))

# calculate quarterly decisions and positive decisions w/o 50+ yos
ukov50 <- ukov501 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for ukcisions,
# positive ukcisions, and asylum recognition rates (posph)
# first get sums for uk
ukov50h <- ukov50 %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))

ukov50h$posh <- ukov50h$posh / 2
ukov50h$dech <- ukov50h$dech / 2


# calculate asylum recongition rate rolling average
ukov50h$posph <- ukov50h$posh / ukov50h$dech * 100

write_xlsx(ukov50h, "ukov50h.xlsx")

# plot posph for uk
ggplot(ukov50h) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o 50+

# select data for UK
ukdb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "UK")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
ukdb$POLSALH <- as.numeric(ukdb$POLSALH)
ukdb$OCLY <- as.numeric(ukdb$OCLY)

# omit missing values
ukdb1 <- na.omit(ukdb)
ukov50h1 <- na.omit(ukov50h)

ukov50h1$posph <- as.numeric(ukov50h1$posph)

# select 2009-2019 data
ukdb2 <- ukdb1 %>%
  slice(33:75)

ukdb2$POSPH <- as.numeric(ukov50h1$posph)


# convert variables to time series objects
sal <- as.ts(ukdb2$POLSALH)
arr <- as.ts(ukdb2$POSPH)
ocl <- as.ts(ukdb2$OCLY)


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

# take first difference of interaction variable to incluuk in moukls
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
M30 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:2))


# rename regressors for better readability
names(M30$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M30)





########################################################################
# remove male asylum seekers
ukfem1 <- uk0819 %>%
  filter(!Sex %in% "Male")

# calculate quarterly decisions and positive decisions w/o men
ukfem <- ukfem1 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for uk
ukfemh <- ukfem %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))

ukfemh$posh <- ukfemh$posh / 2
ukfemh$dech <- ukfemh$dech / 2


# calculate asylum recongition rate rolling average
ukfemh$posph <- ukfemh$posh / ukfemh$dech * 100

write_xlsx(ukfemh, "ukfemh.xlsx")

# plot posph for uk
ggplot(ukfemh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o men

# select data for UK
ukdb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "UK")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
ukdb$POLSALH <- as.numeric(ukdb$POLSALH)
ukdb$OCLY <- as.numeric(ukdb$OCLY)

# omit missing values
ukdb1 <- na.omit(ukdb)
ukfemh1 <- na.omit(ukfemh)

ukfemh1$posph <- as.numeric(ukfemh1$posph)

# select 2009-2019 data
ukdb2 <- ukdb1 %>%
  slice(33:75)

ukdb2$POSPH <- as.numeric(ukfemh1$posph)


# convert variables to time series objects
sal <- as.ts(ukdb2$POLSALH)
arr <- as.ts(ukdb2$POSPH)
ocl <- as.ts(ukdb2$OCLY)


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

# take first difference of interaction variable to incluuk in moukls
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
M31 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:2))


# rename regressors for better readability
names(M31$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M31)




########################################################################
# remove female asylum seekers
ukmal1 <- uk0819 %>%
  filter(Sex %in% "Male")

# calculate quarterly decisions and positive decisions w/o women
ukmal <- ukmal1 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for ukcisions,
# positive ukcisions, and asylum recognition rates (posph)
# first get sums for uk
ukmalh <- ukmal %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))

ukmalh$posh <- ukmalh$posh / 2
ukmalh$dech <- ukmalh$dech / 2


# calculate asylum recongition rate rolling average
ukmalh$posph <- ukmalh$posh / ukmalh$dech * 100

write_xlsx(ukmalh, "ukmalh.xlsx")

# plot posph for uk
ggplot(ukmalh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR w/o female
# omit missing values
ukmalh1 <- na.omit(ukmalh)

ukmalh1$posph <- as.numeric(ukmalh1$posph)

# select 2009-2019 data
ukdb2 <- ukdb1 %>%
  slice(33:75)

ukdb2$POSPH <- as.numeric(ukmalh1$posph)


# convert variables to time series objects
sal <- as.ts(ukdb2$POLSALH)
arr <- as.ts(ukdb2$POSPH)
ocl <- as.ts(ukdb2$OCLY)


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
M32 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:2))


# rename regressors for better readability
names(M32$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M32)





# output regression tables
stargazer(M27, M28, M29, M30, M31, M32,
          type="html",
          out="UK Age, Sex Robustness Check.docx",
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



###############################################################

# Next separate and test Geneva v Other Grants

# Import dataset from Home Office website
url <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1011720/asylum-applications-datasets-jun-2021.xlsx'
ukall <- rio::import(file = url,which = 9)

# Use correct variable names (from row 1)
ukall2 <- ukall %>%
  row_to_names(row_number = 1)

head(ukall2)

unique(ukall2$`Case outcome group`)
unique(ukall2$`Case type`)
unique(ukall2$`Case outcome`)

# filter db to remove resettlements and withdrawn cases
gf <- c("Grant of Other Leave", "Grant of Protection",  "Refused")

ukall3 <- ukall2 %>%
  filter(`Case type` == "Asylum Case") %>%
  filter(`Case outcome group` %in% gf)

head(ukall3)

ukall3$Decisions <- as.numeric(ukall3$Decisions)

# calculate decisions per quarter
ukdec <- ukall3 %>% 
  group_by(Quarter) %>% 
  summarise(decq = sum(Decisions))

# calculate positive Geneva Conv decisions per quarter

ukgen <- ukall3 %>%
  filter(`Case outcome` %in% "Asylum")

ukgen2 <- ukgen %>% 
  group_by(Quarter) %>% 
  summarise(genq = sum(Decisions))


# calculate positive other decisions per quarter

ukoth <- ukall3 %>%
  filter(!`Case outcome` %in% "Asylum") %>%
  filter(!`Case outcome group` %in% "Refused")

ukoth2 <- ukoth %>% 
  group_by(Quarter) %>% 
  summarise(othq = sum(Decisions))

ukgen2$othq <- ukoth2$othq

# merge decq, genq and othq into new db
ukpd <- merge(ukdec, ukgen2, all = TRUE)
ukpd[is.na(ukpd)] <- 0


# create standard date variable
ukpd$date <- as.Date(as.yearqtr(ukpd$Quarter, format = "%Y Q%q"),
                     frac = 1)

################################################################
# filter out specific nationalities for test ARDL models
# calculate 6 month rolling averages for ukcisions,
# positive ukcisions, and asylum recognition rates (posph)
# first get sums for uk
ukgenh <- ukpd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                genh = zoo::rollsum(genq, k = 2, 
                                    fill = NA, align = "right"),
                othh = zoo::rollsum(othq, k = 2, 
                                    fill = NA, align = "right"))

ukgenh$genh <- ukgenh$genh / 2
ukgenh$othh <- ukgenh$othh / 2
ukgenh$dech <- ukgenh$dech / 2


# calculate asylum recongition rate rolling average
ukgenh$genph <- ukgenh$genh / ukgenh$dech * 100
ukgenh$othph <- ukgenh$othh / ukgenh$dech * 100


write_xlsx(ukgenh, "ukgenh.xlsx")

# plot posph for uk
ggplot(ukgenh) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR by decision type

# omit missing values
ukdb1 <- na.omit(ukdb)
ukgenh1 <- na.omit(ukgenh)

ukgenh1$genph <- as.numeric(ukgenh1$genph)
ukgenh1$othph <- as.numeric(ukgenh1$othph)


# select 2002-2019 data
ukdb2 <- ukdb1 %>%
  slice(5:75)

ukgenh2 <- ukgenh1 %>%
  slice(5:75)

ukdb2$GENPH <- as.numeric(ukgenh2$genph)
ukdb2$OTHPH <- as.numeric(ukgenh2$othph)

###############################################################
# ARDL for Geneva decisions only
# convert variables to time series objects
sal <- as.ts(ukdb2$POLSALH)
arr <- as.ts(ukdb2$GENPH)
ocl <- as.ts(ukdb2$OCLY)


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
M33 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:2))


# rename regressors for better readability
names(M33$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M33)




###############################################################
# ARDL for other decisions
# convert variables to time series objects
sal <- as.ts(ukdb2$POLSALH)
arr <- as.ts(ukdb2$OTHPH)
ocl <- as.ts(ukdb2$OCLY)


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
M34 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:2))


# rename regressors for better readability
names(M34$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M34)



# output regression tables
stargazer(M33, M34,
          type="html",
          out="UK Gen v Nongen Decisions.docx",
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









