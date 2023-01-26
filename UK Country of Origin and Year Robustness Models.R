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

# Import database (available from Dataverse)
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

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

# calculate decisions per quarter by nationality
ukdec <- ukall3 %>% 
  group_by(Quarter, Nationality) %>% 
  summarise(decq = sum(Decisions))

# calculate positive decisions per quarter
g <- c("Grant of Other Leave", "Grant of Protection")

ukpos <- ukall3 %>%
  filter(`Case outcome group` %in% g)

ukpos2 <- ukpos %>% 
  group_by(Quarter, Nationality) %>% 
  summarise(posq = sum(Decisions))

# merge decq and posq into new db
ukpd <- merge(ukdec, ukpos2, all = TRUE)
ukpd[is.na(ukpd)] <- 0


# create standard date variable
ukpd$date <- as.Date(as.yearqtr(ukpd$Quarter, format = "%Y Q%q"),
                      frac = 1)




####################################################################
# filter out specific nationalities for ARDL models robustness tests

ukoir <- ukpd %>%
  filter(!Nationality %in% "Iran")

# calculate decisions per quarter w/o Iran
ukoird <- ukoir %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukoirp <- ukoir %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukoird$posq <- ukoirp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukoirh <- ukoird %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukoirh$dech <- ukoirh$dech / 2
ukoirh$posh <- ukoirh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukoirh$posph <- ukoirh$posh / ukoirh$dech * 100


write_xlsx(ukoirh, "ukexsy.xlsx")

# select values to match dates with public opinion variables
ukoirsub <- ukoirh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukoirsub$posph)


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
M18 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:3))


# rename regressors for better readability
names(M18$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)")

summary(M18)



#############################################################################
# filter out specific nationalities for test ARDL models

ukopk <- ukpd %>%
  filter(!Nationality %in% "Pakistan")

# calculate decisions per quarter w/o Pakistan
ukopkd <- ukopk %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukopkp <- ukopkh %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukopkd$posq <- ukopkp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukopkh <- ukopkd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukopkh$dech <- ukopkh$dech / 2
ukopkh$posh <- ukopkh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukopkh$posph <- ukopkh$posh / ukopkh$dech * 100


# select values to match dates with public opinion variables
ukopksub <- ukopkh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukopksub$posph)


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
M19 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:3))


# rename regressors for better readability
names(M19$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)")

summary(M19)



#############################################################################
# filter out specific nationalities for test ARDL models

ukoiq <- ukpd %>%
  filter(!Nationality %in% "Iraq")

# calculate decisions per quarter w/o Iraq
ukoiqd <- ukoiq %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukoiqp <- ukoiq %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukoiqd$posq <- ukoiqp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukoiqh <- ukoiqd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukoiqh$dech <- ukoiqh$dech / 2
ukoiqh$posh <- ukoiqh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukoiqh$posph <- ukoiqh$posh / ukoiqh$dech * 100

write_xlsx(ukoiqh, "ukiq.xlsx")

# select values to match dates with public opinion variables
ukoiqsub <- ukoiqh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukoiqsub$posph)


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
M21 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:3))


# rename regressors for better readability
names(M21$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)")

summary(M21)


#############################################################################
# filter out specific nationalities for test ARDL models

ukoaf <- ukpdh %>%
  filter(!Nationality %in% "Afghanistan")


# calculate decisions per quarter w/o Afghanistan
ukoafd <- ukoaf %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukoafp <- ukoaf %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukoafd$posq <- ukoafp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukoafh <- ukoafd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukoafh$dech <- ukoafh$dech / 2
ukoafh$posh <- ukoafh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukoafh$posph <- ukoafh$posh / ukoafh$dech * 100


# select values to match dates with public opinion variables
ukoafsub <- ukoafh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukoafsub$posph)


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
M22 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:3))


# rename regressors for better readability
names(M22$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)", 
                             "ARR (t-3)")

summary(M22)




#############################################################################
# filter out specific nationalities for test ARDL models

ukozw <- ukpd %>%
  filter(!Nationality %in% "Zimbabwe")

# calculate decisions per quarter w/o Zimbabwe
ukozwd <- ukozw %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukozwp <- ukozw %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukozwd$posq <- ukozwp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukozwh <- ukozwd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukozwh$dech <- ukozwh$dech / 2
ukozwh$posh <- ukozwh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukozwh$posph <- ukozwh$posh / ukozwh$dech * 100


# select values to match dates with public opinion variables
ukozwsub <- ukozwh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukozwsub$posph)


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
M23 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:3))


# rename regressors for better readability
names(M23$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)", 
                             "ARR (t-3)")

summary(M23)





#############################################################################
# filter out specific nationalities for test ARDL models

ukoso <- ukpdh %>%
  filter(!Nationality %in% "Somalia")

# calculate decisions per quarter w/o Somalia
ukosod <- ukoso %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukosop <- ukoso %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukosod$posq <- ukosop$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukosoh <- ukosod %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukosoh$dech <- ukosoh$dech / 2
ukosoh$posh <- ukosoh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukosoh$posph <- ukosoh$posh / ukosoh$dech * 100


# select values to match dates with public opinion variables
ukososub <- ukosoh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukososub$posph)


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
M24 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:3))


# rename regressors for better readability
names(M24$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)", 
                             "ARR (t-3)")

summary(M24)




#############################################################################
# filter out specific nationalities for test ARDL models

ukoer <- ukpdh %>%
  filter(!Nationality %in% "Eritrea")



# calculate decisions per quarter w/o Eritrea
ukoerd <- ukoer %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukoerp <- ukoer %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukoerd$posq <- ukoerp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukoerh <- ukoerd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukoerh$dech <- ukoerh$dech / 2
ukoerh$posh <- ukoerh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukoerh$posph <- ukoerh$posh / ukoerh$dech * 100


# select values to match dates with public opinion variables
ukoersub <- ukoerh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukoersub$posph)


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
M25 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:3))


# rename regressors for better readability
names(M25$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)", 
                             "ARR (t-3)")

summary(M25)



############################################################################
# filter out specific nationalities for test ARDL models

ukocn <- ukpdh %>%
  filter(!Nationality %in% "China")

# calculate decisions per quarter w/o China
ukocnd <- ukocn %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukocnp <- ukocn %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukocnd$posq <- ukocnp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukocnh <- ukocnd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukocnh$dech <- ukocnh$dech / 2
ukocnh$posh <- ukocnh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukocnh$posph <- ukocnh$posh / ukocnh$dech * 100


# select values to match dates with public opinion variables
ukocnsub <- ukocnh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukocnsub$posph)


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
M26 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:3))


# rename regressors for better readability
names(M26$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)", 
                             "ARR (t-3)")

summary(M26)



############################################################################
# filter out specific nationalities for test ARDL models

ukolk <- ukpdh %>%
  filter(!Nationality %in% "Sri Lanka")


# calculate decisions per quarter w/o Sri Lanka
ukolkd <- ukolk %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukolkp <- ukolk %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukolkd$posq <- ukolkp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukolkh <- ukolkd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukolkh$dech <- ukolkh$dech / 2
ukolkh$posh <- ukolkh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukolkh$posph <- ukolkh$posh / ukolkh$dech * 100


# select values to match dates with public opinion variables
ukolksub <- ukolkh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukolksub$posph)


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
M27 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:3))


# rename regressors for better readability
names(M27$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)", 
                             "ARR (t-3)")

summary(M27)



############################################################################
# filter out specific nationalities for test ARDL models

ukong <- ukpdh %>%
  filter(!Nationality %in% "Nigeria")


# calculate decisions per quarter w/o Nigeria
ukongd <- ukong %>% 
  group_by(date) %>% 
  summarise(decq = sum(decq))

# calculate positive decisions per quarter
ukongp <- ukong %>%
  group_by(date) %>% 
  summarise(posq = sum(posq))

# add the posph value to the db
ukongd$posq <- ukongp$posq


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
ukongh <- ukongd %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


ukongh$dech <- ukongh$dech / 2
ukongh$posh <- ukongh$posh / 2


# calculate asylum recognition rate rolling average w/o country
ukongh$posph <- ukongh$posh / ukongh$dech * 100


# select values to match dates with public opinion variables
ukongsub <- ukongh %>%
  slice(6:76)

# run ARDL models for UK
# select public opinion data for UK
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")
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

ukdb2 <- ukdb1 %>%
  slice(5:75)

ukdb2$POSPH <- as.numeric(ukongsub$posph)


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
M28 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:3))


# rename regressors for better readability
names(M28$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)", 
                             "ARR (t-3)")

summary(M28)





####################################################

# Export table for UK Country Robustness Models

stargazer(M18, M19, M21, M22, M23, M24, M25, M26, M27, M28,
         type="html",
         out="UK Citizenship Robustness Models.docx",
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

top <- c("Iran", "Pakistan", "Iraq", "Afghanistan", "Zimbabwe",
         "Somalia", "Eritrea", "China", "Sri Lanka", "Nigeria")

uktop1 <- ukpdh %>%
  filter(Nationality %in% top)

# calculate quarterly decisions and positive decisions
uktop <- uktop1 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for uk
uktoph <- uktop %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
uktoph$posph <- uktoph$posh / uktoph$dech * 100

write_xlsx(uktoph, "uktoph.xlsx")

# plot posph for uk
ggplot(uktoph) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR of top 10 only
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

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
uktoph1 <- na.omit(uktoph)

# select 2002-2019 data
ukdb2 <- ukdb1 %>%
  slice(5:75)

uktoph2 <- uktoph1 %>%
  slice(5:75)

uktoph1$posph <- as.numeric(uktoph1$posph)
ukdb2$POSPH <- as.numeric(uktoph2$posph)


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
M29 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:2))


# rename regressors for better readability
names(M29$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M29)



########################################################################
# next check top 10 countries vs rest of data
# results excluding top 10 sending countries

top <- c("Iran", "Pakistan", "Iraq", "Afghanistan", "Zimbabwe",
         "Somalia", "Eritrea", "China", "Sri Lanka", "Nigeria")

ukrest1 <- ukpdh %>%
  filter(!Nationality %in% top)

# calculate quarterly decisions and positive decisions
ukrest <- ukrest1 %>%
  group_by(date) %>% 
  summarise(decq = sum(decq),
            posq = sum(posq))

# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for uk
ukresth <- ukrest %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))


# calculate asylum recongition rate rolling average
ukresth$posph <- ukresth$posh / ukresth$dech * 100

write_xlsx(ukresth, "ukresth.xlsx")

# plot posph for uk
ggplot(ukresth) +
  geom_line(aes(date, posph))

#####################################################################
# run ARDL models for ARR excluding top 10
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

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
ukresth1 <- na.omit(ukresth)

# select 2002-2019 data
ukdb2 <- ukdb1 %>%
  slice(5:75)

ukresth2 <- ukresth1 %>%
  slice(5:75)

ukresth1$posph <- as.numeric(ukresth1$posph)
ukdb2$POSPH <- as.numeric(ukresth2$posph)


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
M30 <- dynlm(darr ~ L(dsal, 1) +
               L(darr, 1:2))


# rename regressors for better readability
names(M30$coefficients) <- c("Intercept", "Salience (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M30)











# output regression tables
stargazer(M29, M30,
          type="html",
          out="UK Top 10 CoO vs Rest.docx",
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



#####################################################################
# run ARDL models for ARR for 2015 onwards
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for UK
ukdb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY", "POSPH") %>%
  filter(COUNTRY2 == "UK")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
ukdb$POLSALH <- as.numeric(ukdb$POLSALH)
ukdb$OCLY <- as.numeric(ukdb$OCLY)
ukdb$POSPH <- as.numeric(ukdb$POSPH)


# omit missing values
ukdb1 <- na.omit(ukdb)

# select 2015-2019 data
ukdb2 <- ukdb1 %>%
  slice(44:71)



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
M35 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:2))


# rename regressors for better readability
names(M35$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M35)


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
M3 <- dynlm(darr ~ L(dsal, 1) +
               L(ocl, 1) +
               L(dpinteract, 1) +
               L(darr, 1:2))

# rename regressors for better readability
names(M3$coefficients) <- c("Intercept", "Salience (t-1)",
                             "Open Preference Class (t-1)",
                             "Salience*Preference Interaction (t-1)",
                             "ARR (t-1)", "ARR (t-2)")

summary(M3)




#############################################################
# select pre 2015 data
# run ARDL models for ARR for 2015 onwards
# Import database
deukdb <- read_excel("/~/DEUKDB_JPP.xlsx")

# select data for UK
ukdb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2",
                "POLSALH", "OCLY", "POSPH") %>%
  filter(COUNTRY2 == "UK")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
ukdb$POLSALH <- as.numeric(ukdb$POLSALH)
ukdb$OCLY <- as.numeric(ukdb$OCLY)
ukdb$POSPH <- as.numeric(ukdb$POSPH)


# omit missing values
ukdb1 <- na.omit(ukdb)

# select 2002-2014 data
ukdb2 <- ukdb1 %>%
  slice(1:43)



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
M36 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:2))


# rename regressors for better readability
names(M36$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)")

summary(M36)


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
          out="UK Pre v Post 2013 Robustness Checks.docx",
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







