# load required packages
library(tidyverse)
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
library(dataverse)

# Import database
# Import database for German and UK public opinion data
deukdb <- get_dataframe_by_name(
  filename = "DEUKDB_JPP.xlsx",
  dataset = "10.7910/DVN/FSRPMX", 
  server = "dataverse.harvard.edu")

# select data for Germany (DE)
dedb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2", "POSPH",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "DE")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
dedb$POLSALH <- as.numeric(dedb$POLSALH)
dedb$OCLY <- as.numeric(dedb$OCLY)
dedb$POSPH <- as.numeric(dedb$POSPH)

# omit missing values
dedb1 <- na.omit(dedb)

# select 2002-2019 data
dedb2 <- dedb1 %>%
  slice(1:71)

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
              L(darr, 1:3))


# rename regressors for better readability
names(M1$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)")

summary(M1)


# Estimate model with 'open' immigration preference variable
M2 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(darr, 1:3))

# rename regressors for better readability
names(M2$coefficients) <- c("Intercept", "Salience (t-1)", 
                            "Open Preference Class (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)")

summary(M2)

# Estimate model with Interaction Variable
M3 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(dpinteract, 1) +
              L(darr, 1:3))

# rename regressors for better readability
names(M3$coefficients) <- c("Intercept", "Salience (t-1)",
                            "Open Preference Class (t-1)",
                            "Salience*Preference Interaction (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)")

summary(M3)

# run robustness checks for fully specified model
durbinWatsonTest(M3)
Box.test(resid(M3), lag = 3, type = "Ljung-Box")
jarque.bera.test(M3$residuals)
skewness(M3$residuals) 
kurtosis(M3$residuals)
lm.LMtests(M3, listw = col.listw,
           test=c("LMerr"))
acf(resid(M3))

##########################################################################

# select data for UK
ukdb <- deukdb %>%
  dplyr::select("TIMM2", "COUNTRY2", "POSPH",
                "POLSALH", "OCLY") %>%
  filter(COUNTRY2 == "UK")


# covert salience (polsalh), preference (ocly), and 
# asylum recognition rate (posph) to numeric values
ukdb$POLSALH <- as.numeric(ukdb$POLSALH)
ukdb$OCLY <- as.numeric(ukdb$OCLY)
ukdb$POSPH <- as.numeric(ukdb$POSPH)

# omit missing values
ukdb2 <- na.omit(ukdb)

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
M4 <- dynlm(darr ~ L(dsal, 1) +
              L(darr, 1:4))


# rename regressors for better readability
names(M4$coefficients) <- c("Intercept", "Salience (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)", "ARR (t-4)")

summary(M4)


# Estimate model with Prefernce Dummy
M5 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(darr, 1:4))

# rename regressors for better readability
names(M5$coefficients) <- c("Intercept", "Salience (t-1)", 
                            "Open Preference Class (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)", "ARR (t-4)")

summary(M5)

# Estimate model with Interaction Variables
M6 <- dynlm(darr ~ L(dsal, 1) +
              L(ocl, 1) +
              L(dpinteract, 1) +
              L(darr, 1:4))

# rename regressors for better readability
names(M6$coefficients) <- c("Intercept", "Salience (t-1)",
                            "Open Preference Class (t-1)",
                            "Salience*Preference Interaction (t-1)",
                            "ARR (t-1)", "ARR (t-2)", 
                            "ARR (t-3)", "ARR (t-4)")

summary(M6)


# run robustness checks for fully specified model
durbinWatsonTest(M6)
Box.test(resid(M6), lag = 3, type = "Ljung-Box")
jarque.bera.test(M6$residuals)
skewness(M6$residuals) 
kurtosis(M6$residuals)
lm.LMtests(M6, listw = col.listw,
           test=c("LMerr"))
acf(resid(M6))




# Export German (M1-M3) and UK (M4-M6) models as regression table
stargazer(M1, M2, M3, M4, M5, M6,
          type="html",
          out="DE and UK ARDL ARR Final.docx",
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
