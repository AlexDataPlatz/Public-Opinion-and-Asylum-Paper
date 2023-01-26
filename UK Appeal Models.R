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
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(dLagM)
library(stargazer)
library(dataverse)

# Import database for UK asylum appeals and public opinion data
deukdb <- get_dataframe_by_name(
  filename = "DEUKDB_JPP.xlsx",
  dataset = "10.7910/DVN/FSRPMX", 
  server = "dataverse.harvard.edu")

# filter data to select required variables
db <- ukapp %>%
  select("TIMM2", "COUNTRY2", "OCLY",
         "POLSALH", "ASAPPLPH") %>%
  filter(COUNTRY2 == "UK")


# convert variables to numeric
db$POLSALH <- as.numeric(db$POLSALH)
db$OCLY <- as.numeric(db$OCLY)
db$ASAPPLPH <- as.numeric(db$ASAPPLPH)

db <- na.omit(db)

# select years 2010-2019
db1 <- db %>%
  slice(2:41)


# convert into time series variables
sal <- as.ts(db1$POLSALH)
apl <- as.ts(db1$ASAPPLPH)
ocl <- as.ts(db1$OCLY)


# Tests
# test for stationarity
adf.test(apl)
adf.test(sal)


# test for cointegration
coint.test(apl, sal, d = 0, nlag = NULL, output = TRUE)

# variables are not stationary, not cointegrated

# create first diffs
dsal <- diff(sal)
dapl <- diff(apl)

# use AIC to select appropriate number of lags for dapl variable
AR1_dapl = arima(dapl, order = c(1, 1, 0))
AR2_dapl = arima(dapl, order = c(2, 1, 0))
AR3_dapl = arima(dapl, order = c(3, 1, 0))
AR4_dapl = arima(dapl, order = c(4, 1, 0))
AR5_dapl = arima(dapl, order = c(5, 1, 0))
AR6_dapl = arima(dapl, order = c(6, 1, 0))
AR7_dapl = arima(dapl, order = c(7, 1, 0))
AR8_dapl = arima(dapl, order = c(8, 1, 0))
AR9_dapl = arima(dapl, order = c(9, 1, 0))

AIC(AR1_dapl)
AIC(AR2_dapl)
AIC(AR3_dapl)
AIC(AR4_dapl)
AIC(AR5_dapl)
AIC(AR6_dapl)
AIC(AR7_dapl)
AIC(AR8_dapl)
AIC(AR9_dapl)

# create "restrictive" preference variable (nocl)
# ie % of respondents WITHOUT "open" preferences
nocl <- 100 - ocl

# create interactive salience * preference variable
ninteract <- sal*nocl
dninteract <- diff(ninteract)


# Estimate model with t-2 variables
M7 <- dynlm(dapl ~ L(dsal, 2) +
              L(nocl, 2) +
              L(dninteract, 2) +
              L(dapl, 1:3))

# rename regressors for better readability
names(M7$coefficients) <- c("Intercept", "Salience (t-x)", 
                            "Restrictive Preference Class (t-x)",
                            "Salience*Restrictive Preference Class (t-x)",
                            "Appeal Success Rate (t-1)", 
                            "Appeal Success Rate (t-2)", 
                            "Appeal Success Rate (t-3)")
summary(M7)


# Estimate model with t-3 variables
M8 <- dynlm(dapl ~ L(dsal, 3) +
              L(nocl, 3) +
              L(dninteract, 3) +
              L(dapl, 1:3))

# rename regressors for better readability
names(M8$coefficients) <- c("Intercept", "Salience (t-x)", 
                            "Restrictive Preference Class (t-x)",
                            "Salience*Restrictive Preference Class (t-x)",
                            "Appeal Success Rate (t-1)", 
                            "Appeal Success Rate (t-2)", 
                            "Appeal Success Rate (t-3)")
summary(M8)

# Estimate model with t-4 variables
M9 <- dynlm(dapl ~ L(dsal, 4) +
              L(nocl, 4) +
              L(dninteract, 4) +
              L(dapl, 1:3))

# rename regressors for better readability
names(M9$coefficients) <- c("Intercept", "Salience (t-x)", 
                            "Restrictive Preference Class (t-x)",
                            "Salience*Restrictive Preference Class (t-x)",
                            "Appeal Success Rate (t-1)", 
                            "Appeal Success Rate (t-2)", 
                            "Appeal Success Rate (t-3)")
summary(M9)

# Estimate model with t-5 variables
M10 <- dynlm(dapl ~ L(dsal, 5) +
               L(nocl, 5) +
               L(dninteract, 5) +
               L(dapl, 1:3))

# rename regressors for better readability
names(M10$coefficients) <- c("Intercept", "Salience (t-x)", 
                             "Restrictive Preference Class (t-x)",
                             "Salience*Restrictive Preference Class (t-x)",
                             "Appeal Success Rate (t-1)", 
                             "Appeal Success Rate (t-2)", 
                             "Appeal Success Rate (t-3)")

summary(M10)





# Estimate model without Interaction Variables
M11 <- dynlm(dapl ~ L(dsal, 2) +
              L(nocl, 2) +
              L(dapl, 1:3))

# rename regressors for better readability
names(M11$coefficients) <- c("Intercept", "Salience (t-x)", 
                            "Restrictive Preference Class (t-x)",
                            "Appeal Success Rate (t-1)", 
                            "Appeal Success Rate (t-2)", 
                            "Appeal Success Rate (t-3)")
summary(M11)


# Estimate model without Interaction Variables
M12 <- dynlm(dapl ~ L(dsal, 3) +
              L(nocl, 3) +
              L(dapl, 1:3))

# rename regressors for better readability
names(M12$coefficients) <- c("Intercept", "Salience (t-x)", 
                            "Restrictive Preference Class (t-x)",
                            "Appeal Success Rate (t-1)", 
                            "Appeal Success Rate (t-2)", 
                            "Appeal Success Rate (t-3)")
summary(M12)

# Estimate model without Interaction Variables
M13 <- dynlm(dapl ~ L(dsal, 4) +
              L(nocl, 4) +
              L(dapl, 1:3))

# rename regressors for better readability
names(M13$coefficients) <- c("Intercept", "Salience (t-x)", 
                            "Restrictive Preference Class (t-x)",
                            "Appeal Success Rate (t-1)", 
                            "Appeal Success Rate (t-2)", 
                            "Appeal Success Rate (t-3)")
summary(M13)

# Estimate model without Interaction Variables
M14 <- dynlm(dapl ~ L(dsal, 5) +
               L(nocl, 5) +
               L(dapl, 1:3))

# rename regressors for better readability
names(M14$coefficients) <- c("Intercept", "Salience (t-x)", 
                             "Restrictive Preference Class (t-x)",
                             "Appeal Success Rate (t-1)", 
                             "Appeal Success Rate (t-2)", 
                             "Appeal Success Rate (t-3)")

summary(M14)






stargazer(M11, M12, M13, M14,
          type="html",
          out="UK Appeal Models (without Interactions).docx",
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





