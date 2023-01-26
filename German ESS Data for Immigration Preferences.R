# install necessary packages
library(tidyverse)
library(essurvey)
library(tidyr)
library(survey)
library(tidyselect)
library(knitr)
library(questionr)                      
library(qpcR)  
library(gdata)            
library(data.table)
library(xlsx)
library(readxl)

# set email address (same one as used for ESS account log in)
set_email("alexander.hartland@postgrad.manchester.ac.uk")


# import data from each round
rawdata_rnd1 <- import_rounds(c(1))
rawdata_rnd2 <- import_rounds(c(2))
rawdata_rnd3 <- import_rounds(c(3))
rawdata_rnd4 <- import_rounds(c(4))
rawdata_rnd5 <- import_rounds(c(5))
rawdata_rnd6 <- import_rounds(c(6))
rawdata_rnd7 <- import_rounds(c(7))
rawdata_rnd8 <- import_rounds(c(8))
rawdata_rnd9 <- import_rounds(c(9))


which(colnames(rawdata_rnd9)=="imwbcnt" )
which(colnames(rawdata_rnd7)=="region" )
which(colnames(decore7)=="gvrfgap" )

# select admin variables, weights, core politics variables for each rnd
# need to wrangle rnd 1-4 to create combined region variable
# need to rename variables to match for each year
data1 <- rawdata_rnd1 %>%
  select("name", "essround", "idno", "cntry",
         "pweight", "pspwght", "inwmm", "inwyr",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt", "gvrfgap",
         vars_select(names(rawdata_rnd1),
                     starts_with('region')))

data1 <- unite(data1, reg, c(16:37))
data1$reg <- gsub("_","", as.character(data1$reg))
data1$reg <- gsub("NA","", as.character(data1$reg))
data1$region <- paste(data1$cntry, data1$reg, sep = "")
data1 <- subset(data1, select = -c(reg))
data1 <- data1 %>% rename(inwmme = inwmm, 
                          inwyye = inwyr)

data2 <- rawdata_rnd2 %>%
  select("name", "essround", "idno", "cntry",
         "pweight", "pspwght", "inwmm", "inwyr",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt",
         vars_select(names(rawdata_rnd2),
                     starts_with('region')))

data2 <- unite(data2, reg, c(15:38))
data2$reg <- gsub("_","", as.character(data2$reg))
data2$reg <- gsub("NA","", as.character(data2$reg))
data2$region <- paste(data2$cntry, data2$reg, sep = "")
data2 <- subset(data2, select = -c(reg))
data2 <- data2 %>% rename(inwmme = inwmm, 
                          inwyye = inwyr)


data3 <- rawdata_rnd3 %>%
  select("name", "essround", "idno", "cntry",
         "pweight", "pspwght", "inwmme", "inwyye",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt",
         vars_select(names(rawdata_rnd3),
                     starts_with('region')))

data3 <- unite(data3, reg, c(15:35))
data3$reg <- gsub("_","", as.character(data3$reg))
data3$reg <- gsub("NA","", as.character(data3$reg))
data3$region <- paste(data3$cntry, data3$reg, sep = "")
data3 <- subset(data3, select = -c(reg))


data4 <- rawdata_rnd4 %>%
  select("name", "essround", "idno", "cntry",
         "pweight", "pspwght", "inwmme", "inwyye",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt",
         vars_select(names(rawdata_rnd4),
                     starts_with('region')))

data4 <- unite(data4, reg, c(15:37))
data4reg <- gsub("_","", as.character(data4$reg))
data4$reg <- gsub("NA","", as.character(data4$reg))
data4$region <- paste(data4$cntry, data4$reg, sep = "")
data4 <- subset(data4, select = -c(reg))

data5 <- rawdata_rnd5 %>%
  select("name", "essround", "idno", "cntry", "region",
         "pweight", "pspwght", "inwmme", "inwyye",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt")

data6 <- rawdata_rnd6 %>%
  select("name", "essround", "idno", "cntry", "region",
         "pweight", "pspwght", "inwmme", "inwyye",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt")

data7 <- rawdata_rnd7 %>%
  select("name", "essround", "idno", "cntry", "region",
         "pweight", "pspwght", "inwmme", "inwyye",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt", "gvrfgap")


select <- dplyr::select
data8 <- rawdata_rnd8 %>%
  select("name", "essround", "idno", "cntry", "region",
         "pweight", "pspwght", "inwmme", "inwyye",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt")

# recode factor variables to numeric
data8$imwbcnt <- recode(data8$imwbcnt, "Worse place to live" = 0L, "Better place to live" = 10L,
                        "1" = 1L, "2" = 2L, "3" = 3L,
                        "4" = 4L, "5" = 5L, "6" = 6L, 
                        "7" = 7L, "8" = 8L, "9" = 9L)

data8$imueclt <- recode(data8$imueclt, 
                        "Cultural life undermined" = 0L, 
                        "Cultural life enriched" = 10L,
                        "1" = 1L, "2" = 2L, "3" = 3L,
                        "4" = 4L, "5" = 5L, "6" = 6L, 
                        "7" = 7L, "8" = 8L, "9" = 9L)

data8$imbgeco <- recode(data8$imbgeco, 
                        "Bad for the economy" = 0L, 
                        "Good for the economy" = 10L,
                        "1" = 1L, "2" = 2L, "3" = 3L,
                        "4" = 4L, "5" = 5L, "6" = 6L, 
                        "7" = 7L, "8" = 8L, "9" = 9L)

data8$imsmetn <- as.numeric(data8$imsmetn)
data8$imdfetn <- as.numeric(data8$imdfetn)
data8$impcntr <- as.numeric(data8$impcntr)
data8$imbgeco <- as.numeric(data8$imbgeco)
data8$imueclt <- as.numeric(data8$imueclt)
data8$imwbcnt <- as.numeric(data8$imwbcnt)



data9 <- rawdata_rnd9 %>%
  select("name", "essround", "idno", "cntry", "region",
         "pweight", "pspwght", "inwmme", "inwyye",
         "imsmetn", "imdfetn", "impcntr",
         "imbgeco", "imueclt", "imwbcnt")

# recode factor variables to numeric
data9$imwbcnt <- recode(data9$imwbcnt, "Worse place to live" = 0L, 
                        "Better place to live" = 10L,
                        "1" = 1L, "2" = 2L, "3" = 3L,
                        "4" = 4L, "5" = 5L, "6" = 6L, 
                        "7" = 7L, "8" = 8L, "9" = 9L)

data9$imueclt <- recode(data9$imueclt, 
                        "Cultural life undermined" = 0L, 
                        "Cultural life enriched" = 10L,
                        "1" = 1L, "2" = 2L, "3" = 3L,
                        "4" = 4L, "5" = 5L, "6" = 6L, 
                        "7" = 7L, "8" = 8L, "9" = 9L)

data9$imbgeco <- recode(data9$imbgeco, 
                        "Bad for the economy" = 0L, 
                        "Good for the economy" = 10L,
                        "1" = 1L, "2" = 2L, "3" = 3L,
                        "4" = 4L, "5" = 5L, "6" = 6L, 
                        "7" = 7L, "8" = 8L, "9" = 9L)

data9$imsmetn <- as.numeric(data9$imsmetn)
data9$imdfetn <- as.numeric(data9$imdfetn)
data9$impcntr <- as.numeric(data9$impcntr)
data9$imbgeco <- as.numeric(data9$imbgeco)
data9$imueclt <- as.numeric(data9$imueclt)
data9$imwbcnt <- as.numeric(data9$imwbcnt)

head(data9)

# stack each round into one db
data <- bind_rows(data1, data2, data3, data4, data5,
                  data6, data7, data8, data9)

# recode refusal/ don't know as NA
data <- recode_missings(data)

# create anweight variable (post strat weight x population weight)
data$anweight <- data$pspwght*data$pweight*10e3

data$imsmetn <- as.numeric(data$imsmetn)
data$imdfetn <- as.numeric(data$imdfetn)
data$impcntr <- as.numeric(data$impcntr)
data$imbgeco <- as.numeric(data$imbgeco)
data$imueclt <- as.numeric(data$imueclt)
data$imwbcnt <- as.numeric(data$imwbcnt)
data$gvrfgap <- as.numeric(data$gvrfgap)

# Select countries with data for all rounds
decorenas <- data %>%
  filter(cntry == "DE")

# Remove Month and Year variables (too many NAs)
decorenas <- decorenas[c(1:6, 9:17)]

# Remove NAs from round 2:6, 8:9
decorenoref <- decorenas %>%
  filter(decorenas$essround == "2" | decorenas$essround == "3" |
           decorenas$essround == "4" | decorenas$essround == "5" |
           decorenas$essround == "6" | decorenas$essround == "8" |
           decorenas$essround == "9")
decorenoref <- decorenoref[c(1:12, 14, 15)]
decorenoref <- na.omit(decorenoref)

# Remove NAs from round 1 and 7
decoreref <- decorenas %>%
  filter(decorenas$essround == "1" | decorenas$essround == "7")

decoreref <- na.omit(decoreref)

# Bind all rounds together
decore <- bind_rows(decorenoref, decoreref)

# recode same/ diff race q, creates 16 possible combinations
derace <- decore
derace$imsmetn10 <- derace$imsmetn*10
derace$smdf <- derace$imsmetn10 + derace$imdfetn

# form recoded values into 4 groups
# 1= open, 2=same race pref, 3=diff race pref, 4= restrictive
derace$class <- dplyr::recode(derace$smdf, 
                       "11" = 1L,
                       "12" = 1L,
                       "21" = 1L,
                       "22" = 1L,
                       "13" = 2L,
                       "14" = 2L,
                       "23" = 2L,
                       "24" = 2L,
                       "31" = 3L,
                       "32" = 3L,
                       "41" = 3L,
                       "42" = 3L,
                       "33" = 4L,
                       "34" = 4L,
                       "43" = 4L,
                       "44" = 4L)


# recode econ/ cult variables, create 4 groups
deculec <- decore

deculec$cul <- deculec$imueclt
deculec$cul[deculec$imueclt < 5] <- 1
deculec$cul[deculec$cul > 4] <- 2

deculec$econ <- deculec$imbgeco
deculec$econ[deculec$imbgeco > 4] <- 20
deculec$econ[deculec$econ < 5] <- 10

deculec$culec <- deculec$econ + deculec$cul

# group 1= open, 2= econ pref, 3= culture pref, 4=restrict
deculec$class <- dplyr::recode(deculec$culec, 
                        "11" = 4L,
                        "12" = 3L,
                        "21" = 2L,
                        "22" = 1L)

# Add new variables to decore db
decore$culec <- deculec$class
decore$race <- derace$class

# recode imwbcnt into binary pos or neg variable
decore$benefitpn <- decore$imwbcnt
decore$benefitpn[decore$imwbcnt < 5] <- 2
decore$benefitpn[decore$benefitpn > 4] <- 1

# Which variable best predicts refugee variable?
# in Round 1
decore1 <- decore %>%
  filter(essround == "1")

# calculate mean scores with weights for racial prefs
temp.table1 <- wtd.table(decore1$race, 
                         decore1$gvrfgap,
                         weights=decore1$pspwght, 
                         digits = 0)

temp.cprop.table1 <- cprop(temp.table1, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table1

pt1 <- as.data.frame.matrix(t(temp.cprop.table1))

pt1


# calculate mean scores with weights for culture v econ prefs
temp.table2 <- wtd.table(decore1$culec, 
                         decore1$gvrfgap,
                         weights=decore1$pspwght, 
                         digits = 0)

temp.cprop.table2 <- cprop(temp.table2, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table2

pt2 <- as.data.frame.matrix(t(temp.cprop.table2))

pt2


# calculate mean scores with weights for poor, non Eur prefs
temp.table3 <- wtd.table(decore1$impcntr,
                         decore1$gvrfgap, 
                         weights=decore1$pspwght, 
                         digits = 0)

temp.cprop.table3 <- cprop(temp.table3, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table3

pt3 <- as.data.frame.matrix(t(temp.cprop.table3))

pt3


# calculate mean scores with weights for general immig prefs
temp.table4 <- wtd.table(decore1$benefitpn,
                         decore1$gvrfgap, 
                         weights=decore1$pspwght, 
                         digits = 0)

temp.cprop.table4 <- cprop(temp.table4, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table4

pt4 <- as.data.frame.matrix(t(temp.cprop.table4))

pt4

head(decore1)

# calculate mean scores with weights for culture prefs
temp.table5 <- wtd.table(decore1$imueclt,
                         decore1$gvrfgap, 
                         weights=decore1$pspwght, 
                         digits = 0)

temp.cprop.table5 <- cprop(temp.table5, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table5

pt5 <- as.data.frame.matrix(t(temp.cprop.table5))

pt5


# Run same calculations for round 7
decore7 <- decore %>%
  filter(essround == "7")

# calculate mean scores with weights for racial prefs
temp.table1 <- wtd.table(decore7$race, 
                         decore7$gvrfgap,
                         weights=decore7$pspwght, 
                         digits = 0)

temp.cprop.table1 <- cprop(temp.table1, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table1

pt1 <- as.data.frame.matrix(t(temp.cprop.table1))

pt1


# calculate mean scores with weights for culture v econ prefs
temp.table2 <- wtd.table(decore7$culec,
                         decore7$gvrfgap,      
                         weights=decore7$pspwght, 
                         digits = 0)

temp.cprop.table2 <- cprop(temp.table2, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table2

pt2 <- as.data.frame.matrix(t(temp.cprop.table2))

pt2


# calculate mean scores with weights for poor, non Eur prefs
temp.table3 <- wtd.table(decore7$impcntr,
                         decore7$gvrfgap,
                         weights=decore7$pspwght, 
                         digits = 0)

temp.cprop.table3 <- cprop(temp.table3, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table3

pt3 <- as.data.frame.matrix(t(temp.cprop.table3))

pt3


# calculate mean scores with weights for general immig prefs
temp.table4 <- wtd.table(decore7$benefitpn,
                         decore7$gvrfgap,  
                         weights=decore7$pspwght, 
                         digits = 0)

temp.cprop.table4 <- cprop(temp.table4, digits=0, 
                           total=TRUE, n=TRUE, percent=TRUE)
temp.cprop.table4

pt4 <- as.data.frame.matrix(t(temp.cprop.table4))

pt4


# Calculate average "refugee" scores for each group
# calculate means for each q, each rnd
# round 1
rmeans1 <- decore1 %>%
  group_by(race) %>%
  summarise_at(c(15), mean, na.rm = TRUE)

cmeans1 <- decore1 %>%
  group_by(culec) %>%
  summarise_at(c(15), mean, na.rm = TRUE)

pmeans1 <- decore1 %>%
  group_by(impcntr) %>%
  summarise_at(c(15), mean, na.rm = TRUE)

bmeans1 <- decore1 %>%
  group_by(benefitpn) %>%
  summarise_at(c(15), mean, na.rm = TRUE)


# gvrfgap should be column 15 but somehow needs to be c14 here?
econmeans1 <- decore1 %>%
  group_by(imbgeco) %>%
  summarise_at(c(14), mean, na.rm = TRUE)

culmeans1 <- decore1 %>%
  group_by(imueclt) %>%
  summarise_at(c(14), mean, na.rm = TRUE)


# round 7
rmeans7 <- decore7 %>%
  group_by(race) %>%
  summarise_at(c(15), mean, na.rm = TRUE)

cmeans7 <- decore7 %>%
  group_by(culec) %>%
  summarise_at(c(15), mean, na.rm = TRUE)

pmeans7 <- decore7 %>%
  group_by(impcntr) %>%
  summarise_at(c(15), mean, na.rm = TRUE)

bmeans7 <- decore7 %>%
  group_by(benefitpn) %>%
  summarise_at(c(15), mean, na.rm = TRUE)

# gvrfgap should be column 15 but somehow needs to be c14 here?
econmeans7 <- decore7 %>%
  group_by(imbgeco) %>%
  summarise_at(c(14), mean, na.rm = TRUE)

culmeans7 <- decore7 %>%
  group_by(imueclt) %>%
  summarise_at(c(14), mean, na.rm = TRUE)

# plot means for core questions by round (nb unweighted)
long <- pivot_longer(means, cols=2:7, names_to = "variable", values_to = "mean")

head(decore)

# All data LCA
# run package for LCA
library(poLCA)

# make variables camparable and compatible with poLCA
deint <- decore[c(7:12, 16:18)]
deint[,10:17] <- decore[, c(1:6, 13, 14)]

deint$race <- dplyr::recode(deint$race, "1" = 33L, "2" = 23L,
                     "3" = 13L,  "4" = 3L)

deint$impcntr <- dplyr::recode(deint$impcntr,  "1" = 33L, "2" = 23L,
                        "3" = 13L,  "4" = 3L)

deint$culec <- dplyr::recode(deint$culec,  "1" = 33L, "2" = 23L,
                      "3" = 13L,  "4" = 3L)

deint$imwbcnt <- deint$imwbcnt+1
deint$imwbcnt <- deint$imwbcnt*3

target <- c("33", "23")

deopen <- deint %>%
  filter(race == "33" &
           impcntr %in% target &
           benefitpn == "1")

deopen$class <- 1


target <- c("23", "3")

declose <- deint %>%
  filter(race %in% target & 
           impcntr == "3" &
           benefitpn == "2")

declose$class <- 3

target <- c("13", "23")

desel <- deint %>%
  filter(race %in% target |
           impcntr == "13")


deopcl <- bind_rows(deopen, declose)

de3cl <- merge(deint, deopcl, all.x = TRUE)
d[is.na(d)] <- 0
de3cl$class[is.na(de3cl$class)] <- 2



# Calculate weighted size of groups for each round
gstemp <- wtd.table(de3cl$class,
                    de3cl$essround,
                    weights=de3cl$pspwght, 
                    digits = 0)

gscrop <- cprop(gstemp, digits=0, 
                total=FALSE, n=FALSE, percent=TRUE)

de2grp <- as.data.frame.matrix(t(gscrop))

de2grp




# Calculate weighted size of groups for each round
gstemp <- wtd.table(deopcl$class,
                    deopcl$essround,
                    weights=deopcl$pspwght, 
                    digits = 0)

gscrop <- cprop(gstemp, digits=0, 
                total=FALSE, n=FALSE, percent=TRUE)

de2grp <- as.data.frame.matrix(t(gscrop))

de2grp
