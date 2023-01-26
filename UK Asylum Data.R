# access required packages
library(tidyverse)
library(rio)
library(janitor)
library(zoo)

# Calculate UK Asylum Recognition Rates from Home Office data
# Import dataset from Home Office website
url <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1011720/asylum-applications-datasets-jun-2021.xlsx'
ukall <- rio::import(file = url,which = 9)

# Shift row 1 up to create correct variable names
ukall2 <- ukall %>%
  row_to_names(row_number = 1)

# filter db to remove resettlements and withdrawn cases
gf <- c("Grant of Other Leave", "Grant of Protection",  "Refused")

ukall3 <- ukall2 %>%
  filter(`Case type` == "Asylum Case") %>%
  filter(`Case outcome group` %in% gf)

# convert number of decisions to number format
ukall3$Decisions <- as.numeric(ukall3$Decisions)

# calculate number of decisions per quarter 
ukdec <- ukall3 %>% 
  group_by(Quarter) %>% 
  summarise(decq = sum(Decisions))

# calculate positive decisions per quarter
# first filter case outcomes to include only positive decisions
g <- c("Grant of Other Leave", "Grant of Protection")

ukpos <- ukall3 %>%
  filter(`Case outcome group` %in% g)

# then calculate quarterly positive decisions
ukpos2 <- ukpos %>% 
  group_by(Quarter) %>% 
  summarise(posq = sum(Decisions))

# bind the positive decisions to the total decisions database
ukdec$posq <- ukpos2$posq

# create date variable in standard 2022-01-01 format
ukdec$date <- as.Date(as.yearqtr(ukdec$Quarter, format = "%Y Q%q"),
                         frac = 1)


# calculate 6 month rolling averages for decisions,
# positive decisions, and asylum recognition rates (posph)
# first get sums for UK
ukdech <- ukdec %>%
  dplyr::mutate(dech = zoo::rollsum(decq, k = 2, 
                                    fill = NA, align = "right"),
                posh = zoo::rollsum(posq, k = 2, 
                                    fill = NA, align = "right"))

ukdech$dech <- ukdech$dech / 2
ukdech$posh <- ukdech$posh / 2


# calculate asylum recongition rate rolling average
ukdech$posph <- ukdech$posh / ukdech$dech * 100

# plot posph for UK
ggplot(ukdech) +
  geom_line(aes(date, posph))
