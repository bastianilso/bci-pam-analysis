library(gsheet)
library(tidyverse)
source("utils/loadrawdata.R")

options("digits.secs"=6)
# Load data from directories
D <- LoadFromDirectory("data", event="Game")
save(D, file = 'data_pam_raw.rda', compress=TRUE)

# Load data from Google Sheets
L <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1AQRWh7r0inqpacSbgQNvDncr4RxI77ag3BcuwwIYdBQ/edit#gid=237988817')

#############
# Format D
#############
D <- D %>% rename(Condition = i2, Participant = i1)

D <-D %>% mutate(Participant = as.numeric(Participant))

D <- D %>% group_by(Participant, Condition, InputWindowOrder) %>%
  summarise(fishFeedback = sum(FishEvent == "FishCaught")) %>%
  right_join(D)

#############
# Merge
#############
D <- D %>% left_join(L, by=c('Condition' = 'Condition', 'Participant' = 'Participant'))




#############
# Save to RDA
#############
save(D, file = 'data_pam.rda', compress=TRUE)
