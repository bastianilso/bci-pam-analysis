library(gsheet)
library(tidyverse)
source("utils/loadrawdata.R")

options("digits.secs"=6)

# Load data from directories
# For some reason we only have original data from Participant 1-10 in the folder.
# So load the data from the RDA file instead!
#D <- LoadFromDirectory("data", event="Game")
#save(D, file = 'data_pam_raw.rda', compress=TRUE)

load('data_pam_raw_old.rda')

#############
# Format D
#############
D <- D %>% rename(Condition = i2, Participant = i1)

D <-D %>% mutate(Participant = as.numeric(Participant))

D <- D %>% group_by(Participant, Condition, InputWindowOrder) %>%
  summarise(fishFeedback = sum(FishEvent == "FishCaught")) %>%
  right_join(D)

D = D %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  arrange(Timestamp) %>%
  mutate(time_delta = Timestamp - lag(Timestamp),
         time_delta = as.numeric(time_delta),
         time_delta = ifelse(is.na(time_delta), 0, time_delta))

# Filter out data happening before GameRunning event.
# Filter out extra "GameStopped" events.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, isGameOver, isGame) %>% view()
D = D %>% ungroup() %>% group_by(Participant, Condition) %>% arrange(Timestamp) %>%
  mutate(isGame = ifelse(Event == "GameRunning", 1, 0),
         isGame = cumsum(isGame),
         isGameOver = ifelse(Event == "GameStopped", 1,0),
         isGameOver = cumsum(isGameOver),
         isGameOver = ifelse(Event == "GameStopped", isGameOver-1,isGameOver)) %>%
  filter(isGame == 1, isGameOver < 1)

# Create InputWindowClosedFill - creates an identifier for resting periods.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, InputWindowClosedID, InputWindow, InputWindowClosedFill) %>% view()
D = D %>% mutate(InputWindowClosedID = NA,
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", 1, 0),
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", cumsum(InputWindowClosedID), InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "GameStopped", -1, InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "GameRunning", -1, InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Open", -1, InputWindowClosedID),
                 InputWindowClosedFill = ifelse(InputWindowClosedID == 0, NA, InputWindowClosedID)) %>%
  tidyr::fill(InputWindowClosedFill, .direction="down")

# Create InputWindowOrderFilled column - an identifier for open periods.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, InputWindowOrderWithDecision,InputWindowOrderFilled, InputWindow, InputWindowClosedFill) %>% view()
D = D %>% group_by(Participant, Condition) %>% 
  mutate(InputWindowOrder = ifelse(Event == "GameStopped", -1, InputWindowOrder),
         InputWindowOrder = ifelse(Event == "GameRunning", -1, InputWindowOrder),
         InputWindowOrderWithDecision = InputWindowOrder,
         InputWindowOrder = ifelse(InputWindow == "Closed", -1, InputWindowOrder),
         Period = NA,
         Period = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", "RestPeriod", Period),
         Period = ifelse(Event == "InputWindowChange" & InputWindowClosedID == max(InputWindowClosedID, na.rm=T), "PostGame", Period),
         Period = ifelse(Event == "InputWindowChange" & InputWindow == "Open", "OpenPeriod", Period),
         Period = ifelse(Event == "GameRunning", "PreGame", Period),
         InputWindowOrderFilled = InputWindowOrder) %>%
  tidyr::fill(InputWindowOrderFilled, .direction="down") %>%
  tidyr::fill(Period, .direction="down")

#############
# Load Likert Data
#############
# Load data from Google Sheets
L <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1AQRWh7r0inqpacSbgQNvDncr4RxI77ag3BcuwwIYdBQ/edit#gid=237988817')


#############
# Merge
#############
D <- D %>% left_join(L, by=c('Condition' = 'Condition', 'Participant' = 'Participant'))




#############
# Save to RDA
#############
save(D, file = 'data_pam.rda', compress=TRUE)
