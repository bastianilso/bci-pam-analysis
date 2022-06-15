library(plotly)
library(tidyverse)
library(lme4)
library(MuMIn)
options("digits.secs"=6)
options(max.print=1000)

source("utils/visutils.R")
source("utils/calcutils.R")

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_pam.rda')

#############
# Summaries
#############

# TODO: How many successful decisions lead to a fish getting caught?

# TODO: Try to plot with negative feedback rate. / non-negative feedback rate

St <- D %>% group_by(Participant, Condition) %>%
  summarise(rejInput = sum(TrialResult == "RejInput", na.rm=T),
            accInput = sum(TrialResult == "AccInput", na.rm=T),
            posTrial = sum(TrialResult == "OverrideInput" | TrialResult == "AccInput" | TrialResult == "AugSuccess" | TrialResult == "ExplicitSham" | TrialResult == "AssistSuccess", na.rm=T),
            assistInput = sum(TrialResult %in% c("AssistSuccess", "AugSuccess"), na.rm=T),
            explicitSham = sum(TrialResult %in% c("ExplicitSham", "OverrideInput"), na.rm=T),
            mitigateFail = sum(TrialResult %in% c("AssistFail","MitigateFail"), na.rm=T),
            totalTrials2 = sum(!is.na(TrialResult), na.rm=T),
            totalTrials = rejInput+accInput+assistInput+explicitSham+mitigateFail,
            #fishCaught = sum(FishEvent == "FishCaught", na.rm=T),
            #fishCaught2 = sum(Event == "GameDecision" & fishFeedback == 1, na.rm=T),
            fishCaught = sum(TrialFeedback == "FishCaught", na.rm=T),
            fishReel = sum(TrialFeedback == "Reel", na.rm=T),
            fishStay = sum(TrialFeedback == "Stay", na.rm=T),
            fishUnreel = sum(TrialFeedback == "Unreel", na.rm=T),
            fishLost = sum(TrialFeedback == "FishLost", na.rm=T),
            notFishCaught = sum(Event == "GameDecision" & lead(Event) != "FishEvent"),
            reel = sum(Event == "GameDecision" & lead(Event) != "FishEvent" & !(TrialResult %in% c("RejInput", "AssistFail", "MitigateFail"))),
            escape = sum(Event == "GameDecision" & lead(Event) != "FishEvent" & (TrialResult %in% c("RejInput"))),
            trial_rate_accept = (accInput + assistInput) / totalTrials,
            trial_rate_reject = rejInput / totalTrials,
            trial_rate_assist = assistInput / totalTrials,
            trial_rate_sham = explicitSham / totalTrials,
            trial_rate_mitigate = mitigateFail / totalTrials,
            trial_rate_positive = (accInput+assistInput+explicitSham) / (totalTrials-mitigateFail),
            pam_rate = 0,
            pam_rate = unique(ifelse(Condition == "AS", trial_rate_assist, pam_rate)),
            pam_rate = unique(ifelse(Condition == "IO", trial_rate_sham, pam_rate)),
            pam_rate = unique(ifelse(Condition == "MF", trial_rate_mitigate, pam_rate)),
            time_total = sum(time_delta),
            PercNormalized = unique(PercNormalized),
            Gender = unique(Gender),
            Age = unique(Age),
            FrustNormalized = unique(FrustNormalized),
            Order = unique(Order),
            bci_experience = unique(TriedBCIBefore),
            Hardest = unique(Hardest),
            Easiest = unique(Easiest))

# Count the number of motor imagery attempts in total
St <- D %>% ungroup() %>% group_by(Participant, Condition) %>%
  filter(Period %in% c("RestPeriod", "OpenPeriod")) %>%
  summarize(mi_recog = sum(Event == "MotorImagery"),
            mi_recog_openperiod = sum(Event == "MotorImagery" & Period == "OpenPeriod"),
            mi_recog_restperiod = sum(Event == "MotorImagery" & Period == "RestPeriod")) %>% right_join(St)

# Group by input window. Count the number of attempts in each window.
St <- D %>% ungroup() %>% filter(Period %in% c("OpenPeriod")) %>% group_by(Participant, Condition, InputWindowOrderFilled) %>%
  summarize(mi_recog_window = ifelse(sum(Event == "MotorImagery") > 0, 1,0), #Whether MI happened in the window
            mi_recog_window_count = sum(Event == "MotorImagery"), #How much MI happened in the window
            time_window = sum(time_delta)) %>%
  filter(InputWindowOrderFilled > -1) %>% ungroup() %>% group_by(Participant, Condition) %>%
  summarize(mi_recog_trial = sum(mi_recog_window > 0),
            mi_recog_window = sum(mi_recog_window),
            time_window = sum(time_window),
            time_window_min = time_window / 60) %>%
  right_join(St)

St <- St %>% ungroup() %>%
  mutate(rate_trial = (mi_recog_trial)/totalTrials,
         rate_feedback = posTrial/ totalTrials,
         session = 1,
         session = cumsum(session),
         bci_experience = ifelse(grepl("Yes",bci_experience), TRUE, FALSE))


# Group people and count them based on 
# Group Low: Participants with very low (more than half feedback was negative)
# Group Exact: Participants close to the target rate with few false positives.
# Group FP: Participants with more than double the false positives.
St <- St %>% mutate(mi_group = "ExactGroup",
                  mi_group = ifelse(mi_recog_openperiod < (totalTrials*0.5), "LowGroup",mi_group),
                  mi_group = ifelse(mi_recog > (totalTrials*2), "FPGroup",mi_group))

St %>% ungroup() %>% group_by(mi_group) %>%
  summarize(count = n())
  
# Group people and count them based on
# Group low variability: Participants which produced a consistent number of MI events.
# Group high variability: Participants with high variability in MI events.
St %>% ungroup() %>% group_by(Participant) %>%
  summarize(mi_stability = ifelse(length(unique(mi_group)) > 1, "unstable", "stable")) %>%
  ungroup() %>% group_by(mi_stability) %>%
  summarize(count = n())


Sp <- St %>% ungroup() %>% group_by(Participant) %>%
  summarize(Gender = unique(Gender),
            Age = unique(Age),
            mean_perc = mean(PercNormalized),
            mean_frust = mean(FrustNormalized),
            mean_rate = mean(rate_trial),
            mean_feedback = mean(rate_feedback),
            bci_experience = unique(bci_experience))

Sc <- St %>% ungroup() %>% group_by(Condition) %>%
  summarize(
            mean_pam_rate = mean(pam_rate),
            mean_perc = mean(PercNormalized),
            mean_frust = mean(FrustNormalized),
            mean_rate = mean(rate_trial),
            mean_feedback = mean(rate_feedback),
            mean_hardest = sum(ifelse(Hardest,0,1)),
            mean_easiest = sum(ifelse(Easiest,0,1)),
            )

#S %>% select(Participant, Condition, fishCaught, PercNormalized,FrustNormalized) %>% arrange(PercNormalized) %>% view()

save(St, file = 'pamBCI.rda', compress=TRUE)

#############
# Number of rows up.. analysis
#############


#############
# Latex Table: Participants
#############


cri = tibble(lv_perc = c(0.1, 0.35,0.68,0.82,1.1),
             lv_frust = rev(lv_perc),
             lv_rate = c(0.1, 0.35, 0.55,0.85,1.1),
             colors = c("g0","g1", "g2", "g3", "g4"),
             lv_fish = c(0,2,4,6,10),
             lv_lost = c(8,4,2,1,0))

Sp_table <- Sp %>% select(Participant, Gender, Age, , bci_experience, mean_perc, mean_frust, mean_rate, mean_feedback) %>%
  mutate(
        mean_perc_c = t_color(mean_perc, cri$lv_perc, cri$colors),
        mean_frust_c = t_color(mean_frust, cri$lv_frust, cri$colors),
        mean_rate_c = t_color(mean_rate, cri$lv_rate, cri$colors),
        mean_feedback_c = t_color(mean_feedback, cri$lv_rate, cri$colors),
        Gender = ifelse(Gender == "Male", "M", "F"),
        bci_experience = ifelse(bci_experience, "Yes", "No"),
        mean_perc = format(round(mean_perc,2), nsmall = 2),
        mean_frust = format(round(mean_frust,2), nsmall = 2),
        mean_rate = paste0(format(round(mean_rate * 100,0), nsmall = 0),"\\%"),
        mean_feedback = paste0(format(round(mean_feedback * 100, 0), nsmall = 0), "\\%"),
        mean_perc = paste0("\\cellcolor{", mean_perc_c, "}", mean_perc),
        mean_frust = paste0("\\cellcolor{", mean_frust_c, "}", mean_frust),
        mean_rate = paste0("\\cellcolor{", mean_rate_c, "}", mean_rate),
        mean_feedback = paste0("\\cellcolor{", mean_feedback_c, "}", mean_feedback),
        mean_perc_c = NULL, mean_frust_c = NULL, mean_rate_c = NULL, mean_feedback_c = NULL,
        across(everything(), as.character))
Sp_table <- Sp_table %>%
  rename(`Perc. Control` = mean_perc, `Frustration` = mean_frust, `MI Conv. Rate` = mean_rate,
         `Pos. Feedback` = mean_feedback, `BCI Experience` = bci_experience)


Sp_table = Sp_table %>% pivot_longer(cols=-c(Participant), names_to = "Variable") %>%
  pivot_wider(names_from = Participant, values_from = value)

paste(colnames(Sp_table), collapse=" & ")
message(paste(Sp_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

St_table <- St %>% group_by(Condition) %>% select(Participant, PercNormalized, FrustNormalized, rate_trial, rate_feedback,
                                                  fishCaught, fishLost) %>%
  mutate(
    Condition = ifelse (Condition == "MF", "Mit. Failure", Condition),
    Condition = ifelse (Condition == "NO", "Ref. Condition", Condition),
    Condition = ifelse (Condition == "AS", "Aug. Success", Condition),
    Condition = ifelse (Condition == "IO", "Input Override", Condition),
    perc_c = t_color(PercNormalized, cri$lv_perc, cri$colors),
    frust_c = t_color(FrustNormalized, cri$lv_frust, cri$colors),
    rate_c = t_color(rate_trial, cri$lv_rate, cri$colors),
    feedback_c = t_color(rate_feedback, cri$lv_rate, cri$colors),
    fish_c = t_color(fishCaught, cri$lv_fish, cri$colors),
    lost_c = t_color(fishLost, cri$lv_lost, cri$colors),
    PercNormalized = format(round(PercNormalized,2), nsmall = 2),
    FrustNormalized = format(round(FrustNormalized,2), nsmall = 2),
    rate_trial = paste0(format(round(rate_trial * 100,0), nsmall = 0),"\\%"),
    rate_feedback = paste0(format(round(rate_feedback * 100, 0), nsmall = 0), "\\%"),
    PercNormalized = paste0("\\cellcolor{", perc_c, "}", PercNormalized),
    FrustNormalized = paste0("\\cellcolor{", frust_c, "}", FrustNormalized),
    rate_trial = paste0("\\cellcolor{", rate_c, "}", rate_trial),
    rate_feedback = paste0("\\cellcolor{", feedback_c, "}", rate_feedback),
    fishCaught = paste0("\\cellcolor{", fish_c, "}", fishCaught),
    fishLost = paste0("\\cellcolor{", lost_c, "}", fishLost),
    perc_c = NULL, frust_c = NULL, rate_c = NULL, feedback_c = NULL,
    lost_c = NULL, fish_c = NULL,
    across(everything(), as.character)) %>% arrange(Condition) %>%
  rename(`Perc. Control` = PercNormalized, `Frustration` = FrustNormalized, `MI Conv. Rate` = rate_trial,
         `Pos. Feedback` = rate_feedback, `Fish Caught` = fishCaught, `Fish Lost` = fishLost) %>%
  pivot_longer(cols=-c(Participant, Condition), names_to = "Variable") %>%
  pivot_wider(names_from = Participant, values_from = value)

St_table <- St_table %>% group_by(Condition) %>%
  group_modify(~ add_row(Variable=paste("\\underline{",.y,"}"),.before=0, .x)) %>%
  ungroup() %>% replace(is.na(.)," ") %>%
  select(-Condition)

paste(colnames(St_table), collapse=" & ")
writeLines(paste(St_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")



#############
# Linear Mixed Models: All
#############
lmes = list(predictors = c("PercNormalized", "FrustNormalized", "Hardest","Easiest"),
            random = c("bci_experience"),
            fixed = c("rate_trial","rate_feedback","Gender", "Condition", 
                      "pam_rate", "fishCaught","fishLost","fishReel","fishUnreel"),
            null = c("Participant"),
            threshold = 0.05,
            df = St)

table = g_lme_table(lmes)

#models = g_lme(lmes$df,lmes$predictors,lmes$random,lmes$fixed,lmes$null,lmes$threshold)

lme_table <- table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         across(everything(), ~ str_replace_all(.x, c("fishCaught" = "Fish Caught",
                                                  "fishLost" = "Fish Lost",
                                                  "fishUnreel" = "Fish Unreel",
                                                  "rate_feedback" = "Pos. Feedback",
                                                  "pam_rate" = "PAM Rate",
                                                  "fishReel" = "Fish Reel",
                                                  "rate_trial" = "MI Conv. Rate",
                                                  "PercNormalized" = "Perceived Control",
                                                  "FrustNormalized" = "Frustration")))
         ) %>%
  select(Predicted, `Random Intercept`, `Fixed Effect`, AIC, BIC, ML, `$\\chi^2$`, `$R^2_m$`,`$R^2_c$`)
message(paste(colnames(lme_table), collapse=" & "))
message(paste(lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))


#############
# Linear Mixed Models: Perc. Control
#############


# Perceived Control
perc.null = lmer(PercNormalized ~ (1|Participant), data = St, REML=F)
perc.0 <- list("BCI Experience" = lmer(PercNormalized ~ (1|Participant) + (1|bci_experience),  data = St, REML=F)# Perceived Control to Order
)
lapply(perc.0, function(x) { anova(perc.null, x) }) 

perc.null = lmer(PercNormalized ~ (1|Participant), data = St, REML=F)
perc.1 <- list("MI Rate" = lmer(PercNormalized ~ rate_trial + (1|Participant), data = St, REML=F),# Perceived Control to MI Rate
               "PAM Rate" = lmer(PercNormalized ~ pam_rate + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Fish Caught" = lmer(PercNormalized ~ fishCaught + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Fish Lost" = lmer(PercNormalized ~ fishLost + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Fish Reel" = lmer(PercNormalized ~ fishReel + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Fish Unreel" = lmer(PercNormalized ~ fishUnreel + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Gender" = lmer(PercNormalized ~ Gender + (1|Participant), data = St, REML=F), # Perceived Control to Gender
               "Condition" = lmer(PercNormalized ~ Condition + (1|Participant), data = St, REML=F), # Perceived Control to Condition
               "Pos. Feedback" = lmer(PercNormalized ~ rate_feedback + (1|Participant), data = St, REML=F) # Perceived Control to Feedback
)
lapply(perc.1, function(x) { anova(perc.null, x) }) 

perc.feedback = lmer(PercNormalized ~ rate_feedback + (1|Participant), data = St, REML=F)
perc.2 <- list("Pos. Feedback + MI Rate" = lmer(PercNormalized ~ rate_feedback + rate_trial + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate + Feedback
               "Pos. Feedback + PAM Rate" = lmer(PercNormalized ~ rate_feedback + pam_rate + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate + Feedback
               "Pos. Feedback + Condition" = lmer(PercNormalized ~ rate_feedback + Condition + (1|Participant), data = St, REML=F), # Perceived Control to Condition + Positive Feedback
               "Pos. Feedback + Fish Caught" = lmer(PercNormalized ~ rate_feedback + fishCaught + (1|Participant), data = St, REML=F) # Perceived Control to Condition + Positive Feedback
               #"Pos. Feedback + Condition Order" = lmer(PercNormalized ~ rate_feedback + ConditionOrder + (1|Participant), data = St, REML=F) # Perceived Control to Condition + Positive Feedback
)
lapply(perc.2, function(x) { anova(perc.feedback, x) }) 

perc.fish = lmer(PercNormalized ~ fishLost + (1|Participant), data = St, REML=F)
perc.2b <- list("Fish Lost + Condition" = lmer(PercNormalized ~ fishLost + Condition + (1|Participant), data = St, REML=F), # Perceived Control to Condition + Positive Feedback
               "Fish Lost + Pos. Feedback" = lmer(PercNormalized ~ fishLost + rate_feedback + (1|Participant), data = St, REML=F), # Perceived Control to Condition + Positive Feedback
               "Fish Lost + PAM Rate" = lmer(PercNormalized ~ fishLost + pam_rate + (1|Participant), data = St, REML=F), # Perceived Control to Condition + Positive Feedback
               "Fish Lost + Fish Caught" = lmer(PercNormalized ~ fishLost + fishCaught + (1|Participant), data = St, REML=F) # Perceived Control to Condition + Positive Feedback
               #"Pos. Feedback + Condition Order" = lmer(PercNormalized ~ rate_feedback + ConditionOrder + (1|Participant), data = St, REML=F) # Perceived Control to Condition + Positive Feedback
)
lapply(perc.2b, function(x) { anova(perc.fish, x) }) 
perc_lme_table <- g_lme_table(perc.1, perc.null, "null")  %>% bind_rows(g_lme_table(perc.2b, perc.fish, "fishnull")) 
perc_lme_table <- perc_lme_table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Predicted` = "Perceived Control",
         `Random Intercept` = "Participant",
         chi = format(round(`$\\chi^2$`,3), nsmall = 3),
         chi = ifelse(chi == "0.000", "$<$0.001", chi),
         `$\\chi^2$`= chi,
         chi = NULL) %>%
  select(Predicted, `Random Intercept`, `Fixed Effect`, AIC, BIC, ML, `$\\chi^2$`, `$R^2_m$`,`$R^2_c$`)
message(paste(colnames(perc_lme_table), collapse=" & "))
message(paste(perc_lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

#############
# Linear Mixed Models: Frustation
#############

# Frustration
frust.null = lmer(FrustNormalized ~ 1 + (1|Participant), data = St, REML=F)
frust.0 <- list("BCI Experience" = lmer(FrustNormalized ~ 1 + (1|Participant) + (1|bci_experience),  data = St, REML=F))

lapply(frust.0, function(x) { anova(frust.null, x) }) # Not Significant

frust.null = lmer(FrustNormalized ~ (1|Participant),  data = St, REML=F)
frust.1 <- list("MI Rate" = lmer(FrustNormalized ~ rate_trial + (1|Participant), data = St, REML=F),# Frustration to MI Rate
                "PAM Rate" = lmer(FrustNormalized ~ pam_rate + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
                "Gender" = lmer(FrustNormalized ~ Gender + (1|Participant), data = St, REML=F), # Frustration to Gender
                "Fish Caught" = lmer(FrustNormalized ~ fishCaught + (1|Participant), data = St, REML=F), # Frustration to Gender
                "Fish Lost" = lmer(FrustNormalized ~ fishLost + (1|Participant), data = St, REML=F), # Frustration to Gender
                "Fish Reel" = lmer(FrustNormalized ~ fishReel + (1|Participant), data = St, REML=F), # Frustration to Gender
                "Fish Unreel" = lmer(FrustNormalized ~ fishUnreel + (1|Participant), data = St, REML=F), # Frustration to Gender
                "Condition" = lmer(FrustNormalized ~ Condition + (1|Participant), data = St, REML=F), # Frustration to Condition
                "Pos. Feedback" = lmer(FrustNormalized ~ rate_feedback + (1|Participant), data = St, REML=F) # Frustration to Feedback
)

lapply(frust.1, function(x) { anova(frust.null, x) })
lapply(frust.1, function(x) { r.squaredGLMM(x, null=frust.null) })

frust_lme_table <- g_lme_table(frust.1, frust.null, "null")
frust_lme_table <- frust_lme_table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Predicted` = "Frustration",
         `Random Intercept` = "Participant",
         chi = format(round(`$\\chi^2$`,3), nsmall = 3),
         chi = ifelse(chi == "0.000", "$<$0.001", chi),
         `$\\chi^2$`= chi,
         chi = NULL) %>%
  select(Predicted, `Random Intercept`, `Fixed Effect`, AIC, BIC, ML, `$\\chi^2$`, `$R^2_m$`,`$R^2_c$`)

message(paste(colnames(frust_lme_table), collapse=" & "))
message(paste(frust_lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))


#############
# Bar chart depicting mi recognition
#############

fig1 <- fig %>%
  add_trace(data = S %>% arrange(Participant, Order), y=~mi_recog, type='bar', color=I('grey80'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  add_trace(data = S %>% arrange(Participant, Order), y=~mi_recog_openperiod, type='bar', color=I('grey60'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  layout(showlegend=F, barmode='overlay', yaxis = list(title="Recognition"),
         xaxis = list(range=c(-1,70), tickmode='array', tickvals=0:69,ticktext=~paste(Participant,Condition), title=""))  
fig1 <- fig1 %>%
  add_trace(data = tibble(x=c(-10,100), y=c(20,20)), x=~x, y=~y, type='scatter', color=I('black'),
            mode='lines', line=list(width=1))
fig1

orca(fig1, "fig/motor-imagery-event-count-bar-charts.pdf", width=950, height=475)

#############
# Level of Control to Perceived Control
#############

PercLine <- list("NO" = p_supsmu(St %>% filter(Condition == "NO"),"PercNormalized", "trial_rate_positive", b=0.7),
                 "AS" = p_supsmu(St %>% filter(Condition == "AS"),"PercNormalized", "trial_rate_positive", b=0.7),
                 "MF" = p_supsmu(St %>% filter(Condition == "MF"),"PercNormalized", "trial_rate_positive", b=0.7),
                 "IO" = p_supsmu(St %>% filter(Condition == "IO"),"PercNormalized", "trial_rate_positive", b=0.7))

FrustLine <- list("NO" = p_supsmu(St %>% filter(Condition == "NO"),"FrustNormalized", "trial_rate_positive", b=0.7),
                  "AS" = p_supsmu(St %>% filter(Condition == "AS"), "FrustNormalized", "trial_rate_positive", b=0.7),
                  "MF" = p_supsmu(St %>% filter(Condition == "MF"),"FrustNormalized", "trial_rate_positive", b=0.7),
                  "IO" = p_supsmu(St %>% filter(Condition == "IO"),"FrustNormalized", "trial_rate_positive", b=0.7))

CombLine <- list("NO" = p_supsmu(St %>% filter(Condition == "NO"),"FrustNormalized", "PercNormalized", b=0.7),
                  "AS" = p_supsmu(St %>% filter(Condition == "AS"), "FrustNormalized", "PercNormalized", b=0.7),
                  "MF" = p_supsmu(St %>% filter(Condition == "MF"),"FrustNormalized", "PercNormalized", b=0.7),
                  "IO" = p_supsmu(St %>% filter(Condition == "IO"),"FrustNormalized", "PercNormalized", b=0.7))

# Perceived Control to Condition

fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(PercNormalized,amount=.02), color=I('rgba(0.8,0.8,0.8,0.15)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=PercLine[["NO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["AS"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["MF"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["IO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(PercNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=PercLine[[cond]], x=~x, y=~y, color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Perceived Control', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Perceived Control"), xaxis=list(title="Positive Feedback"))
fig_c
orca(fig_c, "fig/perc_control_pos_feedback.pdf", width=1150, height=350)


fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(FrustNormalized,amount=.02), color=I('rgba(0.8,0.8,0.8,0.15)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=FrustLine[["NO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=FrustLine[["AS"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=FrustLine[["MF"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=FrustLine[["IO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(FrustNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=FrustLine[[cond]], x=~x, y=~y, color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Frustration', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Frustration"), xaxis=list(title="Positive Feedback"))
fig_c
orca(fig_c, "fig/frust_control_pos_feedback.pdf", width=1150, height=350)


fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~jitter(PercNormalized,amount=.02), y=~jitter(FrustNormalized,amount=.02), color=I('rgba(0.8,0.8,0.8,0.15)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=CombLine[["NO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["AS"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["MF"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["IO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~jitter(PercNormalized,amount=.02), y=~jitter(FrustNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=CombLine[[cond]], x=~x, y=~y, color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Perceived Control', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Frustration', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Frustration"), xaxis=list(title="Perceived Control"))
fig_c
orca(fig_c, "fig/frust_perc_control.pdf", width=1150, height=350)

figf <- fig %>%
  add_trace(name= "NO", data = St %>% filter(Condition == "NO"), x=~trial_rate_positive, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("black"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["Pure"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(name= "AS", data = St %>% filter(Condition == "AS"), x=~trial_rate_positive, text=S_vis$Condition, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("blue"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["AS"]], x=~x, y=~y, color=I('blue'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(name= "MF", data = St %>% filter(Condition == "MF"), x=~trial_rate_positive, text=S_vis$Condition, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("red"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["MF"]], x=~x, y=~y, color=I('red'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(name= "IO", data = St %>% filter(Condition == "IO"), x=~trial_rate_positive, text=S_vis$Condition,  y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("orange"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["IO"]], x=~x, y=~y, color=I('orange'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Perceived Control"), xaxis = list(range=c(-0.05,1.1), title="MI Control"))
figf


fig_p <- fig %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$PercNormalized, color=S_vis$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$PercNormalized, text=S_vis$Condition,
            type='scattergl', mode='text') %>%
  add_trace(data=PercLine[["Pure"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Control"),
         yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))

fig_f <- fig %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$FrustNormalized, color=S_vis$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$FrustNormalized, text=S_vis$Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Control"),
         yaxis=list(range=c(-0.1,1.1), title="Frustration"))

fig_p
fig_f


vis <- subplot(fig_f, fig_p) %>% layout(showlegend=FALSE)
vis

#############
# Variance in Level of Control between participants
#############

fig %>%
  add_trace(x=Sc$Participant, y=Sc$trial_rate_accept, mode='markers', type='scattergl')


#############
# Perceived Control vs number of fish
#############

fig_p <- fig %>%
  add_trace(x=St$fishCaught, y=St$PercNormalized, color=St$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=St$fishCaught, y=St$PercNormalized, text=St$Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,11), title="FishCaught"),
         yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))

fig_f <- fig %>%
  add_trace(x=S_vis$fishCaught, y=S_vis$FrustNormalized, color=S_vis$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=S_vis$fishCaught, y=S_vis$FrustNormalized, text=S_vis$Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,11), title="FishCaught"),
         yaxis=list(range=c(-0.1,1.1), title="Frustration"))

fig_p
fig_f


vis <- subplot(fig_f, fig_p) %>% layout(showlegend=FALSE)
vis


# fig_f <- fig %>%
# add_trace(x=S_vis$trial_rate_positive, y=S_vis$FrustNormalized, color=I("white"),
          #marker=list(symbol='circle', size=19, line=list(width=1,color=I("black"))),
          #type='scatter', mode='markers') %>%
  #add_trace(x=S_vis$trial_rate_positive, y=S_vis$FrustNormalized, text=S_vis$Condition,
            #type='scatter', mode='text', textfont=list(size=9)) %>%
  #layout(xaxis=list(range=c(-0.1,1.1), title="Control"),
         #yaxis=list(range=c(-0.1,1.1), title="Frustration"))


#
