library(plotly)
library(tidyverse)
options("digits.secs"=6)

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_pam.rda')

#############
# Summaries
#############

S <- D %>% group_by(Participant, Condition) %>%
  summarise(rejInput = sum(TrialResult == "RejInput", na.rm=T),
            accInput = sum(TrialResult == "AccInput", na.rm=T),
            assistInput = sum(TrialResult %in% c("AssistSuccess", "AugSuccess"), na.rm=T),
            explicitSham = sum(TrialResult %in% c("ExplicitSham", "OverrideInput"), na.rm=T),
            mitigateFail = sum(TrialResult %in% c("AssistFail","MitigateFail"), na.rm=T),
            totalTrials2 = sum(!is.na(TrialResult), na.rm=T),
            totalTrials = rejInput+accInput+assistInput+explicitSham+mitigateFail,
            fishCaught = sum(FishEvent == "FishCaught", na.rm=T),
            trial_rate_accept = (accInput + assistInput) / totalTrials,
            trial_rate_reject = rejInput / totalTrials,
            trial_rate_assist = assistInput / totalTrials,
            trial_rate_sham = explicitSham / totalTrials,
            trial_rate_mitigate = mitigateFail / totalTrials,
            trial_rate_positive = (accInput+assistInput+explicitSham) / (totalTrials-mitigateFail),
            PercNormalized = unique(PercNormalized),
            FrustNormalized = unique(FrustNormalized))

#S %>% select(Participant, Condition, fishCaught, PercNormalized,FrustNormalized) %>% arrange(PercNormalized) %>% view()

#############
# Level of Control to Perceived Control
#############

PercRateCurve.Pure.lm <- lm(PercNormalized ~ trial_rate_accept, data = S %>% filter(Condition %in% c("NO")))
PercRateCurve.Pure <- data.frame(Condition = "NO", x = (0:100)/100, y = predict(PercRateCurve.Pure.lm, data.frame(trial_rate_accept = (0:100)/100), se.fit=T))
PercRateCurve.AS.lm <- lm(PercNormalized ~ trial_rate_accept, data = S %>% filter(Condition %in% c("AS")))
PercRateCurve.AS <- data.frame(Condition = "AS",x = (0:100)/100, y = predict(PercRateCurve.AS.lm, data.frame(trial_rate_accept = (0:100)/100), se.fit=T))
PercRateCurve.MF.lm <- lm(PercNormalized ~ trial_rate_accept, data = S %>% filter(Condition %in% c("MF")))
PercRateCurve.MF <- data.frame(Condition = "MF",x = (0:100)/100, y = predict(PercRateCurve.MF.lm, data.frame(trial_rate_accept = (0:100)/100), se.fit=T))
PercRateCurve.IO.lm <- lm(PercNormalized ~ trial_rate_accept, data = S %>% filter(Condition %in% c("IO")))
PercRateCurve.IO <- data.frame(Condition = "IO",x = (0:100)/100, y = predict(PercRateCurve.IO.lm, data.frame(trial_rate_accept = (0:100)/100), se.fit=T))
PercRateCurve <- PercRateCurve.Pure %>% bind_rows(PercRateCurve.AS, PercRateCurve.MF, PercRateCurve.IO)

FrustRateCurve.Pure.lm <- lm(FrustNormalized ~ trial_rate_accept, data = S %>% filter(Condition %in% c("NO")))
FrustRateCurve.Pure <- data.frame(Condition = "NO", x = (0:100)/100, y = predict(FrustRateCurve.Pure.lm, data.frame(trial_rate_accept = (0:100)/100), se.fit=T))
FrustRateCurve.AS.lm <- lm(FrustNormalized ~ trial_rate_accept, data = S %>% filter(Condition %in% c("AS")))
FrustRateCurve.AS <- data.frame(Condition = "AS",x = (0:100)/100, y = predict(FrustRateCurve.AS.lm, data.frame(trial_rate_accept = (0:100)/100), se.fit=T))
FrustRateCurve.MF.lm <- lm(FrustNormalized ~ trial_rate_accept, data = S %>% filter(Condition %in% c("MF")))
FrustRateCurve.MF <- data.frame(Condition = "MF",x = (0:100)/100, y = predict(FrustRateCurve.MF.lm, data.frame(trial_rate_accept = (0:100)/100), se.fit=T))
FrustRateCurve.IO.lm <- lm(FrustNormalized ~ trial_rate_accept, data = S %>% filter(Condition %in% c("IO")))
FrustRateCurve.IO <- data.frame(Condition = "IO",x = (0:100)/100, y = predict(FrustRateCurve.IO.lm, data.frame(trial_rate_accept = (0:100)/100), se.fit=T))
FrustRateCurve <- FrustRateCurve.Pure %>% bind_rows(FrustRateCurve.AS, FrustRateCurve.MF, FrustRateCurve.IO)

S_vis = S %>% mutate(trial_rate_accept = jitter(trial_rate_accept,amount=.01),
                     PercNormalized = jitter(PercNormalized, amount=.01),
                     FrustNormalized = jitter(FrustNormalized, amount=.01))

fig_p <- fig %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$PercNormalized, color=S_vis$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$PercNormalized, text=as.character(S_vis$Participant),
            type='scattergl', mode='text') %>%
  add_trace(x=PercRateCurve$x, y=PercRateCurve$y.fit, color=PercRateCurve$Condition, mode='lines', type='scattergl')%>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Control"),
         yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))

fig_f <- fig %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$FrustNormalized, color=S_vis$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$FrustNormalized, text=S_vis$Condition,
            type='scattergl', mode='text') %>%
  add_trace(x=FrustRateCurve$x, y=FrustRateCurve$y.fit, color=FrustRateCurve$Condition, mode='lines', type='scattergl')%>%
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
  add_trace(x=S$Participant, y=S$trial_rate_accept, mode='markers', type='scattergl')


#############
# Perceived Control vs number of fish
#############

fig_p <- fig %>%
  add_trace(x=S_vis$fishCaught, y=S_vis$PercNormalized, color=S_vis$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=S_vis$fishCaught, y=S_vis$PercNormalized, text=S_vis$Condition,
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
