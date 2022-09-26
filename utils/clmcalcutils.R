library(ordinal)
library(tidyverse)

# g_clmm_table <- function(models, null, nullname) {
#   model_clmm <- lapply(seq_along(models), function(i) { 
#     n=names(models)[[i]]
#     a=anova(null, models[[n]]) %>% bind_rows() %>% mutate(model = c(nullname, n))
#     return(a) })
#   model_r2 <- lapply(seq_along(models), function(i) { 
#     n=names(models)[[i]]
#     r=r.squaredGLMM(models[[n]], null=null) %>% as.data.frame() %>% mutate(model = n)
#     return(r) }) 
#   model_r2 <- model_r2 %>% bind_rows()
#   model_clmm <- model_clmm %>% bind_rows()
#   table <- model_clmm %>% left_join(model_r2)  
#   table <- table %>%
#     mutate(AIC = format(round(AIC,2), nsmall = 2),
#            BIC = format(round(BIC,2), nsmall = 2),
#            logLik = format(round(logLik,2), nsmall = 2),
#            R2m = format(round(R2m,2), nsmall = 2),
#            R2c = format(round(R2c,2), nsmall = 2)
#     ) %>%
#     arrange(desc(R2m)) %>%
#     rename(`$\\chi^2$` = `Pr(>Chisq)`, `$R^2_m$` = R2m, `$R^2_c$` = R2c, `Fixed Effect` = model, `ML` = logLik)
#   
#   return(table)
# }

g_clmm_table <- function(clmms) {
  g_clmm <- function(df, predictors, random, fixed, null, threshold) {
    p = last(predictors)
    
    form_null = paste("(1|",null,")")
    form_base = form_null
    models = tibble(Predicted = p, nullmodel = NA, model = form_null, AIC = NA, BIC = NA, logLik = NA,
                    `Pr(>Chisq)` = NA, p_pass = T, aic_pass = T,
                    npar = NA, Chisq = NA, Df = NA, deviance = NA)
    #models = models %>% bind_rows(g_clmm_add(df, p, random, form_base, form_null, threshold, models, "random"))
    #form_null = models %>% filter(p_pass, aic_pass) %>% arrange(-AIC) %>% select(model) %>% slice(n()) %>% pull()
    models = models %>% bind_rows(g_clmm_add(df, p, fixed, form_base, form_null, threshold, models, "fixed"))
    models = models %>% mutate(Predicted = p)
    
    predictors = predictors[-length(predictors)]
    if (length(predictors) == 0) {
      return(models)
    } else {
      models = models %>% bind_rows(g_clmm(df, predictors, random, fixed, null, threshold))
      return(models)
    }
  }
  
  g_clmm_add <- function(df, p, features, form_base, form_null, threshold, models, mode) {
    
    if (mode == "random") {
      # 1) See how many random effects are needed
      new_models = g_clmm_random(df, p, features, form_null)
    } else if (mode == "fixed") {
      # 1) See how many random effects are needed
      new_models = g_clmm_fixed(df, p, features, form_null)
    }
    new_models = new_models %>% mutate(p_pass = ifelse(`Pr(>Chisq)` < threshold, T,F),
                                       aic_pass = ifelse(p_pass & AIC == min(AIC), T, F))
    
    models = models %>% bind_rows(new_models)
    
    
    form_base = models %>% filter(p_pass, aic_pass) %>% select(model) %>% dplyr:::slice(n()) %>% pull()
    print(form_base)
    print(form_null)
    if (!identical(form_base, form_null)) {
      form_null = form_base
      features = features [ !str_detect(form_null, features) ]
      
      new_models = new_models %>% bind_rows(g_clmm_add(df, p, features, form_base, form_null, threshold, models, mode = mode))
      return(new_models)
    } else {
      return(new_models)
    }
    
  }
  
  
  g_clmm_random <- function(df, p, random, form_null) {
    r <- last(random)
    form_test = paste(form_null,"+","(1|",r,")")
    form_r = paste(p,"~", "1 +", form_test)
    form_n = paste(p,"~", "1 +", form_null)
    m_r = clmm(as.formula(form_r), data = df)
    m_n = clmm(as.formula(form_n), data = df)
    a = ordinal:::anova.clm(m_n, m_r) %>% bind_rows() %>% 
      mutate(nullmodel = form_null, model = form_test) %>% dplyr:::slice(n())
    
    #r2=r.squaredGLMM(m_r, null=m_n) %>% as.data.frame() %>% mutate(nullmodel = form_null, model = form_test)
    #model = a %>% left_join(r2)
    
    random = random[-length(random)]
    if (length(random) == 0) {
      return(model)
    } else {
      model = model %>% bind_rows(g_clmm_random(df, p, random, form_null))
      return(model)
    }
  }
  
  g_clmm_fixed <- function(df, p, fixed, form_null) {
    f <- last(fixed)
    form_test = paste(form_null,"+",f)
    form_r = paste(p,"~","1 +", form_test)
    form_n = paste(p,"~","1 +", form_null)
    print(form_r)
    m_f = clmm(as.formula(form_r), data = df)
    m_n = clmm(as.formula(form_n), data = df)
    a = ordinal:::anova.clm(m_n, m_f) %>% bind_rows() %>% 
      mutate(nullmodel = form_null, model = form_test) %>% dplyr:::slice(n())
    #r2=r.squaredGLMM(m_f, null=m_n) %>% as.data.frame() %>% mutate(nullmodel = form_null, model = form_test)
    model = a #%>% left_join(r2)
    
    fixed = fixed[-length(fixed)]
    if (length(fixed) == 0) {
      return(model)
    } else {
      model = model %>% bind_rows(g_clmm_fixed(df, p, fixed, form_null))
      return(model)
    }
  }
  
  table = g_clmm(clmms$df,clmms$predictors,clmms$random,clmms$fixed,clmms$null,clmms$threshold)
  
  table <- table %>%
    mutate(AIC = format(round(AIC,2), nsmall = 2),
           BIC = format(round(BIC,2), nsmall = 2),
           logLik = format(round(logLik,2), nsmall = 2),
           LR.stat = format(round(LR.stat,2), nsmall = 2)
    ) %>%
    arrange(Predicted, AIC) %>%
    rename(`$\\chi^2$` = `Pr(>Chisq)`, `Fixed Effect` = model, `ML` = logLik,  `LR` = LR.stat) %>%
    filter(`$\\chi^2$` < clmms$threshold) %>% select(-p_pass, -aic_pass)
  
  
  
  return(table)
}
