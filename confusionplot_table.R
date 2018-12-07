#########################
## CONFUSION PLOT
#########################

confusion_plot <- function(model_name, random = TRUE) {
  
  set.seed(1337)
  
  training_set <- Bear_scaled[Bear_scaled$.folds != 5,]
  testing_set <- Bear_scaled[Bear_scaled$.folds == 5,]
  
  
  # TRAINING
  if (isTRUE(random)){
    
    model <- glmer(model_name, training_set, family = "binomial")
    
  } else {
    
    model <- glm(model_name, training_set, family = "binomial")
    
  }
  
  
  # OUTPUT
  mod_df <- testing_set %>%
    ungroup() %>%
    mutate(pred_best_num = inv.logit(predict(model, testing_set, 
                                             allow.new.levels=TRUE)),
           pred_best = ifelse(pred_best_num < 0.5, 0, 1),
           pred_tab = case_when(pred_best == 0 & diagnosis == 0 ~ "true negative",
                                pred_best == 0 & diagnosis == 1 ~ "false negative",
                                pred_best == 1 & diagnosis == 0 ~ "false positive",
                                pred_best == 1 & diagnosis == 1 ~ "true positive"))
  
  mod_summary <- mod_df %>%
    group_by(pred_best) %>%
    summarise(n = n())

  a <- table(mod_df$pred_tab)
  tab_performance <<- a
  
  # PLOT
  plot <- mod_df %>%
    mutate(diagnosis = ifelse(diagnosis == 0, "control", "schizophrenic"),
           pred_best = ifelse(pred_best == 0, "control", "schizophrenic")) %>%
    ggplot(aes(pred_best, pred_best_num, fill = factor(pred_tab))) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank()) +
    labs(title = "Model performance", 
         subtitle = model_name,
         fill = "truth value", 
         y = "number of model predictions", 
         x = "model prediction", 
         caption = "trainted on folds 1 to 4, tested on fold 5")
  
  return(plot)
}

pl_bri1 <- confusion_plot("diagnosis ~ coef_var + se + Gender", random = FALSE)
pl_bri1



#########################
## VALUES for diagnosis ~ coef_var + se + Gender
#########################
kable(tab_performance)

