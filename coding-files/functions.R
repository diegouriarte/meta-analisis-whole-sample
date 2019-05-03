# This file contains functions to calculate summary effects by 

# Combine effects across time and gender ----------------------------------

combineEffectsAcrossTime <- function(y, se, corr = 0.5) {
  #input:
  # y: vector of effects of size m
  # se: vector of standard errors
  # corr: correlations between effects that are going to be combined
  if (length(y) == 1) {
    return(c(y, se))
  }
  m <- length(se)
  y_mean <- mean(y)
  var_y <- se^2
  outside_diagonal <- 0
  for (i in 1:m) {
    for (j in i+1:m) {
      outside_diagonal <- sum(outside_diagonal, 2*corr*se[i]*se[j], 
                              na.rm = TRUE)
    }
  }
  var_y_mean <-  (1/m)^2 * (sum(var_y) + outside_diagonal)
  return(c(y_mean, sqrt(var_y_mean)))
}


combineEffectsGender <- function(y, se) {
  if (length(y) == 1) {
    return(c(y, se))
  }
  var_y <- se^2
  weight_y <- 1/var_y
  y_mean <- sum(weight_y*y)/sum(weight_y)
  V_mean <- 1 / sum(weight_y)
  se_mean <- sqrt(V_mean)
  return(c(y_mean, se_mean))
}



# Collapse effects by evaluation ------------------------------------------


combineEffectsPooled <- function(data) {
  #it combines the effect sizes for evaluations that are pooled both in gender
  #and in age.
  wages_meta <- data %>%
    filter(gender == "pooled",
           is.na(age_group)) %>%
    group_by(evaluation_id) %>%
    summarise(
      y = combineEffectsAcrossTime(y = smd_treated_control, se = sqrt(vd))[1],
      se = combineEffectsAcrossTime(y = smd_treated_control, se = sqrt(vd))[2],
      number_effects = n()
    ) %>%
    ungroup()
  
  wages_meta <- wages_meta %>% 
    mutate(gender = "pooled") %>%
    select(evaluation_id, gender, y:number_effects)
  
  return(wages_meta)
}


GetListGenderOnly <- function(data) {
  #It returns a dataframe with only one column, the evaluations that do not have
  #pooled effects in gender
  
  df1 <- data %>%
    filter(gender == "pooled") %>%
    select(evaluation_id) %>%
    distinct()
  
  df2 <- data %>%
    filter(gender %in% c("male", "female")) %>%
    select(evaluation_id) %>%
    distinct()
  
  evaluation_female_or_male <-
    anti_join(df2, df1, by = "evaluation_id") 
  
  return(evaluation_female_or_male)
  
}

CollapseByGender <- function(data) {
  # Collapses effects in gender for evaluation that only have male or female
  # effects. 
  # Output should contain only one female and male summary effect for each 
  # evaluation
  evaluation_female_or_male <- pull(GetListGenderOnly(data))
  
  data %>%
    filter(evaluation_id %in% evaluation_female_or_male,
           is.na(age_group)) %>%
    group_by(evaluation_id, gender) %>%
    summarise(
      beta = combineEffectsAcrossTime(y = smd_treated_control, se = sqrt(vd))[1],
      se = combineEffectsAcrossTime(y = smd_treated_control, se = sqrt(vd))[2],
      number_effects = n()
    )
}


CombineGenderSyn <- function(data) {
  # It combines the effects male and female to get a synthetic pooled estimated
  # which is different than the a direct pooled estimate from a evaluation
  effects_collapsed_by_gender <- CollapseByGender(data)
  effects_evaluation_with_gender <- effects_collapsed_by_gender %>%
    group_by(evaluation_id) %>%
    mutate(
      y_pooled = combineEffectsGender(y = beta, se = se)[1],
      se_pooled = combineEffectsGender(y = beta, se = se)[2],
      total_number_effects = sum(number_effects)
    ) %>%
    ungroup() %>%
    mutate(
      gender_evaluation = ifelse(
        total_number_effects == number_effects,
        as.character(gender),
        "s.pooled"
      ),
      gender_evaluation = parse_factor(gender_evaluation)
    ) %>%
    select(-gender, -beta, -se, -number_effects) %>%
    distinct() %>%
    select(
      evaluation_id,
      gender = gender_evaluation,
      y = y_pooled,
      se = se_pooled,
      number_effects = total_number_effects
    ) 
  
  return(effects_evaluation_with_gender)
}

CollapseMeta <- function(data) {
  # It creates a dataframe with one summary effect by evaluation
  wages_meta <-  combineEffectsPooled(data)
  effects_evaluation_with_gender <- CombineGenderSyn(data)
  
  wages_meta <- wages_meta %>% 
    bind_rows(effects_evaluation_with_gender) %>%
    mutate(gender = parse_factor(gender))
  
  return(wages_meta)
  
}
