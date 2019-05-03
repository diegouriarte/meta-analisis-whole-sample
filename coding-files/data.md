---
title: "Cleaning excel file"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---

# Load libraries

First, we load required libraries for importing and data cleaning

```r
#install.packages(c("readxl","ggplot2","tidyverse","purrr","knitr","magrittr",
#"openxlsx","labelled","dplyr","here", "metafor"))
library(readxl)
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.0     v purrr   0.2.5
## v tibble  1.4.2     v dplyr   0.7.8
## v tidyr   0.8.2     v stringr 1.3.1
## v readr   1.2.1     v forcats 0.3.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(openxlsx)
library(dplyr)
library(here)
```

```
## here() starts at C:/Users/duriarte/Dropbox/BID-GRADE/meta-analisis
```

```r
library(janitor)
library(skimr)
library(stringr)
library(ggplot2)
```

# Import file from excel file 

We import two sheets from excel file. Sheet 8.Effects, and sheet 7. Evaluations.


```r
ruta_file <- here("data", "effect-sizes-raw.xlsx")
effects <- read_excel(ruta_file, sheet = "8. Effects", col_names = TRUE,
                      na = c("","-")) %>%
  clean_names()
#importamos la hoja 7. Evaluations el excel
evaluations <- read_excel(ruta_file, sheet = "7. Evaluations", col_names = TRUE) %>%
  clean_names() 
```
# Correct type of variables

Select only required variables from evaluations sheet, and convert categorical variables to factor


```r
evaluations_filter <- evaluations %>%
  select(paper_id, evaluation_id, target_age,
         skills = ends_with("technical_soft_job_readiness"),
         training = ends_with("class_internship"), 
         jovenes,
         implementer = implementer_again,
         where_internship = starts_with("where_is_the_internship"),
         where_class = starts_with("where_is_the_classroom"),
         length_class_hours = ends_with("classroom_hours"),
         length_class_months = ends_with("classroom_months"),
         length_internship_hours = ends_with("internship_hours"),
         length_internship_months = ends_with("internship_months"),
         lac = ends_with("lac")) %>%
  mutate_at(c("skills","training","where_internship","where_class",
              "jovenes","implementer"), 
            .fun = parse_factor, levels = NULL)

evaluations_filter <- evaluations_filter %>%
  mutate(implementer=recode(implementer,government="Government")) 

skim(evaluations_filter)
```

```
## Skim summary statistics
##  n obs: 59 
##  n variables: 14 
## 
## -- Variable type:character -----------------------------------------------------
##       variable missing complete  n min max empty n_unique
##  evaluation_id       0       59 59   8  26     0       51
##            lac       0       59 59   2   3     0        2
##       paper_id       0       59 59   8  23     0       54
##     target_age       8       51 59   3   5     0       34
## 
## -- Variable type:factor --------------------------------------------------------
##          variable missing complete  n n_unique
##       implementer       0       59 59        3
##           jovenes       0       59 59        3
##            skills       0       59 59        5
##          training       0       59 59        3
##       where_class       0       59 59        4
##  where_internship       0       59 59        3
##                        top_counts ordered
##   Gov: 43, NGO: 15, Mul: 1, NA: 0   FALSE
##   amb: 41, men: 14, may: 4, NA: 0   FALSE
##  tec: 32, bot: 16, tec: 7, sof: 3   FALSE
##   cla: 37, bot: 21, on-: 1, NA: 0   FALSE
##    pri: 41, pub: 9, Pub: 8, NA: 1   FALSE
##           NA: 37, pri: 16, pri: 6   FALSE
## 
## -- Variable type:numeric -------------------------------------------------------
##                  variable missing complete  n   mean     sd    p0   p25
##        length_class_hours      24       35 59 389.76 366.98 37.5  142.5
##       length_class_months      13       46 59   6.14   4.79  0.25   3  
##   length_internship_hours      51        8 59 274.25 143.47 80    208.5
##  length_internship_months      37       22 59   4.22   3.46  2      3  
##  p50 p75   p100     hist
##  225 475 1200   <U+2587><U+2585><U+2583><U+2581><U+2581><U+2581><U+2581><U+2582>
##    6   8   24   <U+2586><U+2587><U+2583><U+2581><U+2581><U+2581><U+2581><U+2581>
##  240 380  480   <U+2585><U+2581><U+2581><U+2587><U+2581><U+2582><U+2581><U+2585>
##    3   4   16.8 <U+2587><U+2582><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
```
Select only required variables from effects sheet, and convert categorical variables to factor


```r
effects_filter <- effects %>%
  select(paper_id, evaluation_id:age_group,months_of_delay:outcome_type,
         n_total = n, nt:se,t, p_value, se_calculated, 
         starts_with("smd"),
         starts_with("vd")) %>%
  mutate(vd = if_else(is.na(vd_mean), vd_regress, vd_mean),
         gender = recode_factor(gender, Female = "female",
                                Pooled = "pooled", Male = "male"),
         ) %>%
  select(-starts_with("vd_")) 

skim(effects_filter)
```

```
## Skim summary statistics
##  n obs: 882 
##  n variables: 17 
## 
## -- Variable type:character -----------------------------------------------------
##         variable missing complete   n min max empty n_unique
##        age_group     658      224 882   3   7     0       28
##    evaluation_id       0      882 882   8  26     0       51
##  months_of_delay     104      778 882   1   4     0       46
##         paper_id       0      882 882   8  23     0       54
## 
## -- Variable type:factor --------------------------------------------------------
##  variable missing complete   n n_unique
##    gender       0      882 882        3
##                           top_counts ordered
##  poo: 466, fem: 224, mal: 192, NA: 0   FALSE
## 
## -- Variable type:numeric -------------------------------------------------------
##             variable missing complete   n      mean         sd          p0
##                 beta       0      882 882 1115.43    5523.1    -9836      
##            effect_id       0      882 882   19.55      18.62       1      
##              n_total      12      870 882 7481.84   19520.58     115      
##                   nc      12      870 882 5076.17   16516.92      45.25   
##                   nt      12      870 882 2403.78    3538.95      38      
##         outcome_type       0      882 882    2.42       1.67       1      
##              p_value     594      288 882    0.4        0.26       0      
##                   se     370      512 882  492.39    2381.71       0      
##        se_calculated       0      882 882  763.75    5770.96    -158.63   
##  smd_treated_control     356      526 882    0.059      0.14      -0.6    
##                    t     563      319 882    1.03       0.82      -1.64   
##                   vd     356      526 882    0.0047     0.0066     4.2e-05
##        p25       p50       p75       p100     hist
##   -0.006      0.058    50.48    70861     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##    5         13        29          82     <U+2587><U+2583><U+2582><U+2582><U+2581><U+2581><U+2581><U+2581>
##  744.75    1941      4914.54   178636     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##  344        819.24   2068      150308     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##  352.25    1093      2948       29917     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##    1          2         3           8     <U+2587><U+2587><U+2582><U+2581><U+2581><U+2582><U+2581><U+2581>
##    0.1        0.5       0.5         0.98  <U+2585><U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581>
##    0.031      0.12     90.02    23355     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##    0.03       0.23    161.52    82027.73  <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##   -0.0056     0.05      0.12        0.77  <U+2581><U+2581><U+2581><U+2587><U+2583><U+2581><U+2581><U+2581>
##    0.67       0.67      1.64        3.7   <U+2581><U+2581><U+2582><U+2587><U+2581><U+2582><U+2582><U+2581>
##    0.00086    0.0022    0.0058      0.045 <U+2587><U+2582><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
```
Somehow, `months_of_delay` is categorized as character, let's explore it:


```r
effects_filter %>% 
  filter(!is.na(months_of_delay)) %>%
  select(paper_id, months_of_delay) %>%
  mutate(months_numeric = as.numeric(months_of_delay)) %>%
  filter(is.na(months_numeric))
```

```
## Warning in evalq(as.numeric(months_of_delay), <environment>): NAs
## introduced by coercion
```

```
## # A tibble: 15 x 3
##    paper_id            months_of_delay months_numeric
##    <chr>               <chr>                    <dbl>
##  1 Chakravarty 2016 ii 9-11                        NA
##  2 Chakravarty 2016 ii 9-11                        NA
##  3 Chakravarty 2016 ii 9-11                        NA
##  4 Chakravarty 2016 ii 9-11                        NA
##  5 Chakravarty 2016 ii 9-11                        NA
##  6 Chakravarty 2016 ii 9-11                        NA
##  7 Chakravarty 2016 ii 9-11                        NA
##  8 Chakravarty 2016 ii 9-11                        NA
##  9 Chakravarty 2016 ii 9-11                        NA
## 10 Chakravarty 2016 ii 9-11                        NA
## 11 Chakravarty 2016 ii 9-11                        NA
## 12 Chakravarty 2016 ii 9-11                        NA
## 13 Chakravarty 2016 ii 9-11                        NA
## 14 Chakravarty 2016 ii 9-11                        NA
## 15 Chakravarty 2016 ii 9-11                        NA
```
The problem is how Chakravarty 2016 ii coded its months of delay. Let's change it:


```r
effects_filter <- effects_filter %>%
  mutate(months_of_delay = replace(months_of_delay, months_of_delay == "9-11", 10),
         months_of_delay = as.numeric(months_of_delay)) 

effects_filter$months_of_delay %>% skim()
```

```
## 
## Skim summary statistics
## 
## -- Variable type:numeric -------------------------------------------------------
##  variable missing complete   n  mean    sd p0 p25 p50 p75 p100     hist
##         .     104      778 882 28.79 25.39 -6  12  21  36  156 <U+2587><U+2586><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581>
```



#Merge datasets (two sheets)
We merge the two previous datasets by paper_id and evaluation_id:


```r
effects_evaluations <- effects_filter %>%
  left_join(evaluations_filter, by= c("paper_id","evaluation_id"))

skim(effects_evaluations)
```

```
## Skim summary statistics
##  n obs: 882 
##  n variables: 29 
## 
## -- Variable type:character -----------------------------------------------------
##       variable missing complete   n min max empty n_unique
##      age_group     658      224 882   3   7     0       28
##  evaluation_id       0      882 882   8  26     0       51
##            lac       0      882 882   2   3     0        2
##       paper_id       0      882 882   8  23     0       54
##     target_age     100      782 882   3   5     0       34
## 
## -- Variable type:factor --------------------------------------------------------
##          variable missing complete   n n_unique
##            gender       0      882 882        3
##       implementer       0      882 882        3
##           jovenes       0      882 882        3
##            skills       0      882 882        5
##          training       0      882 882        3
##       where_class       0      882 882        4
##  where_internship       0      882 882        3
##                            top_counts ordered
##   poo: 466, fem: 224, mal: 192, NA: 0   FALSE
##      Gov: 791, NGO: 82, Mul: 9, NA: 0   FALSE
##    amb: 415, men: 399, may: 68, NA: 0   FALSE
##  tec: 484, bot: 287, tec: 81, sof: 18   FALSE
##     cla: 715, bot: 166, on-: 1, NA: 0   FALSE
##   pri: 558, Pub: 192, pub: 131, NA: 1   FALSE
##           NA: 624, pri: 155, pri: 103   FALSE
## 
## -- Variable type:numeric -------------------------------------------------------
##                  variable missing complete   n      mean         sd
##                      beta       0      882 882 1115.43    5523.1   
##                 effect_id       0      882 882   19.55      18.62  
##        length_class_hours     324      558 882  579.58     401.27  
##       length_class_months     195      687 882    5.75       3.41  
##   length_internship_hours     819       63 882  250.92     115.99  
##  length_internship_months     624      258 882    3.79       3.1   
##           months_of_delay     104      778 882   28.79      25.39  
##                   n_total      12      870 882 7481.84   19520.58  
##                        nc      12      870 882 5076.17   16516.92  
##                        nt      12      870 882 2403.78    3538.95  
##              outcome_type       0      882 882    2.42       1.67  
##                   p_value     594      288 882    0.4        0.26  
##                        se     370      512 882  492.39    2381.71  
##             se_calculated       0      882 882  763.75    5770.96  
##       smd_treated_control     356      526 882    0.059      0.14  
##                         t     563      319 882    1.03       0.82  
##                        vd     356      526 882    0.0047     0.0066
##           p0       p25       p50       p75       p100     hist
##  -9836        -0.006      0.058    50.48    70861     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##      1         5         13        29          82     <U+2587><U+2583><U+2582><U+2582><U+2581><U+2581><U+2581><U+2581>
##     37.5     225        400      1000        1200     <U+2587><U+2586><U+2587><U+2583><U+2583><U+2581><U+2586><U+2587>
##      0.25      3          6         8          24     <U+2587><U+2585><U+2587><U+2581><U+2581><U+2581><U+2581><U+2581>
##     80       114        240       360         480     <U+2585><U+2581><U+2581><U+2587><U+2581><U+2583><U+2581><U+2582>
##      2         3          3         3          16.8   <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##     -6        12         21        36         156     <U+2587><U+2586><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581>
##    115       744.75    1941      4914.54   178636     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##     45.25    344        819.24   2068      150308     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##     38       352.25    1093      2948       29917     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##      1         1          2         3           8     <U+2587><U+2587><U+2582><U+2581><U+2581><U+2582><U+2581><U+2581>
##      0         0.1        0.5       0.5         0.98  <U+2585><U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581>
##      0         0.031      0.12     90.02    23355     <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##   -158.63      0.03       0.23    161.52    82027.73  <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
##     -0.6      -0.0056     0.05      0.12        0.77  <U+2581><U+2581><U+2581><U+2587><U+2583><U+2581><U+2581><U+2581>
##     -1.64      0.67       0.67      1.64        3.7   <U+2581><U+2581><U+2582><U+2587><U+2581><U+2582><U+2582><U+2581>
##      4.2e-05   0.00086    0.0022    0.0058      0.045 <U+2587><U+2582><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
```
Since `age_group` contains information about the age of the group exposed to the effect
("22-30"), or is `NA` (when the effects is for the whole sample), we extract that information
to create four new variables, age_min and age_max for the age of the sample for the effect and
age_min_target and age_max_target for the evaluation.


```r
effects_evaluations <- effects_evaluations %>%
  mutate(age_min = case_when(grepl("\\d\\d-\\d\\d",age_group) ~ substr(age_group,1,2),
                             grepl("\\d\\d-",age_group) ~ NA_character_,
                             grepl("\\d\\d+",age_group) ~ substr(age_group,1,2),
                             TRUE ~ NA_character_),
         age_max = case_when(grepl("\\d\\d-\\d\\d",age_group) ~ substr(age_group,4,5),
                             grepl("\\d\\d-",age_group) ~ substr(age_group,1,2),
                             grepl("\\d\\d+",age_group) ~ NA_character_,
                             TRUE ~ NA_character_),
         age_min_target = case_when(grepl("\\d\\d-\\d\\d",target_age) ~ substr(target_age,1,2),
                             grepl("\\d\\d-",target_age) ~ NA_character_,
                             grepl("\\d\\d+",target_age) ~ substr(target_age,1,2),
                             TRUE ~ NA_character_),
         age_max_target = case_when(grepl("\\d\\d-\\d\\d",target_age) ~ substr(target_age,4,5),
                             grepl("\\d\\d-",target_age) ~ substr(target_age,1,2),
                             grepl("\\d\\d+",target_age) ~ NA_character_,
                             TRUE ~ NA_character_)) %>%
  mutate_at(c("age_min_target", "age_max_target", "age_min", "age_max"), parse_number) %>%
  mutate(age_min = ifelse(is.na(age_group), age_min_target, age_min),
         age_max = ifelse(is.na(age_group), age_max_target, age_max))


effects_evaluations
```

```
## # A tibble: 882 x 33
##    paper_id evaluation_id effect_id gender age_group months_of_delay
##    <chr>    <chr>             <dbl> <fct>  <chr>               <dbl>
##  1 Acero 2~ Acero 2009            1 pooled <NA>                    1
##  2 Acero 2~ Acero 2009            2 pooled <NA>                    2
##  3 Acero 2~ Acero 2009            3 pooled <NA>                    3
##  4 Acero 2~ Acero 2009            4 pooled <NA>                    4
##  5 Acero 2~ Acero 2009            5 pooled <NA>                    1
##  6 Acero 2~ Acero 2009            6 pooled <NA>                    2
##  7 Acero 2~ Acero 2009            7 pooled <NA>                    3
##  8 Acero 2~ Acero 2009            8 pooled <NA>                    4
##  9 Acero 2~ Acero 2009            9 pooled <NA>                    1
## 10 Acero 2~ Acero 2009           10 pooled <NA>                    2
## # ... with 872 more rows, and 27 more variables: outcome_type <dbl>,
## #   n_total <dbl>, nt <dbl>, nc <dbl>, beta <dbl>, se <dbl>, t <dbl>,
## #   p_value <dbl>, se_calculated <dbl>, smd_treated_control <dbl>,
## #   vd <dbl>, target_age <chr>, skills <fct>, training <fct>,
## #   jovenes <fct>, implementer <fct>, where_internship <fct>,
## #   where_class <fct>, length_class_hours <dbl>,
## #   length_class_months <dbl>, length_internship_hours <dbl>,
## #   length_internship_months <dbl>, lac <chr>, age_min <dbl>,
## #   age_max <dbl>, age_min_target <dbl>, age_max_target <dbl>
```


Defining when an intervention is long or short depending on how many months in total there were


```r
effects_evaluations <- effects_evaluations %>%
   rowwise() %>% 
   mutate(length_internship_hours = if_else(is.na(length_internship_hours), 
                                      length_internship_months*80, 
                                      length_internship_hours),
         length_class_hours =if_else(is.na(length_class_hours), 
                                    length_class_months*80, 
                                    length_class_hours),
        length_intervention= sum(length_internship_hours, length_class_hours, na.rm = TRUE))


median_length_intervention <- median(effects_evaluations$length_intervention)
median_length_intervention
```

```
## [1] 480
```

```r
median_length_interventrion_old <- effects_evaluations %>%
  filter(jovenes!="menor a 25") %>%
  pull(length_intervention) %>%
  median()

effects_evaluations <- effects_evaluations %>%
   mutate(length_intervention2=if_else(length_intervention<=median_length_intervention, 
                                      "Less or equal than 480 hours", "More than 480 hours"),
          length_intervention3=if_else(length_intervention<=median_length_interventrion_old, 
                                      "Less or equal than 400 hours", "More than 400 hours"))
```

Now, we create a histogram of how these intervention hours are distributed:


```r
plot <- effects_evaluations %>%
  select(evaluation_id, length_intervention) %>%
  distinct() %>%
  ggplot(data = ., aes(length_intervention))+
  geom_histogram(binwidth = 200)+
  labs(title = "Distribution of intervention length", 
       x = "Hours", y = "Count")+
  theme_minimal()

plot 
```

![plot of chunk unnamed-chunk-7](data/unnamed-chunk-7-1.png)

```r
ggsave(here("plots","distribution_of_intervention_length.png"), plot = plot)
```

```
## Saving 7 x 7 in image
```

```r
effects_evaluations %>%
  select(evaluation_id,length_intervention2 ) %>%
  distinct() %>%
  count(length_intervention2)    
```

```
## Warning: Grouping rowwise data frame strips rowwise nature
```

```
## # A tibble: 2 x 2
##   length_intervention2             n
##   <chr>                        <int>
## 1 Less or equal than 480 hours    36
## 2 More than 480 hours             15
```

We modify categories, not including job-readiness since it is always part of 
technical o soft training.


```r
effects_evaluations <- effects_evaluations %>%
  mutate(skills= recode(skills,soft="Soft",technical="Technical",both="Both",
                        "technical, job-readniess"= "Technical",
                        "soft, job-readiness"= "Soft"),
         training=recode(training,both="Both",classroom="Classroom"),
         where_internship=recode(where_internship,"private-subsidized"="Private-subsidized",
                                 private="Private"),
         where_class=recode(where_class,public="Public",private="Private"),
         implementer=as.character(implementer),
        implementer=replace(implementer, implementer=="Multilateral","Mulilateral_NGO"),
        implementer=replace(implementer, implementer=="NGO","Mulilateral_NGO")) 
```

We perform a difference in means test to check whether there are statiscally significant
difference between men and women's sample sizes.

```r
t_test <- effects_evaluations %>% 
  filter(gender != "pooled" ) %>%
  t.test(formula = n_total ~ gender, data = .)

broom::tidy(t_test) %>%
  select(difference = estimate, female_mean_N = estimate1, 
         male_mean_N = estimate2, t = statistic,
         -parameter, conf.low:alternative) %>%
  write.xlsx(file = here("tables","t-test-gender.xlsx"))
```
Ordering for the forest plot subgroups

```r
effects_evaluations<-effects_evaluations %>%
  mutate(skills_order=recode(skills,Soft=1, Technical=2, Both=3),
         ) 
```


#Save file as RDS

Now, we save the file to be used in following steps


```r
saveRDS(effects_evaluations, file = here("data","cleanDatafile.rds"))
```



```r
effects_evaluations %>%
  filter(jovenes!="menor a 25",
         length_intervention == 480) %>%
  select(evaluation_id,starts_with("length")) %>%
  distinct()
```

```
## Source: local data frame [8 x 8]
## Groups: <by row>
## 
## # A tibble: 8 x 8
##   evaluation_id length_class_ho~ length_class_mo~ length_internsh~
##   <chr>                    <dbl>            <dbl>            <dbl>
## 1 Adoho 2014 i               480                6               NA
## 2 Attanasio 20~              240                3              240
## 3 Bendewald 20~              480                6               NA
## 4 Calero 2016                480                6               NA
## 5 Honorati 2015              240                3              240
## 6 Kluve 2008 i               480                6               NA
## 7 Kluve 2008 ii               NA               NA              480
## 8 Macours 2013               480                6               NA
## # ... with 4 more variables: length_internship_months <dbl>,
## #   length_intervention <dbl>, length_intervention2 <chr>,
## #   length_intervention3 <chr>
```

```r
effects_evaluations$outcome[effects_evaluations$outcome_type ==1 ] <- "Employment"
```

```
## Warning: Unknown or uninitialised column: 'outcome'.
```

```r
effects_evaluations$outcome[effects_evaluations$outcome_type ==3 ] <- "Formality"
effects_evaluations$outcome[effects_evaluations$outcome_type %in% c(2,8) ] <- "Wages all"
effects_evaluations$outcome[effects_evaluations$outcome_type %in% c(4,5,6,7) ] <- "Wages employed"

effects_evaluations%>%
  count(outcome, outcome_type)
```

```
## Warning: Grouping rowwise data frame strips rowwise nature
```

```
## # A tibble: 8 x 3
##   outcome        outcome_type     n
##   <chr>                 <dbl> <int>
## 1 Employment                1   295
## 2 Formality                 3    61
## 3 Wages all                 2   350
## 4 Wages all                 8     4
## 5 Wages employed            4    34
## 6 Wages employed            5    49
## 7 Wages employed            6    76
## 8 Wages employed            7    13
```

```r
effects_evaluations%>%
  filter(jovenes!="menor a 25") %>%
  count(outcome)
```

```
## Warning: Grouping rowwise data frame strips rowwise nature
```

```
## # A tibble: 4 x 2
##   outcome            n
##   <chr>          <int>
## 1 Employment       153
## 2 Formality         34
## 3 Wages all        192
## 4 Wages employed   104
```

```r
effects_evaluations%>%
  filter(jovenes!="menor a 25") %>%
  count()
```

```
## Warning: Grouping rowwise data frame strips rowwise nature
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1   483
```

```r
hist(effects_evaluations$age_max_target)
```

![plot of chunk unnamed-chunk-12](data/unnamed-chunk-12-1.png)

```r
skim(effects_evaluations$age_max_target)
```

```
## 
## Skim summary statistics
## 
## -- Variable type:numeric -------------------------------------------------------
##                            variable missing complete   n  mean    sd p0
##  effects_evaluations$age_max_target     228      654 882 30.22 11.35 18
##  p25 p50 p75 p100     hist
##   24  25  29   65 <U+2582><U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
```

