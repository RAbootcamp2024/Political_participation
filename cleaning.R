# install.packages("haven")
# packages
library(haven)
library(tidyverse)

# loading 
State_Level_Dataset <- read_dta("112082-V1/Replication-data/Political-Responsiveness--State-Level-/Generate-Dataset/State_Level_Dataset.dta")

head(State_Level_Dataset)

# PREREGISTRATION 
## treatment
State_Level_Dataset <- State_Level_Dataset %>%
  mutate(pre_reg_s = case_when(
           state=="California" & year>= 2009 ~ 1,
           state=="Colorado" & year>=2013 ~ 1,
           state=="Delaware" & year>=2010 ~ 1,
           state=="Florida" & year>=2007 ~ 1,
           state=="Louisiana" & year>=2014~ 1,
           state=="Maine" & year>=2011 ~ 1,
           state=="Maryland" & year>=2010 ~ 1,
           state=="Massachusetts" & year>=2014 ~ 1,
           state=="North Carolina" & year>=2009 & year<=2013 ~ 1,
           state=="Oregon" & year>=2007 ~ 1,
           state=="Rhode Island" & year>=2010 ~ 1,
           state=="Utah" & year>=2015 ~ 1,
           state=="Hawaii" & year>=1993 ~ 1,
           state=="New Jersey" & year>=2015 ~ 1,
           TRUE ~ 0))

# other reforms
## EDR
State_Level_Dataset <-  State_Level_Dataset %>% 
  mutate(same = case_when(
    state == "California" & year>=2012 ~ 1,
    state=="Colorado" & year>=2013 ~ 1,
    state=="Connecticut" & year>=2013 ~ 1,
    state=="Idaho" & year>=1994 ~ 1,
    state=="Illinois" & year>=2014 ~ 1,
    state=="Iowa" & year>=2007 ~ 1,
    state=="Maine" & year>=1973 ~ 1,
    state=="Maryland" & year>=2013 ~ 1,
    state=="Minnesota" & year>=1974 ~ 1,
    state=="Montana" & year>=2005 ~ 1,
    state=="New Hampshire" & year>=1996 ~ 1,
    state=="Wisconsin" & year>=1975 ~ 1,
    state=="Wyoming" & year>=1994 ~ 1,
    TRUE ~ 0)) 
# %>% select(year, state, same) %>% view()

## OR
State_Level_Dataset <- State_Level_Dataset %>% 
  mutate(online = case_when(
    state=="Arizona" & year>=2002 ~ 1,
    state=="California" & year>=2012 ~ 1,
    state=="Colorado" & year>=2010 ~ 1,
    state=="Connecticut" & year>=2014 ~ 1,
    state=="Delaware" & year>=2006 ~ 1,
    state=="Georgia" & year>=2014 ~ 1,
    state=="Illinois" & year>=2014 ~ 1,
    state=="Indiana" & year>=2010 ~ 1,
    state=="Kansas" & year>=2009 ~ 1,
    state=="Louisiana" & year>=2010 ~ 1,
    state=="Maryland" & year>=2012 ~ 1,
    state=="Minnesota" & year>=2013 ~ 1,
    state=="Missouri" & year>=2013 ~ 1,
    state=="Nevada" & year>=2012 ~ 1,
    state=="New York" & year>=2012 ~ 1,
    state=="Oregon" & year>=2010 ~ 1,
    state=="South Carolina" & year>=2012 ~ 1,
    state=="Utah" & year>=2010 ~ 1,
    state=="Virginia" & year>=2013 ~ 1,
    state=="Washington" & year>=2008 ~ 1,
    TRUE ~ 0)) 


# taking logs
State_Level_Dataset <- State_Level_Dataset %>%
  mutate(across(c(e019, e021, e030, e031, e041, e052, 
                  e080, e084, e134, e137, e001, r01, 
                  r04, r05, population, p_income, enrollment),
                ~ log(1 + .),
                .names = "ln{.col}"))

State_Level_Dataset <- State_Level_Dataset %>%
    mutate(current_exp = log(1 + (e004/e001)),
           enrolment1=enrollment/1000,
           population1=population/1000,
           rel_margin=vote_margin_correct/tot_vote,
           turnout_rate=turnout/VEP1,
           term_limited=case_when(limit_exists+years_left_before_limit==0 ~11,
                                  limit_exists+years_left_before_limit>4 ~0,
                                  TRUE ~1),
           lnconstr=log(1+e049+e074+e065),
           lnpub_wel=log(1+e090+e009),
           lnele1=log(0.01+ elementary1/population))

# first year
State_Level_Dataset <- State_Level_Dataset %>%
  mutate(pre_reg = lag(pre_reg_s) == 0 & pre_reg_s == 1) 

# take leads and lags
State_Level_Dataset <- State_Level_Dataset %>%
  mutate(F0_pre = lead(pre_reg, n = 0),
         F1_pre = lead(pre_reg, n = 1),
         F2_pre = lead(pre_reg, n = 2),
         F3_pre = lead(pre_reg, n = 3),
         F4_pre = lead(pre_reg, n = 4),
         F5_pre = lead(pre_reg, n = 5),
         F6_pre = lead(pre_reg, n = 6),
         F7_pre = lead(pre_reg, n = 7),
         F8_pre = lead(pre_reg, n = 8),
         F9_pre = lead(pre_reg, n = 9),
         F10_pre = lead(pre_reg, n = 10)) 

#State_Level_Dataset %>%
#  mutate(across(pre_reg,
#                .fns = list,
#                .names = "pre_reg_{.col}_{.row}"),
#         pre_reg = map(1:10, ~lead(pre_reg, .x))) %>% view()

#Otsukaresamadesita!!! Konotyouside tanosiku ikimasyou!!!

State_Level_Dataset <- State_Level_Dataset %>% 
  mutate(L1_pre = lag(pre_reg, n = 1),
         L2_pre = lag(pre_reg, n = 2),
         L3_pre = lag(pre_reg, n = 3),
         L4_pre = lag(pre_reg, n = 4),
         L5_pre = lag(pre_reg, n = 5),
         L6_pre = lag(pre_reg, n = 6))

State_Level_Dataset <- State_Level_Dataset %>% 
  group_by(state) %>% 
  mutate(treated_state = max(pre_reg)) %>% 
  ungroup()

# may be < instead of <=
State_Level_Dataset <- State_Level_Dataset %>% 
  mutate(F10_last_year= case_when(pre_reg == 1 ~ (year - 10),
                                  TRUE ~ NA) ) 

State_Level_Dataset <- State_Level_Dataset %>%
  group_by(state) %>% 
  mutate(max_F10_last_year = max(F10_last_year, na.rm = TRUE)) %>% 
  mutate(F10_last_pre = year <= max_F10_last_year & treated_state == 1) 

State_Level_Dataset <- State_Level_Dataset %>% 
  mutate(F5_last_year= case_when(pre_reg == 1 ~ (year - 5),
                                  TRUE ~ NA) ) 

State_Level_Dataset <- State_Level_Dataset %>%
  group_by(state) %>% 
  mutate(max_F5_last_year = max(F5_last_year, na.rm = TRUE)) %>% 
  mutate(F5_last_pre = year <= max_F5_last_year & treated_state == 1) 

State_Level_Dataset <- State_Level_Dataset %>% 
  mutate(L4_last_year= case_when(pre_reg == 1 ~ (year + 4),
                                 TRUE ~ NA) ) 

State_Level_Dataset <- State_Level_Dataset %>%
  group_by(state) %>% 
  mutate(max_L4_last_year = max(L4_last_year, na.rm = TRUE)) %>% 
  mutate(L4_last_pre = (year >= max_L4_last_year & treated_state == 1) |
           (state=="North Carolina" & year>2013) ) 

State_Level_Dataset <-  State_Level_Dataset %>%
  group_by(state) %>% 
  mutate(treated_year = case_when(
    pre_reg == 1 ~ year,
    TRUE ~ NA
  )) %>%
  mutate(treated_year = max(treated_year, na.rm = TRUE)) 
  
State_Level_Dataset <-  State_Level_Dataset %>% 
  mutate(rel_year = case_when(treated_state == 1 ~ (year - treated_year),
                              TRUE ~ -1000)) 
# revise NC
State_Level_Dataset <-  State_Level_Dataset %>% 
  mutate(rel_year = case_when(state == "North Carolina" & year == 2014 ~ -1000,
                              TRUE ~ rel_year)) 

# save cleaned data

saveRDS(State_Level_Dataset, "State_Level_Dataset.rds")