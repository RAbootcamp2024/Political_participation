# load data
cleaned_df <- read_rds("State_Level_Dataset.rds")

# packages for estimation
library(tidyverse)
library(fixest)

model <- fixest::feols(lne031 ~ i(rel_year, treated_state, ref=c(-1, -1000))
                       | state + year, data = cleaned_df, cluster="state")
#lne031 = "Higher Education Exp."(log)

summary(model)
coefplot(model)

filtered_cleaned_df <- cleaned_df %>% 
  filter(rel_year == -1000 | (rel_year >= -10 & rel_year <= 4))

filtered_model <- fixest::feols(lne031 ~ i(rel_year, treated_state, ref=c(-1, -1000))
                       | state + year, data = filtered_cleaned_df, cluster="state")
summary(filtered_model)
coefplot(filtered_model)
