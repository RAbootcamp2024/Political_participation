# load data
cleaned_df <- read_rds("State_Level_Dataset.rds")

# packages for estimation
library(tidyverse)
library(fixest)

# column 1
model_eve <- feols(lne031 ~ i(rel_year, treated_state, ref=c(-1, -1000)) + lnp_income
                   | state + year, data = cleaned_df, cluster="state")
# lne031 = "Higher Education Exp."(log)
summary(model_eve)
coefplot(model_eve, drop = "!rel_year::(-10|-[1-9]|[0-4]):treated_state")

# column 2 CHECK!
model_ief <- feols(lne031 ~ i(rel_year, treated_state, ref=c(-1, -1000)) + lnp_income + i(state, year)
                   | state + year, data = cleaned_df, cluster="state")

summary(model_ief)
coefplot(model_ief)

# column 3 CHECK!
model_ovr <- feols(lne031 ~ i(rel_year, treated_state, ref=c(-1, -1000)) + lnp_income +i(state, year) + same + online + nrva 
                   | state + year, data = cleaned_df, cluster="state")
# lne031 = "Higher Education Exp."(log)
summary(model_ovr)
coefplot(model_ovr, drop = "!rel_year::(-10|-[1-9]|[0-4]):treated_state")

# column 4 CHECK!
model_cv <- feols(lne031 ~ i(rel_year, treated_state, ref=c(-1, -1000)) + lnp_income +i(state, year) + same + online + nrva + 
                     med_age + sh16_25 + lnenrollment + blk_pop_sh + wht_pop_sh + gini + lnpopulation + unemp
                   + i(gub_years) + democ + incumbent + gov_run_again + past_gov_dem + rel_margin + turnout_rate + term_limited
                 + lne001 + current_exp + lnr05
                   | state + year, data = cleaned_df, cluster="state")
summary(model_cv)

# column 5
model_att <- feols(lne031 ~ pre_reg_s + lnp_income +i(state, year) + same + online + nrva + 
                     med_age + sh16_25 + lnenrollment + blk_pop_sh + wht_pop_sh + gini + lnpopulation + unemp
                   + i(gub_years) + democ + incumbent + gov_run_again + past_gov_dem + rel_margin + turnout_rate + term_limited
                   + lne001 + current_exp + lnr05
                   | state + year, data = cleaned_df, cluster="state")
summary(model_att)

table_2 <- etable(model_eve, model_ief, model_ovr, model_cv, model_att,
                  cluster="state", drop= "!(treated_state x rel_year = [0-4]$|pre_reg_s)")
table_2
