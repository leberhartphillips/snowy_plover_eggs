# Model age-dependent variation in lay date

# Product: stats and figure of the lay date ~ senescence model

#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")

load("data/ceuta_egg_chick_female_data.rds")

#### Data wrangle ----
# subset to nest level and first nest attempts of the season for each female
first_nests_age_data <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, first_laydate, est_age_t_deviation, year,
                first_age_t, last_age_t, n_years_obs, avg_ad_tarsi,
                age_first_cap) %>% 
  # dplyr::filter(nest_order == 1) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_t_deviation)) %>% 
  mutate(age_first_cap_dummy = ifelse(age_first_cap == "J", 1, 0))

first_nests_age_data %>% 
  summarise(n_ind = n_distinct(ring),
            n_nests = n_distinct(ID))

#### Modeling ----
# Procedure:
# mixed effects regression of laydate ~ senescence with mother ID as random
# effects
mod_laydate_I <-
  lmer(first_laydate ~ est_age_t_deviation + I(est_age_t_deviation^2) + 
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006"))

# run tidy bootstrap to obtain model diagnostics
tidy_mod_laydate <-
  tidy(mod_laydate_I, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run rptR to obtain repeatabilities of random effects
rpt_mod_laydate <-
  rpt(first_laydate ~ est_age_t_deviation + I(est_age_t_deviation^2) + 
        first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
        (1|ring) + (1|year),
      grname = c("ring", "year", "Fixed"),
      data = filter(first_nests_age_data, year != "2006"),
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 to obtain marginal R2, parameter estimates, and beta weights
mod_laydate_poly <-
  lmer(first_laydate ~ poly(est_age_t_deviation, 2) + 
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap_dummy +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006"))

R2m_mod_laydate <-
  partR2(mod_laydate_poly,
         partvars = c("poly(est_age_t_deviation, 2)",
                      "first_age_t",
                      "last_age_t",
                      "avg_ad_tarsi",
                      "age_first_cap_dummy"),
         R2_type = "marginal",
         nboot = 1000, CI = 0.95, max_level = 1)

R2c_mod_laydate <-
  partR2(mod_laydate_poly,
         partvars = c("poly(est_age_t_deviation, 2)",
                      "first_age_t",
                      "last_age_t",
                      "avg_ad_tarsi",
                      "age_first_cap_dummy"),
         R2_type = "conditional",
         nboot = 1000, CI = 0.95, max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_laydate_mod <-
  list(mod_I = mod_laydate_I,
       mod_poly = mod_laydate_poly,
       tidy = tidy_mod_laydate,
       rptR = rpt_mod_laydate,
       partR2m = R2m_mod_laydate,
       partR2c = R2c_mod_laydate)
# 
# save(stats_laydate_mod,
#      file = "output/stats_laydate_mod.rds")

load(file = "output/stats_laydate_mod.rds")

model_parameters(stats_laydate_mod$mod_I, standardize = "refit")
plot(allEffects(stats_laydate_mod$mod_I))
random_parameters(stats_laydate_mod$mod_I)