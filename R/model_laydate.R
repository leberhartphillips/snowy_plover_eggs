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
  dplyr::select(ring, ID, jul_lay_date_std_num, est_age_trans, year,
                firstage, lastage, nest_order, n_years_obs, avg_ad_tarsi) %>% 
  dplyr::filter(nest_order == 1) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_trans))

first_nests_age_data %>% 
  summarise(n_ind = n_distinct(ring),
            n_nests = n_distinct(ID))

#### Modeling ----
# Procedure:
# mixed effects regression of laydate ~ senescence with mother ID as random
# effects
mod_date_age_tarsi <-
  lmer(jul_lay_date_std_num ~ est_age_trans + I(est_age_trans^2) + firstage + lastage + avg_ad_tarsi +
         (1|ring) + (1|year),
       data = first_nests_age_data)

# run tidy bootstrap to obtain model diagnostics
tidy_date_age_tarsi <-
  tidy(mod_date_age_tarsi, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run rptR to obtain repeatabilities of random effects
rpt_date_age_tarsi <-
  rpt(jul_lay_date_std_num ~ est_age_trans + I(est_age_trans^2) + firstage + lastage + avg_ad_tarsi +
        (1|ring) + (1|year),
      grname = c("ring", "year", "Fixed"),
      data = first_nests_age_data,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 to obtain marginal R2, parameter estimates, and beta weights
R2m_date_age_tarsi <-
  partR2(mod_date_age_tarsi,
         partvars = c("est_age_trans",
                      "I(est_age_trans^2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000, CI = 0.95, max_level = 1)

R2c_date_age_tarsi <-
  partR2(mod_date_age_tarsi,
         partvars = c("est_age_trans",
                      "I(est_age_trans^2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi"),
         R2_type = "conditional",
         nboot = 1000, CI = 0.95, max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_date_age_tarsi <-
  list(mod = mod_date_age_tarsi,
       tidy = tidy_date_age_tarsi,
       rptR = rpt_date_age_tarsi,
       partR2m = R2m_date_age_tarsi,
       partR2c = R2c_date_age_tarsi)

save(stats_date_age_tarsi,
     file = "output/stats_date_age_tarsi_.rds")

load(file = "output/stats_date_age_tarsi.rds")

stats_date_age_tarsi$tidy


#### Quick model diagnostics ----
plot(allEffects(stats_date_age_tarsi$mod))
plot(allEffects(mod_date_age_tarsi))

coefplot2(stats_date_age_tarsi$mod)
coefplot2(mod_date_age_tarsi)


summary(glht(stats_date_age_tarsi$mod))
summary(glht(mod_date_age_tarsi))

summary(stats_date_age_tarsi$mod)
summary(mod_date_age_tarsi)
