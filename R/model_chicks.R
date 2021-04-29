# Model the relationship between chick hatch weight and egg volume to confirm 
# that egg volume is a relevant fitness measure

#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")

load("data/ceuta_egg_chick_female_data.rds")

#### Data wrangle of nest summary ----
# summarize egg morphometric data by nest
eggs_and_chicks_nest_summary <- 
  ceuta_egg_chick_female_data %>% 
  group_by(ID, ring, year, jul_lay_date_std_num, 
           avg_chick_tarsus, sd_chick_tarsus, avg_chick_bill, 
           sd_chick_bill, avg_chick_weight, sd_chick_weight, avg_chick_BMI, 
           sd_chick_BMI) %>% 
  summarise(avg_egg_length = mean(length_cm, na.rm = TRUE),
            sd_egg_length = sd(length_cm, na.rm = TRUE),
            avg_egg_width = mean(width_cm, na.rm = TRUE),
            sd_egg_width = sd(width_cm, na.rm = TRUE),
            avg_egg_volume = mean(volume_cm, na.rm = TRUE),
            sd_egg_volume = sd(volume_cm, na.rm = TRUE),
  ) %>% 
  rename(mother_ring = ring)

#### Model relationship between egg volume and chick weight ----
# Procedure:
# linear mixed effects regression of chick weight ~ egg volume with mother ID and
# year as random effects 
mod_chickw <-
  lmer(avg_chick_weight ~ avg_egg_volume +
         (1|mother_ring) + (1|year),
       data = dplyr::filter(eggs_and_chicks_nest_summary,
                            !is.na(avg_chick_weight)))

# run tidy bootstrap to obtain model diagnostics
tidy_mod_chickw <-
  tidy(mod_chickw, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run rptR to obtain repeatabilities of random effects
rpt_mod_chickw <-
  rpt(avg_chick_weight ~ avg_egg_volume +
        (1|mother_ring) + (1|year),
      grname = c("mother_ring", "year", "Fixed"),
      data = dplyr::filter(eggs_and_chicks_nest_summary, !is.na(avg_chick_weight)),
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = FALSE, ncores = 4, parallel = TRUE)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
R2m_mod_chickw <-
  partR2(mod_chickw,
         partvars = c("avg_egg_volume"),
         R2_type = "marginal", nboot = 1000, CI = 0.95, max_level = 1)

R2c_mod_chickw <-
  partR2(mod_chickw,
         partvars = c("avg_egg_volume"),
         R2_type = "conditional", nboot = 1000, CI = 0.95, max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_chick_mod <-
  list(mod = mod_chickw,
       tidy = tidy_mod_chickw,
       rptR = rpt_mod_chickw,
       partR2m = R2m_mod_chickw,
       partR2c = R2c_mod_chickw)

# save(stats_chick_mod,
#      file = "output/stats_chick_mod.rds")

load("output/stats_chick_mod.rds")

model_parameters(stats_chick_mod$mod, standardize = "refit")
random_parameters(stats_chick_mod$mod)
plot(allEffects(stats_chick_mod$mod))
