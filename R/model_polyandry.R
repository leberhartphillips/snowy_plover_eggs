# Model the relationship between liklihood of polyandry and lay date of first 
# breeding attempt

# Product: stats and figure of the polyandry ~ lay date model

#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")

load("data/ceuta_egg_chick_female_data.rds")

#### Modeling ----
# Modeling the relationship between mating behavior and initiation date
# of first breeding attempt

# wrangle data to include only first nests
first_nests_data <-
  ceuta_egg_chick_female_data %>%
  dplyr::filter(nest_order == 1) %>% 
  dplyr::select(polyandry, year, ring, first_laydate, n_nests, ID) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  mutate(poly = ifelse(polyandry == "poly", 1, 0),
         mono = ifelse(polyandry == "mono", 1, 0)) %>%
  mutate(poly_plot = ifelse(poly == 1, poly + 0.1, poly - 0.1))

# sample size summary
first_nests_data %>% 
  summarise(n_inds = n_distinct(ring),
            n_nests = n_distinct(ID))

first_nests_data %>% 
  mutate(multinest = ifelse(n_nests > 1, "multi", "single")) %>% 
  group_by(polyandry, multinest) %>% 
  summarise(n_cases = n()) %>% 
  group_by(polyandry) %>% 
  mutate(prop_cases = n_cases/sum(n_cases))

# Procedure:
# binomial mixed effects regression of polyandry ~ lay date with mother ID and
# year as random effects
mod_polyandry <-
  glmer(cbind(poly, mono) ~ first_laydate +
          (1|ring) + (1|year),
        data = first_nests_data, family = "binomial")

model_parameters(mod_polyandry)
random_parameters(mod_polyandry)

# run tidy bootstrap to obtain model diagnostics
tidy_polyandry <-
  tidy(mod_polyandry, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run rptR to obtain repeatabilities of random effects
rpt_polyandry <-
  rpt(poly ~ first_laydate +
        (1|ring) + (1|year),
      grname = c("ring", "year", "Fixed"),
      data = first_nests_data,
      datatype = "Binary",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
R2m_polyandry <-
  partR2(mod_polyandry,
         partvars = c("first_laydate"),
         R2_type = "marginal",
         nboot = 1000, CI = 0.95, max_level = 1)

R2c_polyandry <-
  partR2(mod_polyandry,
         partvars = c("first_laydate"),
         R2_type = "conditional",
         nboot = 1000, CI = 0.95)

# save model, tidy, rptR, and partR2 output as a list
stats_polyandry_mod <-
  list(mod = mod_polyandry,
       tidy = tidy_polyandry,
       rptR = stats_poly_date$rptR,
       partR2m = R2m_polyandry,
       partR2c = R2c_polyandry)

save(stats_polyandry_mod,
     file = "output/stats_polyandry_mod.rds")

# load the saved results
load("output/stats_polyandry_mod.rds")

model_parameters(stats_polyandry_mod$mod, standardize = "refit")
random_parameters(stats_polyandry_mod$mod)
plot(allEffects(stats_polyandry_mod$mod))