# Model the relationship between liklihood of polyandry and lay date of first 
# breeding attempt

# Product: stats and figure of the polyandry ~ lay date model

#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")

ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")

#### Modeling ----
# Modeling the relationship between renesting and initiation date
# of first breeding attempt

# wrangle data
renesting_data <-
  ceuta_egg_chick_female_data %>% 
  # 1) subset to only cases in which the first nest failed
  dplyr::filter(nest_order == 1) %>% 
  select(ID, ring, year, nest_1_fate, first_laydate, n_mates, n_nests, polyandry, multiclutch) %>% 
  distinct() %>% 
  # 2) subset to nests that are confirmed failed
  filter(nest_1_fate != "Hatch" & nest_1_fate != "Unknown") %>% 
  mutate(multiclutch = as.factor(multiclutch)) %>%
  mutate(multi = ifelse(multiclutch == "multi", 1, 0),
         single = ifelse(multiclutch == "single", 1, 0)) %>%
  mutate(multi_plot = ifelse(multi == 1, multi + 0.1, multi - 0.1))

# sample sizes
renesting_data %>% 
  summarise(Years = n_distinct(year),  # N = 14 years
            Individuals = n_distinct(ring),    # N = 430 females
            Nests = n_distinct(ID),    # N = 850 nests
            Eggs = nrow(.))  # N = 2451 eggs

# number of re-nesting attempts that were polyandrous vs. monogamous
renesting_data %>% 
  group_by(polyandry) %>% 
  summarise(n = n())

renesting_data %>% 
  mutate(multinest = ifelse(n_nests > 1, "multi", "single")) %>% 
  group_by(polyandry, multinest) %>% 
  summarise(n_cases = n()) %>% 
  group_by(polyandry) %>% 
  mutate(prop_cases = n_cases/sum(n_cases))

renesting_data %>% 
  summarise(max_date = max(first_laydate, na.rm = TRUE),
            min_date = min(first_laydate, na.rm = TRUE))

# Procedure:
# binomial mixed effects regression of multiclutch ~ lay date with mother ID and
# year as random effects
mod_renesting <-
  glmer(cbind(multi, single) ~ first_laydate +
          (1|ring) + (1|year),
        data = renesting_data, family = "binomial")

model_parameters(mod_renesting, standardize = "refit")
random_parameters(mod_renesting)
plot(allEffects(mod_renesting))

# run tidy bootstrap to obtain model diagnostics
tidy_renesting <-
  tidy(mod_renesting, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run rptR to obtain repeatabilities of random effects
rpt_renesting <-
  rpt(multi ~ first_laydate +
        (1|ring) + (1|year),
      grname = c("ring", "year", "Fixed"),
      data = renesting_data,
      datatype = "Binary",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
set.seed(12345)
mod_renesting_opt <-
  glmer(cbind(multi, single) ~ first_laydate +
          (1|ring) + (1|year),
        data = renesting_data, family = "binomial",
        control = glmerControl(optimizer = "bobyqa",
                               optCtrl = list(maxfun = 2e5)))
R2m_renesting <-
  partR2(mod_renesting_opt,
         partvars = c("first_laydate"),
         R2_type = "marginal",
         nboot = 1000, CI = 0.95, max_level = 1)

R2c_renesting <-
  partR2(mod_renesting,
         partvars = c("first_laydate"),
         R2_type = "conditional",
         nboot = 1000, CI = 0.95, max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_renesting_mod <-
  list(mod = mod_renesting,
       tidy = tidy_renesting,
       rptR = rpt_renesting,
       partR2m = R2m_renesting,
       partR2c = R2c_renesting)

save(stats_renesting_mod,
     file = "output/Stats_renesting_mod.rds")

# load the saved results
load("output/stats_renesting_mod.rds")