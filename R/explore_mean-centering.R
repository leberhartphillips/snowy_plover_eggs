install.packages("parameters")
install.packages("see")
library(parameters)
library(tidyverse)
library(lme4)
library(see)
data("qol_cancer")
load("data/ceuta_egg_chick_female_data.rds")

describe_distribution(dplyr::select(ceuta_egg_chick_female_data, est_age_trans, volume_cm, width_cm, length_cm, jul_lay_date_std_num))

check_heterogeneity(qol_cancer, select = c("phq4", "education"), group = "ID")

check_heterogeneity(ceuta_egg_chick_female_data_mc, select = c("est_age_trans"), group = "ring")

qol_cancer <- cbind(
  qol_cancer,
  demean(qol_cancer, select = c("phq4", "QoL"), group = "ID")
)

ceuta_egg_chick_female_data_mc <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, est_age_trans) %>%
  distinct() %>% 
  bind_cols(., demean(., select = c("est_age_trans"), group = "ring")) %>% 
  left_join(ceuta_egg_chick_female_data, ., by = c("ring", "est_age_trans")) %>% 
  dplyr::filter(n_years_obs > 2)

mixed_1 <- lmer(
  volume_cm ~ est_age_trans_within + est_age_trans_between + poly(jul_lay_date_std_num, 2) +
    (1 | ID) + (1 | ring) + (1 | year),
  data = ceuta_egg_chick_female_data_mc
)

model_parameters(mixed_1)
random_parameters(mixed_1)

ggplot(ceuta_egg_chick_female_data_mc, aes(est_age_trans, volume_cm)) +
  geom_smooth(mapping = aes(colour = ring), 
              method = "lm", se = FALSE, formula = y ~ poly(x, 2)) +
  geom_point(mapping = aes(colour = ring), size = 2.2, alpha = .6) +
  geom_smooth(mapping = aes(x = est_age_trans_between, y = volume_cm), 
              method = "lm", se = TRUE, formula = y ~ poly(x, 2),
              colour = "#444444") +
  see::scale_color_flat() +
  see::theme_modern() +
  theme(legend.position = "none") +
  labs(x = "Estimated age (years)", y = "Egg volume", colour = "Individual")