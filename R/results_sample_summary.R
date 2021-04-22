#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

load("data/ceuta_egg_chick_female_data.rds")
load("data/raw_encounter_histories_females_2006_2020.rds")
load("output/BaSTA_age_estimates_2006-2020.rds")

# extract the first and last observations of each individual
ceuta_egg_chick_female_data_3_year <-  
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(n_years_obs > 2)

max_min_obs <-
  encounter_histories %>% 
  dplyr::filter(ring %in% ceuta_egg_chick_female_data_3_year$ring) %>% 
  left_join(., dplyr::select(BaSTA_ages, ring, est_b), by = "ring") %>%
  mutate(est_age = as.numeric(year) - as.numeric(est_b)) %>% 
  group_by(ring) %>% 
  summarise(min_age = min(est_age),
            max_age = max(est_age),
            obs_span = max(est_age) - min(est_age)) %>% 
  as.data.frame() %>% 
  left_join(., dplyr::select(ceuta_egg_chick_female_data_3_year, ring, age_first_cap)) %>% 
  distinct()

# manually adjust the ages of CA1526 and CA2036 to reflect their first encounter
# during the 2004 pilot study as chicks (i.e., aged 0)
max_min_obs[which(max_min_obs$ring %in% c("CA1526", "CA2036")), "min_age"] <-
  c(0, 0)

# manually adjust the min age estimate of CA1579 to relect that it was first
# encountered as an adult during the 2004 pilot study (i.e., subtract 2 years
# from BaSTA estimate of age at first encounter)
max_min_obs[which(max_min_obs$ring == "CA1579"), "min_age"] <-
  max_min_obs[which(max_min_obs$ring == "CA1579"), "min_age"] - 2
max_min_obs[which(max_min_obs$ring == "CA1579"), "obs_span"] <-
  max_min_obs[which(max_min_obs$ring == "CA1579"), "max_age"] -
  max_min_obs[which(max_min_obs$ring == "CA1579"), "min_age"]

# specify the factor levels according to the rank (for plotting)
max_min_obs$ring_ordered <- 
  factor(max_min_obs$ring, 
         levels = unique(max_min_obs$ring[order(max_min_obs$min_age, 
                                                max_min_obs$obs_span, 
                                                decreasing = TRUE)]), 
         ordered = TRUE)

# calculate the number of clutches per year for each individual and join back to 
# ranked dataframe
eggdf_clutches <- 
  ceuta_egg_chick_female_data_3_year %>% 
  dplyr::group_by(ring, year) %>%
  dplyr::summarise(n_clutches = n_distinct(ID)) %>% 
  dplyr::right_join(dplyr::select(max_min_obs, 
                                  c(ring, min_age, obs_span, age_first_cap)), 
                    by = "ring") %>% 
  dplyr::right_join(dplyr::select(ceuta_egg_chick_female_data_3_year, c(ring, year, est_age, polyandry)), 
                    by = c("ring", "year")) %>% 
  distinct() %>% 
  mutate(polyandry = ifelse(polyandry == "poly", "2", "1"))

# specify the factor levels according to the rank (for plotting)
eggdf_clutches$ring_ordered <- 
  factor(eggdf_clutches$ring, 
         levels = unique(eggdf_clutches$ring[order(eggdf_clutches$min_age, 
                                                   eggdf_clutches$obs_span, 
                                                   decreasing = TRUE)]), 
         ordered = TRUE)

# create a ranking variable and confidence interval limits of age estimate
eggdf_n_upper <-
  ceuta_egg_chick_female_data_3_year %>%
  dplyr::group_by(ring) %>%
  dplyr::summarise(CI_age = max(est_death_age))


eggdf_age_CI <-
  ceuta_egg_chick_female_data_3_year %>%
  dplyr::group_by(ring) %>%
  dplyr::summarise(CI_age = 1) %>%
  dplyr::bind_rows(eggdf_n_upper) %>%
  mutate(ring = as.factor(ring)) %>% 
  arrange(ring) %>% 
  dplyr::left_join(., dplyr::select(max_min_obs, 
                                    c(ring, min_age, obs_span, age_first_cap)))

# specify the factor levels according to the rank (for plotting)
eggdf_age_CI$ring_ordered <- 
  factor(eggdf_age_CI$ring, 
         levels = unique(eggdf_age_CI$ring[order(eggdf_age_CI$min_age, 
                                                 eggdf_age_CI$obs_span,
                                                 decreasing = TRUE)]), 
         ordered = TRUE)

# # join polyandry information
# eggdf_mating_sys_all <-
#   dbReadTable(Ceuta_OPEN, "BirdRef") %>% 
#   dplyr::select(year, ID, male, female) %>%
#   filter(female %in% eggdf$ring) %>% 
#   arrange(female) %>%
#   group_by(female, year) %>%
#   summarise(n_mates = n_distinct(male, na.rm = TRUE)) %>% 
#   mutate(polyandry = ifelse(n_mates > 1, "poly", "mono"),
#          year = as.integer(year),
#          n_mates = ifelse(n_mates == 0, 1, n_mates)) %>% 
#   rename(ring = female)
# 
# matings_without_egg_obs <- 
#   anti_join(eggdf_mating_sys_all, eggdf, 
#             by = c("ring", "year")) %>% 
#   left_join(., dplyr::select(eggdf, ring, age_first_cap, est_b), by = "ring") %>% 
#   mutate(est_age = year - est_b) %>% 
#   distinct()

# # specify the factor levels according to the rank (for plotting)
# matings_without_egg_obs_plot <- 
#   matings_without_egg_obs %>% 
#   dplyr::filter(ring %in% ceuta_egg_chick_female_data_3_year$ring)
# 
# matings_without_egg_obs_plot$ring_ordered <- 
#   factor(matings_without_egg_obs_plot$ring, 
#          levels = levels(eggdf_age_CI$ring_ordered),
#          ordered = TRUE)
# 
# dplyr::filter(ceuta_egg_chick_female_data_3_year, is.na(volume) & !is.na(polyandry))

# specify point size for number of clutches per year
point_size <- c(1, 2, 3, 4)
point_colors <- c("black", "#f03b20")

# create the first captured as adult plot
Imm_plot <-
  ggplot2::ggplot() +
  geom_line(data = dplyr::filter(eggdf_age_CI, age_first_cap == "A"),
            aes(x = CI_age, y = ring_ordered), color = "grey90",
            size = 2.5, lineend = "round") +
  geom_linerange(data = dplyr::filter(max_min_obs, age_first_cap == "A"),
                 orientation = "y", aes(y = ring_ordered, x = min_age,
                                        xmin = min_age, xmax = max_age),
                 size = 0.85, color = "grey60") +
  geom_point(data = dplyr::filter(eggdf_clutches, age_first_cap == "A"), 
             aes(x = est_age, y = ring_ordered, size = as.factor(n_clutches), 
                 fill = as.factor(polyandry)), shape = 21) +
  # geom_point(data = dplyr::filter(matings_without_egg_obs_plot, age_first_cap == "A"),
  #            aes(x = est_age, y = ring_ordered), size = 2, shape = 4) +
  scale_x_continuous(limits = c(0, 24),
                     breaks = c(0:24)) +
  ylab("First captured as adults") +
  xlab("Age ± 95% CI") +
  luke_theme +
  theme(legend.position = c(0.78, 0.8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  scale_size_manual(values = point_size) +
  scale_fill_manual(values = point_colors) +
  labs(size = "Number of clutches observed",
       fill = "Number of mates observed") +
  annotate(geom = "text", y = 28, x = 16,
           label = "For visual purposes, only individuals\nwith more than 2 years of\nobservations are shown",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# create the recruit plot
Rec_plot <- 
  ggplot2::ggplot() +
  geom_linerange(data = dplyr::filter(max_min_obs, age_first_cap == "J"), 
                 orientation = "y", aes(y = ring_ordered, x = min_age,
                                        xmin = min_age, xmax = max_age), 
                 size = 0.85, color = "grey60") +
  geom_point(data = dplyr::filter(eggdf_clutches, age_first_cap == "J"), 
             aes(x = est_age, y = ring_ordered, size = as.factor(n_clutches), 
                 fill = as.factor(polyandry)), shape = 21) +
  
  # geom_point(data = dplyr::filter(matings_without_egg_obs_plot, age_first_cap == "J"),
  #            aes(x = est_age, y = ring_ordered), size = 2, shape = 4) +
  scale_x_continuous(limits = c(0, 23),
                     breaks = c(0:23)) +
  ylab("Locally recruited") +
  xlab("Age") +
  luke_theme +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_size_manual(values = point_size) +
  scale_fill_manual(values = point_colors)

# draw the two panels together for Fig 1
plot_of_sample_population <- 
  ggarrange(Rec_plot, Imm_plot, nrow = 2, align = "v",
            # heights = c(0.5, 0.5))
            heights = c(0.19, 0.81))

plot_of_sample_population

ggsave(plot = plot_of_sample_population,
       filename = "products/figures/sample_distribution_plot.jpg",
       width = 7,
       height = 10, units = "in")