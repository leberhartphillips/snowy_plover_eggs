#### Libraries and data ----
source("R/001_libraries.R")
source("R/002_functions.R")

load("data/ceuta_egg_chick_female_data.rds")

#### Exploratory plots ----
# all nests for all females
dist_text1 <- 
  data.frame(y = 35, 
             x = min(ceuta_egg_chick_female_data$jul_lay_date_std_num, na.rm = TRUE), 
             lab = "All nests for all females",
             polyandry = factor("mono", levels = c("mono", "poly")))
ceuta_egg_chick_female_data %>% 
  dplyr::select(polyandry, jul_lay_date_std_num, ID) %>% 
  distinct() %>% 
  ggplot() + 
  geom_boxplot(aes(x = jul_lay_date_std_num, y = 23, group = polyandry, 
                   fill = polyandry), color = "grey50",
               width = 5, alpha = 0.5) +
  geom_jitter(aes(x = jul_lay_date_std_num, y = 30, group = polyandry, 
                  fill = polyandry, color = polyandry), height = 3, alpha = 0.4) +
  geom_density(alpha = 0.3, aes(jul_lay_date_std_num,
                                after_stat(count),
                                fill = polyandry,
                                color = polyandry), adjust = 2) +
  geom_histogram(alpha = 0.5, aes(jul_lay_date_std_num,
                                  fill = polyandry,
                                  color = polyandry), binwidth = 1) +
  luke_theme +
  facet_grid(polyandry ~ ., 
             labeller = labeller(polyandry = polyandry.labs)) +
  theme(axis.title.y = element_text(hjust = 0.05),
        legend.position = "none") +
  ylab("Number of nests") +
  xlab("Nest initiation date (scaled by year)") +
  scale_x_continuous(limits = c(-60, 60)) +
  scale_y_continuous(limits = c(0, 35),
                     breaks = c(0, 5, 10, 15, 20)) +
  geom_text(data = dist_text1, x = dist_text1$x,
            y = dist_text1$y, label = dist_text1$lab,
            hjust = 0, color = "grey30", size = 4, fontface = 'italic') +
  scale_color_manual(values = plot_palette_polyandry) +
  scale_fill_manual(values = plot_palette_polyandry)

# only females with multiple nests
dist_text2 <- 
  data.frame(y = 35, 
             x = min(ceuta_egg_chick_female_data$jul_lay_date_std_num, na.rm = TRUE), 
             lab = "Only females with multiple nests",
             polyandry = factor("mono", levels = c("mono", "poly")))

two_nesters <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(nest_order %in% c(2))

ceuta_egg_chick_female_data %>%
  # dplyr::filter(nest_order %in% c(1, 2)) %>% 
  dplyr::filter(ring_year %in% two_nesters$ring_year) %>%
  dplyr::select(polyandry, jul_lay_date_std_num, ID) %>% 
  distinct() %>% 
  ggplot() + 
  geom_boxplot(aes(x = jul_lay_date_std_num, y = 23, group = polyandry, 
                   fill = polyandry), color = "grey50",
               width = 5, alpha = 0.5) +
  geom_jitter(aes(x = jul_lay_date_std_num, y = 30, group = polyandry, 
                  fill = polyandry, color = polyandry), height = 3, alpha = 0.4) +
  geom_density(alpha = 0.3, aes(jul_lay_date_std_num,
                                after_stat(count),
                                fill = polyandry,
                                color = polyandry), adjust = 2) +
  geom_histogram(alpha = 0.5, aes(jul_lay_date_std_num,
                                  fill = polyandry,
                                  color = polyandry), binwidth = 1) +
  luke_theme +
  facet_grid(polyandry ~ ., 
             labeller = labeller(polyandry = polyandry.labs)) +
  theme(axis.title.y = element_text(hjust = 0.05),
        legend.position = "none") +
  ylab("Number of nests") +
  xlab("Nest initiation date (scaled by year)") +
  scale_x_continuous(limits = c(-60, 60)) +
  scale_y_continuous(limits = c(0, 35),
                     breaks = c(0, 5, 10, 15, 20)) +
  geom_text(data = dist_text2, x = dist_text2$x,
            y = dist_text2$y, label = dist_text2$lab,
            hjust = 0, color = "grey30", size = 4, fontface = 'italic') +
  scale_color_manual(values = plot_palette_polyandry) +
  scale_fill_manual(values = plot_palette_polyandry)

# Hatched first nests of females with multiple nests
dist_text3 <- 
  data.frame(y = 35, 
             x = min(ceuta_egg_chick_female_data$jul_lay_date_std_num, na.rm = TRUE), 
             lab = "Hatched first nests of females with multiple nests",
             polyandry = factor("mono", levels = c("mono", "poly")))
ceuta_egg_chick_female_data %>%
  # filter(ring_year %in% second_nests$ring_year) %>% 
  dplyr::filter(ring_year %in% two_nesters$ring_year) %>%
  dplyr::filter(nest_order == 1) %>% 
  dplyr::filter(fate == "Hatch") %>% 
  dplyr::select(polyandry, jul_lay_date_std_num, ID) %>% 
  distinct() %>% 
  ggplot() + 
  geom_boxplot(aes(x = jul_lay_date_std_num, y = 23, group = polyandry, 
                   fill = polyandry), color = "grey50",
               width = 5, alpha = 0.5) +
  geom_jitter(aes(x = jul_lay_date_std_num, y = 30, group = polyandry, 
                  fill = polyandry, color = polyandry), height = 3, alpha = 0.4) +
  geom_density(alpha = 0.3, aes(jul_lay_date_std_num,
                                after_stat(count),
                                fill = polyandry,
                                color = polyandry), adjust = 2) +
  geom_histogram(alpha = 0.5, aes(jul_lay_date_std_num,
                                  fill = polyandry,
                                  color = polyandry), binwidth = 1) +
  luke_theme +
  facet_grid(polyandry ~ ., 
             labeller = labeller(polyandry = polyandry.labs)) +
  theme(axis.title.y = element_text(hjust = 0.05),
        legend.position = "none") +
  ylab("Number of nests") +
  xlab("Nest initiation date (scaled by year)") +
  scale_x_continuous(limits = c(-60, 60)) +
  scale_y_continuous(limits = c(0, 35),
                     breaks = c(0, 5, 10, 15, 20)) +
  geom_text(data = dist_text3, x = dist_text1$x,
            y = dist_text3$y, label = dist_text3$lab,
            hjust = 0, color = "grey30", size = 4, fontface = 'italic') +
  scale_color_manual(values = plot_palette_polyandry) +
  scale_fill_manual(values = plot_palette_polyandry)