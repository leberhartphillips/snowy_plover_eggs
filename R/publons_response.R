# two plots associated with the Publons response to Martin Bulla's review

#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")

# plot for response 8
ceuta_egg_chick_female_data %>% 
  mutate(date_bin = cut(jul_lay_date_std_num, c(seq(from = min(jul_lay_date_std_num - 0.1), 
                                                    to = max(jul_lay_date_std_num + 0.1), 
                                                    length.out = 11)), 
                        labels = 1:10)) %>% 
  ggplot(aes(x = date_bin, y = volume_cm, color = polyandry, fill = polyandry)) + 
  geom_jitter(alpha = 0.2,
              position = position_dodge(width = 0.6)) +
  geom_boxplot(outlier.alpha = 0,
               color = "grey50",
               width = 0.5, alpha = 0.5, 
               position = position_dodge(width = 0.6)) +
  luke_theme +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Date bin within season") +
  scale_color_manual(values = rev(plot_palette_polyandry),
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Monogamous", "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry),
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Monogamous", "Polyandrous")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5,"cm"))

# plot for response 9
ceuta_egg_chick_female_data %>% 
  filter(n_years_obs > 5) %>% 
  ggplot(data = ., 
         aes(x = jul_lay_date_std_num, y = volume_cm, 
             color = polyandry)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(ring ~ est_age_t_deviation) +
  luke_theme +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_color_manual(values = rev(plot_palette_polyandry),
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Monogamous", "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry),
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Monogamous", "Polyandrous")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5,"cm"),
        panel.grid.major = element_line(colour = "grey70", size=0.25))
