#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

#### Results ----
load("output/stats_eggv_age_date_tarsi_.rds")
load("output/stats_date_age_tarsi_.rds")
load("output/stats_poly_date.rds")

#### Data ----
load("data/ceuta_egg_chick_female_data.rds")

# wrangle data to include only first nests
first_nests_data <-
  ceuta_egg_chick_female_data %>%
  dplyr::filter(nest_order == 1) %>%
  dplyr::select(polyandry, jul_lay_date_std_num, ID, year, ring, avg_ad_tarsi) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  mutate(poly = ifelse(polyandry == "poly", 1, 0),
         mono = ifelse(polyandry == "mono", 1, 0)) %>%
  mutate(poly_plot = ifelse(poly == 1, poly + 0.1, poly - 0.1))

# subset to nest level and first nest attempts of the season for each female
first_nests_age_data <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, jul_lay_date_std_num, est_age_trans, year,
                firstage, lastage, nest_order, n_years_obs, avg_ad_tarsi) %>% 
  dplyr::filter(nest_order == 1) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_trans))

#### Plotting of Figure ----
# extract fitted values
poly_mod_fits <- function(offs) {
  model <- lme4::glmer(cbind(poly, mono) ~ 
                         I(jul_lay_date_std_num - offs) + (1| ring) + (1 | year), 
                       data = first_nests_data, family = binomial)
  
  ests <- summary(model)$coefficients[1,1:2]
  
  # backlink the coefficients to the probability scale
  return(c(offs, ests, invlogit(ests[1] + c(-1, 0, 1) * 1.96 * ests[2])))
}

# specify the offs (i.e., vector of numbers from min to max dates stepped by 1)
offs_jul_lay_date_std <- 
  seq(min(first_nests_data$jul_lay_date_std_num, na.rm = TRUE), 
      max(first_nests_data$jul_lay_date_std_num, na.rm = TRUE), 1)

# apply the offs vector to the function (retuning a matrix)
poly_fits <- sapply(offs_jul_lay_date_std, poly_mod_fits)

# transpose the matrix
poly_fits <- t(poly_fits)

# convert the matrix to a data.frame
poly_fits <- data.frame(poly_fits)

# define the column names
colnames(poly_fits) <- 
  c("jul_lay_date_std_num", "Estimate", "Std. Error", "Upper", "Mean", "Lower")

polyandry_date_mod_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = first_nests_data, 
               aes(x = jul_lay_date_std_num, y = poly_plot, 
                   group = polyandry, fill = polyandry), 
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_jitter(data = first_nests_data, 
              aes(x = jul_lay_date_std_num, y = poly, 
                  group = polyandry, 
                  fill = polyandry, color = polyandry), 
              height = 0.02, alpha = 0.4, shape = 19) +
  geom_ribbon(data = poly_fits, 
              aes(x = jul_lay_date_std_num, y = Mean, ymin = Lower, ymax = Upper), 
              fill = "grey50", alpha = 0.25) +
  geom_line(data = poly_fits, 
            aes(x = jul_lay_date_std_num, y = Mean), lwd = 0.5, colour = "grey20") +
  luke_theme +
  theme(legend.position = c(0.5, -0.04),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.background = element_blank()) +
  scale_y_continuous(limits = c(-0.15, 1.2),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(-60, 60)) +
  ylab("Probabilty of polyandry ± 95% CI") +
  scale_color_manual(values = rev(plot_palette_polyandry),
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Monogamous", "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry),
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Monogamous", "Polyandrous")) +
  annotate(geom = "text", y = 1.2, x = -58,
           label = "Lay dates for first nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# plot the posterior age at peak distribution
polyandry_date_dist_plot <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(polyandry, jul_lay_date_std_num, ID, year, ring) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  dplyr::filter(jul_lay_date_std_num > -50) %>%
  mutate(jul_lay_date_std_num = as.numeric(jul_lay_date_std_num)) %>% 
  ggplot(data = ., aes(x = jul_lay_date_std_num, y = 1, group = polyandry)) + 
  geom_violin(data = . %>% dplyr::filter(polyandry == "mono"), 
              alpha = 0.5, fill = plot_palette_polyandry[2], color = "grey50",
              trim = FALSE) +
  geom_violin(data = . %>% dplyr::filter(polyandry == "poly"), 
              alpha = 0.5, fill = plot_palette_polyandry[1], color = "grey50",
              trim = FALSE) +
  theme_void() +
  theme(legend.position = c(0.85, 0.2),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5,"cm"),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25)) +
  scale_x_continuous(limits = c(-60, 60)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  annotate(geom = "text", y = 1.7, x = -58,
           label = "Lay date distributions for all nests",
           color = "black", size = 3, fontface = 'italic', hjust = 0) +
  scale_fill_manual(values = plot_palette_polyandry,
                    guide = guide_legend(title.position = "top", nrow = 2),
                    labels = c("Monogamous", "Polyandrous"))

#### Trend plot of egg volume over season ----
# extract the fitted values of the polynomial season effect
eggv_mod_date_fits <- 
  as.data.frame(effect("poly(jul_lay_date_std_num, 2)", stats_eggv_age_date_tarsi$mod, 
                       xlevels = list(jul_lay_date_std_num = seq(min(ceuta_egg_chick_female_data$jul_lay_date_std_num), 
                                                                 max(ceuta_egg_chick_female_data$jul_lay_date_std_num), 1))))
# summary of fitted trend
eggv_mod_date_fits %>% 
  summarise(min_eggv_fit = min(fit),
            max_eggv_fit = max(fit),
            min_eggv_date = jul_lay_date_std_num[which.min(fit)],
            max_eggv_date = jul_lay_date_std_num[which.max(fit)],
            min_eggv_lower = lower[which.min(fit)],
            min_eggv_upper = upper[which.min(fit)],
            max_eggv_lower = lower[which.max(fit)],
            max_eggv_upper = upper[which.max(fit)])

# plot the quadratic trend, pre- and post-peak trend, and raw data
eggv_date_mod_plot <-
  ggplot() +
  geom_point(data = ceuta_egg_chick_female_data, alpha = 0.4,
             aes(x = jul_lay_date_std_num, y = volume_cm), #color = "grey50", shape = 16,
             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_date_fits, aes(x = jul_lay_date_std_num, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = eggv_mod_date_fits, aes(x = jul_lay_date_std_num, 
                                             ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank()) +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-60, 60)) +
  theme(legend.position = "none") +
  annotate(geom = "text", y = 9, x = -58,
           label = "Lay dates for all eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

#### Combo plot of seasonal dynamics ----
# run the "model_polyandry script to get the polyandry plots
# source("R/revision/model_polyandry.R")
Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

Season_plot

ggsave(plot = Season_plot,
       filename = "products/figures/Season_plot.png",
       width = 10,
       height = 20, units = "cm", dpi = 600)


polyandry_date_mod_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = eggs_mb, aes(x = jul_std_date, y = poly_plot, group = polyandry, 
                                   fill = polyandry), color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_jitter(data = eggs_mb, aes(x = jul_std_date, y = poly, group = polyandry, 
                                  fill = polyandry, color = polyandry), 
              height = 0.02, alpha = 0.4, shape = 19) +
  geom_ribbon(data = poly_fits, 
              aes(x = lay_date_std, y = Mean, ymin = Lower, ymax = Upper), 
              fill = "grey50", alpha = 0.25) +
  geom_line(data = poly_fits, 
            aes(x = lay_date_std, y = Mean), lwd = 0.5, colour = "grey20") +
  luke_theme +
  theme(legend.position = c(0.5, -0.04),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.background = element_blank()) +
  scale_y_continuous(limits = c(-0.15, 1.2),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(-50, 50)) +
  ylab("Probabilty of polyandry ± 95% CI") +
  # xlab("Standardized lay date (of first nest)") +
  scale_color_manual(values = rev(plot_palette_polyandry),
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Monogamous", "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry),
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Monogamous", "Polyandrous")) +
  annotate(geom = "text", y = 1.2, x = -48,
           label = "Lay dates for first nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# plot the laydate distribution
polyandry_date_dist_plot <-
  eggs_50 %>% 
  dplyr::select(ID, polyandry, jul_std_date) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = jul_std_date, y = 1, group = polyandry)) + 
  geom_violin(data = . %>% dplyr::filter(polyandry == "mono"), 
              alpha = 0.5, fill = plot_palette_polyandry[2], color = "grey50",
              trim = FALSE) +
  geom_violin(data = . %>% dplyr::filter(polyandry == "poly"), 
              alpha = 0.5, fill = plot_palette_polyandry[1], color = "grey50",
              trim = FALSE) +
  
  # alpha = 0.5, aes(x = jul_std_date, y = 1, fill = polyandry)) + 
  # geom_density(alpha = 0.5, aes(jul_std_date, fill = polyandry)) + 
  theme_void() +
  theme(legend.position = c(0.85, 0.2),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5,"cm"),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25)) +
  scale_x_continuous(limits = c(-50, 50)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  # geom_hline(yintercept = 0, colour = "white", size = 1) +
  annotate(geom = "text", y = 1.7, x = -48,
           label = "Lay date distributions for all nests",
           color = "black", size = 3, fontface = 'italic', hjust = 0) +
  scale_fill_manual(values = plot_palette_polyandry,
                    guide = guide_legend(title.position = "top", nrow = 2),
                    labels = c("Monogamous", "Polyandrous"))# +
# labs(fill = "Number of mates observed")

# plot the quadratic trend, pre- and post-peak trend, and raw data
eggv_date_mod_plot <-
  ggplot() +
  geom_point(data = eggs_50, alpha = 0.4,
             aes(x = jul_std_date, y = eggv/1000), #color = "grey50", shape = 16,
             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = mod6a_season_fits, aes(x = jul_std_date, y = fit/1000),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod6a_season_fits, aes(x = jul_std_date, ymax = upper/1000, ymin = lower/1000),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank()) +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-50, 50)) +
  theme(legend.position = "none") +
  annotate(geom = "text", y = 9, x = -48,
           label = "Lay dates for all eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)# +
# scale_color_manual(values = polyandry_colors)

Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

ggsave(plot = Season_plot,
       filename = "products/figures/Fig_3.jpg",
       width = 10,
       height = 24,
       units = "cm", dpi = 600)

#### Trend plot of egg volume over age ----
# extract fitted values
eggv_mod_age_fits <- 
  as.data.frame(effect(term = "poly(est_age_trans, 2)", mod = stats_eggv_age_date_tarsi$mod_poly, 
                       xlevels = list(est_age_trans = seq(min(ceuta_egg_chick_female_data$est_age_trans, na.rm = TRUE), 
                                                          max(ceuta_egg_chick_female_data$est_age_trans, na.rm = TRUE), 1))))

# summary of fitted trend
eggv_mod_age_fits %>% 
  summarise(min_eggv_fit = min(fit),
            max_eggv_fit = max(fit),
            min_eggv_date = est_age_trans[which.min(fit)],
            max_eggv_date = est_age_trans[which.max(fit)],
            min_eggv_lower = lower[which.min(fit)],
            min_eggv_upper = upper[which.min(fit)],
            max_eggv_lower = lower[which.max(fit)],
            max_eggv_upper = upper[which.max(fit)])

# plot fitted values and raw data
eggv_age_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank()) +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age_trans + 1, y = volume_cm),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_age_fits, aes(x = est_age_trans + 1, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = eggv_mod_age_fits, 
              aes(x = est_age_trans + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Estimated age (years)") +
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13)) +
  annotate(geom = "text", y = 9, x = 1,
           label = "All eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

#### Plot of trend ----
# extract fitted values
date_age_mod_fits <- 
  as.data.frame(effect(term = "poly(est_age_trans, 2)", mod = stats_date_age_tarsi$mod_poly, 
                       xlevels = list(est_age_trans = seq(min(first_nests_age_data$est_age_trans, na.rm = TRUE), 
                                                          max(first_nests_age_data$est_age_trans, na.rm = TRUE), 1))))

# plot predicted trend and raw data
date_age_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  geom_jitter(data = first_nests_age_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age_trans + 1, y = jul_lay_date_std_num),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = date_age_mod_fits, aes(x = est_age_trans + 1, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = date_age_mod_fits, 
              aes(x = est_age_trans + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Estimated age (years)") +
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13)) +
  annotate(geom = "text", y = 50, x = 1,
           label = "First nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

#### Combo plot of age dynamics ----
# run the "model_laydate script to get the laydate plot
# source("R/revision/model_laydate.R")
Age_plot <-
  (date_age_trend_plot / eggv_age_trend_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 7), c('cm', 'cm')))

Age_plot

ggsave(plot = Age_plot,
       filename = "products/figures/svg/Age_plot.svg",
       width = 4.5,
       height = 6.75, units = "in")

ggsave(plot = Age_plot,
       filename = "products/figures/jpg/Age_plot.jpg",
       width = 4.5,
       height = 6.75, units = "in")

#### Trend plot of egg volume over tarsus ----
# extract fitted values
eggv_mod_tarsus_fits <- 
  as.data.frame(effect(term = "avg_ad_tarsi", mod = stats_eggv_age_date_tarsi$mod_I, 
                       xlevels = list(avg_ad_tarsi = seq(min(ceuta_egg_chick_female_data$avg_ad_tarsi, na.rm = TRUE), 
                                                        max(ceuta_egg_chick_female_data$avg_ad_tarsi, na.rm = TRUE), 0.5))))

# summary of fitted trend
eggv_mod_tarsus_fits %>% 
  summarise(min_eggv_fit = min(fit),
            max_eggv_fit = max(fit),
            min_eggv_tarsus = avg_ad_tarsi[which.min(fit)],
            max_eggv_tarsus = avg_ad_tarsi[which.max(fit)],
            min_eggv_lower = lower[which.min(fit)],
            min_eggv_upper = upper[which.min(fit)],
            max_eggv_lower = lower[which.max(fit)],
            max_eggv_upper = upper[which.max(fit)])

# plot fitted values and raw data
eggv_tarsus_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank()) +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = avg_ad_tarsi, y = volume_cm),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_tarsus_fits, aes(x = avg_ad_tarsi, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = eggv_mod_tarsus_fits, 
              aes(x = avg_ad_tarsi, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Mother tarsus length (mm)") #+
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13))

#### Plot of trend ----
# extract fitted values
date_tarsus_mod_fits <- 
  as.data.frame(effect(term = "avg_ad_tarsi", mod = stats_date_age_tarsi$mod_I, 
                       xlevels = list(avg_ad_tarsi = seq(min(first_nests_age_data$avg_ad_tarsi, na.rm = TRUE), 
                                                          max(first_nests_age_data$avg_ad_tarsi, na.rm = TRUE), 0.5))))

# plot predicted trend and raw data
date_tarsus_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  geom_jitter(data = first_nests_age_data, 
              alpha = 0.4, width = 0.3,
              aes(x = avg_ad_tarsi, y = jul_lay_date_std_num),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = date_tarsus_mod_fits, aes(x = avg_ad_tarsi, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = date_tarsus_mod_fits, 
              aes(x = avg_ad_tarsi, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Estimated age (years)")# +
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13))

#### Combo plot of age dynamics ----
# run the "model_laydate script to get the laydate plot
# source("R/revision/model_laydate.R")
Tarsus_plot <-
  (date_tarsus_trend_plot / eggv_tarsus_trend_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 7), c('cm', 'cm')))

Tarsus_plot

ggsave(plot = Tarsus_plot,
       filename = "products/figures/svg/Tarsus_plot.svg",
       width = 4.5,
       height = 6.75, units = "in")

ggsave(plot = Tarsus_plot,
       filename = "products/figures/jpg/Tarsus_plot.jpg",
       width = 4.5,
       height = 6.75, units = "in")

