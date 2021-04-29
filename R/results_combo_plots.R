#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

#### Results ----
load("output/stats_eggv_mod.rds")
load("output/stats_laydate_mod.rds")
load("output/stats_polyandry_mod.rds")

#### Data ----
load("data/ceuta_egg_chick_female_data.rds")

# wrangle data to include only first nests
first_nests_data <-
  ceuta_egg_chick_female_data %>%
  dplyr::select(polyandry, year, ring, first_laydate, n_nests, ID) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  mutate(poly = ifelse(polyandry == "poly", 1, 0),
         mono = ifelse(polyandry == "mono", 1, 0)) %>%
  mutate(poly_plot = ifelse(poly == 1, poly + 0.1, poly - 0.1))

# subset to nest level and first nest attempts of the season for each female
first_nests_age_data <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, first_laydate, est_age_t_deviation, year,
                first_age_t, last_age_t, n_years_obs, avg_ad_tarsi,
                age_first_cap) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_t_deviation)) %>% 
  mutate(age_first_cap_dummy = ifelse(age_first_cap == "J", 1, 0))

#### Plotting of Figure ----
# extract fitted values
polyandry_mod_fits <- function(offs) {
  model <- lme4::glmer(cbind(poly, mono) ~ 
                         I(first_laydate - offs) + (1| ring) + (1 | year), 
                       data = first_nests_data, family = binomial)
  
  ests <- summary(model)$coefficients[1,1:2]
  
  # backlink the coefficients to the probability scale
  return(c(offs, ests, invlogit(ests[1] + c(-1, 0, 1) * 1.96 * ests[2])))
}

# specify the offs (i.e., vector of numbers from min to max dates stepped by 1)
offs_first_laydate <- 
  seq(min(first_nests_data$first_laydate, na.rm = TRUE), 
      max(first_nests_data$first_laydate, na.rm = TRUE), 1)

# apply the offs vector to the function (retuning a matrix)
polyandry_fits <- sapply(offs_first_laydate, polyandry_mod_fits)

# transpose the matrix
polyandry_fits <- t(polyandry_fits)

# convert the matrix to a data.frame
polyandry_fits <- data.frame(polyandry_fits)

# define the column names
colnames(polyandry_fits) <- 
  c("first_laydate", "Estimate", "Std. Error", "Upper", "Mean", "Lower")

polyandry_date_mod_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = first_nests_data, 
               aes(x = first_laydate, y = poly_plot, 
                   group = polyandry, fill = polyandry), 
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_jitter(data = first_nests_data, 
              aes(x = first_laydate, y = poly, 
                  group = polyandry, 
                  fill = polyandry, color = polyandry), 
              height = 0.02, alpha = 0.4, shape = 19) +
  geom_ribbon(data = polyandry_fits, 
              aes(x = first_laydate, y = Mean, ymin = Lower, ymax = Upper), 
              fill = "grey50", alpha = 0.25) +
  geom_line(data = polyandry_fits, 
            aes(x = first_laydate, y = Mean), lwd = 0.5, colour = "grey20") +
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
  dplyr::select(polyandry, first_laydate, ID, year, ring) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  dplyr::filter(first_laydate > -50) %>%
  mutate(first_laydate = as.numeric(first_laydate)) %>% 
  ggplot(data = ., aes(x = first_laydate, y = 1, group = polyandry)) + 
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
  as.data.frame(effect("poly(first_laydate, 2)", stats_eggv_mod$mod, 
                       xlevels = list(first_laydate = seq(min(ceuta_egg_chick_female_data$first_laydate), 
                                                          max(ceuta_egg_chick_female_data$first_laydate), 1))))
# summary of fitted trend
eggv_mod_date_fits %>% 
  summarise(min_eggv_fit = min(fit),
            max_eggv_fit = max(fit),
            min_eggv_date = first_laydate[which.min(fit)],
            max_eggv_date = first_laydate[which.max(fit)],
            min_eggv_lower = lower[which.min(fit)],
            min_eggv_upper = upper[which.min(fit)],
            max_eggv_lower = lower[which.max(fit)],
            max_eggv_upper = upper[which.max(fit)])

# plot the quadratic trend, pre- and post-peak trend, and raw data
eggv_date_mod_plot <-
  ggplot() +
  geom_point(data = ceuta_egg_chick_female_data, alpha = 0.4,
             aes(x = first_laydate, y = volume_cm),
             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_date_fits, aes(x = first_laydate, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = eggv_mod_date_fits, aes(x = first_laydate, 
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
Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

Season_plot

# write plot to disk
ggsave(plot = Season_plot,
       filename = "products/figures/Season_plot.png",
       width = 10,
       height = 20, units = "cm", dpi = 600)

#### Trend plot of egg volume over age ----
# extract fitted values
eggv_mod_age_fits <- 
  as.data.frame(effect(term = "poly(est_age_t_deviation, 2)", mod = stats_eggv_mod$mod_poly, 
                       xlevels = list(est_age_t_deviation = seq(min(ceuta_egg_chick_female_data$est_age_t_deviation, na.rm = TRUE), 
                                                                max(ceuta_egg_chick_female_data$est_age_t_deviation, na.rm = TRUE), 1))))

# summary of fitted trend
eggv_mod_age_fits %>% 
  summarise(min_eggv_fit = min(fit),
            max_eggv_fit = max(fit),
            min_eggv_date = est_age_t_deviation[which.min(fit)],
            max_eggv_date = est_age_t_deviation[which.max(fit)],
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
              aes(x = est_age_t + 1, y = volume_cm),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_age_fits, aes(x = est_age_t + 1, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = eggv_mod_age_fits,
              aes(x = est_age_t + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Estimated age (years)") +
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13)) +
  annotate(geom = "text", y = 9, x = 1,
           label = "All eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

#### Plot of trend ----
# extract fitted values
laydate_mod_age_fits <- 
  as.data.frame(effect(term = "poly(est_age_t_deviation, 2)", mod = stats_laydate_mod$mod_poly, 
                       xlevels = list(est_age_t_deviation = seq(min(ceuta_egg_chick_female_data$est_age_t_deviation, na.rm = TRUE), 
                                                                max(ceuta_egg_chick_female_data$est_age_t_deviation, na.rm = TRUE), 1))))

# plot predicted trend and raw data
date_age_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  geom_jitter(data = first_nests_age_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age_t + 1, y = first_laydate),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = laydate_mod_age_fits, aes(x = est_age_t_deviation + 1, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = laydate_mod_age_fits, 
              aes(x = est_age_t_deviation + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Estimated age (years)") +
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13)) +
  annotate(geom = "text", y = 50, x = 1,
           label = "First nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

#### Combo plot of age dynamics ----
Age_plot <-
  (date_age_trend_plot / eggv_age_trend_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 7), c('cm', 'cm')))
Age_plot

# write plot to disk
ggsave(plot = Age_plot,
       filename = "products/figures/svg/Age_plot.svg",
       width = 4.5,
       height = 6.75, units = "in")

ggsave(plot = Age_plot,
       filename = "products/figures/jpg/Age_plot_van_de_Pol.jpg",
       width = 4.5,
       height = 6.75, units = "in")

#### Trend plot of egg volume over tarsus ----
# extract fitted values
eggv_mod_tarsus_fits <- 
  as.data.frame(effect(term = "avg_ad_tarsi", mod = stats_eggv_mod$mod_poly, 
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
  as.data.frame(effect(term = "avg_ad_tarsi", mod = stats_laydate_mod$mod_poly, 
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
              aes(x = avg_ad_tarsi, y = first_laydate),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = date_tarsus_mod_fits, aes(x = avg_ad_tarsi, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = date_tarsus_mod_fits,
              aes(x = avg_ad_tarsi, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Estimated age (years)")# +
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13))

#### Combo plot of Tarsus dynamics ----
Tarsus_plot <-
  (date_tarsus_trend_plot / eggv_tarsus_trend_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 7), c('cm', 'cm')))

Tarsus_plot

# write plot to disk
ggsave(plot = Tarsus_plot,
       filename = "products/figures/svg/Tarsus_plot.svg",
       width = 4.5,
       height = 6.75, units = "in")

ggsave(plot = Tarsus_plot,
       filename = "products/figures/jpg/Tarsus_plot.jpg",
       width = 4.5,
       height = 6.75, units = "in")

#### Plot of trend ----
# extract fitted values
date_firstage_mod_fits <- 
  as.data.frame(effect(term = "first_age_t", mod = stats_laydate_mod$mod_poly, 
                       xlevels = list(first_age_t = seq(min(first_nests_age_data$first_age_t, na.rm = TRUE), 
                                                          max(first_nests_age_data$first_age_t, na.rm = TRUE), 1))))

# plot predicted trend and raw data
date_firstage_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank()) +#,
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank()
        # ) +
  geom_jitter(data = first_nests_age_data, 
              alpha = 0.4, width = 0.3,
              aes(x = first_age_t + 1, y = first_laydate),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = date_firstage_mod_fits, aes(x = first_age_t + 1, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = date_firstage_mod_fits, 
              aes(x = first_age_t + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Age at first local breeding attempt") +
  scale_x_continuous(limits = c(0.5, 7.5), breaks = c(1:7)) +
  annotate(geom = "text", y = 50, x = 1,
           label = "First nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

coefplot2(stats_date_van_de_Pol$mod_I)
plot(allEffects(stats_date_van_de_Pol$mod_I))
