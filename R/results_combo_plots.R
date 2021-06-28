#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

#### Results ----
load("output/stats_eggv_mod.rds")
load("output/stats_laydate_mod.rds")
load("output/stats_polyandry_age_mod.rds")
load("output/stats_renesting_mod.rds")


#### Data ----
ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")

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

# subset to nest level and first nest attempts of the season for each female
first_nests_age_data <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, first_laydate, est_age_t_deviation, year,
                first_age_t, last_age_t, n_years_obs, avg_ad_tarsi,
                age_first_cap, nest_order) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_t_deviation) & nest_order == 1) %>% 
  mutate(age_first_cap_dummy = ifelse(age_first_cap == "J", 1, 0)) %>%
  mutate(age_first_cap_plot = ifelse(age_first_cap == "J", 2.2, 0.8))

# wrangle data for renesting plot
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

#### Plotting of polyandry potential Figure ----
# extract fitted values
polyandry_mod_fits <- function(offs) {
  model <- lme4::glmer(cbind(poly, mono) ~ 
                         I(first_laydate - offs) +
                       (1| ring) + (1 | year), 
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
  theme(legend.position = c(0.5, -0.08),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 11),
        legend.background = element_blank()) +
  scale_y_continuous(limits = c(-0.15, 1.2),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  # ylab("Probabilty of polyandry ± 95% CI") +
  ylab("P(polyandry) ± 95% CI") +
  scale_color_manual(values = rev(plot_palette_polyandry),
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Monogamous", "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry),
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Monogamous", "Polyandrous")) +
  annotate(geom = "text", y = 0.5, x = 5,
           label = "Lay dates for first\nnests of the season",
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
  scale_x_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  annotate(geom = "text", y = 1.7, x = -58,
           label = "Lay date distributions for all nests",
           color = "black", size = 3, fontface = 'italic', hjust = 0) +
  scale_fill_manual(values = plot_palette_polyandry,
                    guide = guide_legend(title.position = "top", nrow = 2),
                    labels = c("Monogamous", "Polyandrous"))

# plot the posterior age at peak distribution
renesting_date_dist_plot <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(multiclutch, jul_lay_date_std_num, ID, year, ring) %>%
  distinct() %>%
  mutate(multiclutch = as.factor(multiclutch)) %>%
  dplyr::filter(jul_lay_date_std_num > -50) %>%
  mutate(jul_lay_date_std_num = as.numeric(jul_lay_date_std_num)) %>% 
  ggplot(data = ., aes(x = jul_lay_date_std_num, y = 1, group = multiclutch)) + 
  geom_violin(data = . %>% dplyr::filter(multiclutch == "single"), 
              alpha = 0.5, fill = plot_palette_renesting[2], color = "grey50",
              trim = FALSE) +
  geom_violin(data = . %>% dplyr::filter(multiclutch == "multi"), 
              alpha = 0.5, fill = plot_palette_renesting[1], color = "grey50",
              trim = FALSE) +
  theme_void() +
  theme(legend.position = c(0.85, 0.2),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5,"cm"),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25)) +
  scale_x_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  annotate(geom = "text", y = 1.7, x = -58,
           label = "Lay date distributions for all nests",
           color = "black", size = 3, fontface = 'italic', hjust = 0) +
  scale_fill_manual(values = plot_palette_renesting,
                    guide = guide_legend(title.position = "top", nrow = 2),
                    labels = c("Single", "Re-nester"))

#### Plotting of re-nesting potential Figure ----
# extract fitted values
renesting_mod_fits <- function(offs) {
  model <- lme4::glmer(cbind(multi, single) ~ 
                         I(first_laydate - offs) +
                         (1| ring) + (1 | year), 
                       data = renesting_data, family = binomial)
  
  ests <- summary(model)$coefficients[1,1:2]
  
  # backlink the coefficients to the probability scale
  return(c(offs, ests, invlogit(ests[1] + c(-1, 0, 1) * 1.96 * ests[2])))
}

# specify the offs (i.e., vector of numbers from min to max dates stepped by 1)
offs_first_laydate <- 
  seq(min(renesting_data$first_laydate, na.rm = TRUE), 
      max(renesting_data$first_laydate, na.rm = TRUE), 1)

# apply the offs vector to the function (retuning a matrix)
renesting_fits <- sapply(offs_first_laydate, renesting_mod_fits)

# transpose the matrix
renesting_fits <- t(renesting_fits)

# convert the matrix to a data.frame
renesting_fits <- data.frame(renesting_fits)

# define the column names
colnames(renesting_fits) <- 
  c("first_laydate", "Estimate", "Std. Error", "Upper", "Mean", "Lower")

renesting_date_mod_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = renesting_data, 
               aes(x = first_laydate, y = multi_plot, 
                   group = multiclutch, fill = multiclutch), 
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_jitter(data = renesting_data, 
              aes(x = first_laydate, y = multi, 
                  group = multiclutch, 
                  fill = multiclutch, color = multiclutch), 
              height = 0.02, alpha = 0.4, shape = 19) +
  geom_ribbon(data = renesting_fits, 
              aes(x = first_laydate, y = Mean, ymin = Lower, ymax = Upper), 
              fill = "grey50", alpha = 0.25) +
  geom_line(data = renesting_fits, 
            aes(x = first_laydate, y = Mean), lwd = 0.5, colour = "grey20") +
  luke_theme +
  theme(legend.position = c(0.5, 1.1),
    #legend.position = c(0.5, -0.04),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        legend.background = element_blank()) +
  scale_y_continuous(limits = c(-0.15, 1.2),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  # ylab("Probabilty of renesting after failure ± 95% CI") +
  ylab("P(re-nesting) ± 95% CI") +
  xlab("Standardized lay date") +
  scale_color_manual(values = plot_palette_renesting,
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Re-nest", "Single")) +
  scale_fill_manual(values = plot_palette_renesting,
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Re-nest", "Single")) +
  annotate(#geom = "text", y = 1.2, x = -58,
           geom = "text", y = 0.5, x = 5,
           label = "Lay dates for first\nnests of the season\nthat failed",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

renesting_date_mod_plot


#### Trend plot of egg volume over season ----
# extract the fitted values of the polynomial season effect
eggv_mod_within_fits <- 
  as.data.frame(effect("laydate_deviation", stats_eggv_mod$mod_poly, 
                       xlevels = list(laydate_deviation = seq(min(ceuta_egg_chick_female_data$laydate_deviation), 
                                                          max(ceuta_egg_chick_female_data$laydate_deviation), 1))))

# summary of fitted trend
eggv_mod_within_fits %>% 
  summarise(min_eggv_fit = min(fit),
            max_eggv_fit = max(fit),
            min_eggv_dev = laydate_deviation[which.min(fit)],
            max_eggv_dev = laydate_deviation[which.max(fit)],
            min_eggv_lower = lower[which.min(fit)],
            min_eggv_upper = upper[which.min(fit)],
            max_eggv_lower = lower[which.max(fit)],
            max_eggv_upper = upper[which.max(fit)]) %>% 
  mutate(diff = max_eggv_fit - min_eggv_fit,
         diff_upper = max_eggv_upper - min_eggv_lower,
         diff_lower = max_eggv_lower - min_eggv_upper) 

7.788539 - 7.575227
7.683123 - 7.509994
7.893956 - 7.64046

# extract the fitted values of the polynomial season effect
eggv_mod_date_fits <- 
  as.data.frame(effect("poly(first_laydate, 2)", stats_eggv_mod$mod_poly, 
                       xlevels = list(first_laydate = seq(min(ceuta_egg_chick_female_data$jul_lay_date_std_num), 
                                                          max(ceuta_egg_chick_female_data$jul_lay_date_std_num), 1))))

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
             aes(x = jul_lay_date_std_num, y = volume_cm),
             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_date_fits, aes(x = first_laydate, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = eggv_mod_date_fits, aes(x = first_laydate, 
                                             ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
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

Season_plot2 <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot / renesting_date_dist_plot / renesting_date_mod_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(3.5, 1.5, 7, 1.5, 3.5), c('cm', 'cm', 'cm')),
              widths = unit(c(7, 7, 7, 7, 7), c('cm', 'cm', 'cm')))

Season_plot2

Season_plot3 <-
  (polyandry_date_mod_plot / renesting_date_mod_plot / eggv_date_mod_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(4.5, 4.5, 7), c('cm', 'cm', 'cm')),
              widths = unit(c(7, 7, 7), c('cm', 'cm', 'cm')))

Season_plot3

# write plot to disk
ggsave(plot = Season_plot2,
       filename = "products/figures/Figure_5.png",
       width = 7 * 1.3,
       height = sum(c(3.5, 1.5, 7, 1.5, 3.5)) * 1.3, units = "cm", dpi = 600)

#### Trend plot of egg volume over age ----
# extract fitted values
eggv_mod_age_fits <- 
  as.data.frame(effect(term = "poly(est_age_t_deviation, 2)", mod = mod_eggv_poly, #stats_eggv_mod$mod_poly, 
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
              aes(x = est_age_t_deviation + 1, y = volume_cm),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_age_fits, aes(x = est_age_t_deviation + 1, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = eggv_mod_age_fits,
              aes(x = est_age_t_deviation + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Years since first breeding") +
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

# summary of fitted trend
laydate_mod_age_fits %>% 
  summarise(min_laydate_fit = min(fit),
            max_laydate_fit = max(fit),
            min_laydate_age = est_age_t_deviation[which.min(fit)],
            max_laydate_age = est_age_t_deviation[which.max(fit)],
            min_laydate_lower = lower[which.min(fit)],
            min_laydate_upper = upper[which.min(fit)],
            max_laydate_lower = lower[which.max(fit)],
            max_laydate_upper = upper[which.max(fit)])

(laydate_mod_age_fits[1, 2] - laydate_mod_age_fits[6, 2])/5
(laydate_mod_age_fits[1, 4] - laydate_mod_age_fits[6, 4])/5
(laydate_mod_age_fits[1, 5] - laydate_mod_age_fits[6, 5])/5

# plot predicted trend and raw data
date_age_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.ticks.y = element_blank()) +#,
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank()) +
  geom_jitter(data = first_nests_age_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age_t_deviation, y = first_laydate),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = laydate_mod_age_fits, aes(x = est_age_t_deviation, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = laydate_mod_age_fits, 
              aes(x = est_age_t_deviation, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Years since first local breeding attempt") +
  scale_x_continuous(limits = c(-0.5, 12.5), breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  annotate(geom = "text", y = 55, x = 0,
           label = "First nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# extract fitted values
laydate_mod_rec_fits <- 
  as.data.frame(effect(term = "age_first_cap", mod = stats_laydate_mod$mod_poly, 
                       xlevels = list(age_first_cap = c("A", "J")))) %>%
  mutate(age_first_cap_plot = ifelse(age_first_cap == "J", 1.8, 1.2))

plot_palette_recruit <- brewer.pal(6, "Dark2")[c(2,3)]

laydate_mod_rec_fits[1, 2] - laydate_mod_rec_fits[2, 2] 
laydate_mod_rec_fits[1, 4] - laydate_mod_rec_fits[2, 4] 
laydate_mod_rec_fits[1, 5] - laydate_mod_rec_fits[2, 5] 

date_recruit_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = first_nests_age_data,
               aes(x = age_first_cap_plot, y = first_laydate,
                   group = age_first_cap, fill = age_first_cap),
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_errorbar(data = laydate_mod_rec_fits, 
                aes(x = age_first_cap_plot, ymax = upper, ymin = lower),
                alpha = 1, color = "black", width = 0.05, lwd = 0.5) + 
  geom_point(data = laydate_mod_rec_fits, 
             aes(x = age_first_cap_plot, y = fit, fill = age_first_cap),
             lwd = 1, shape = 21, color= "black") +
  geom_jitter(data = first_nests_age_data, 
                aes(x = age_first_cap, y = first_laydate, 
                    group = age_first_cap, 
                    fill = age_first_cap, color = age_first_cap), 
                width = 0.02, alpha = 0.2, shape = 19) +
  luke_theme +
  theme(legend.position = "none",
        panel.border = element_blank(),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.x = element_text(size = 11)) +
  scale_y_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  scale_x_discrete(labels = c("A" = "Immigrant",
                              "J" = "Local\nrecruit")) +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Origin") +
  scale_color_manual(values = rev(plot_palette_recruit)) +
  scale_fill_manual(values = rev(plot_palette_recruit))# +
  # annotate(geom = "text", x = 0.5, y = 58,
  #          label = "First nests of the season",
  #          color = "black", size = 3, fontface = 'italic', hjust = 0)

date_recruit_plot

#### Combo plot of age dynamics ----
Age_plot <-
  (date_age_trend_plot | date_recruit_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 7), c('cm', 'cm')),
              widths = unit(c(7, 4), c('cm', 'cm')))
Age_plot

# write plot to disk
ggsave(plot = Age_plot,
       filename = "products/figures/svg/Age_plot.svg",
       width = 6,
       height = 4, units = "in")

ggsave(plot = Age_plot,
       filename = "products/figures/jpg/Figure_4.jpg",
       width = 6,
       height = 3.5, units = "in")

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
            max_eggv_upper = upper[which.max(fit)]) %>% 
  mutate(diff = max_eggv_fit - min_eggv_fit,
         diff_upper = max_eggv_upper - min_eggv_lower,
         diff_lower = max_eggv_lower - min_eggv_upper) 

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
  xlab("Mother tarsus length (mm)") +
  scale_x_continuous(limits = c(22, 27))

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
  xlab("Estimated age (years)") +
  scale_x_continuous(limits = c(22, 27)) +
  scale_y_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60))
  

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
       filename = "products/figures/jpg/Figure_S7.jpg",
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

#### Residuals plot ----
load("output/stats_eggv_mod.rds")
load("output/stats_laydate_mod.rds")
load("output/stats_polyandry_age_mod.rds")

eggv_residuals_plot <- 
  ggplot(augment(stats_eggv_mod$mod_poly), aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.4, shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "grey") +
  ylab("Residual") +
  xlab("Fitted value") +
  luke_theme

qqnorm(resid(stats_eggv_mod$mod_poly))
qqmath(stats_eggv_mod$mod_poly, id=0.025)

laydate_residuals_plot <- 
  ggplot(augment(stats_laydate_mod$mod_poly), aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.4, shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "grey") +
  ylab("Residual") +
  xlab("Fitted value") +
  luke_theme

qqnorm(resid(stats_laydate_mod$mod_poly))
qqmath(stats_laydate_mod$mod_poly, id=0.025)

residuals_combo_plot <- 
  (eggv_residuals_plot / laydate_residuals_plot) +
  plot_annotation(tag_levels = 'A')

residuals_combo_plot

ggsave(plot = residuals_combo_plot,
       filename = "products/figures/jpg/Figure_S6.jpg",
       width = 4.5,
       height = 9, units = "in")
