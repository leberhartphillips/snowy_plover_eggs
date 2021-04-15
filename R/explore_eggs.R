#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

load("data/ceuta_egg_chick_female_data.rds")

#### Egg dimension plots ----
egg_widths_plot <- 
  ceuta_egg_chick_female_data %>% 
  ggplot() + 
  geom_boxplot(aes(x = width/10, y = 465), 
               fill = brewer.pal(8, "Set1")[c(2)], 
               color = brewer.pal(8, "Set1")[c(2)],
               width = 18, alpha = 0.5) +
  geom_jitter(aes(x = width/10, y = 428), 
              fill = brewer.pal(8, "Set1")[c(2)], 
              color = brewer.pal(8, "Set1")[c(2)],
              height = 8, alpha = 0.1) +
  geom_histogram(alpha = 0.5, aes(width/10), 
                 fill = brewer.pal(8, "Set1")[c(2)], 
                 # color = "white",
                 binwidth = 0.02) +
  luke_theme +
  theme(axis.title.x = element_text(hjust = 0.4),
        panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0.5, "cm")) +
  ylab("Number of eggs") +
  xlab("Egg width (cm)") +
  scale_x_reverse(limits = c(25/10, 20/10),
                  breaks = c(seq(from = 2, to = 2.4, by = 0.1))) +
  scale_y_continuous(limits = c(0, 475),
                     breaks = c(0, 100, 200, 300), position = "right") +
  coord_flip()

ggsave(plot = egg_widths_plot,
       filename = "products/figures/egg_widths_plot.png",
       width = 11,
       height = 8, units = "cm")

egg_lengths_plot <- 
  ceuta_egg_chick_female_data %>% 
  ggplot() + 
  geom_boxplot(aes(x = length/10, y = 275), 
               fill = brewer.pal(8, "Set1")[c(2)], 
               color = brewer.pal(8, "Set1")[c(2)],
               width = 10, alpha = 0.5) +
  geom_jitter(aes(x = length/10, y = 255), 
              fill = brewer.pal(8, "Set1")[c(2)], 
              color = brewer.pal(8, "Set1")[c(2)],
              height = 4, alpha = 0.1) +
  geom_histogram(alpha = 0.5, aes(length/10), 
                 fill = brewer.pal(8, "Set1")[c(2)], 
                 # color = "white",
                 binwidth = 0.02) +
  luke_theme +
  theme(axis.title.y = element_text(hjust = 0.7),
        axis.title.x = element_text(vjust = -1),
        panel.border = element_blank(),
        plot.margin = margin(0, 0, 0.5, 0, "cm")) +
  ylab("Number of eggs") +
  xlab("Egg length (cm)") +
  scale_x_continuous(limits = c(27/10, 35/10),
                     breaks = c(2.8, 3, 3.2, 3.4)) +
  scale_y_reverse(limits = c(280, 0),
                  breaks = c(0, 50, 100, 150, 200))

ggsave(plot = egg_lengths_plot,
       filename = "products/figures/egg_lengths_plot.png",
       width = 11,
       height = 9, units = "cm")

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

#### Exploratory plots of chick vs. egg ----
# plot the relationship between egg size and chick size
tarsus_v_length_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_length, y = avg_chick_tarsus),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_tarsus, x = avg_egg_length, 
                     xmin = avg_egg_length - sd_egg_length, 
                     xmax = avg_egg_length + sd_egg_length), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_tarsus, x = avg_egg_length, 
                    ymin = avg_chick_tarsus - sd_chick_tarsus, 
                    ymax = avg_chick_tarsus + sd_chick_tarsus), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_length, y = avg_chick_tarsus), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank()) +
  ylab("Avg. chick tarsus length\n(mm ± 1 SD)") +
  xlab("Avg. egg length\n(cm ± 1 SD)")

tarsus_v_width_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_width, y = avg_chick_tarsus),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_tarsus, x = avg_egg_width, 
                     xmin = avg_egg_width - sd_egg_width, 
                     xmax = avg_egg_width + sd_egg_width), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_tarsus, x = avg_egg_width, 
                    ymin = avg_chick_tarsus - sd_chick_tarsus, 
                    ymax = avg_chick_tarsus + sd_chick_tarsus), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_width, y = avg_chick_tarsus), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylab("Avg. chick tarsus length\n(mm ± 1 SD)") +
  xlab("Avg. egg width\n(cm ± 1 SD)")

tarsus_v_volume_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_volume, y = avg_chick_tarsus),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_tarsus, x = avg_egg_volume, 
                     xmin = avg_egg_volume - sd_egg_volume, 
                     xmax = avg_egg_volume + sd_egg_volume), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_tarsus, x = avg_egg_volume, 
                    ymin = avg_chick_tarsus - sd_chick_tarsus, 
                    ymax = avg_chick_tarsus + sd_chick_tarsus), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_volume, y = avg_chick_tarsus), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylab("Avg. chick tarsus length\n(mm ± 1 SD)") +
  xlab(expression(paste("Avg. egg volume\n(cm", ''^{3}, "± 1 SD)", sep = "")))

bill_v_length_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_length, y = avg_chick_bill),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_bill, x = avg_egg_length, 
                     xmin = avg_egg_length - sd_egg_length, 
                     xmax = avg_egg_length + sd_egg_length), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_bill, x = avg_egg_length, 
                    ymin = avg_chick_bill - sd_chick_bill, 
                    ymax = avg_chick_bill + sd_chick_bill), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_length, y = avg_chick_bill), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank()) +
  ylab("Avg. chick bill length\n(mm ± 1 SD)") +
  xlab("Avg. egg length\n(cm ± 1 SD)")

bill_v_width_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_width, y = avg_chick_bill),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_bill, x = avg_egg_width, 
                     xmin = avg_egg_width - sd_egg_width, 
                     xmax = avg_egg_width + sd_egg_width), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_bill, x = avg_egg_width, 
                    ymin = avg_chick_bill - sd_chick_bill, 
                    ymax = avg_chick_bill + sd_chick_bill), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_width, y = avg_chick_bill), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylab("Avg. chick bill length\n(mm ± 1 SD)") +
  xlab("Avg. egg width\n(cm ± 1 SD)")

bill_v_volume_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_volume, y = avg_chick_bill),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_bill, x = avg_egg_volume, 
                     xmin = avg_egg_volume - sd_egg_volume, 
                     xmax = avg_egg_volume + sd_egg_volume), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_bill, x = avg_egg_volume, 
                    ymin = avg_chick_bill - sd_chick_bill, 
                    ymax = avg_chick_bill + sd_chick_bill), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_volume, y = avg_chick_bill), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylab("Avg. chick bill length\n(mm ± 1 SD)") +
  xlab(expression(paste("Avg. egg volume\n(cm", ''^{3}, "± 1 SD)", sep = "")))

weight_v_length_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_length, y = avg_chick_weight),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_weight, x = avg_egg_length, 
                     xmin = avg_egg_length - sd_egg_length, 
                     xmax = avg_egg_length + sd_egg_length), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_weight, x = avg_egg_length, 
                    ymin = avg_chick_weight - sd_chick_weight, 
                    ymax = avg_chick_weight + sd_chick_weight), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_length, y = avg_chick_weight), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank()) +
  ylab("Avg. chick weight\n(g ± 1 SD)") +
  xlab("Avg. egg length\n(cm ± 1 SD)")

weight_v_width_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_width, y = avg_chick_weight),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_weight, x = avg_egg_width, 
                     xmin = avg_egg_width - sd_egg_width, 
                     xmax = avg_egg_width + sd_egg_width), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_weight, x = avg_egg_width, 
                    ymin = avg_chick_weight - sd_chick_weight, 
                    ymax = avg_chick_weight + sd_chick_weight), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_width, y = avg_chick_weight), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylab("Avg. chick weight\n(g ± 1 SD)") +
  xlab("Avg. egg width\n(cm ± 1 SD)")

weight_v_volume_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_volume, y = avg_chick_weight),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_weight, x = avg_egg_volume, 
                     xmin = avg_egg_volume - sd_egg_volume, 
                     xmax = avg_egg_volume + sd_egg_volume), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_weight, x = avg_egg_volume, 
                    ymin = avg_chick_weight - sd_chick_weight, 
                    ymax = avg_chick_weight + sd_chick_weight), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_volume, y = avg_chick_weight), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylab("Avg. chick weight\n(g ± 1 SD)") +
  xlab(expression(paste("Avg. egg volume\n(cm", ''^{3}, "± 1 SD)", sep = "")))

BMI_v_width_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_width, y = avg_chick_BMI),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_BMI, x = avg_egg_width, 
                     xmin = avg_egg_width - sd_egg_width, 
                     xmax = avg_egg_width + sd_egg_width), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_BMI, x = avg_egg_width, 
                    ymin = avg_chick_BMI - sd_chick_BMI, 
                    ymax = avg_chick_BMI + sd_chick_BMI), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_width, y = avg_chick_BMI), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.y = element_blank()) +
  ylab("Avg. chick BMI\n(g ± 1 SD)") +
  xlab("Avg. egg width\n(cm ± 1 SD)")

BMI_v_length_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_length, y = avg_chick_BMI),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_BMI, x = avg_egg_length, 
                     xmin = avg_egg_length - sd_egg_length, 
                     xmax = avg_egg_length + sd_egg_length), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_BMI, x = avg_egg_length, 
                    ymin = avg_chick_BMI - sd_chick_BMI, 
                    ymax = avg_chick_BMI + sd_chick_BMI), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_length, y = avg_chick_BMI), 
              method = lm, se = TRUE) +
  luke_theme +
  ylab("Avg. chick BMI\n(± 1 SD)") +
  xlab("Avg. egg length\n(cm ± 1 SD)")

BMI_v_volume_plot <- 
  ggplot(data = eggs_and_chicks_nest_summary) +
  geom_point(aes(x = avg_egg_volume, y = avg_chick_BMI),
             alpha = 0.5) +
  geom_errorbarh(aes(y = avg_chick_BMI, x = avg_egg_volume, 
                     xmin = avg_egg_volume - sd_egg_volume, 
                     xmax = avg_egg_volume + sd_egg_volume), 
                 alpha = 0.3, size = 0.8, linetype = "solid") +
  geom_errorbar(aes(y = avg_chick_BMI, x = avg_egg_volume, 
                    ymin = avg_chick_BMI - sd_chick_BMI, 
                    ymax = avg_chick_BMI + sd_chick_BMI), 
                alpha = 0.2, size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = avg_egg_volume, y = avg_chick_BMI), 
              method = lm, se = TRUE) +
  luke_theme +
  theme(axis.title.y = element_blank()) +
  ylab("Avg. chick BMI\n(g ± 1 SD)") +
  xlab(expression(paste("Avg. egg volume (cm", ''^{3}, "± 1 SD)", sep = "")))

# patchwork plot of all egg and chick morphometric combinations
# qualitative interpretation: 
# chick weight is best predicted by egg morphometrics
chick_egg_plot <- 
  (tarsus_v_length_plot + 
     tarsus_v_width_plot + 
     tarsus_v_volume_plot) / 
  (bill_v_length_plot +
     bill_v_width_plot +
     bill_v_volume_plot) /
  (weight_v_length_plot +
     weight_v_width_plot +
     weight_v_volume_plot) /
  (BMI_v_length_plot +
     BMI_v_width_plot +
     BMI_v_volume_plot) + 
  plot_annotation(tag_levels = 'A')

chick_egg_plot

ggsave(plot = chick_egg_plot,
       filename = "products/figures/chick_egg_plot.png",
       width = 25,
       height = 25, units = "cm")

 # check distributions of chick weight and egg volume
hist(eggs_and_chicks_nest_summary$avg_chick_weight)
hist(eggs_and_chicks_nest_summary$avg_egg_length)
hist(eggs_and_chicks_nest_summary$avg_egg_width)
hist(eggs_and_chicks_nest_summary$avg_egg_volume)

#### Model relationship between egg volume and chick weight ----
# Procedure:
# linear mixed effects regression of chick weight ~ egg volume with mother ID and
# year as random effects 
# mod_chickw_eggv <- 
#   lmer(avg_chick_weight ~ avg_egg_volume +
#          (1|mother_ring) + (1|year), 
#        data = dplyr::filter(eggs_and_chicks_nest_summary, 
#                             !is.na(avg_chick_weight)))
# 
# # run tidy bootstrap to obtain model diagnostics
# tidy_chickw_eggv <-
#   tidy(mod_chickw_eggv, conf.int = TRUE, conf.method = "boot", nsim = 1000)
# 
# # run rptR to obtain repeatabilities of random effects
# rpt_chickw_eggv <-
#   rpt(avg_chick_weight ~ avg_egg_volume +
#         (1|mother_ring) + (1|year), 
#       grname = c("mother_ring", "year", "Fixed"), 
#       data = dplyr::filter(eggs_and_chicks_nest_summary, !is.na(avg_chick_weight)), 
#       datatype = "Gaussian", 
#       nboot = 1000, npermut = 1000, ratio = TRUE,
#       adjusted = FALSE, ncores = 4, parallel = TRUE)
# 
# # run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# # weights
# R2_chickw_eggv <- 
#   partR2(mod_chickw_eggv,  
#          partvars = c("avg_egg_volume"), 
#          R2_type = "marginal", nboot = 1000, CI = 0.95, max_level = 1)
# 
# # save model, tidy, rptR, and partR2 output as a list
# stats_chickw_eggv <- 
#   list(mod = mod_chickw_eggv,
#        tidy = tidy_chickw_eggv,
#        rptR = rpt_chickw_eggv,
#        partR2 = R2_chickw_eggv)
# 
# save(stats_chickw_eggv,
#      file = "output/stats_chickw_eggv.rds")

load("output/stats_chickw_eggv.rds")

# Marginal R2
stats_chickw_eggv$partR2$R2

# Parameter estimates
stats_chickw_eggv$partR2$Ests

# Repeatabilities
stats_chickw_eggv$rptR

# extract fitted values of chick weight v egg volume model
mod_chickw_eggv_fits <- 
  as.data.frame(effect(term = "avg_egg_volume", mod = mod_chickw_eggv, 
                       xlevels = list(avg_chick_weight = seq(min(eggs_and_chicks_nest_summary[, "avg_chick_weight"], na.rm = TRUE),
                                                             max(eggs_and_chicks_nest_summary[, "avg_chick_weight"], na.rm = TRUE), 0.01))))
# model summary a diagnostics
summary(mod_chickw_eggv)
plot(allEffects(mod_chickw_eggv))
coefplot2(mod_chickw_eggv)
summary(glht(mod_chickw_eggv))

#### Manuscript plot: chick weight v egg volume ----
chickw_eggv_plot <-
  ggplot() +
  geom_errorbarh(data = eggs_and_chicks_nest_summary,
                 aes(y = avg_chick_weight, x = avg_egg_volume/1000, 
                     xmin = avg_egg_volume/1000 - sd_egg_volume/1000, 
                     xmax = avg_egg_volume/1000 + sd_egg_volume/1000), 
                 alpha = 0.3, size = 0.5, linetype = "solid",
                 color = brewer.pal(8, "Set1")[c(2)]) +
  geom_errorbar(data = eggs_and_chicks_nest_summary,
                aes(y = avg_chick_weight, x = avg_egg_volume/1000, 
                    ymin = avg_chick_weight - sd_chick_weight, 
                    ymax = avg_chick_weight + sd_chick_weight), 
                alpha = 0.2, size = 0.5, linetype = "solid",
                color = brewer.pal(8, "Set1")[c(2)]) +
  geom_point(data = eggs_and_chicks_nest_summary,
             aes(x = avg_egg_volume/1000, y = avg_chick_weight),
             alpha = 0.4,
             shape = 19, #21, 
             color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = mod_chickw_eggv_fits, aes(x = avg_egg_volume/1000, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = mod_chickw_eggv_fits, aes(x = avg_egg_volume/1000, 
                                               ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0.5, "cm"),
        axis.title.y = element_text(vjust = 5)) +
  scale_y_continuous(limits = c(min(eggs_and_chicks_nest_summary$avg_chick_weight, na.rm = TRUE), 
                                max(eggs_and_chicks_nest_summary$avg_chick_weight, na.rm = TRUE) * 1.05)) +
  ylab("Avg. chick weight at hatch (g)") +
  xlab(expression(paste("Avg. egg volume (cm", ''^{3}, ")", sep = ""))) 

ggsave(plot = chickw_eggv_plot,
       filename = "products/figures/chickw_eggv_plot.png",
       width = 5.29*2,
       height = 5.29*2, units = "cm")

#### Modeling egg width ----
# same model structure as above but predicting egg width
mod_eggw_age_date_tarsi <- 
  lmer(width_cm ~ poly(est_age, 2) + firstage + lastage + avg_ad_tarsi +
         poly(jul_lay_date_std_num, 2) +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data)

# run tidy bootstrap to obtain model diagnostics
tidy_eggw_age_date_tarsi <-
  tidy(mod_eggw_age_date_tarsi, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
rpt_eggw_age_date_tarsi <-
  rpt(width_cm ~ poly(est_age, 2) + firstage + lastage + avg_ad_tarsi +
        poly(jul_lay_date_std_num, 2) +
        (1|ID) + (1|ring) + (1|year),
      grname = c("ID", "ring", "year", "Fixed"),
      data = ceuta_egg_chick_female_data,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = FALSE, ncores = 4, parallel = TRUE)

# run rptR to obtain repeatabilities of random effects
R2_eggw_age_date_tarsi <-
  partR2(mod_eggw_age_date_tarsi,
         partvars = c("poly(est_age, 2)",
                      "poly(jul_lay_date_std_num, 2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_eggw_age_date_tarsi <- 
  list(mod = mod_eggw_age_date_tarsi,
       tidy = tidy_eggw_age_date_tarsi,
       rptR = rpt_eggw_age_date_tarsi,
       partR2 = R2_eggw_age_date_tarsi)

save(stats_eggw_age_date_tarsi,
     file = "output/stats_eggw_age_date_tarsi.rds")

# model summary a diagnostics
summary(eggw_age_date_tarsi_mod)
plot(allEffects(eggw_age_date_tarsi_mod))
coefplot2(eggw_age_date_tarsi_mod)
summary(glht(eggw_age_date_tarsi_mod))

#### Modeling egg length ----
mod_eggl_age_date_tarsi <- 
  lmer(length_cm ~ poly(est_age, 2) + firstage + lastage + avg_ad_tarsi +
         poly(jul_lay_date_std_num, 2) +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data)

# run tidy bootstrap to obtain model diagnostics
tidy_eggl_age_date_tarsi <-
  tidy(mod_eggl_age_date_tarsi, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
rpt_eggl_age_date_tarsi <-
  rpt(length_cm ~ poly(est_age, 2) + firstage + lastage + avg_ad_tarsi +
        poly(jul_lay_date_std_num, 2) +
        (1|ID) + (1|ring) + (1|year),
      grname = c("ID", "ring", "year", "Fixed"),
      data = ceuta_egg_chick_female_data,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = FALSE, ncores = 4, parallel = TRUE)

# run rptR to obtain repeatabilities of random effects
R2_eggl_age_date_tarsi <-
  partR2(mod_eggl_age_date_tarsi,
         partvars = c("poly(est_age, 2)",
                      "poly(jul_lay_date_std_num, 2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_eggl_age_date_tarsi <- 
  list(mod = mod_eggl_age_date_tarsi,
       tidy = tidy_eggl_age_date_tarsi,
       rptR = rpt_eggl_age_date_tarsi,
       partR2 = R2_eggl_age_date_tarsi)

save(stats_eggl_age_date_tarsi,
     file = "output/stats_eggl_age_date_tarsi.rds")

# model summary a diagnostics
summary(eggl_age_date_tarsi_mod)
plot(allEffects(eggl_age_date_tarsi_mod))
coefplot2(eggl_age_date_tarsi_mod)
summary(glht(eggl_age_date_tarsi_mod))

#### Repeatabilities of egg morphometrics (Table) ----
## ---- load_rpt_out --------
load("output/stats_eggv_age_date_tarsi.rds")
load("output/stats_eggw_age_date_tarsi.rds")
load("output/stats_eggl_age_date_tarsi.rds")

eggv_mod_rpt_R <- 
  cbind(t(eggv_mod_rpt$R), eggv_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggv_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Volume")

eggw_mod_rpt_R <- 
  cbind(t(eggw_mod_rpt$R), eggw_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggw_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Width")

eggl_mod_rpt_R <- 
  cbind(t(eggl_mod_rpt$R), eggl_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggl_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Length")

egg_shape_rpt_R <- 
  bind_rows(eggv_mod_rpt_R, eggw_mod_rpt_R, eggl_mod_rpt_R)

egg_shape_rpt_R %>% 
  dplyr::filter(group != "Fixed") %>% 
  mutate(group = ifelse(group == "ring", "Individual", 
                        ifelse(group == "ID", "Nest",
                               ifelse(group == "year", "Year", "marginal R@2~")))) %>% 
  mutate(confid_int = ifelse(!is.na(lower95),
                             paste0("[", 
                                    round(lower95, 3), ", ", 
                                    round(upper95, 3), "]"),
                             NA),
         mean = round(mean_estimate, 3)) %>% 
  dplyr::select(trait, group, mean, confid_int) %>% 
  gt(rowname_col = "row",
     groupname_col = "trait") %>% 
  cols_label(group = "Random effect",
             mean = "Adjusted repeatability",
             confid_int = "95% confidence interval") %>% 
  tab_options(row_group.font.weight = "bold",
              row_group.background.color = brewer.pal(9,"Greys")[3],
              table.font.size = 12,
              data_row.padding = 3,
              row_group.padding = 4,
              summary_row.padding = 2,
              column_labels.font.size = 14,
              row_group.font.size = 12,
              table.width = pct(80)) %>% 
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      str_replace_all(x,
                      pattern = "@",
                      replacement = "<sup>") %>% 
        str_replace_all("~",
                        "</sup>") }
  )

#### Forest Plots of Marginal R2 egg morphometrics models ####
## ---- load_R2_out --------
load("output/stats_eggv_age_date_tarsi.rds")
load("output/stats_eggw_age_date_tarsi.rds")
load("output/stats_eggl_age_date_tarsi.rds")

## ---- eggv model ---------
eggv_mod_out = eggv_mod_R2[["R2"]]
col_all <- "#2E3440"

eggv_mod_out[eggv_mod_out$term == "Full", 1] <- "Model"
names(eggv_mod_out) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
eggv_mod_out <- 
  eggv_mod_out %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)", "Quadratic age", 
                        ifelse(combs == "poly(jul_std_date, 2)", "Quadratic lay date",
                               ifelse(combs == "firstage", "First age breeding",
                                      ifelse(combs == "lastage", "Last age breeding", "Model")))))
eggv_mod_out$combs <- factor(eggv_mod_out$combs, levels = rev(eggv_mod_out$combs))

eggv_mod_R2_plot <- 
  eggv_mod_R2[["R2"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)", "Senescence", 
                       ifelse(term == "poly(jul_std_date, 2)", "Seasonality",
                              ifelse(term == "firstage", "First-age breeding",
                                     ifelse(term == "lastage", "Last-age breeding", "Model"))))) %>% 
  mutate(term = factor(term, levels = rev(c("Model", "Senescence", "First-age breeding", "Last-age breeding", "Seasonality")))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_text(color = col_all, size = 11, face = "italic"),
        axis.text = element_text(color = col_all), 
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25),
        plot.title = element_text(color = col_all, size = 11, hjust = 0.5, face = "italic")) + 
  xlab(expression(paste("R", ''^{2}, sep = ""))) +
  ylab("Volume model") +
  ggtitle(expression(paste(italic("Partitioned R"), ''^{2}, sep = ""))) +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(0, 0.11))

## ---- eggl model ---------
eggl_mod_out = eggl_mod_R2[["R2"]]
col_all <- "#2E3440"

eggl_mod_out[eggl_mod_out$term == "Full", 1] <- "Model"
names(eggl_mod_out) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
eggl_mod_out <- 
  eggl_mod_out %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)", "Quadratic age", 
                        ifelse(combs == "poly(jul_std_date, 2)", "Quadratic lay date",
                               ifelse(combs == "firstage", "First age breeding",
                                      ifelse(combs == "lastage", "Last age breeding", "Model")))))
eggl_mod_out$combs <- factor(eggl_mod_out$combs, levels = rev(eggl_mod_out$combs))

eggl_mod_R2_plot <- 
  eggl_mod_R2[["R2"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)", "Senescence", 
                       ifelse(term == "poly(jul_std_date, 2)", "Seasonality",
                              ifelse(term == "firstage", "First-age breeding",
                                     ifelse(term == "lastage", "Last-age breeding", "Model"))))) %>% 
  mutate(term = factor(term, levels = rev(c("Model", "Senescence", "First-age breeding", "Last-age breeding", "Seasonality")))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_text(color = col_all, size = 11, face = "italic"),
        axis.text = element_text(color = col_all), 
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25)) + 
  xlab(expression(paste("R", ''^{2}, sep = ""))) +
  ylab("Length model") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(0, 0.11))

## ---- eggw model ---------
eggw_mod_out = eggw_mod_R2[["R2"]]
col_all <- "#2E3440"

eggw_mod_out[eggw_mod_out$term == "Full", 1] <- "Model"
names(eggw_mod_out) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
eggw_mod_out <- 
  eggw_mod_out %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)", "Quadratic age", 
                        ifelse(combs == "poly(jul_std_date, 2)", "Quadratic lay date",
                               ifelse(combs == "firstage", "First age breeding",
                                      ifelse(combs == "lastage", "Last age breeding", "Model")))))
eggw_mod_out$combs <- factor(eggw_mod_out$combs, levels = rev(eggw_mod_out$combs))

eggw_mod_R2_plot <- 
  eggw_mod_R2[["R2"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)", "Senescence", 
                       ifelse(term == "poly(jul_std_date, 2)", "Seasonality",
                              ifelse(term == "firstage", "First-age breeding",
                                     ifelse(term == "lastage", "Last-age breeding", "Model"))))) %>% 
  mutate(term = factor(term, levels = rev(c("Model", "Senescence", "First-age breeding", "Last-age breeding", "Seasonality")))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_text(color = col_all, size = 11, face = "italic"),
        axis.text = element_text(color = col_all), 
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25)) + 
  xlab("Variance explained") +
  ylab("Width model")  +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(0, 0.11))

## ---- combo R2 plot --------
egg_shape_R2_plot <- 
  eggv_mod_R2_plot / eggl_mod_R2_plot / eggw_mod_R2_plot + 
  plot_annotation(tag_levels = "A")
egg_shape_R2_plot


#### Forest Plots of egg morphometrics model estimates ####
## ---- load_R2_out --------
load("output/stats_eggv_age_date_tarsi.rds")
load("output/stats_eggw_age_date_tarsi.rds")
load("output/stats_eggl_age_date_tarsi.rds")

## ---- eggv model ---------
eggv_mod_out_ests = eggv_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggv_mod_out_ests[eggv_mod_out_ests$term == "Full", 1] <- "Model"
names(eggv_mod_out_ests) <- c("combs", "pe", "CI_lower", "CI_upper")
eggv_mod_out_ests <- 
  eggv_mod_out_ests %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggv_mod_out_ests$combs <- factor(eggv_mod_out_ests$combs, levels = rev(eggv_mod_out_ests$combs))

eggv_mod_ests_plot <- 
  eggv_mod_R2[["Ests"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_text(color = col_all), 
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11),
        plot.title = element_text(color = col_all, size = 11, hjust = 0.5, face = "italic")) + 
  xlab(expression(paste("Volume (mm", ''^{3},")", sep = ""))) +
  ggtitle("Model estimates") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) #+
# scale_x_continuous(limits = c(0, 0.11))

## ---- eggl model ---------
eggl_mod_out_ests = eggl_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggl_mod_out_ests[eggl_mod_out_ests$term == "Full", 1] <- "Model"
names(eggl_mod_out_ests) <- c("combs", "pe", "CI_lower", "CI_upper")
eggl_mod_out_ests <- 
  eggl_mod_out_ests %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggl_mod_out_ests$combs <- factor(eggl_mod_out_ests$combs, levels = rev(eggl_mod_out_ests$combs))

eggl_mod_ests_plot <- 
  eggl_mod_R2[["Ests"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_text(color = col_all), 
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11)) + 
  xlab("Length (mm)") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) #+
# scale_x_continuous(limits = c(0, 0.11))

## ---- eggw model ---------
eggw_mod_out_ests = eggw_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggw_mod_out_ests[eggw_mod_out_ests$term == "Full", 1] <- "Model"
names(eggw_mod_out_ests) <- c("combs", "pe", "CI_lower", "CI_upper")
eggw_mod_out_ests <- 
  eggw_mod_out_ests %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggw_mod_out_ests$combs <- factor(eggw_mod_out_ests$combs, levels = rev(eggw_mod_out_ests$combs))

eggw_mod_ests_plot <- 
  eggw_mod_R2[["Ests"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_text(color = col_all), 
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11)) + 
  xlab("Width (mm)") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) #+
# scale_x_continuous(limits = c(0, 0.11))

## ---- combo ests plot --------
egg_shape_ests_plot <- 
  eggv_mod_ests_plot / eggl_mod_ests_plot / eggw_mod_ests_plot + 
  plot_annotation(tag_levels = "A")
egg_shape_ests_plot

#### Forest Plots of egg morphometrics beta weights (standardized estimates) ####
## ---- load_R2_out --------
load("output/stats_eggv_age_date_tarsi.rds")
load("output/stats_eggw_age_date_tarsi.rds")
load("output/stats_eggl_age_date_tarsi.rds")

## ---- eggv model ---------
eggv_mod_out_BW = eggv_mod_R2[["BW"]]
col_all <- "#2E3440"

eggv_mod_out_BW[eggv_mod_out_BW$term == "Full", 1] <- "Model"
names(eggv_mod_out_BW) <- c("combs", "pe", "CI_lower", "CI_upper")
eggv_mod_out_BW <- 
  eggv_mod_out_BW %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggv_mod_out_BW$combs <- factor(eggv_mod_out_BW$combs, levels = rev(eggv_mod_out_BW$combs))

eggv_mod_BW_plot <- 
  eggv_mod_R2[["BW"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(color = col_all), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25),
        plot.title = element_text(color = col_all, size = 11, hjust = 0.5, face = "italic")) + 
  xlab("Standardized\nvolume") +
  ggtitle("Standardized\nestimates") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(-0.17, 0.22))

## ---- eggl model ---------
eggl_mod_out_BW = eggl_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggl_mod_out_BW[eggl_mod_out_BW$term == "Full", 1] <- "Model"
names(eggl_mod_out_BW) <- c("combs", "pe", "CI_lower", "CI_upper")
eggl_mod_out_BW <- 
  eggl_mod_out_BW %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggl_mod_out_BW$combs <- factor(eggl_mod_out_BW$combs, levels = rev(eggl_mod_out_BW$combs))

eggl_mod_BW_plot <- 
  eggl_mod_R2[["BW"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(color = col_all), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25)) + 
  xlab("Standardized\nlength") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(-0.17, 0.22))

## ---- eggw model ---------
eggw_mod_out_BW = eggw_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggw_mod_out_BW[eggw_mod_out_BW$term == "Full", 1] <- "Model"
names(eggw_mod_out_BW) <- c("combs", "pe", "CI_lower", "CI_upper")
eggw_mod_out_BW <- 
  eggw_mod_out_BW %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggw_mod_out_BW$combs <- factor(eggw_mod_out_BW$combs, levels = rev(eggw_mod_out_BW$combs))

eggw_mod_BW_plot <- 
  eggw_mod_R2[["BW"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(color = col_all), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25)) + 
  xlab("Standardized\nwidth") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(-0.17, 0.22))

## ---- combo BW plot --------
egg_shape_BW_plot <- 
  eggv_mod_BW_plot / eggl_mod_BW_plot / eggw_mod_BW_plot + 
  plot_annotation(tag_levels = "A")
egg_shape_BW_plot

#### Full forest plot of model results ---- 
egg_shape_plot <- 
  (eggv_mod_R2_plot + eggv_mod_ests_plot + eggv_mod_BW_plot) /
  (eggl_mod_R2_plot + eggl_mod_ests_plot + eggl_mod_BW_plot) /
  (eggw_mod_R2_plot + eggw_mod_ests_plot + eggw_mod_BW_plot) +
  plot_annotation(tag_levels = "A")
egg_shape_plot