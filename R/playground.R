ceuta_egg_chick_female_data %>% 
  # dplyr::filter(avg_ad_tarsi < 22) %>% 
  dplyr::select(ring, avg_ad_tarsi, n_ad_tarsi, sd_ad_tarsi) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = avg_ad_tarsi, y = sd_ad_tarsi)) +
  geom_point() +
  geom_smooth(method = "lm")

ceuta_egg_chick_female_data %>% 
  # dplyr::filter(sd_ad_tarsi < 1) %>%
  dplyr::select(ring, avg_ad_tarsi, volume_cm) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = avg_ad_tarsi, y = volume_cm)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(volume_cm ~ avg_ad_tarsi, data = dplyr::filter(ceuta_egg_chick_female_data, sd_ad_tarsi < 1))
lm(volume_cm ~ avg_ad_tarsi, data = ceuta_egg_chick_female_data)

questionable_tarsi_variation <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(sd_ad_tarsi > 1) %>% 
  dplyr::select(ring, avg_ad_tarsi, n_ad_tarsi, sd_ad_tarsi) %>% 
  distinct() %>% 
  arrange(desc(sd_ad_tarsi))

library(tidyverse)

# connect to CeutaCLOSED
CeutaCLOSED <- 
  dbConnect(SQLite(), 
            dbname = "/Users/Luke/Documents/Academic_Projects/Postdoc_Seewiesen/Ceuta_Open/Ceuta_CLOSED/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v2-0-0.sqlite")

# chick measurement is included
dbReadTable(CeutaCLOSED, "Captures") %>% 
  filter(ring %in% questionable_tarsi_variation$ring) %>% 
  dplyr::select(ring, age) %>% 
  distinct() %>% 
  group_by(ring) %>% 
  summarise(n_ages = n_distinct(age)) %>% 
  arrange(desc(n_ages))

# dbReadTable(CeutaCLOSED, "Captures") %>% 
  ceuta_egg_chick_female_data %>% 
  filter(ring == "CN0517") #%>% 
  plover_date_convert(input = "Rdate")
  
  
junk <- 
  data.frame(ind = c("A", "B", "C"),
             first_date = c(10, 11, 15),
             second_date = c(20, 18, 30),
             first_vol = c(5, 6, 10),
             second_vol = c(15, 13, 16)) %>% 
  mutate(within_dev_app = second_date - first_date) %>% 
  pivot_longer(c(first_date, second_date, first_vol, second_vol), names_to = c("nest_order", "egg_vol"), values_to = c("date", "volume"))

ceuta_egg_chick_female_data_mc <- 
  ceuta_egg_chick_female_data %>% 
  select(ID, ring_year, jul_lay_date_std_num, jul_lay_date) %>%
  distinct() %>% 
  arrange(ring_year, jul_lay_date) %>% 
  group_by(ring_year) %>% 
  mutate(date_deviation_w = jul_lay_date_std_num - jul_lay_date_std_num[which.min(jul_lay_date_std_num)],
         first_date_b = jul_lay_date_std_num[which.min(jul_lay_date_std_num)],
         last_date_b = jul_lay_date_std_num[which.max(jul_lay_date_std_num)]) %>% 
  bind_cols(., demean(., select = c("jul_lay_date_std_num"), group = "ring_year")) %>% 
  ungroup() %>% 
  mutate(first_date_week = cut(first_date_b, 
                               breaks = seq(from = -60, to = 59, by = 7), 
                               labels = FALSE)) %>% 
  mutate(lay_date_week = cut(jul_lay_date_std_num, 
                             breaks = seq(from = -60, to = 59, by = 7), 
                             labels = FALSE)) %>% 
  mutate(week_deviation = lay_date_week - first_date_week) %>% 
  left_join(select(ceuta_egg_chick_female_data, -jul_lay_date_std_num, -jul_lay_date), .,
            by = c("ID", "ring_year")) %>% 
  mutate(estage_deviation_w = est_age_trans - firstage)

# first_date_bin = quantile(ceuta_egg_chick_female_data_mc$first_date, c(0, 1/3, 2/3, 1))
# first_date_bin[1] = first_date_bin[1] - 0.00005
# 
# seq(from = -60, to = 59, by = 7)
# 
# 
# ceuta_egg_chick_female_data_mc <- 
#   ceuta_egg_chick_female_data_mc %>% 
#   mutate(first_date_week = cut(first_date, 
#                               breaks = seq(from = -60, to = 59, by = 7), 
#                               labels = FALSE)) %>% 
#   mutate(lay_date_week = cut(jul_lay_date_std_num, 
#                              breaks = seq(from = -60, to = 59, by = 7), 
#                              labels = FALSE)) %>% 
#   mutate(week_deviation = lay_date_week - first_date_week) %>% 
#   left_join(select(ceuta_egg_chick_female_data, -jul_lay_date_std_num, -jul_lay_date), .,
#             by = c("ID", "ring_year"))
# 
# ceuta_egg_chick_female_data_mc %>% 
#   filter(ring == "CN0056") %>% 
#   select(ID, ring_year, nest_order, first_date_bin, date_deviation, first_date, volume_cm) %>% 
#   arrange(ring_year, ID)

mod_junk <-
  lmer(volume_cm ~ poly(est_age_trans, 2) + 
         firstage + lastage + avg_ad_tarsi + 
         week_deviation + poly(first_date_week, 2) +
         (1|ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data_mc)

mod_junk_week <-
  lmer(volume_cm ~ est_age_trans + I(est_age_trans^2) +
         firstage + lastage + avg_ad_tarsi + 
         week_deviation + first_date_week + I(first_date_week^2) +
         (1|ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data_mc)

mod_junk_real <-
  lmer(volume_cm ~ est_age_trans + I(est_age_trans^2) +
         firstage + lastage + avg_ad_tarsi + 
         date_deviation +
         first_date + I(first_date^2) +
         (1|ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data_mc)

mod_junk_real_poly_mc <-
  lmer(volume_cm ~ poly(estage_deviation_w, 2) +
         firstage + lastage + avg_ad_tarsi + 
         date_deviation_w +
         poly(first_date_b, 2) +
         # poly(last_date_b, 2) +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data_mc)

# model summary a diagnostics
summary(mod_junk_real_poly_mc)
plot(allEffects(mod_junk_real_poly_mc))
coefplot2(mod_junk_real_poly_mc)
summary(glht(mod_junk_real_poly_mc))

model_parameters(mod_junk_real_poly_mc, standardize = "refit")
random_parameters(mod_junk_real_poly_mc)

# extract the fitted values of the polynomial season effect
mod_junk_real_poly_mc_fits <- 
  as.data.frame(effect("date_deviation_w", mod_junk_real_poly_mc, 
                       xlevels = list(date_deviation_w = seq(min(ceuta_egg_chick_female_data_mc$date_deviation_w), 
                                                                 max(ceuta_egg_chick_female_data_mc$date_deviation_w), 1)))) %>%
  mutate(date_deviation_w_adj = date_deviation_w - max(date_deviation_w)/2)
mod_junk_real_poly_mc_fits_1 <- 
  as.data.frame(effect("poly(first_date_b, 2)", mod_junk_real_poly_mc, 
                       xlevels = list(first_date_b = seq(min(ceuta_egg_chick_female_data_mc$first_date_b), 
                                                         max(ceuta_egg_chick_female_data_mc$first_date_b), 1))))
eggv_date_mod_plot_test <- 
  ceuta_egg_chick_female_data_mc %>% 
  group_by(ID) %>% 
  mutate(avg_egg_volume = mean(volume_cm)) %>% 
  ggplot(.) +
  geom_point(aes(x = jul_lay_date_std_num, y = volume_cm),
             shape = 19, color = brewer.pal(8, "Set1")[c(2)], alpha = 0.1) +
  geom_line(aes(x = jul_lay_date_std_num, y = avg_egg_volume, group = ring_year),
            alpha = 0.1) +
  geom_line(data = mod_junk_real_poly_mc_fits, aes(x = date_deviation_w_adj, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_junk_real_poly_mc_fits, aes(x = date_deviation_w_adj,
                                                        ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  geom_line(data = mod_junk_real_poly_mc_fits_1, aes(x = first_date_b, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_junk_real_poly_mc_fits_1, aes(x = first_date_b, 
                                                       ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-60, 60)) +
  annotate(geom = "text", y = 9, x = -58,
           label = "Lay dates for all eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot_test) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

ggsave(plot = Season_plot,
       filename = "products/figures/Season_plot_test.png",
       width = 10,
       height = 20, units = "cm", dpi = 600)

mod_junk_real_poly_nms <-
  lmer(volume_cm ~ poly(est_age_trans, 2) +
         firstage + lastage + avg_ad_tarsi + 
         poly(jul_lay_date_std_num, 2) +
         first_date_b +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data_mc)

# extract the fitted values of the polynomial season effect
mod_junk_real_poly_mc_fits <- 
  as.data.frame(effect("poly(jul_lay_date_std_num, 2)", mod_junk_real_poly_nms, 
                       xlevels = list(jul_lay_date_std_num = seq(min(ceuta_egg_chick_female_data_mc$jul_lay_date_std_num), 
                                                                         max(ceuta_egg_chick_female_data_mc$jul_lay_date_std_num), 1))))
mod_junk_real_poly_mc_fits_1 <- 
  as.data.frame(effect("first_date_b", mod_junk_real_poly_nms, 
                       xlevels = list(first_date_b = seq(min(ceuta_egg_chick_female_data_mc$first_date_b), 
                                                                        max(ceuta_egg_chick_female_data_mc$first_date_b), 1))))
eggv_date_mod_plot_test <- 
  ceuta_egg_chick_female_data_mc %>% 
  group_by(ID) %>% 
  mutate(avg_egg_volume = mean(volume_cm)) %>% 
  ggplot(.) +
  geom_point(aes(x = jul_lay_date_std_num, y = volume_cm),
             shape = 19, color = brewer.pal(8, "Set1")[c(2)], alpha = 0.1) +
  geom_line(aes(x = jul_lay_date_std_num, y = avg_egg_volume, group = ring_year),
            alpha = 0.1) +
  # geom_line(data = mod_junk_real_poly_mc_fits, aes(x = jul_lay_date_std_num, y = fit),
  #           lwd = 0.5, colour = "grey20") +
  # geom_ribbon(data = mod_junk_real_poly_mc_fits, aes(x = jul_lay_date_std_num, 
  #                                                       ymax = upper, ymin = lower),
  #             lwd = 1, fill = "grey20", alpha = 0.25) +
  geom_line(data = mod_junk_real_poly_mc_fits_1, aes(x = first_date_b, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_junk_real_poly_mc_fits_1, aes(x = first_date_b, 
                                                      ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-60, 60)) +
  annotate(geom = "text", y = 9, x = -58,
           label = "Lay dates for all eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot_test) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

ggsave(plot = Season_plot,
       filename = "products/figures/Season_plot_test.png",
       width = 10,
       height = 20, units = "cm", dpi = 600)
  
mod_junk_real_poly_b_w <-
  lmer(volume_cm ~ poly(est_age_trans, 2) +
         firstage + lastage + avg_ad_tarsi + 
         poly(jul_lay_date_std_num_between, 2) +
         jul_lay_date_std_num_within +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data_mc)

# model summary a diagnostics
summary(mod_junk_real_poly_nms)
plot(allEffects(mod_junk_real_poly_b_w))
coefplot2(mod_junk_real_poly_nms)
summary(glht(mod_junk_real_poly_nms))

model_parameters(mod_junk_real_poly_b_w, standardize = "refit")
random_parameters(mod_junk_real_poly_b_w)

# extract the fitted values of the polynomial season effect
mod_junk_real_poly_b_w_fits <- 
  as.data.frame(effect("poly(jul_lay_date_std_num_between, 2)", mod_junk_real_poly_b_w, 
                       xlevels = list(jul_lay_date_std_num_between = seq(min(ceuta_egg_chick_female_data_mc$jul_lay_date_std_num_between), 
                                                       max(ceuta_egg_chick_female_data_mc$jul_lay_date_std_num_between), 1))))
mod_junk_real_poly_b_w_fits_1 <- 
  as.data.frame(effect("jul_lay_date_std_num_within", mod_junk_real_poly_b_w, 
                       xlevels = list(jul_lay_date_std_num_within = seq(min(ceuta_egg_chick_female_data_mc$jul_lay_date_std_num_within), 
                                                                 max(ceuta_egg_chick_female_data_mc$jul_lay_date_std_num_within), 1))))

eggv_date_mod_plot_test <- 
  ceuta_egg_chick_female_data_mc %>% 
  group_by(ID) %>% 
  mutate(avg_egg_volume = mean(volume_cm)) %>% 
  ggplot(.) +
  geom_point(aes(x = jul_lay_date_std_num, y = volume_cm),
             shape = 19, color = brewer.pal(8, "Set1")[c(2)], alpha = 0.1) +
  geom_line(aes(x = jul_lay_date_std_num, y = avg_egg_volume, group = ring_year),
            alpha = 0.1) +
  geom_line(data = mod_junk_real_poly_b_w_fits, aes(x = jul_lay_date_std_num_between, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_junk_real_poly_b_w_fits, aes(x = jul_lay_date_std_num_between,
                                                        ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  geom_line(data = mod_junk_real_poly_b_w_fits_1, aes(x = jul_lay_date_std_num_within, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_junk_real_poly_b_w_fits_1, aes(x = jul_lay_date_std_num_within, 
                                                       ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-60, 60)) +
  annotate(geom = "text", y = 9, x = -58,
           label = "Lay dates for all eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot_test) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

ggsave(plot = Season_plot,
       filename = "products/figures/Season_plot_test.png",
       width = 10,
       height = 20, units = "cm", dpi = 600)

# mod_junk_real_plot <-
  ggplot() +
  geom_point(data = ceuta_egg_chick_female_data_mc, alpha = 0.4,
             aes(x = jul_lay_date_std_num, y = volume_cm), #color = "grey50", shape = 16,
             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = mod_junk_real_fits_1, aes(x = date_deviation_w, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_junk_real_fits_1, aes(x = date_deviation_w, 
                                             ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  geom_line(data = mod_junk_real_fits, aes(x = first_date_b, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_junk_real_fits, aes(x = first_date_b, 
                                             ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  # geom_line(data = mod_junk_real_fits_2, aes(x = last_date_b, y = fit),
  #           lwd = 0.5, colour = "grey20") +
  # geom_ribbon(data = mod_junk_real_fits_2, aes(x = last_date_b, 
  #                                              ymax = upper, ymin = lower),
  #             lwd = 1, fill = "grey20", alpha = 0.25) +
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

mod_junk_real_plot
