# Model the relationship between liklihood of polyandry and lay date of first 
# breeding attempt

# Product: stats and figure of the polyandry ~ lay date model

#### Libraries and data ----
source("R/001_libraries.R")
source("R/002_functions.R")

load("data/ceuta_egg_chick_female_data.rds")

#### Modeling ----
# Modeling the relationship between mating behavior and initiation date
# of first breeding attempt

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

# Procedure:
# binomial mixed effects regression of polyandry ~ lay date with mother ID and
# year as random effects
# mod_poly_date <-
#   glmer(cbind(poly, mono) ~ jul_lay_date_std_num +
#           (1|ring) + (1|year),
#         data = first_nests_data, family = "binomial")
# 
# # run tidy bootstrap to obtain model diagnostics
# tidy_poly_date <-
#   tidy(mod_poly_date, conf.int = TRUE, conf.method = "boot", nsim = 1000)
# 
# # run rptR to obtain repeatabilities of random effects
rpt_poly_date <-
  rpt(poly ~ jul_lay_date_std_num +
        (1|ring) + (1|year),
      grname = c("ring", "year", "Fixed"),
      data = first_nests_data,
      datatype = "Binary",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)
# 
# # run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# # weights
# R2m_poly_date <-
#   partR2(mod_poly_date,
#          partvars = c("jul_lay_date_std_num"),
#          R2_type = "marginal",
#          nboot = 1000, CI = 0.95, max_level = 1)
# 
# R2c_poly_date <-
#   partR2(mod_poly_date,
#          partvars = c("jul_lay_date_std_num"),
#          R2_type = "conditional",
#          nboot = 1000, CI = 0.95, max_level = 1)
# 
# # save model, tidy, rptR, and partR2 output as a list
# stats_poly_date <-
#   list(mod = mod_poly_date,
#        tidy = tidy_poly_date,
#        rptR = rpt_poly_date,
#        partR2c = R2m_poly_date,
#        partR2c = R2c_poly_date)
# 
# save(stats_poly_date,
#      file = "output/stats_poly_date.rds")

# load the saved results
load("output/stats_poly_date.rds")

# Marginal R2
stats_poly_date$partR2$R2

# Parameter estimates
stats_poly_date$partR2$Ests

# Repeatabilities
bind_cols(as.data.frame(t(stats_poly_date$rptR$R)), 
          stats_poly_date$rptR$CI_emp$CI_org) %>% 
  dplyr::select(-2)

# model summary a diagnostics
summary(stats_poly_date$mod)
plot(allEffects(stats_poly_date$mod))
coefplot2(stats_poly_date$mod)
summary(glht(stats_poly_date$mod))

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
offs_jul_lay_date_std2 <- 
  seq(min(first_nests_data$jul_lay_date_std_num, na.rm = TRUE), 
      max(first_nests_data$jul_lay_date_std_num, na.rm = TRUE), 1)

# apply the offs vector to the function (retuning a matrix)
poly_fits <- sapply(offs_jul_lay_date_std2, poly_mod_fits)

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
