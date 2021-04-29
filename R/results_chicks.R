#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

#### Results and data----
load("output/stats_chick_mod.rds")
load("data/ceuta_egg_chick_female_data.rds")

# extract fitted values of chick weight v egg volume model
mod_chickw_eggv_fits <- 
  as.data.frame(effect(term = "avg_egg_volume", mod = stats_chick_mod$mod, 
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
                 aes(y = avg_chick_weight, x = avg_egg_volume, 
                     xmin = avg_egg_volume - sd_egg_volume, 
                     xmax = avg_egg_volume + sd_egg_volume), 
                 alpha = 0.3, size = 0.5, linetype = "solid",
                 color = brewer.pal(8, "Set1")[c(2)]) +
  geom_errorbar(data = eggs_and_chicks_nest_summary,
                aes(y = avg_chick_weight, x = avg_egg_volume, 
                    ymin = avg_chick_weight - sd_chick_weight, 
                    ymax = avg_chick_weight + sd_chick_weight), 
                alpha = 0.2, size = 0.5, linetype = "solid",
                color = brewer.pal(8, "Set1")[c(2)]) +
  geom_point(data = eggs_and_chicks_nest_summary,
             aes(x = avg_egg_volume, y = avg_chick_weight),
             alpha = 0.4,
             shape = 19, #21, 
             color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = mod_chickw_eggv_fits, aes(x = avg_egg_volume, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = mod_chickw_eggv_fits, aes(x = avg_egg_volume, 
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
       filename = "products/figures/chickw_eggv_plot.svg",
       width = 5.29*2,
       height = 5.29*2, units = "cm")