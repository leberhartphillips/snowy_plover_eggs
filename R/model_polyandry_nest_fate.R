# Model the relationship between liklihood of polyandry and lay date of first 
# breeding attempt

# Product: stats and figure of the polyandry ~ lay date model

#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")

poly_fate_data <-
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(nest_order == 2) %>% 
  select(ID, ring, year, nest_1_fate, first_laydate, n_mates, n_nests, polyandry, multiclutch) %>% 
  distinct() %>% 
  filter(nest_1_fate != "Unknown") %>% 
  mutate(success = ifelse(nest_1_fate == "Hatch", "success", "fail")) %>% 
  mutate(polyandry = as.factor(polyandry)) %>%
  mutate(poly = ifelse(polyandry == "poly", 1, 0),
         mono = ifelse(polyandry == "mono", 1, 0)) %>%
  mutate(poly_plot = ifelse(poly == 1, poly + 0.1, poly - 0.1))

# Procedure:
# binomial mixed effects regression of multiclutch ~ lay date with mother ID and
# year as random effects
mod_ploy_fate <-
  glmer(cbind(poly, mono) ~ success +
          (1|ring) + (1|year),
        data = poly_fate_data, family = "binomial")

mod_ploy_fate <-
  glm(cbind(poly, mono) ~ success,
      data = poly_fate_data, family = "binomial")

model_parameters(mod_ploy_fate, standardize = "refit")
random_parameters(mod_ploy_fate)
plot(allEffects(mod_ploy_fate))

#### Plotting of Figure ----
# extract fitted values
fate_mod_fits <- function(offs) {
  model <- lme4::glmer(cbind(poly, mono) ~ 
                         I(success - offs) +
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

poly_fate_mod_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = poly_fate_data, 
               aes(x = success, y = poly_plot, 
                   group = multiclutch, fill = multiclutch), 
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_jitter(data = renesting_data, 
              aes(x = first_laydate, y = poly, 
                  group = multiclutch, 
                  fill = multiclutch, color = multiclutch), 
              height = 0.02, alpha = 0.4, shape = 19) +
  geom_ribbon(data = renesting_fits, 
              aes(x = first_laydate, y = Mean, ymin = Lower, ymax = Upper), 
              fill = "grey50", alpha = 0.25) +
  geom_line(data = renesting_fits, 
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
  scale_x_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  ylab("Probabilty of renesting after failure ± 95% CI") +
  scale_color_manual(values = plot_palette_polyandry,
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Multiple", "Single")) +
  scale_fill_manual(values = plot_palette_polyandry,
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Multiple", "Single")) +
  annotate(geom = "text", y = 1.2, x = -58,
           label = "Lay dates for first nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

renesting_date_mod_plot