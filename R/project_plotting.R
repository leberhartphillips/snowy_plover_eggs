source("R/project_libraries.R")

# define the plotting theme to be used in subsequent ggplots
luke_theme <- 
  theme_bw() +
  theme(
    text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x  = element_text(size = 10), 
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 0.5, colour = "grey40"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey"),
    legend.position = c(0.1, 0.9)
  )

# Find fonts from computer that you want. Use regular expressions to do this
# For example, load all fonts that are 'candara' or 'Candara'
extrafont::font_import(pattern = "[F/f]ranklin", prompt = FALSE)

# check which fonts were loaded
extrafont::fonts()
extrafont::fonttable()
extrafont::loadfonts() # load these into R

# set plotting color palettes
plot_palette_sex <- RColorBrewer::brewer.pal(8, "Dark2")[c(2,1)]
plot_palette_polyandry <- RColorBrewer::brewer.pal(8, "Dark2")[c(6,1)]
plot_palette_nest_order <- c("black", "#f03b20")

# specify the facet labels for each species
polyandry.labs <- c("Polyandrous", "Monogamous")
names(polyandry.labs) <- c("poly", "mono")

# image of a plover egg
egg_image <- image_read("media/plover_egg.png")
egg_image_grob <- rasterGrob(egg_image, interpolate = TRUE, height = 0.6)

# some nice color palettes
show_col(wsj_pal(palette = "rgby")(6))
ggthemes_data$wsj

# color of mean estimate point in forest plots
col_all <- "#2E3440"