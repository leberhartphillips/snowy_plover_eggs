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
  