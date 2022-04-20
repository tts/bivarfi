library(geofi)
library(janitor)
library(pxweb)
library(tidyverse)
library(sf)

# https://ropengov.github.io/geofi/index.html
# https://ropengov.github.io/geofi/articles/geofi_joining_attribute_data.html

mun <- get_municipalities(year = 2020)

pxweb_query_list <-
  list("Alue 2020"=c("*"),
       "Tiedot"=c("*"),
       "Vuosi"=c("2019"))

# Get statistics by region (=municipality)
px_raw <-
  pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/Kuntien_avainluvut/2020/kuntien_avainluvut_2020_aikasarja.px",
            query = pxweb_query_list)

px_data <- as_tibble(
  as.data.frame(px_raw, 
                column.name.type = "text", 
                variable.value.type = "text")
) %>% setNames(make_clean_names(names(.))) %>% 
  pivot_longer(names_to = "information", values_to = "municipal_key_figures", 3:ncol(.))

map_data <- right_join(mun, 
                       px_data, 
                       by = c("municipality_name_fi" = "region_2020"))
# Filter not-NA's
map_data <- map_data %>% 
  filter(!is.na(municipal_key_figures))

# Get population, and summarise by county
pop <- get_municipality_pop(year = 2020) %>%  
  dplyr::group_by(hyvinvointialue_name_fi) %>%
  summarise(vaesto = sum(vaesto),
            miehet = sum(miehet)) %>% 
  mutate(share_of_men = miehet/vaesto*100,
         share_of_women = 100 - share_of_men)

# Drop geometry so that join is possible
map_data <- st_drop_geometry(map_data)
map_data_pop <- inner_join(pop, map_data)

pop_info <- map_data_pop %>% 
  select(hyvinvointialue_name_fi, starts_with("share"), information, municipal_key_figures) 

saveRDS(pop_info, "map_data.RDS")
