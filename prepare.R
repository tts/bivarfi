library(geofi)
library(janitor)
library(pxweb)
library(tidyr)
library(dplyr)
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

# Note that join with municipality_name_fi leaves out (at least some of) those with Swedish as the majority language
map_data <- right_join(mun, 
                       px_data, 
                       by = c("municipality_name_en" = "region_2020"))

# Municipality

map_data_m <- map_data %>% 
  select(nimi, information, municipal_key_figures)

pop_m <- get_municipality_pop(year = 2020) %>% 
  dplyr::group_by(nimi) %>% 
  mutate(share_of_men = miehet/vaesto*100,
         share_of_women = 100 - share_of_men) 

# Drop geometry so that join is possible
map_data_m <- st_drop_geometry(map_data_m)
map_pop_m <- inner_join(pop_m, map_data_m)

pop_info <- map_pop_m %>% 
  select(nimi, starts_with("share"), municipal_key_figures, information) 

pop_info <- pop_info %>% 
  filter(!is.na(municipal_key_figures))

saveRDS(pop_info, "data_m.RDS")

# County

map_data_c <- map_data %>% 
  select(hyvinvointialue_name_fi, information, municipal_key_figures)

pop_c <- get_municipality_pop(year = 2020) %>%  
  dplyr::group_by(hyvinvointialue_name_fi) %>%
  summarise(vaesto = sum(vaesto),
           miehet = sum(miehet)) %>%
  mutate(share_of_men = miehet/vaesto*100,
         share_of_women = 100 - share_of_men)

# Drop geometry so that join is possible
map_data_c <- st_drop_geometry(map_data_c)
map_pop_c <- inner_join(pop_c, map_data_c)

pop_info <- map_pop_c %>% 
  select(hyvinvointialue_name_fi, starts_with("share"), information, municipal_key_figures) 

pop_info <- pop_info %>% 
  filter(!is.na(municipal_key_figures))

saveRDS(pop_info, "data_c.RDS")

