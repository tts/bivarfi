library(geofi)
library(janitor)
library(pxweb)
library(tidyr)
library(dplyr)
library(purrr)
library(sf)

# https://ropengov.github.io/geofi/index.html
# https://ropengov.github.io/geofi/articles/geofi_joining_attribute_data.html

mun <- get_municipalities(year = 2020)

pxweb_query_list <-
  list("Alue 2020"=c("*"),
       "Tiedot"=c("*"),
       "Vuosi"=c("2019"))

px_raw <-
  pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/Kuntien_avainluvut/2020/kuntien_avainluvut_2020_aikasarja.px",
            query = pxweb_query_list)

px_data <- as_tibble(
  as.data.frame(px_raw, 
                column.name.type = "text", 
                variable.value.type = "text")
) %>% setNames(make_clean_names(names(.))) %>% 
  pivot_longer(names_to = "information", values_to = "municipal_key_figures", 3:ncol(.))

# Join with municipality_name_fi leaves out (at least some of) those with Swedish as the majority language
map_data <- right_join(mun, 
                       px_data, 
                       by = c("municipality_name_en" = "region_2020"))

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
  filter(!is.na(municipal_key_figures)) %>% # Newest work-related ones are from 2018
  group_by(nimi, information) %>%
  mutate(mean_val = mean(municipal_key_figures)) %>%
  select(-municipal_key_figures) %>%
  ungroup() %>%
  spread(information, mean_val)

saveRDS(pop_info, "data_m.RDS")

# Province

map_data_p <- map_data %>% 
  select(maakunta_name_fi, information, municipal_key_figures)

pop_p <- get_municipality_pop(year = 2020) %>%  
  dplyr::group_by(maakunta_name_fi) %>%
  summarise(vaesto = sum(vaesto),
           miehet = sum(miehet)) %>%
  mutate(share_of_men = miehet/vaesto*100,
         share_of_women = 100 - share_of_men)

map_data_p <- st_drop_geometry(map_data_p)
map_pop_p <- inner_join(pop_p, map_data_p)

pop_info_p <- map_pop_p %>% 
  select(maakunta_name_fi, starts_with("share"), information, municipal_key_figures) %>% 
  rename(nimi = maakunta_name_fi) %>% 
  filter(!is.na(municipal_key_figures)) %>% 
  group_by(nimi, information) %>%
  mutate(mean_val = mean(municipal_key_figures)) %>%
  select(-municipal_key_figures) %>%
  filter(!duplicated(cbind(nimi, information)))  %>%
  spread(information, mean_val)
  
saveRDS(pop_info_p, "data_p.RDS")

