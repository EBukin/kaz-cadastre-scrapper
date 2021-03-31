# Kvartals indexes harvester



library(pacman)
pacman::p_load(sf, here, fs, tidyverse, stringi, jsonlite, 
               httr, RCurl, progress, rgdal, glue)
force_reharvest <- FALSE

source("R/find-plots-calls-processors.R")


# Data to start from --------------------------

rayons_index <-
  read_rds(here(
    "data-clean",
    "02-rayon-shapes",
    "kaz-rayons-shapes_2021-03-31-13-24-01.rds"
  ))


layer_rayon_index <- 
  rayons_index %>% 
  st_drop_geometry() %>% 
  select(leyer_id, obl_id, rayon_id, obl_rus, raj_rus )



index_kaz_adm <- 
  here("data-clean", "01-layers-index-clean.csv") %>% 
  read_csv() %>% 
  left_join(layer_rayon_index)



# Layers of interest ---------------------------------

# to_harvest <- 
#   index_kaz_adm %>% 
#   filter(obl_id == "03", rayon_id %in% c("323","044","050"))
# 
# raw_base_fldr <- "~/kaz-cad-raw/" %>% normalizePath()


## Harvesting all kvartals id -----------------------------------------


raw_base_fldr = "~/kaz-cad-raw/" %>% normalizePath()

layer_interm_folder <-
  file.path(
    raw_base_fldr,
    str_c("layer-", one_rayon$id, "-obl-", one_line$obl_id)
  ) %>% normalizePath()


all_rayons <- 
  layer_interm_folder %>% 
  file.path("all-rayons") %>% 
  fs::dir_info() %>% 
  filter(basename(path) %>% str_detect("id")) %>% 
  arrange(desc(modification_time   )) %>% 
  slice(1) %>% 
  pull(path) %>% 
  read_rds()


one_rayon <- all_rayons %>% slice(1)

kvartal_call <- 
  "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText={obl_id}{actual_ray_id}{base_digit}{new_digit}&contains=true&searchFields=KAD_NOMER&layers={leyer_id}&returnGeometry=true&f=pjson"

one_digit_kvartal <- 
  get_one_digit_geometries(one_rayon, kvartal_call)


two_dig_harv <- 
  one_digit_kvartal %>% 
  filter(response_success ) %>% 
  unnest(response_geo_attrs ) %>%
  filter(!empty) %>%
  mutate(base_digit = str_c(base_digit, new_digit)) %>%
  select(any_of(names(one_rayon))) %>%
  distinct() 

pbkv2 <- progress::progress_bar$new(#
  total = nrow(two_dig_harv),
  force = FALSE,
  format = "two-dig-kvartal :spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta")

two_digit_kvartal <-
  two_dig_harv %>% 
  pmap(~{
    pbkv2$tick()
    the_line <- rlang::dots_list(...) %>% as_tibble()
    get_one_digit_geometries(the_line, kvartal_call)
  }) %>%  
  bind_rows()

three_dig_harv <- 
  two_digit_kvartal %>% 
  filter(response_success ) %>% 
  unnest(response_geo_attrs ) %>%
  filter(!empty) %>%
  mutate(base_digit = str_c(base_digit, new_digit)) %>%
  select(any_of(names(one_rayon))) %>%
  distinct() 

pbkv3 <- progress::progress_bar$new(#
  total = nrow(three_dig_harv),
  force = FALSE,
  format = "three-dig-kvartal :spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta")

three_digit_kvartal <- 
  three_dig_harv %>% 
  pmap(~{
    pbkv3$tick()
    the_line <- rlang::dots_list(...) %>% as_tibble()
    get_one_digit_geometries(the_line, kvartal_call)
  }) %>%  
  bind_rows()


three_digit_kvartal_index <- 
  three_digit_kvartal %>% 
  filter(response_success ) %>% 
  unnest(response_geo_attrs ) %>%
  filter(!empty) %>%
  mutate(actual_kvartal_id = str_c(base_digit, new_digit)) %>%
  select(id, leyer_id, name, obl_id, rayon_id, actual_ray_id, "actual_kvartal_id") %>% 
  distinct() %>% 
  arrange(actual_ray_id , actual_kvartal_id)


all_kvartals_raw <-
  one_digit_kvartal %>%
  bind_rows(two_digit_kvartal)%>%
  bind_rows(three_digit_kvartal)

all_kvartals <-
  all_kvartals_raw %>%
  unnest(response_geo_attrs) %>%
  mutate(actual_kvartal_id = str_sub(KAD_NOMER, 6,8)) %>%
  filter(!is.na(actual_ray_id)) %>% 
  select(id, leyer_id, name, obl_id, rayon_id, actual_ray_id, "actual_kvartal_id") %>%
  distinct() %>% 
  arrange(actual_ray_id , actual_kvartal_id)


all_kvartal_fldr <- file.path(layer_interm_folder, "all-kvartals") %>% normalizePath()
if (!fs::dir_exists(all_kvartal_fldr)) fs::dir_create(all_kvartal_fldr)

all_kvartal_file <-
  all_kvartal_fldr %>%
  file.path(str_c(
    "rayon-", one_rayon$actual_ray_id,"-all-kvartal-id",
    ".rds"
  )) %>%
  normalizePath()

write_rds(all_kvartals, all_kvartal_file)

all_kvartal_harvested_data_file <-
  all_kvartal_fldr %>%
  file.path(str_c(
    "rayon-", one_rayon$actual_ray_id,"-all-kvartal-id",
    ".rds"
  )) %>%
  normalizePath()

write_rds(all_kvartals_raw, all_kvartal_harvested_data_file, compress = "gz")





## Harvesting all plots kvartals id -----------------------------------------

all_kvartals <- 
  layer_interm_folder %>% 
  file.path("all-kvartals") %>% 
  fs::dir_info() %>% 
  filter(basename(path) %>% str_detect("id")) %>% 
  arrange(desc(modification_time   )) %>% 
  slice(1) %>% 
  pull(path) %>% 
  read_rds()
