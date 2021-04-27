# Harvesting lots metadata

library(pacman)
pacman::p_load(sf, here, fs, tidyverse, stringi, jsonlite, 
               httr, RCurl, progress, rgdal, glue)
force_reharvest <- FALSE

source("R/find-plots-calls-processors.R")




# Geometries --------------------------------------------------------------

ray_geoms <- 
  here("data-clean",
       "02-rayon-shapes",
       "kaz-rayons-shapes_2021-03-31-13-24-01.rds") %>% 
  read_rds() %>% 
  filter(rayon_id %in% c("323","044","050")) %>% 
  select(leyer_id, obl_id, rayon_id, obl_rus, rayon_rus = raj_rus) %>% 
  st_drop_geometry() %>% 
  as_tibble()

plot_geoms <-
  here("data-clean",
       "05-plots-shapes",
       "kaz-all-plots-shapes-clean.rds") %>%
  read_rds() %>%
  inner_join(tibble(obl_id = "03", rayon_id = c("323", "044", '050')))

plot_metadata <-
  here("data-clean", "06-plots-metadata-clean.rds") %>%
  read_rds() %>%
  select(cadastre_id, contains("rus_"), -rus_NA, -contains("kadastrovyj")) %>% 
  rename_at(vars(contains("rus")), ~str_c("meta_", str_replace(., "rus_", "")))


plot_export_clean <-
  plot_geoms %>%
  mutate(area_ha = Shape_Area / 10000) %>% 
  select(cadastre_id,
         leyer_id = layerId,
         obl_id,
         rayon_id,
         actual_rayon_id,
         kvartal_id,
         area_ha,
         api_nazv = NAZV,
         api_category_rus = CATEGORY_RUS,
         api_pravo_rus = PRAVO_RUS,
         api_tsn_rus = TSN_RUS
         ) %>% 
  left_join(ray_geoms, by = c("leyer_id", "obl_id", "rayon_id")) %>% 
  left_join(plot_metadata, by = "cadastre_id") %>% 
  select(
    cadastre_id:kvartal_id, 
    area_ha,
    api_nazv, 
    obl_rus:rayon_rus,
    contains("api_"),
    contains("meta_")
    )




# Saving data -------------------------------------------------------------

plot_export_clean %>%
  select(cadastre_id ) %>%
  st_write(
    here(
      "data-clean",
      "10.1-sarah-request-clean",
      "plot-geom-shp",
      "plot-geom.shp"
    ),
    delete_layer = TRUE
  )

plot_export_clean %>%
  select(cadastre_id ) %>% 
  st_write(
    here(
      "data-clean",
      "10.1-sarah-request-clean",
      "plot-geom-api-meta-shp",
      "plot-geom-api-meta.shp"
    ),
    delete_layer = TRUE
  )

plot_export_clean %>% 
  write_rds(
    here(
      "data-clean",
      "10.1-sarah-request-clean",
      "plot-geom-api-meta.rds"
    ), 
    compress = "gz"
  )




plot_export_clean %>% 
  select(cadastre_id ) %>% 
  write_rds(
    here(
      "data-clean",
      "10.1-sarah-request-clean",
      "plot-geom.rds"
    ), 
    compress = "gz"
  )


plot_export_clean %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  write_rds(
    here(
      "data-clean",
      "10.1-sarah-request-clean",
      "plot-api-meta.rds"
    ), 
    compress = "gz"
  )


plot_export_clean %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  write_csv(
    here(
      "data-clean",
      "10.1-sarah-request-clean",
      "plot-api-meta.csv"
    )
  )

plot_export_clean %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  write_rds(
    here(
      "data-clean",
      "10.1-sarah-request-clean",
      "plot-api-meta.rds"
    ), compress = "gz"
  )



