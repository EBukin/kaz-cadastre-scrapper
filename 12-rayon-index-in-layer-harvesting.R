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



# Harvesting all rayons IDs in one layer ------------------------------------


# geom_call <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText={obl_id}{base_digit}{new_digit}&contains=true&searchFields=KAD_NOMER&layers={leyer_id}&returnGeometry=true&f=pjson"
# one_line <- to_harvest %>% slice(2) %>% mutate(base_digit = "")
# 

get_rayons_for_one_line <-
  function(one_line,
           geom_call,
           raw_base_fldr = "~/kaz-cad-raw/" ) {
    intem_file_name <-
      str_c("layer-", one_line$id, "_obl-", one_line$obl_id, ".rds")
    
    
    # Uncomment to reharvest rayons
    one_digit_geo <-
      get_one_digit_geometries(one_line, geom_call)
    
    
    pb <- progress::progress_bar$new(#
      total = 10,
      force = FALSE,
      format = "two-dig-rayons :spin :current/:total (dig-:sttep) [:bar] :percent in :elapsedfull ETA: :eta")
    
    
    two_digit_geo <-
      one_digit_geo %>%
      mutate(base_digit = str_c(base_digit, new_digit)) %>%
      select(any_of(names(one_line))) %>%
      distinct() %>%
      pmap( ~ {
        pb$tick()
        the_line <- rlang::dots_list(...) %>% as_tibble()
        get_one_digit_geometries(the_line, geom_call)
      }) %>%
      bind_rows()
    
    all_rayons_raw <-
      two_digit_geo %>%
      bind_rows(one_digit_geo)
    
    all_rayons <-
      all_rayons_raw %>%
      unnest(response_geo_attrs) %>%
      mutate(actual_ray_id = str_sub(KAD_NOMER, 3, 5)) %>%
      distinct(actual_ray_id) %>%
      filter(!is.na(actual_ray_id))
      pmap_dfr( ~ {
        rlang::dots_list(...) %>%
          as_tibble() %>%
          bind_cols(one_line)
      })
    
    
    all_ray_file <-
      file.path(raw_base_fldr,
                "all-rayons-index",
                intem_file_name) %>%
      normalizePath()
    
    write_rds(all_rayons, all_ray_file)
    
    harvested_data_file <-
      file.path(raw_base_fldr,
                "all-rayons-raw",
                intem_file_name) %>%
      normalizePath()
    
    write_rds(all_rayons_raw, harvested_data_file, compress = "gz")
    
  }


# Actual harvesting of the rayon indexes
harvested_layers <-
  "~/kaz-cad-raw/all-rayons-index/" %>% 
  list.files() %>% 
  tibble(name = .) %>% 
    tidyr::separate(name, c("code1", "layer", "code2", "obl"))
  
  
ray_call <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText={obl_id}{base_digit}{new_digit}&contains=true&searchFields=KAD_NOMER&layers={leyer_id}&returnGeometry=true&f=pjson"

index_kaz_adm %>%
  filter(obl_id == "03", !id %in% as.numeric(harvested_layers$layer)) %>%
  mutate(base_digit = "") %>%
  mutate(n = row_number()) %>% 
  group_by(n) %>% 
  nest() %>% 
  pull(data) %>% 
  walk(~{
    cat("new run + \n")
    .x %>%
      get_rayons_for_one_line(ray_call)
  })
  

# # Same harvesting in parallel
# parallel_rayons_ids <- function(lines_to_harvest) {
#   ray_call <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText={obl_id}{base_digit}{new_digit}&contains=true&searchFields=KAD_NOMER&layers={leyer_id}&returnGeometry=true&f=pjson"
#   
#   p <- progressor(steps = nrow(lines_to_harvest))
#   lines_to_harvest %>%
#     furrr::future_pmap( ~ {
#       rlang::dots_list(...) %>%
#         as_tibble() %>%
#         get_rayons_for_one_line(ray_call)
#     })
# }
# 
# 
# library(progressr)
# library(furrr)
# 
# handlers(global = TRUE)
# handlers(list(
#   handler_progress(
#     format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
#     width    = 80,
#     complete = "+"
#   )
# ))
# 
# plan(multisession)
# 
# index_kaz_adm %>%
#   filter(obl_id == "03",!id %in% as.numeric(harvested_layers$layer)) %>%
#   mutate(base_digit = "") %>%
#   parallel_rayons_ids()


# Saving all rayon indexes by oblast in one file ------------------

"~/kaz-cad-raw/all-rayons-index/" %>% 
  list.files(full.names = T) %>%  
  map_dfr(~read_rds(.x)) %>% 
  filter(!is.na(actual_ray_id)) %>% 
  mutate(obl_id2 = obl_id ) %>% 
  group_by(obl_id2) %>% 
  nest() %>% 
  purrr::pwalk( ~ {
    write_rds(.y,
              here("data-clean", "03-rayon-indexes", str_c("obl-", .x, '.rds')),
              compress = "gz")
  })

