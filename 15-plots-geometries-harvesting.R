# Plots harvester


library(pacman)
pacman::p_load(sf, here, fs, tidyverse, stringi, jsonlite, 
               httr, RCurl, progress, rgdal, glue)
force_reharvest <- FALSE

source("R/find-plots-calls-processors.R")

# Function =============================================

parallel_harvest_one_extra_dig <- function(lines_to_harvest, interm_folder, geometry_call) {
  
  p <- progressor(steps = nrow(lines_to_harvest))
  
  # Non parallel harvest
  lines_to_harvest %>%
    future_pwalk(~ {
      one_line <- rlang::dots_list(...) %>% as_tibble()
      line_file <- here(interm_folder, one_line$file_to)
      line_geometries <-
        0:9 %>%
        map_dfr(~ {
          one_line_base <- 
            one_line %>%
            mutate(new_digit = .x) %>%
            mutate(url = glue(geometry_call)) 
          
          one_line_base %>% 
            bind_cols(
              one_line_base %>% 
              pull(url) %>%
                get_res_geo_attrs
            )
        })
      
      p()
      
       line_geometries %>%
        write_rds(line_file, compress = "gz")
      
    })
  
}

# Generic setups ===================================================

library(progressr)
library(furrr)
handlers(global = TRUE)

handlers(list(
  handler_progress(
    format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
    width    = 80,
    complete = "+"
  )
))

plan(multisession)


find_plots_call_generic <-
  "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText={obl_id}{rayon_id}{base_digit}{new_digit}&contains=true&searchFields=KAD_NOMER&layers={leyer_id}&returnGeometry=true&f=pjson"



# Detecting first digit of a plot ===================================

interm_one_folder <- "~/kaz-cad-raw/plot-1-dig/"

kvartals_digits_clean <-
  read_rds("data-clean/04-kvartal-indexes/obl-03.rds") %>% 
  mutate(rayon_id = actual_ray_id,
         base_digit = actual_kvartal_id) %>% 
  select(leyer_id, obl_id, rayon_id, base_digit) %>% 
  mutate(file_to = glue("{leyer_id}_{obl_id}_{rayon_id}_{base_digit}x.rds")) %>% 
  filter(!file_to %in% list.files(interm_one_folder))

if (nrow(kvartals_digits_clean) > 0) {
  kvartals_digits_clean %>%
    # sample_n(10) %>%
    parallel_harvest_one_extra_dig(interm_one_folder, find_plots_call_generic)
}


# Two-digit + third harvesting  ========================================

interm_two_folder <- "~/kaz-cad-raw/plot-2-dig/"
harvested <- list.files(here(interm_two_folder))

plot_one_level_digits_clean <-
  list.files("~/kaz-cad-raw/plot-1-dig/", full.names = T) %>%
  map_dfr(read_rds) %>% 
  mutate(base_digit = str_c(base_digit, new_digit)) %>% 
  filter(response_n_elements > 0, exceededTransferLimit ) %>% 
    
  select(leyer_id : base_digit) %>% 
  distinct() %>% 
  mutate(
    file_to = glue("{leyer_id}_{obl_id}_{rayon_id}_{base_digit}x.rds")
  ) %>% 
  filter(!file_to %in% harvested)

if (nrow(plot_one_level_digits_clean) > 0) {
  plot_one_level_digits_clean %>% 
    # sample_n(10) %>% 
    parallel_harvest_one_extra_dig(interm_two_folder, find_plots_call_generic)
  
}


# three-digit + four harvesting  ========================================

interm_three_folder <- "~/kaz-cad-raw/plot-3-dig/"
harvested <- list.files(here(interm_three_folder))

plot_two_level_digits_clean <-
  list.files("~/kaz-cad-raw/plot-2-dig/", full.names = T) %>%
  map_dfr(read_rds) %>% 
  mutate(base_digit = str_c(base_digit, new_digit)) %>% 
  filter(response_n_elements > 0, exceededTransferLimit) %>%
  select(leyer_id : base_digit) %>% 
  distinct() %>% 
  mutate(
    file_to = glue("{leyer_id}_{obl_id}_{rayon_id}_{base_digit}x.rds")
  ) %>% 
  filter(!file_to %in% harvested)


if (nrow(plot_two_level_digits_clean) > 0) {
  plot_two_level_digits_clean %>%
    # sample_n(10) %>%
    parallel_harvest_one_extra_dig(interm_three_folder, find_plots_call_generic)
}

# four-digit + five harvesting  ========================================
# 
interm_four_folder <- "~/kaz-cad-raw/plot-4-dig/"
harvested <- list.files(here(interm_four_folder))

plot_three_level_digits_clean <-
  list.files("~/kaz-cad-raw/plot-3-dig/", full.names = T) %>%
  map_dfr(read_rds) %>%
  mutate(base_digit = str_c(base_digit, new_digit)) %>%
  filter(response_n_elements > 0, exceededTransferLimit) %>%
  select(leyer_id : base_digit) %>%
  distinct() %>%
  mutate(
    file_to = glue("{leyer_id}_{obl_id}_{rayon_id}_{base_digit}x.rds")
  ) %>%
  filter(!file_to %in% harvested)


if (nrow(plot_three_level_digits_clean) > 0) {
  plot_three_level_digits_clean %>%
    parallel_harvest_one_extra_dig(interm_four_folder, find_plots_call_generic)
  
}




# list.files("~/kaz-cad-raw/plot-4-dig/", full.names = T) %>%
#   map_dfr(read_rds) %>%
#   mutate(base_digit = str_c(base_digit, new_digit)) %>%
#   # unnest(response_geo_attrs) %>%
#   filter(exceededTransferLimit) %>%
#   select(leyer_id : base_digit, exceededTransferLimit, KAD_NOMER ) %>%
#   distinct()



# Resaving all data in one file =========================================


drop_nonalphanum <-
  . %>%
  stringi::stri_trans_general("Latin-ASCII" ) %>%
  stringi::stri_trans_general("Any-Hex/Unicode" )  %>%
  str_replace_all("U\\+02B9", "") %>%
  stringi::stri_trans_general("Hex-Any/Unicode") %>%
  str_replace_all("[^[:alnum:]_]", "")


translit_kaz <-
  . %>%
  stringi::stri_trans_general( "Kazakh-Latin/BGN") %>%
  stringi::stri_trans_general("Latin-ASCII")


translit_rus <-
  . %>%
  stringi::stri_trans_general( "Cyrillic-Latin/BGN") %>%
  stringi::stri_trans_general("Latin-ASCII")




plots_indx <-
  list(
    interm_one_folder,
    interm_two_folder,
    interm_three_folder,
    interm_four_folder
    ) %>%
  map( ~ list.files(.x, full.names = T)) %>%
  unlist() %>%
  map( ~ .x %>% read_rds() %>% mutate(path = .x))

# # # Remove unsuccessful harvests and 
# plots_indx %>%
#   bind_rows() %>%
#   filter(!response_success ) %>%
#   pull(path) %>%
#   unique() %>% 
#   map(~file.remove(.x))

# plots_indx[[123]]$response_json[[1]]$results[[10]]$geometry$spatialReference$latestWkid
# srs_data <-
#   plots_indx %>%
#   # `[`(30:35) %>%
#   map("response_json") %>%
#   unlist(recursive = FALSE) %>%
#   map("results") %>%
#   keep( ~ !is.null(.x)) %>%
#   unlist(recursive = F) %>%
#   map("geometry") %>%
#   map("spatialReference") %>%
#   map_df( ~ as_tibble(.x))
# srs_data %>% distinct()


# plots_indx %>%
#   bind_rows() %>%
#   filter(!response_success ) %>%
#   pull(path) %>%
#   unique() %>% 
#   map(~file.remove(.x))

# plots_indx %>%
#   bind_rows() %>% 
#   filter(exceededTransferLimit )


extended_index <-
  plots_indx %>%
  map_dfr( ~ .x %>% pull(response_geo_attrs)) %>%
  filter(!empty) %>%
  dplyr::filter(!is.na(KAD_NOMER)) %>% 
  group_by(KAD_NOMER) %>% 
  mutate(n = row_number()) %>% 
  filter(n == 1) %>% 
  ungroup() %>% 
  mutate(
    obl_id = str_sub(layerName, 3, 4),
    rayon_id = str_sub(layerName, 6, 8),
    actual_rayon_id = str_sub(KAD_NOMER, 3, 5),
    kvartal_id = str_sub(KAD_NOMER, 6, 8),
    NAZV = NAZV %>% translit_kaz,
    CATEGORY_RUS = CATEGORY_RUS %>% translit_rus,
    CATEGORY_KAZ  = CATEGORY_KAZ  %>% translit_kaz ,
    PRAVO_RUS   = PRAVO_RUS   %>% translit_rus ,
    PRAVO_KAZ   = PRAVO_KAZ   %>% translit_kaz ,
    TSN_RUS    = TSN_RUS    %>% translit_rus ,
    TSN_KAZ    = TSN_KAZ    %>% translit_kaz ,
    Shape_Area = str_replace_all(Shape_Area, ",", "\\.") %>% as.numeric()
    ) %>% 
  select(
    cadastre_id = KAD_NOMER, 
    layerId, obl_id, rayon_id, actual_rayon_id, kvartal_id, 
    CATEGORY_RUS, CATEGORY_KAZ, PRAVO_RUS, PRAVO_KAZ, TSN_RUS, TSN_KAZ, NAZV,
    Shape_Area, Shape_Length)



# Saving all ==================================

extended_index %>%
  write_rds(
    here(
      "data-clean",
      "05-plots-shapes",
      "kaz-all-plots-shapes-clean.rds"
    ),
    compress = "gz"
  )
#
sarah_export <-
  tibble(obl_id = "03",
         rayon_id = c("323", "044", '050')) %>%
  inner_join(extended_index)


sarah_export %>%
  write_rds(
    here(
      "data-clean",
      "10-sarah-request",
      "kaz-akmol-3-ray-clean.rds"
    ),
    compress = "gz"
  )



sarah_export %>%
  st_write(
    here(
      "data-clean",
      "10-sarah-request",
      "kaz-akmol-3-ray-clean-shp",
      "kaz-akmol-3-ray-clean.shp"
    ), 
    delete_layer = TRUE
  )


# 1 ha and more
sarah_export %>%
  filter(Shape_Area > 10000)  %>%
  write_rds(
    here(
      "data-clean",
      "10-sarah-request",
      "kaz-akmol-3-ray-1ha-clean.rds"
    ),
    compress = "gz"
  )



sarah_export %>%
  filter(Shape_Area > 10000)  %>%
  st_write(
    here(
      "data-clean",
      "10-sarah-request",
      "kaz-akmol-3-ray-1ha-clean-shp",
      "kaz-akmol-3-ray-1ha-clean.shp"
    ),
    delete_layer = TRUE
  )




## Checking what was not harvested compare to the previous attempt =======

old_harvest <-
  read_rds("data-clean/05-SR-request/plots-geometry-3-rayons.rds") %>% 
  select(KAD_NOMER, Shape_Area)


area_missed_second <- 
  old_harvest %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(cadastre_id = KAD_NOMER, Shape_Area) %>% 
  anti_join(
    extended_index %>% 
      select(cadastre_id )
  ) 

area_missed_second


sum(area_missed_second$Shape_Area) / 1000000





# OLD HARVESTING APPROACH -------------------------------------------------
# 
# # Loading all harvested geometries and saving in one file -----------------
# 
# three_dig_harvest <- 
#   list.files(interm_plot_fldr, pattern = "rds", full.names = T) %>% 
#   map_dfr(read_rds)
# 
# # Checing those that have 0 observations (invalid digits)
# # three_dig_harvest %>% 
# #   filter(!response_success) %>% 
# #   sample_n(10) %>% 
# #   pull(response_url)
# 
# exceeded_responces <- 
#   three_dig_harvest%>% 
#   filter(response_success) %>% 
#   filter(exceededTransferLimit) %>% 
#   select(response_geo_attrs ) %>% 
#   unnest(response_geo_attrs )
# 
# 
# interm_plot_fldr_five <- "data-raw/05-SR-request/04-kvartal-4-dig-plots-geometries"
# harvested_five <- list.files(here(interm_plot_fldr_five))
# 
# 
# # # Uncomment this part to reharvest
# # to_reharvest <-
# #   exceeded_responces %>%
# #   select(KAD_NOMER) %>%
# #   mutate(
# #     obl_id = str_sub(KAD_NOMER, 1,2),
# #     rayon_id  = str_sub(KAD_NOMER, 3,5),
# #     base_digit = str_sub(KAD_NOMER, 6,9),
# #     extra_digit = str_sub(KAD_NOMER, 10,10)
# #   ) %>%
# #   select(-KAD_NOMER   ) %>%
# #   distinct() %>%
# #   left_join(four_level_digits_clean) %>%
# #   mutate(base_digit = str_c(base_digit, extra_digit)) %>%
# #   mutate(
# #     file_to =
# #       str_c(obl_id, rayon_id, base_digit, sep = "-") %>%
# #       str_c(., ".rds")
# #   ) %>%
# #   filter(!file_to %in% harvested_five)
# 
# # library(progressr)
# # library(furrr)
# # handlers(global = TRUE)
# # 
# # handlers(list(
# #   handler_progress(
# #     format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
# #     width    = 80,
# #     complete = "+"
# #   )
# # ))
# # plan(multisession)
# # parallel_harvest(to_reharvest, interm_plot_fldr = interm_plot_fldr_five)
# 
# # Checking another level if something was not harvested --------------------
# 
# four_dig_harvest <- 
#   list.files(interm_plot_fldr_five, pattern = "rds", full.names = T) %>% 
#   map_dfr(read_rds)
# 
# four_dig_harvest %>% filter(exceededTransferLimit)
# 
# 
# interm_plot_fldr_six <- "data-raw/05-SR-request/04-kvartal-6-dig-plots-geometries"
# harvested_six <- list.files(here(interm_plot_fldr_five))
# 
# 
# # # Uncomment to re-harvest
# # to_reharvest_again <-
# #   four_dig_harvest%>% 
# #   filter(response_success) %>% 
# #   filter(exceededTransferLimit) %>% 
# #   select(response_geo_attrs ) %>% 
# #   unnest(response_geo_attrs ) %>%
# #   select(KAD_NOMER) %>%
# #   mutate(
# #     obl_id = str_sub(KAD_NOMER, 1,2),
# #     rayon_id  = str_sub(KAD_NOMER, 3,5),
# #     base_digit = str_sub(KAD_NOMER, 6,9),
# #     extra_digit = str_sub(KAD_NOMER, 10,11)
# #   ) %>%
# #   select(-KAD_NOMER   ) %>%
# #   distinct() %>%
# #   left_join(four_level_digits_clean) %>%
# #   mutate(base_digit = str_c(base_digit, extra_digit)) %>%
# #   mutate(
# #     file_to =
# #       str_c(obl_id, rayon_id, base_digit, sep = "-") %>%
# #       str_c(., ".rds")
# #   ) %>%
# #   filter(!file_to %in% harvested_six)
# # library(progressr)
# # library(furrr)
# # handlers(global = TRUE)
# # 
# # handlers(list(
# #   handler_progress(
# #     format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
# #     width    = 80,
# #     complete = "+"
# #   )
# # ))
# # plan(multisession)
# # parallel_harvest(to_reharvest_again,  interm_plot_fldr_six)
# 
# 
# 
# # Checking one more level of digits ----------------------------------------
# 
# five_dig_harvest <- 
#   list.files(interm_plot_fldr_six, pattern = "rds", full.names = T) %>% 
#   map_dfr(read_rds)
# 
# five_dig_harvest %>% filter(exceededTransferLimit)
# 
# 
# # Combining raw_data ----------------------------------------------------
# 
# # list(
# #   three_dig_harvest,
# #   four_dig_harvest,
# #   five_dig_harvest
# # ) %>% 
# #   bind_rows() %>% 
# #   dplyr::filter(response_success, !exceededTransferLimit) %>% 
# #   write_rds(
# #     here('data-raw', "05-SR-request", "04-kvartal-3-4-5-dig-plots-geometries-raw.rds"), compress = "gz")
# 
# 
# 
# # Cleaning data -----------------------------------------------------------
# 
# drop_nonalphanum <- 
#   . %>%
#   stringi::stri_trans_general("Latin-ASCII" ) %>% 
#   stringi::stri_trans_general("Any-Hex/Unicode" )  %>%
#   str_replace_all("U\\+02B9", "") %>%
#   stringi::stri_trans_general("Hex-Any/Unicode") %>%
#   str_replace_all("[^[:alnum:]_]", "")
# 
# 
# translit_kaz <- 
#   . %>% 
#   stringi::stri_trans_general(.x, "Kazakh-Latin/BGN") %>%
#   stringi::stri_trans_general("Latin-ASCII") %>%
#   drop_nonalphanum
# 
# 
# translit_rus <- 
#   . %>% 
#   stringi::stri_trans_general(.x, "Cyrillic-Latin/BGN") %>%
#   stringi::stri_trans_general("Latin-ASCII") %>%
#   drop_nonalphanum
# 
# 
# raw_requests <-
#   read_rds(
#     here(
#       'data-raw',
#       "05-SR-request",
#       "04-kvartal-3-4-5-dig-plots-geometries-raw.rds"
#     )
#   ) %>% 
#   pull(response_geo_attrs ) %>% 
#   bind_rows()
# 
# 
# rayon_index <- 
#   read_rds("data-clean/02-1-ray-shapes-clean-2021-03-28-07-45-13.rds") %>% 
#   st_drop_geometry() %>% 
#   as_tibble() %>% 
#   select(leyer_id, obl_id , rayon_id, obl_name = obl_rus, ray_name = raj_rus)
# 
# 
# clean_plots_requests <- 
#   raw_requests %>% 
#   mutate(obl_id = str_sub(KAD_NOMER, 1,2),
#          rayon_id = str_sub(KAD_NOMER, 3,5),
#          kvartal_id = str_sub(KAD_NOMER, 6,8)) %>% 
#   left_join(rayon_index) %>% 
#   select(KAD_NOMER, layerId, obl_id, rayon_id, kvartal_id, obl_name, ray_name, 
#          server_plot_id = OBJECTID, 
#          SRVGET, Shape_Length, Shape_Area, geometry ) %>% 
#   mutate(Shape_Area = str_replace(Shape_Area, ",", ".") %>% as.numeric())
# 
# 
# ## Saving Shapes ---------------------------
# clean_plots_requests %>% 
#   write_rds(
#     here("data-clean", "05-SR-request", "plots-geometry-3-rayons.rds"),
#     compress = "gz"
#   )
# 
# clean_plots_requests %>% 
#   sf::write_sf(
#     here("data-clean", "05-SR-request", "shpfl", "plots-geometry-3-rayons.shp")
#   )
# 
# 
# 
# clean_plots_requests %>% 
#   filter(Shape_Area > 100*100) %>% 
#   sf::write_sf(
#     here("data-clean", "05-SR-request", "shpfl_1ha", "plots-geometry-3-rayons.shp")
#   )
# 
# 
# clean_plots_requests  %>%
#   st_drop_geometry() %>% 
#   as_tibble() %>% 
#   group_by(rayon_id) %>% 
#   dplyr::count()
# 
# clean_plots_requests %>% 
#   st_drop_geometry() %>% 
#   as_tibble() %>% 
#   filter(rayon_id  == "044") %>% 
#   count(kvartal_id, sort = T)
# 
# clean_plots_requests %>% 
#   filter(rayon_id  == "050") %>% 
#   ggplot() + 
#   geom_sf()
# 
# clean_plots_requests %>% 
#   filter(rayon_id  == "323") %>% 
#   ggplot() + 
#   geom_sf()
# 
# clean_plots_requests %>% 
#   filter(rayon_id  == "044", kvartal_id == "180") %>% 
#   ggplot() + 
#   geom_sf()
# 
# 
# three_dig_harvest%>% 
#   filter(!response_success) %>% 
#   pull(response_url )
#   # filter(exceededTransferLimit) %>% 
#   select(response_geo_attrs ) %>% 
#   unnest(response_geo_attrs )
# 
# 
# 
# 
# 
# 
