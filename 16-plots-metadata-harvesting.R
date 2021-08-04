# Harvesting lots metadata

library(pacman)
pacman::p_load(sf, here, fs, tidyverse, stringi, jsonlite, 
               httr, RCurl, progress, rgdal, glue)
force_reharvest <- FALSE

source("R/find-plots-calls-processors.R")

# Function =============================================


get_plot_mtdt_raw <- function(id) {
  out <- 
    try({
      POST("http://www.aisgzk.kz/aisgzk/Index/FindObjInfoForMap",
           body = list(kadNum = id)) %>% 
        content(., type = "text", encoding = "UTF-8")
      
      # RCurl::getURLContent()
    })
  
  if ("try-error" %in% class(out)) return("failed") else return(out)
}



# Getting kadastre numbers index -------------------------------------

existing_plots <- read_rds(here(
  "data-clean",
  "05-plots-shapes",
  "kaz-all-plots-shapes-clean-2021-07-20.rds"
)) %>%
  st_drop_geometry() %>%
  distinct(cadastre_id)


# Metadata harvester ===================================================

to_fold <- "~/kaz-cad-raw/2021-07-19-plots-harvesting/plot-metadata/"


parallel_metadata_harvest <- 
  function(existing_plots) {
  
  p <- progressor(steps = nrow(existing_plots))
  
  existing_plots %>%   
    mutate(
      raw_mtdt_responce = furrr::future_map(cadastre_id, ~ {
        p()
        get_plot_mtdt_raw(.x)
      }),
      harv_time = Sys.time()
    )
  
}



# Actual harvesting ====================================================


harvested <-
  list.files(to_fold, full.names = T) %>%
  map_dfr(read_rds) %>%
  mutate(raw_mtdt_responce = unlist(raw_mtdt_responce, recursive = F)) %>%
  filter(raw_mtdt_responce != "failed") %>%
  group_by(cadastre_id) %>%
  filter(harv_time == max(harv_time)) %>%
  arrange(desc(harv_time)) %>%
  mutate(n = row_number()) %>%
  ungroup() %>%
  filter(n == 1)



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

# plan(multisession, workers = 7)

# one_plot <- existing_plots %>%
#   sample(1)

extra_metadata <-
  existing_plots %>%
  filter(!cadastre_id %in% harvested$cadastre_id) %>%
  group_by(row_number() %% 1) %>%
  nest() %>%
  # pull(data) %>%
  pmap(~{
    # browser()
    cat("step: ", .x, "\n")
    interm_harvest <-
      .y %>%
      parallel_metadata_harvest

    write_rds(
      interm_harvest,
      str_c(to_fold, "metadata-part-", str_replace_all(Sys.time(), "[^[:alnum:]]", "-"), ".rds"),
      compress = "gz"
    )
    interm_harvest
  })

# write_rds(
#   extra_metadata,
#   str_c(to_fold, "metadata-", str_replace_all(Sys.time(), "[^[:alnum:]]", "-"), ".rds"),
#   compress = "gz"
# )

# Saving RAW metadata ------------------------------------------------------


# harvested_raw <-
#   list.files(to_fold, full.names = T) %>%
#   map_dfr(read_rds) %>%
#   mutate(raw_mtdt_responce = unlist(raw_mtdt_responce, recursive = F)) %>%
#   filter(raw_mtdt_responce != "failed") %>%
#   group_by(cadastre_id) %>%
#   filter(harv_time == max(harv_time)) %>%
#   arrange(desc(harv_time)) %>%
#   mutate(n = row_number()) %>%
#   ungroup() %>%
#   filter(n == 1)
# 
# 
# write_rds(harvested_raw, here("data-raw", "18-plot-metadata-all-2021-07-20.rds"), compress = "gz")
harvested_raw <- read_rds( here("data-raw", "18-plot-metadata-all-2021-07-20.rds"))
# harvested_raw2 <- 
#   "~/kaz-cad-raw/2021-07-19-plots-harvesting/plot-metadata/metadata-part-2021-07-21-15-17-34.rds" %>% 
#     map_dfr(read_rds) %>%
#     mutate(raw_mtdt_responce = unlist(raw_mtdt_responce, recursive = F)) %>%
#     filter(raw_mtdt_responce != "failed") %>%
#     group_by(cadastre_id) %>%
#     filter(harv_time == max(harv_time)) %>%
#     arrange(desc(harv_time)) %>%
#     mutate(n = row_number()) %>%
#     ungroup() %>%
#     filter(n == 1)

# Parcing metadata --------------------------------------------------------

translit_kaz_text <-
  . %>%
  stringi::stri_trans_general( "Kazakh-Latin/BGN") %>%
  stringi::stri_trans_general("Latin-ASCII")


translit_rus_text <-
  . %>%
  stringi::stri_trans_general( "Cyrillic-Latin/BGN") %>%
  stringi::stri_trans_general("Latin-ASCII")

parse_metadata <- function(rwjson) {
  
  # raw_json <- 
  rwjson %>%
    get_response_json(rwjson) %>% 
    map_dfr(~{tibble(var = .x$Key, kaz = .x$Name$Kz, rus = .x$Name$Ru)})
  
  # tibble(
  #   names = 
  #     map_chr(raw_json, "Key") %>%
  #     translit_rus_text %>%
  #     str_replace_all("[^[:alnum:] ]", "")%>%
  #     str_replace_all(" ", "_") %>%
  #     str_to_lower() ,
  #   kaz = map_chr(raw_json, ~.x$Name$Kz)  %>% translit_kaz_text,
  #   rus = map_chr(raw_json, ~.x$Name$Ru)  %>% translit_rus_text 
  # ) %>% 
  #   # select(names, kaz, rus) %>% 
  #   pivot_wider(names_from = names, values_from = c(kaz, rus))
  
  
}

parse_metadata_safely <- 
  function(rwjson) {
    out <- try({parse_metadata(rwjson) %>% mutate(success = TRUE)})
    if ("try-error" %in% class(out)) return(list(tibble(success = FALSE))) else return(list(out))
  }


# Parsing all metadata ---------------------------------------

parallel_parcing <- 
  function(existing_plots) {
    p <- progressor(steps = nrow(existing_plots))
    existing_plots %>%
      mutate(#
        meatadata =
          map(raw_mtdt_responce, ~ {
            # browser()
            p()
            parse_metadata_safely(.x)
          }))
  }

# plan(sequential)


library(progressr)
# library(furrr)
handlers(global = TRUE)

handlers(list(
  handler_progress(
    format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
    width    = 80,
    complete = "+"
  )
))


# # Uncomment to start parcing
# parced_metadata <-
#   harvested_raw %>%
#   parallel_parcing
# parced_metadata %>%
#   write_rds(here("data-raw","18.2-plots-metadata-parced-raw-2021-07-20.rds"), compress = "gz")

# parced_metadata2 <-
#   harvested_raw2 %>%
#   parallel_parcing
# parced_metadata2 %>%
#   write_rds(here("data-raw","18.2-plots-metadata-parced-raw2-2021-07-20.rds"), compress = "gz")


# Cleaning parced metadata ---------------------------------------

parced_metadata_raw <- 
  read_rds(here("data-raw","18.2-plots-metadata-parced-raw-2021-07-20.rds")) %>% 
  bind_rows(read_rds(here("data-raw","18.2-plots-metadata-parced-raw2-2021-07-20.rds")))


parced_metadata_clean <-
  parced_metadata_raw %>% 
  select(-raw_mtdt_responce) %>% 
  unnest(meatadata) %>%
  unnest(meatadata) %>% #glimpse()
  mutate(var = var %>%
           translit_rus_text %>%
           str_replace_all("[^[:alnum:] ]", "") %>%
           str_replace_all(" ", "_") %>%
           str_to_lower() ) %>% 
  pivot_wider(names_from = var, 
              values_from = c(kaz, rus)) %>% #names()
  select(cadastre_id, success,# kadastrovyj_nomer , 
         contains("rus_"), contains("kaz_"), 
         -rus_kadastrovyj_nomer, -matches("_NA", ignore.case = F)) %>% 
  filter(success )


# parced_metadata_clean <-
#   parced_metadata_raw %>%
#   slice(1:1000) %>%
#   select(cadastre_id, meatadata) %>% 
#   unnest() %>% 
#   mutate(ncol = map_dbl(meatadata, ncol)) %>%
#   filter(ncol > 1) %>% 
#   unnest() %>% 
#   select(-ncol, -success)


parced_metadata_clean %>% names()

parced_metadata_clean %>% glimpse()

parced_metadata_clean %>% 
  count(rus_predostavlennoe_pravo, sort = T) %>% 
  mutate(across(where(is.character), ~ str_replace_all(., "[^[:alnum:] ]", "")))

parced_metadata_clean %>% count(rus_kategoria_zemel, sort = T)

parced_metadata_clean %>% count(rus_celevoe_naznacenie, sort = T)

parced_metadata_clean %>% count(rus_zemlepolzovateli, sort = T)

parced_metadata_clean %>% count(rus_ogranicenia, sort = T)



parced_metadata_clean %>%
  write_csv(here("data-clean","06-plots-metadata-clean-2021-07-20.csv"))

parced_metadata_clean %>%
  write_rds(here("data-clean","06-plots-metadata-clean-2021-07-20.rds"), compress = "gz")



