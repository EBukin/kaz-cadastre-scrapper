

# Setup
## install ("pacman") # This package will install amd prepare packages in the lines below:


library(pacman)
library (dplyr)
pacman::p_load(sf, tidyverse, rgdal, mapview)
# I use sf and tidyverse for data manipulation.

rm(list=ls()) 

setwd ("C:\\Users\\sarah\\Documents\\IAMO\\Fieldwork\\SPATIAL_DATA\\Kazakhstan\\Cadastre_GIS\\Cadastre_rds\\")

# Loading data -----------------------------------------------------------------

# Use rds instead of shapefiles
# We immediately convert the csr:
# This file 

plots_geoms <- read_rds("plot-geom-api-meta.rds") %>% 
  st_transform(st_crs(3857))

# No need to transform because it is transformed:
plots_ugods_geoms <- read_rds("plot-geom-ugod-areas.rds") 
st_crs(plots_ugods_geoms) == st_crs(plots_geoms)


# Analysis -----------------------------------------------------------------

# Showing data without geometries:
plots_geoms %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  glimpse


# Plotting data with colours by rayons. 
# May take some time:
plots_geoms %>% 
  filter(rayon_rus %in% c("Kegenskij")) %>% 
  ggplot() + 
  aes(fill = rayon_rus) + 
  geom_sf()



# Getting summary about ugodias types and plots---------------------------

plots_ugods_geoms %>% 
  select(cadastre_id, plot_area, contains("ugod"))


# Interactive map
# Takes time too
plots_ugods_geoms %>% 
  mapview(zcol = "rayon_rus")

# Create categories for land tenure and ownership polygons

plots_ugods_geoms[1,]
class(plots_ugods_geoms)

plots_ugods_geoms[,"tenure"] <- NA
colnames(plots_ugods_geoms)

## We create a single land tenure column which uses a combination of the  meta_celevoe_naznacenie and meta_kategoria_zemel to set a number of tenure categories.
## In the meta_celevoe_naznacenie many different ways and spellings for the same thing have been used, which is why the set of conditions is complex.

## Individual farms on agricultural land - here the string 'krest' for krestizanski khozyaistvo is the key
plots_ugods_geoms$tenure <- ifelse(grepl("krest", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = TRUE, fixed=FALSE) &
                                     grepl("Zemli sel'skohozajstvennogo naznacenia", plots_ugods_geoms$meta_kategoria_zemel, fixed=FALSE), "Individual farm on agricultural land", NA)

## Individual farms on village land- here the string 'krest' for krestizanski khozyaistvo is the key
plots_ugods_geoms$tenure <- ifelse(grepl("krest", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = TRUE, fixed=FALSE) &
                                     grepl("Zemli naselennyh punktov", plots_ugods_geoms$meta_kategoria_zemel, fixed=FALSE), "Individual farm on village land", plots_ugods_geoms$tenure )

## Enterprises on agricultural land - all these include words related to agriculture and enterprise, but do not include 'krest':  
## e.g. predpriyatia or firma on agricultural land, and tovarnoe sel'skoe khozyastvo, or TOO, and vedenie sel'skogo hozajstva without 'krest'.   
plots_ugods_geoms$tenure <-ifelse(grepl("pred", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli sel'skohozajstvennogo naznacenia", plots_ugods_geoms$meta_kategoria_zemel,fixed=F), "Enterprise on agricultural land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("firm", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli sel'skohozajstvennogo naznacenia", plots_ugods_geoms$meta_kategoria_zemel, fixed=F), "Enterprise on agricultural land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("TOO", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli sel'skohozajstvennogo naznacenia", plots_ugods_geoms$meta_kategoria_zemel, fixed=F), "Enterprise on agricultural land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("tovarn", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("sel'skogo", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli sel'skohozajstvennogo naznacenia", plots_ugods_geoms$meta_kategoria_zemel, fixed=FALSE), "Enterprise on agricultural land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("vedenia sel'skogo hozajstva", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli sel'skohozajstvennogo naznacenia", plots_ugods_geoms$meta_kategoria_zemel, fixed = T), "Enterprise on agricultural land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("vedenie sel'skogo hozajstva", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli sel'skohozajstvennogo naznacenia", plots_ugods_geoms$meta_kategoria_zemel, fixed = T), "Enterprise on agricultural land", plots_ugods_geoms$tenure)

## Enterprises on village land
plots_ugods_geoms$tenure <-ifelse(grepl("pred", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("sel'sk", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli naselennyh punktov", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "Enterprise on village land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("firm", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli naselennyh punktov", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "Enterprise on village land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("TOO", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli naselennyh punktov", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "Enterprise on village land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("tovarn", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("sel'skogo", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli naselennyh punktov", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "Enterprise on village land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("vedenia sel'skogo hozajstva", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli naselennyh punktov", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "Enterprise on village land", plots_ugods_geoms$tenure)
plots_ugods_geoms$tenure <-ifelse(grepl("vedenie sel'skogo hozajstva", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli naselennyh punktov", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "Enterprise on village land", plots_ugods_geoms$tenure)

## Forest land
plots_ugods_geoms$tenure <-ifelse(grepl("lesnogo", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) &
                                    grepl("Zemli lesnogo fonda", plots_ugods_geoms$meta_kategoria_zemel,  fixed=FALSE), "Forest lands", plots_ugods_geoms$tenure)
                                                                        
## Nature reserves
plots_ugods_geoms$tenure <-ifelse(grepl("prirod", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) &
                                    grepl("prirod", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "Protected areas", plots_ugods_geoms$tenure)

## State reserve
plots_ugods_geoms$tenure <-ifelse(grepl("Zemli zapasa", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "State Reserve", plots_ugods_geoms$tenure)

## Land for households on village land
plots_ugods_geoms$tenure <-ifelse((grepl("licnogo", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) |
                                    grepl("podsob", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE)) &
                                    grepl("Zemli naselennyh punktov", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE),
                                    "Households", plots_ugods_geoms$tenure)

## Land for households on agricultural land
plots_ugods_geoms$tenure <-ifelse(grepl("podsob", plots_ugods_geoms$meta_celevoe_naznacenie, ignore.case = T, fixed=FALSE) & 
                                    grepl("Zemli sel'skohozajstvennogo naznacenia", plots_ugods_geoms$meta_kategoria_zemel, ignore.case = T, fixed=FALSE), "Household on agricultural land", plots_ugods_geoms$tenure)

## Next we create a land tenure category column for each ugodi type, by assigning the new tenure categories to those columns if the land use type in question takes up over 50% of the plot
## Many plots which are obivously for grazing are made up of ugodi type pasture, pasture on slopes, rocky areas and ice (other) so we create a new pasture_all category which is the sum of these types.

# Make a total pasture column, ignoring the NAs  - treating them as zeros
plots_ugods_geoms[,"pasture_all"] <- NA
plots_ugods_geoms<-mutate(plots_ugods_geoms, pasture_all = rowSums(cbind(ugod_pasture, ugod_pasture_on_slopes, ugod_rocky_areas), na.rm = T))

# Then make the tenure columns which specify, for each ugodi type, the tenure category if that category takes up over 50% of the plot.
plots_ugods_geoms$tenure_past<-ifelse(plots_ugods_geoms$pasture_all>(0.5*plots_ugods_geoms$plot_area), plots_ugods_geoms$tenure, NA)
plots_ugods_geoms$tenure_crop<-ifelse(plots_ugods_geoms$ugod_cropland>(0.5*plots_ugods_geoms$plot_area), plots_ugods_geoms$tenure, NA)
plots_ugods_geoms$tenure_hay<-ifelse(plots_ugods_geoms$ugod_hayfield>(0.5*plots_ugods_geoms$plot_area), plots_ugods_geoms$tenure, NA)
plots_ugods_geoms$tenure_forest<-ifelse(plots_ugods_geoms$ugod_forests>(0.5*plots_ugods_geoms$plot_area), plots_ugods_geoms$tenure, NA)
plots_ugods_geoms$tenure_other<-ifelse(plots_ugods_geoms$ugod_forests>(0.5*plots_ugods_geoms$plot_area), plots_ugods_geoms$tenure, NA)
plots_ugods_geoms$tenure_unharvested<-ifelse(plots_ugods_geoms$ugod_others_nonharvested>(0.5*plots_ugods_geoms$plot_area), plots_ugods_geoms$tenure, NA)


## Statistics
#plots_ugods_geoms<-plots_ugods_geoms %>% mutate(across(is.numeric, ~ round(., 2)))
#plots_ugods_geoms$plot_area<-round(plots_ugods_geoms$plot_area, 2)
#plots_ugods_geoms$all_harvested<-round(plots_ugods_geoms$all_harvested, 2)
#plots_ugods_geoms$pasture_all<-round(plots_ugods_geoms$pasture_all, 2)
#plots_ugods_geoms$pasture_all<-as.integer(plots_ugods_geoms$pasture_all)

## Write as shapefile for mapping - shapefile does not like the large number of decimal places.  Tried to round (above) but rounding not carried through to shapefile.

st_write(plots_ugods_geoms, dsn = "C:\\Users\\sarah\\Documents\\IAMO\\Fieldwork\\SPATIAL_DATA\\Kazakhstan\\Cadastre_GIS\\Cadastre_shape\\plots_ugods_geom_classified", layer = "plots_ugods_geom_classified2.shp", driver = "ESRI Shapefile", append=FALSE)

## Statistics of areas of plots which are predominantly pasture, by plot_area

past_mean<-aggregate(plots_ugods_geoms$plot_area, by=list(plots_ugods_geoms$tenure_past), FUN=mean)
past_median<-aggregate(plots_ugods_geoms$plot_area, by=list(plots_ugods_geoms$tenure_past), FUN=median)
past_max<-aggregate(plots_ugods_geoms$plot_area, by=list(plots_ugods_geoms$tenure_past), FUN=max)
past_sum<-aggregate(plots_ugods_geoms$plot_area, by=list(plots_ugods_geoms$tenure_past), FUN=sum)
past_length<-aggregate(plots_ugods_geoms$plot_area, by=list(plots_ugods_geoms$tenure_past), FUN=length)

## To check produce also a sum column using the area_ha field - for comparison with that produced using plot_area, in m2
past_sum_testha<-aggregate(plots_ugods_geoms$area_ha, by=list(plots_ugods_geoms$tenure_past), FUN=sum)


# Bind into single table and name columns
tenure_ugodpast_summary<-cbind(past_mean, past_median, past_max, past_sum, past_length, past_sum_testha)
tenure_ugodpast_summary<- tenure_ugodpast_summary[ -c(3,5,7,9,11) ]
colnames(tenure_ugodpast_summary) <- c("tenure", "past_mean", "past_median", "past_max", "past_sum", "past_length", "past_sum_testha")

# These statistics are in metres, we convert to ha.
tenure_ugodpast_summary$past_mean_ha <- tenure_ugodpast_summary$past_mean/10000
tenure_ugodpast_summary$past_median_ha <- tenure_ugodpast_summary$past_median/10000
tenure_ugodpast_summary$past_max_ha <- tenure_ugodpast_summary$past_max/10000
tenure_ugodpast_summary$past_sum_ha <- tenure_ugodpast_summary$past_sum/10000

# past_sum_ha should be the same as past_sum_testha

tenure_ugodpast_summary<-tenure_ugodpast_summary %>% mutate(across(is.numeric, ~ round(., 2)))

write.csv (tenure_ugodpast_summary, "C:\\Users\\sarah\\Documents\\IAMO\\Fieldwork\\SPATIAL_DATA\\Kazakhstan\\Cadastre_GIS\\Data_extraction_tables\\tenure_ugodpast_summary2.csv")

## Next thing to do is to work out how much pasture land (using  ugodi) is not covered by any plots at all. 