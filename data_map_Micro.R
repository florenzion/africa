setwd("C:/Users/flore/Desktop/lavoro/UniTo_EUI/EUI/")

library(readstata13)
library(sf)
library(mapview)
library(dplyr)
library(tidyr)
library(leaflet)

##################################################
######## PLOTTING SOME MAPS          #############
##################################################

mapviewOptions(fgb = FALSE)

##### Add geometry and create counting variable ########################################################################################################################
#################################################################################################################################################################


# Upload dataset

FDI <- read.csv("FDI_harmonized.csv")
#WBES <- read.dta13("Micro/Results/WBES_final.dta")

# Append WBES data onto FDI dataset

# Convert dataframe in a sf object. Firstly, we should drop rows without coordinates.

FDI_clean <- FDI[!is.na(FDI$g_lon),]

FDI_sf <- st_as_sf(FDI_clean,
                   coords = c("g_lon", "g_lat"),
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
rm(FDI_clean)
# Create variable for NUMBER OF PROJECTS IN A CITY
FDI_sf$n_proj <- 1


# Plot interactive map of the variable of interest for each project.

plot(FDI_sf["fdi_jobs"])
mapview(FDI_sf["fdi_jobs"])

# Drop point in Australia!

FDI_sf <- st_crop(FDI_sf, xmin=-24.98638, xmax=58, ymin=-34.23, ymax=37.27442)


##### Set up spatial point datasets with aggregate variables ########################################################################################################################
#################################################################################################################################################################

############### Map 1 ####################################
### Total Info for each city #############################

# Aggregate variables of interest for each point in the map

FDI_sf_pts <- aggregate(FDI_sf["fdi_jobs"], by = FDI_sf["geometry"], FUN = sum)
identical(st_geometry(FDI_sf),st_geometry(FDI_sf_pts))  #Check if same geometry: yes!

FDI_sf_ag <- aggregate(FDI_sf["n_proj"], by = FDI_sf["geometry"], FUN = sum) 
FDI_sf_pts$n_proj <- FDI_sf_ag$n_proj

FDI_sf_ag <- aggregate(FDI_sf["revenues_usd_ml"], by = FDI_sf["geometry"], FUN = sum) 
FDI_sf_pts$revenues_usd_ml <- FDI_sf_ag$revenues_usd_ml

FDI_sf_ag <- aggregate(FDI_sf["Headcount"], by = FDI_sf["geometry"], FUN = sum) 
FDI_sf_pts$Headcount <- FDI_sf_ag$Headcount

FDI_sf_ag <- aggregate(FDI_sf["r_d_exp"], by = FDI_sf["geometry"], FUN = sum) 
FDI_sf_pts$r_d_exp <- FDI_sf_ag$r_d_exp

FDI_sf_ag <- aggregate(FDI_sf["capital"], by = FDI_sf["geometry"], FUN = sum) 
FDI_sf_pts$capital <- FDI_sf_ag$capital


FDI_sf_pts <- st_difference(FDI_sf_pts) #Drop duplicate coordinates
rm(FDI_sf_ag)

# Create label for interactive map (pop up query)
FDI_label <- st_difference(FDI_sf)

FDI_sf_pts <- FDI_sf_pts %>%mutate(popup_info = paste("Country:", FDI_label$country, "<br/>", "Region:", FDI_label$region,"<br/>" , "City:", FDI_label$city, "<br/>","Number of projects:", n_proj, "<br/>" ,"Jobs created:", fdi_jobs,"<br/>", "Revenues:", revenues_usd_ml, "<br/>","Headcount:", Headcount,"<br/>", "R&D:" ,r_d_exp, "<br/>", "Capital:", capital))
FDI_sf_pts$popup_info
FDI_sf_pts$country <- FDI_label$country


rm(FDI_label)


############### Map 2 ####################################
### Disaggregated info per sector for each city ##########

# Aggregate number of projects per city per sector
FDI_sf_sec <- aggregate(FDI_sf[, "n_proj"], with(FDI_sf, list(sector=isic, city=city)), sum)

# Make data wide
FDI_sf_wide <- spread(FDI_sf_sec, sector, n_proj)
rm(FDI_sf_sec)

FDI_sf_wide <- st_difference(FDI_sf_wide) #Drop duplicates

# Clean and add total column

FDI_sf_wide[is.na(FDI_sf_wide)] <- 0

FDI_sf_wide_ng <- st_drop_geometry(FDI_sf_wide)
FDI_sf_wide$total <- rowSums(FDI_sf_wide_ng[,2:50])
FDI_sf_wide$main_code <- names(FDI_sf_wide_ng)[2:50][max.col(FDI_sf_wide_ng[2:50])]
rm(FDI_sf_wide_ng)

# Create label for interactive map (pop up query)
FDI_label <- st_difference(FDI_sf)

FDI_sf_wide <- FDI_sf_wide %>%mutate(popup_info = paste("Country:", FDI_label$country, "<br/>", "Region:", FDI_label$region,"<br/>" , "City:", FDI_label$city, "<br/>","Number of projects:", total, "<br/>", "Main product sector (ISIC): ", FDI_sf_wide$main_code))
FDI_sf_wide$popup_info
FDI_sf_wide$country <- FDI_label$country


rm(FDI_label)

# Add qualitative sector column (see isic_rev3.pdf)

FDI_sf_wide$main <- ifelse(FDI_sf_wide$main %in% c(10:14), "Mining and Quarrying",
            ifelse(FDI_sf_wide$main %in% c(15:37), "Manufacturing",
                   ifelse(FDI_sf_wide$main %in% c(40,41), "Electricity and Water Supply",
                          ifelse(FDI_sf_wide$main %in% c(45), "Construction",
                                 ifelse(FDI_sf_wide$main %in% c(50:52), "Wholesale and Retail",
                                        ifelse(FDI_sf_wide$main %in% c(55), "Hotels and Restaurants",
                                               ifelse(FDI_sf_wide$main %in% c(60:64), "Transport, Storage and Communications",
                                                      ifelse(FDI_sf_wide$main %in% c(65:67), "Financial Intermediation",
                                                             ifelse(FDI_sf_wide$main %in% c(70:74), "Real Estate",
                                                                    ifelse(FDI_sf_wide$main %in% c(75), "Public Administration",
                                                                           ifelse(FDI_sf_wide$main %in% c(80), "Education",
                                                                                  ifelse(FDI_sf_wide$main %in% c(85),"Health and Social Work",
                                                                                         ifelse(FDI_sf_wide$main %in% c(90:93), "Other Community",
                                                                                                ifelse(FDI_sf_wide$main %in% c(95), "Private Households",""))))))))))))))




##### Plot maps ########################################################################################################################
#################################################################################################################################################################

# Plot interactive maps
m1 <- mapview(FDI_sf_pts, cex = "n_proj",popup =  FDI_sf_pts$popup_info)
m1

m2 <- mapview(FDI_sf_wide, cex = "total", zcol= "main", popup = FDI_sf_wide$popup_info)
m2

mapshot(m1, url = paste0(getwd(), "/map1.html"))
mapshot(m2, url = paste0(getwd(), "/map2.html"))




        