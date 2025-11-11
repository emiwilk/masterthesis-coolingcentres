library(sf)
library(terra)
library(dplyr)  
library(ggplot2)
library(scales)
library(raster)

############## ATHENS DATA PREPARATION ############
## ensure targetcrs for all raster and sf objects is EPSG:4326
targetcrs <- "EPSG:4326"


####### BOUNDARY SHAPES #######

## read boundary shape file into an sf object
athens_districts <- st_read("Data/Boundary Shapefiles/athens_district_boundaries/dimotikes_koinotites1.shp")

## see the first few rows and geometry type
print(head(athens_districts))
st_geometry_type(athens_districts)
st_crs(athens_districts)

## Reproject the sf object to targetcrs EPSG:4326
athens_districts_4326 <- st_transform(athens_districts, crs = targetcrs)

## rename for ease of use
athens_districts <- athens_districts_4326
# check crs, if reprojection successful
st_crs(athens_districts)
# shows EPSG:4326

## plot districts
plot(st_geometry(athens_districts))


####### INCOME #######

## read csv for income in athens
income_athens_uncropped <- rast("Data/socio-demographic - income/income_athens.tif")
crs(income_athens_uncropped)

## cut to athens boundary shape
# Bring athens districts into terra
athens_vect <- vect(athens_districts)

# Crop & mask global raster to athens boundary
inc_crop   <- crop(income_athens_uncropped, ext(athens_vect))
income_athens <- mask(inc_crop, athens_vect)

## plot for first visual control
plot(
  income_athens,
  main   = "2013 Annual Income – Athens",
  legend = TRUE,
  plg    = list(title = "€ / year")
)

## overview & metadata
income_athens             # header summary
names(income_athens)      # layer (band) names
nlyr(income_athens)       # number of layers

## add normalised index column for income > index is reversed, showing vulnerability due to low income
head(income_athens)
mm_inc   <- minmax(income_athens)       # returns c(min, max)
income_athens_index  <- 1 - (income_athens - mm_inc[1]) / (mm_inc[2] - mm_inc[1])
names(income_athens_index) <- "income.index"

## Inspect the first few rows
head(income_athens_index)

## sanity check: what’s the range?
range(income_athens_index$income.index, na.rm = TRUE)

## turn raster into data frame for ggplot
income_athens_index_df <- as.data.frame(income_athens_index, xy = TRUE, na.rm = TRUE)
val_col <- setdiff(names(income_athens_index_df), c("x", "y"))[1]
names(income_athens_index_df)[names(income_athens_index_df) == val_col] <- "income.index"

## plot income index
plot_income_athens <-
  ggplot(income_athens_index_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = income.index)) +
  geom_sf(data = athens_districts, inherit.aes = FALSE,
          fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Income Vulnerability index",
    direction = -1,
    na.value = "transparent"
  ) +
  labs(
    title = "Normalised Income Vulnerability Index in Athens",
    subtitle = "1 = high vulnerability due to low income"
  ) +
  coord_sf(crs = targetcrs, expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# print plot
plot_income_athens

## save plot for later
ggsave(
  filename = "Data/socio-demographic - income/athens_income_index_map.png",  # can be .png, .pdf, .jpg, etc.
  plot     = plot_income_athens,                           # omit if it’s the last plot you printed
  width    = 8,                           # in inches
  height   = 6,                           # in inches
  dpi      = 300                          # for raster formats
)

## save income index file
#writeRaster(
#  income_athens_index,
#  "Data/socio-demographic - income/income_athens_index.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)

## save income in numbers file
#writeRaster(
#  income_athens,
#  "Data/socio-demographic - income/income_athens_numbers.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)


# read it later with
#income_athens_index <- st_read("Data/income_athens_index.gpkg", layer = "income_athens_index")


####### AGE #######

## List all layers in the GeoPackage
st_layers("Data/socio-demographic - age/ESTAT_Census_2021_V2.gpkg")

## Read the layer census2021
census2021 <- st_read(
  dsn   = "Data/socio-demographic - age/ESTAT_Census_2021_V2.gpkg",
  layer = "census2021"
)

## Inspect crs
st_crs(census2021)

## It is a large data set, so we crop it in its original crs before reprojecting
athens_3035 <- st_transform(athens_districts, crs = 3035)

## Check they now match
st_crs(athens_3035)
# should also report EPSG:3035 (ETRS89-extended / LAEA Europe)

## Convert to terra SpatVector if you need to mask/crop a terra raster
athens_vect <- vect(athens_3035)

## Crop Census Raster to athens boundary
athens_census_3035 <- st_intersection(census2021, athens_3035)

## Reproject census raster to targetcrs EPSG:4326 
athens_census <- st_transform(athens_census_3035, targetcrs)
# Check successful transformation
st_crs(athens_census)

## Check
plot(st_geometry(athens_districts), border="black")
plot(st_geometry(athens_census), col=NA, border="red", add=TRUE)

## Now we only keep the relevant data of census_athens
age_athens <- athens_census [, c("GRD_ID", "T", "Y_GE65", "no", "name_en", "geom")]

## add columns for age ratio and age index based on the absolute numbers provided
age_athens_index <- age_athens %>%
  mutate(
    # 1) ensure numeric
    T        = as.numeric(T),
    Y_GE65   = as.numeric(Y_GE65),
    # 2) replace any negative or -9999 with 0 (there are -9999 in the Y_GE65 column)
    Y_GE65 = pmax(Y_GE65, 0),
    # 3) raw ratio
    age.ratio = Y_GE65 / T
  ) %>%
  # 4) rescale age.ratio into [0,1] index
  mutate(
    age.index = (age.ratio - min(age.ratio, na.rm = TRUE)) /
      (max(age.ratio, na.rm = TRUE) - min(age.ratio, na.rm = TRUE))
  ) %>%
  # optional: clamp tiny floating-point drift
  mutate(
    age.index = pmin(pmax(age.index, 0), 1)
  )

## sanity check
range(age_athens_index$age.ratio,  na.rm = TRUE)
range(age_athens_index$age.index,  na.rm = TRUE)
head(age_athens_index, 10)

## check numbers and validity
# Top 10 highest age.index
top10_age <- age_athens_index %>%
  slice_max(order_by = age.index, n = 10)
# Bottom 10 lowest age.index
bottom10_age <- age_athens_index %>%
  slice_min(order_by = age.index, n = 10)
# Display
top10_age
bottom10_age

## plot age index raster
plot_age_athens <- ggplot() +
  # plot the grid cells, no borders between cells
  geom_sf(
    data    = age_athens_index,
    aes(fill = age.index),
    colour  = NA
  ) +
  # add district outlines
  geom_sf(
    data    = athens_districts,
    fill    = NA,
    colour  = "black",
    size    = 0.3
  ) +
  # continuous single‑hue gradient (you can swap low/high to your taste)
  scale_fill_gradientn(
    colours = c("lightblue", "darkblue", "orange"),
    na.value = "white",
    name = "Population Aged >65 Years"
  ) +
  labs(
    title    = "Share of Population Aged >65 by 1 km Grid in athens (Index)",
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# print plot
plot_age_athens

## we see white squares, those are squares with no population 
## darkest square has 80% residents over 65, because it is T=4 and >65=3

## Save files 
# save plot
#ggsave(
#  filename = "Data/socio-demographic - age/athens_age_index_map.png",  # can be .png, .pdf, .jpg, etc.
#  plot     = plot_age_athens,                           # omit if it’s the last plot you printed
#  width    = 8,                           # in inches
#  height   = 6,                           # in inches
#  dpi      = 300                          # for raster formats
#)

# save age index file
#st_write(
#  obj    = age_athens_index,
#  dsn    = "Data/socio-demographic - age/age_athens_index.gpkg",
#  layer  = "age_athens_index",
#  driver = "GPKG",
#  delete_layer = TRUE  # overwrite if it exists
#)

# save age in numbers file, cropped to athens
#st_write(
#  obj    = age_athens,
#  dsn    = "Data/socio-demographic - age/age_athens.gpkg",
#  layer  = "age_athens",
#  driver = "GPKG",
#  delete_layer = TRUE  # overwrite if it exists
#)


####### POPULATION DENSITY #######

## load global pop-density tif file with total population numbers
pop_globe <- rast("Data/demand - population density/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0.tif")

## inspect raster crs
crs(pop_globe)

## It is a large data set, so we crop it in its original crs before reprojecting
## Bring athens districts into terra
athens_vect <- vect(athens_districts)

## Reproject to the raster’s World_Mollweide CRS
athens_vect_moll <- project(athens_vect, crs(pop_globe))

## Crop & mask global raster to athens boundary
pop_crop   <- crop(pop_globe, ext(athens_vect_moll))
pop_athens <- mask(pop_crop, athens_vect_moll)

# plot for first visual check
plot(
  pop_athens,
  main   = "2025 Population Density – athens",
  legend = TRUE,
  plg    = list(title = "residents / 100 m²")
)

## now for better compatibility we reproject the raster into target crs
## Project the masked athens raster into that CRS
#    - method = "bilinear" is good for continuous data like density
pop_athens_4326 <- project(
  x      = pop_athens,
  y      = targetcrs,
  method = "bilinear"
)

## we crop and mask once more to receive a perfectly clean edge
## turn district boundary into terra object

## Crop to the bounding box of athens
pop_athens_4326_crop <- crop(
  x = pop_athens_4326,
  y = ext(athens_districts)
)

## Mask out everything outside the true district shapes
pop_athens_4326_mask <- mask(
  x    = pop_athens_4326_crop,
  mask = athens_districts
)

## rename 
pop_athens <- pop_athens_4326_mask

## Plot to check perfectly cropped edges and save
png(
  filename = "Data/demand - population density/athens_population_density_numbers.png", 
  width    = 1600,     # pixels
  height   = 1200,     # pixels
  res      = 300       # dpi
)
plot(
  pop_athens,
  main   = "2025 Population Density – Athens",
  legend = TRUE,
  plg    = list(title = " residents
                / 100 m²")
)
plot(
  athens_vect,
  add    = TRUE,
  border = "white",
  lwd    = 1
)
dev.off()

## normalise values onto 0 to 1 scale 
names(pop_athens)
class(pop_athens)

## compute the min and max
mm    <- minmax(pop_athens)
minv  <- mm[1]
maxv  <- mm[2]

## linearly rescale: (x - min) / (max - min)
pop_athens_index <- (pop_athens - minv) / (maxv - minv)

## give it a name
names(pop_athens_index) <- "pop_athens_index"

## quick check on the new range
global(pop_athens_index, fun = c("min","max"), na.rm = TRUE)
# should return 0 and 1

## plot for visual verification
## Create df for ggplot 
pop_athens_df <- as.data.frame(
  pop_athens_index,
  xy      = TRUE,
  na.rm   = TRUE    # <— this drops rows where the value is NA
)
names(pop_athens_df)[3] <- "pop.index"

## Plot
plot_pop_athens <- ggplot() +
  geom_raster(
    data = pop_athens_df,
    aes(x = x, y = y, fill = pop.index)
  ) +
  geom_sf(
    data        = athens_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE  # <— prevent it looking for x,y
  ) +
  scale_fill_gradientn(
    colours = c("white", "orange", "red"),
    values  = c(0, 0.5, 1),
    name    = "Population Density",
    na.value = "white"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Normalised Population Density in Athens"
  ) +
  theme_void() +
  theme(
    legend.title   = element_text(size = 10),
    legend.text    = element_text(size = 8),
    legend.key.size= unit(0.6, "lines")
  )

plot_pop_athens

## save plot and files

## Save the plot
#ggsave(
#  filename = "Data/demand - population density/athens_pop_density_map.png", 
#  plot     = plot_pop_athens, 
#  width    = 8, 
#  height   = 6, 
#  dpi      = 300
#)

## Write out the data.frame
#write.csv(
#  pop_athens_df,
#  file      = "Data/demand - population density/pop_athens_df.csv",
#  row.names = FALSE
#)

## Save the SpatRaster
#writeRaster(
#  pop_athens_index,
#  "Data/demand - population density/pop_athens_index.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)

## save the total numbers as raster
#writeRaster(
#  pop_athens,
#  "Data/demand - population density/pop_athens.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)

####### HEAT STRESS #######

## Load the heat-stress raster
athens_heat_rast <- rast(
  "Data/climate - moderate heat stress/impact_geo_curpol_athens_urbclim-WBGT-dayover25_0.5_absolute_2030_0.0009963815993430103.tif"
)

crs(athens_heat_rast)

## Reproject & crop/mask to athens
heat_crop   <- crop(athens_heat_rast, ext(athens_vect))
heat_athens <- mask(heat_crop, athens_vect)

## Plot raw days‐per‐year map
heat_athens_df <- as.data.frame(heat_athens, xy = TRUE, na.rm = TRUE)
names(heat_athens_df)[3] <- "heatstress_days"

plot_athens_heat <- ggplot() +
  geom_raster(data = heat_athens_df, aes(x = x, y = y, fill = heatstress_days)) +
  geom_sf(data = athens_districts, 
          fill = NA, colour = "black", size = 0.3, inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Days/year", na.value = "white") +
  labs(title = "Days a Year with Moderate Heat Stress in Athens") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_athens_heat) 

#save as csv
#write.csv(
#  heat_athens_df,
#  file      = "Data/climate - moderate heat stress/athens_heat_stress_df.csv",
#  row.names = FALSE
#)

## Save the SpatRaster
#writeRaster(
#  heat_athens,
#  "Data/climate - moderate heat stress/heat_athens.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)

## Normalize to [0,1]
mm       <- minmax(heat_athens)    # c(min, max)
minv     <- mm[1]; maxv <- mm[2]
heat_index_athens <- (heat_athens - minv) / (maxv - minv)
names(heat_index_athens) <- "heat_index_athens"

## Plot indexed map
heat_index_athens_df <- as.data.frame(heat_index_athens, xy = TRUE, na.rm = TRUE)
names(heat_index_athens_df)[3] <- "heat_index_athens"

plot_heat_index_athens <- ggplot() +
  geom_raster(data = heat_index_athens_df, aes(x = x, y = y, fill = heat_index_athens)) +
  geom_sf(data = st_transform(athens_districts, crs(heat_index_athens)), 
          fill = NA, colour = "black", size = 0.3, inherit.aes = FALSE) +
  scale_fill_gradientn(
    colours = c("yellow", "orange", "darkred"),
    name    = "Heat Stress Index"
  ) +
  labs(title = "Days a Year with Moderate Heat Stress in Athens (Index)") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_heat_index_athens)  # display it

## Save the indexed plot, data.frame, and raster
#ggsave(
#  filename = "Data/climate - moderate heat stress/athens_heat_stress_index_map.png",
#  plot     = plot_heat_index_athens,
#  width    = 8, height = 6, dpi = 300
#)

#write.csv(
#  heat_index_athens_df,
#  file      = "Data/climate - moderate heat stress/athens_heat_stress_index_df.csv",
#  row.names = FALSE
#)

## Save the SpatRaster
#writeRaster(
#  heat_index_athens,
#  "Data/climate - moderate heat stress/athens_heat_stress_index.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)

####### BUILDING DENSITY #######

## Load the building density raster
building_dens_globe <- rast(
  "Data/built environment - building density/GHS_building_density.tif"
)
crs(building_dens_globe)

## It is a large data set, so we crop it in its original crs before reprojecting
## Bring Athens districts into terra
athens_vect <- vect(athens_districts)

## Reproject to the raster’s World_Mollweide CRS
athens_vect_moll <- project(athens_vect, crs(building_dens_globe))

## Crop & mask global raster to athens boundary
building_dens_crop   <- crop(building_dens_globe, ext(athens_vect_moll))
building_dens_mask <- mask(building_dens_crop, athens_vect_moll)

# plot for first visual check
plot(
  building_dens_mask,
  main   = "2025 Building Density – Athens",
  legend = TRUE,
  plg    = list(title = "building mass m³ / 100 m²")
)

## now for better compatibility we reproject the raster into target crs
## Project the masked Athens raster into that CRS
#    - method = "bilinear" is good for continuous data like density
builddens_athens <- project(
  x      = building_dens_mask,
  y      = targetcrs,
  method = "bilinear"
)

## we crop and mask once more to receive a perfectly clean edge
## turn district boundary into terra object

## Crop to the bounding box of Athens
builddens_athens_crop <- crop(
  x = builddens_athens,
  y = ext(athens_districts)
)

## Mask out everything outside the true district shapes
builddens_athens_mask <- mask(
  x    = builddens_athens_crop,
  mask = athens_districts
)

## rename and check crs
builddens_athens <- builddens_athens_mask
crs(builddens_athens)
# shows EPSG:4326

## Plot raw building mass (m³/100 m²)
builddens_athens_df <- as.data.frame(builddens_athens, xy = TRUE, na.rm = TRUE)
names(builddens_athens_df)[3] <- "mass_m3"

## check extent of numbers with bottom10 and top10
# Top 10 highest building mass
top10_builddens <- builddens_athens_df %>%
  slice_max(order_by = mass_m3, n = 10)
# Bottom 10 lowest age.index
bottom10_builddens <- builddens_athens_df %>%
  slice_min(order_by = mass_m3, n = 10)
# display
top10_builddens
bottom10_builddens


## plot
plot_builddens_athens_numbers <- ggplot() +
  geom_raster(data = builddens_athens_df, aes(x = x, y = y, fill = mass_m3)) +
  geom_sf(
    data        = athens_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(
    name     = "building mass 
    m³ / 100 m²",
    option   = "inferno",
    direction = -1,
    na.value = "white",
    labels = label_number(
      accuracy   = 1,       # round to integer
      big.mark   = ",",      # no thousands separator
    )
  ) +
  labs(title = "Building Density in Athens") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_builddens_athens_numbers)

## Normalize to [0,1]
mm_bld_athens   <- minmax(builddens_athens)      # c(min, max)
min_bld_athens  <- mm_bld_athens[1];  max_bld_athens <- mm_bld_athens[2]
builddens_athens_index  <- (builddens_athens - min_bld_athens) / (max_bld_athens - min_bld_athens)
names(builddens_athens_index) <- "builddens_index_athens"

## Plot indexed map
builddens_athens_index_df <- as.data.frame(builddens_athens_index, xy = TRUE, na.rm = TRUE)
names(builddens_athens_index_df)[3] <- "builddens_index_athens"

plot_builddens_athens_index <- ggplot() +
  geom_raster(data = builddens_athens_index_df, aes(x = x, y = y, fill = builddens_index_athens)) +
  geom_sf(
    data        = athens_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(
    name     = "Building Density Index",
    option   = "inferno",
    direction = -1,
    na.value = "white"
  )+
  labs(title = "Building Density in Athens (Index)") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_builddens_athens_index)

## Save indexed plot, data.frame and raster
#ggsave(
#  filename = "Data/built environment - building density/athens_building_density_index_map.png",
#  plot     = plot_builddens_athens_index,
##  width    = 8, height = 6, dpi = 300
#)

#write.csv(
#  builddens_athens_index_df,
#  file      = "Data/built environment - building density/athens_building_density_index_df.csv",
#  row.names = FALSE
#)


#writeRaster(
#  builddens_athens_index,
#  "Data/built environment - building density/athens_building_density_index.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)

####### GREEN VEGETATION COVER #######

## Load the green‐cover raster
veg_rast <- rast(
  "Data/built environment - green land cover/Athens/c_gls_FCOVER300-RT2_202506300000_GLOBE_OLCI_V1.1.1_EL30_FCOVER.tif"
)
crs(veg_rast)

## Crop & mask to Athens
veg_crop    <- crop(veg_rast,    ext(athens_vect))
veg_athens  <- mask(veg_crop,    athens_vect)

## Convert raw fractions to a data.frame
veg_athens_df <- as.data.frame(veg_athens, xy = TRUE, na.rm = TRUE)
names(veg_athens_df)[3] <- "fcover"  # fraction of green cover per 100 m²

## Plot raw green cover
plot_veg_athens_raw <- ggplot(veg_athens_df, aes(x = x, y = y, fill = fcover)) +
  geom_raster() +
  geom_sf(
    data        = athens_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_gradient(
    low  = "white",
    high = "darkgreen",
    name = "Fraction of 
    Vegetation Cover 
    per Grid Cell
    (max = 250)"
  ) +
  labs(title = "Green Vegetation Cover in Athens") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_veg_athens_raw)

## Normalize to [0,1]
mm_veg   <- minmax(veg_athens)       # returns c(min, max)
veg_index_athens  <- (veg_athens - mm_veg[1]) / (mm_veg[2] - mm_veg[1])
names(veg_index_athens) <- "veg_index_athens"

## Data.frame for the indexed raster
veg_index_athens_df <- as.data.frame(veg_index_athens, xy = TRUE, na.rm = TRUE)
names(veg_index_athens_df)[3] <- "veg_index_athens"

## Plot the index map
plot_veg_index_athens <- ggplot(veg_index_athens_df, aes(x = x, y = y, fill = veg_index_athens)) +
  geom_raster() +
  geom_sf(
    data        = athens_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colours = c("white", "yellowgreen", "darkgreen"),
    values  = c(0, 0.5, 1),
    name    = "Cover Index"
  ) +
  labs(title = "Green Vegetation Cover in Athens (Index)") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_veg_index_athens)

## Save the indexed plot and data.frame
#ggsave(
#  filename = "Data/built environment - green land cover/vegetation_cover_index_athens_map.png",
#  plot     = plot_veg_index_athens,
#  width    = 8, height = 6, dpi = 300
#)

#write.csv(
#  veg_index_athens_df,
#  file      = "Data/built environment - green land cover/vegetation_cover_index__athens_df.csv",
#  row.names = FALSE
#)


## Reverse Index in veg_athens_index to show heat vulnerability (high vegetation = low vulnerability)
veg_index_vuln_athens <- 1 - veg_index_athens

## Give it a clear layer name
names(veg_index_vuln_athens) <- "veg_index_vuln_athens"

## Quick check
minmax(veg_index_vuln_athens)
# should return 0 and 1

## turn into dataframe
veg_vulnerability_index_athens_df <- as.data.frame(veg_index_vuln_athens, xy = TRUE, na.rm = TRUE)
names(veg_vulnerability_index_athens_df)[3] <- "veg_index_vuln_athens"

## Plot vulnerability (higher index = more vulnerable)
plot_veg_vulnerability_index_athens <- ggplot() +
  geom_raster(
    data = veg_vulnerability_index_athens_df,
    aes(x = x, y = y, fill = veg_index_vuln_athens)
  ) +
  geom_sf(
    data        = athens_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(
    option    = "inferno",
    direction = -1,
    name      = "Low Vegetation Vulnerability Index",
    labels    = label_number(accuracy = 0.25)
  ) +
  labs(
    title = "Vulnerability Due To Low Green Vegetation Cover in Athens",
    subtitle = "1 = high vulnerability due to low vegetation cover"
  ) +
  coord_sf(datum = NA) +
  theme_void()

print(plot_veg_vulnerability_index_athens)

## Save the new data.frame, raster, and the plot
#write.csv(
#  veg_vulnerability_index_athens_df,
#  file      = "Data/built environment - green land cover/vegetation_vulnerability_index_athens_df.csv",
#  row.names = FALSE
#)

#ggsave(
#  filename = "Data/built environment - green land cover/vegetation_vulnerability_index_athens_map.png",
#  plot     = plot_veg_vulnerability_index_athens,
#  width    = 8,
#  height   = 6,
#  dpi      = 300
#)

#writeRaster(
#  veg_index_vuln_athens,
#  "Data/built environment - green land cover/vegetation_vulnerability_index_athens.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)

####### RISK INDEX #######
# objective of this section: create one risk index per city by
# 1: align all 6 indexed rasters into same grid resolution (crs is the same, but resolution not)
# 2: build arithmetic mean of the 6 rasters cell by cell (= vulnerability score for each cell)
# 3: plot the risk index into city map

## Use heat_index_athens as the “template” for CRS, extent, origin & resolution
# it is original EPSG:4326 with around 100m res and hence the best template
template_vi <- heat_index_athens

## List six input rasters (in any original CRS)
athens_rasters_raw <- list(
  pop    = pop_athens_index, 
  age    = age_athens_index,
  income = income_athens_index,
  heat   = heat_index_athens,
  bld    = builddens_athens_index,
  veg    = veg_index_vuln_athens 
)

## inspect classes
sapply(athens_rasters_raw, class)
# age_athens_index is not yet formatted as SpatRaster, but as sf object

# convert the age sf to a SpatVector
age_athens_index_vect <- vect(age_athens_index)  
# rasterize that vector by its index field
age_athens_index_rast <- rasterize(
  x      = age_athens_index_vect,
  y      = template_vi,
  field  = "age.index",
  fun    = "mean"            # in case of overlapping polygons
)
# give it a clear name
names(age_athens_index_rast) <- "age_athens_index"

# add age raster in return for the sf object
athens_rasters_raw$age <- age_athens_index_rast
#  double check classes
sapply(athens_rasters_raw, class)
# all SpatRaster now

## Reproject & resample every raw raster onto the heat raster’s grid
athens_rasters_final <- lapply(athens_rasters_raw, function(r) {
  project(r, template_vi, method = "bilinear")
})

## Restore the names
names(athens_rasters_final) <- names(athens_rasters_raw)

## Quick sanity-check: confirm CRS and resolution match the template
sapply(athens_rasters_final, function(x) {
  list(
    crs = crs(x),    
    res = res(x)    
  )
})

## Stack them for downstream cell-wise math
athens_stack <- rast(athens_rasters_final)

## Compute the arithmetic mean risk index in lon/lat
athens_vulnerability_index <- app(
  athens_stack,
  fun   = mean,
  na.rm = TRUE
)

names(athens_vulnerability_index) <- "vulnerability_index"

## Crop the vulnerability raster to the athens bounding box once more for sharp edges
vuln_crop <- crop(
  x = athens_vulnerability_index,
  y = ext(athens_vect)
)

## Mask out everything outside the true district shapes
vuln_mask <- mask(
  x    = vuln_crop,
  mask = athens_vect
)

## Assign back to the original name
athens_vulnerability_index <- vuln_mask


## Save the SpatRaster
#writeRaster(
#  athens_vulnerability_index,
#  "Data/risk index/athens_vulnerability_index.tif",
#  filetype   = "GTiff",
#  overwrite= TRUE
#)

## Convert risk_index SpatRaster to a data.frame
athens_vulnerability_index_df <- as.data.frame(
  athens_vulnerability_index,
  xy    = TRUE,
  na.rm = TRUE
)

## Build the ggplot 
plot_vulnerability_index_athens <- ggplot() +
  # raster layer
  geom_raster(
    data = athens_vulnerability_index_df,
    aes(x = x, y = y, fill = vulnerability_index)
  ) +
  # district outlines
  geom_sf(
    data        = athens_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE
  ) +
  # colour scale
  scale_fill_viridis_c(
    name   = "Heat Risk Index",
    option = "magma",      
    limits = c(0,1)
  ) +
  # title and no axes
  labs(title = "Combined Heat Risk Index – Athens") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.6, "lines")
  )

# display it
print(plot_vulnerability_index_athens)

## Save the plot 
ggsave(
  filename = "Data/risk index/athens_vulnerability_index_map.png",
  plot     = plot_vulnerability_index_athens,
  width    = 8,
  height   = 6,
  dpi      = 300
)

## save data frame 
#write.csv(athens_vulnerability_index_df, "Data/risk index/athens_vulnerability_index_df.csv", row.names = FALSE)

## inspect range of risk index by looking at lowest and highest values
# Get lowest 5 values
athens_vul_low_5 <- sort(athens_vulnerability_index_df$vulnerability_index)[1:5]
# Get highest 5 values
athens_vul_high_5 <- sort(athens_vulnerability_index_df$vulnerability_index, decreasing = TRUE)[1:5]
# Print results
athens_vul_low_5
athens_vul_high_5
# values range from 0.14 to 0.97

## Plot into histogram for visual index distribution
plot_athens_vulnerability_distribution <- ggplot(athens_vulnerability_index_df, aes(x = vulnerability_index)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 0.02, fill = "steelblue", color = "white") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), limits = c(0, 0.2), expand = c(0, 0)) +
  labs(
    title = "Distribution of Heat Risk Index Values in Athens",
    x = "Heat Risk Index Value",
    y = "% of Grid Cells per Risk Value"
  ) +
  theme_minimal()

# display it
print(plot_athens_vulnerability_distribution)

# save the plot
ggsave(
  filename = "Data/risk index/athens_vulnerability_index_distribution.png",
  plot     = plot_athens_vulnerability_distribution,
  width    = 8,
  height   = 6,
  dpi      = 300
)


####### EXISTING COOLING CENTRES ######

## read athens existing cooling centres
athens_cc_df <- read.csv("Data/existing cooling centres/athens_existing_coolingcentres.csv", sep = ";")

## Convert and reproject point CSV
athens_cc <- st_as_sf(
  athens_cc_df,
  coords = c("lon", "lat"),
  crs    = 4326             # WGS84
)

## Use ggplot with vulnerability raster + district outlines + cc locations
plot_vulnerability_index_athens_cc <- ggplot() +
  # raster layer
  geom_raster(
    data = athens_vulnerability_index_df,
    aes(x = x, y = y, fill = vulnerability_index)
  ) +
  # district outlines
  geom_sf(
    data        = athens_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE
  ) +
  # cooling centre points
  geom_sf(
    data        = athens_cc,
    inherit.aes = FALSE,
    shape       = 21,
    fill        = "white",
    colour      = "white",
    size        = 1
  ) +
  # colour scale
  scale_fill_viridis_c(
    name   = "Heat Risk Index",
    option = "magma",
    limits = c(0,1)
  ) +
  # title and no axes
  labs(title = "Existing Cooling Centres Against Heat Risk Index in Athens") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.6, "lines")
  )

# display it
print(plot_vulnerability_index_athens_cc)

## Save plot
ggsave(
  filename = "Data/risk index/athens_vulnerability_index_cc.png",
  plot     = plot_vulnerability_index_athens_cc,
  width    = 8,
  height   = 6,
  dpi      = 300
)


####### TRAVEL TIME #######
library(devtools)
install_github("https://github.com/giacfalk/locationallocation", force = TRUE)
library(locationallocation)

## compute travel time (walking minutes) from each grid cell to existing cc
athens_cc_traveltime <- traveltime(facilities=athens_cc, bb_area=athens_districts, dowscaling_model_type= "lm", mode="walk", res_output=100)

## plot travel time
athens_cc_traveltime_plot <- 
  traveltime_plot(traveltime=athens_cc_traveltime,  bb_area=athens_districts, facilities = athens_cc)

print(athens_cc_traveltime_plot)

# save travel time plot
dev.copy(png, "Data/travel time/athens_traveltime_existingcc.png", width = 800, height = 600)
dev.off()

## save travel time output for later use
save(athens_cc_traveltime, athens_cc_traveltime_plot, file = "output_athens_traveltime")

## calculate travel time thresholds
## as demand, the number of residents is extracted from the pop_athens raster

## this requires a raster layer instead of SpatRaster
pop_athens_raster <- raster(pop_athens)

## adjusted traveltime_stats function
traveltime_stats_adjusted <- function (traveltime, demand_raster, breaks = c(5, 10, 15, 30), objectiveminutes = 15, xlab_title = "Travel Time in Minutes", ylab_title = "Population Within Threshold (%)", point_size = 2, label_size = 3, legend_title = "Minutes", label_nudge = 1) {
  if (!inherits(traveltime, "list") || length(traveltime) != 2 || !inherits(traveltime[[1]], "RasterLayer") || !inherits(traveltime[[2]], "list") || length(traveltime[[2]]) != 3) {
    stop("Error: 'traveltime' must be an output object from the locationallocation::traveltime function.")
  }
  if (!inherits(demand_raster, "RasterLayer")) {
    stop("Error: 'demand_raster' must be a raster layer.")
  }
  if (!is.numeric(breaks) || length(breaks) < 1) {
    stop("Error: 'breaks' must be a numeric vector.")
  }
  if (!is.numeric(objectiveminutes) || length(objectiveminutes) != 1) {
    stop("Error: 'objectiveminutes' must be a numeric value.")
  }
  
  # require ggrepel for non-overlapping labels
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package 'ggrepel' is required for non-overlapping labels. Install with install.packages('ggrepel').")
  }
  
  raster::crs(traveltime[[1]]) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
  traveltime_proj <- raster::projectRaster(traveltime[[1]], demand_raster)
  raster::crs(traveltime_proj) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
  data_curve <- data.frame(time = raster::values(traveltime_proj), pop = raster::values(demand_raster))
  data_curve <- stats::na.omit(data_curve)
  data_curve <- data_curve %>% dplyr::arrange(time) %>% dplyr::mutate(P15_cumsum = (cumsum(pop) / sum(pop)) * 100)
  data_curve$th <- cut(data_curve$time, c(-Inf, sort(unique(breaks)), Inf), labels = c(paste0("<", sort(unique(breaks))), paste0(">", dplyr::last(sort(unique(breaks))))))
  breaks_sorted <- sort(unique(breaks))
  df_breaks <- data.frame(x = breaks_sorted, y = sapply(breaks_sorted, function(b) {
    vals <- data_curve$P15_cumsum[data_curve$time <= b]
    if (length(vals) == 0) return(NA_real_)
    max(vals, na.rm = TRUE)
  }))
  df_breaks <- df_breaks[!is.na(df_breaks$y), , drop = FALSE]
  p <- ggplot2::ggplot(data_curve) + ggplot2::theme_classic() + ggplot2::geom_step(ggplot2::aes(x = time, y = P15_cumsum, colour = th)) + ggplot2::ylab(ylab_title) + ggplot2::xlab(xlab_title) + ggplot2::scale_colour_brewer(palette = "Reds", direction = 1, name = legend_title)
  if (nrow(df_breaks) > 0) {
    p <- p + ggplot2::geom_point(data = df_breaks, ggplot2::aes(x = x, y = y), size = point_size) +
      ggrepel::geom_text_repel(
        data = df_breaks,
        ggplot2::aes(x = x, y = y, label = paste0(round(y, 0), "%")),
        nudge_y = label_nudge,
        size = label_size,
        box.padding = 0.35,
        point.padding = 0.6,
        segment.size = 0.25,
        min.segment.length = 0,
        max.overlaps = Inf,
        seed = 42
      )
  }
  print(p)
  pct <- round((sum(data_curve$pop[data_curve$time <= objectiveminutes], na.rm = TRUE) / sum(data_curve$pop)) * 100, 2)
  return(p)
}


## run adjusted function
plot_traveltime_stats_athens <- traveltime_stats_adjusted(traveltime = athens_cc_traveltime, demand_raster = pop_athens_raster, breaks=c(5, 10, 15, 30), objectiveminutes=15)

# save output
dev.copy(png, "Data/travel time/athens_traveltime_existingcc_stats.png", width = 800, height = 600)
dev.off()


####### ALLOCATION PREP #######

## read potential cooling centres facilities
potential_facilities <- read.csv("Data/allocation/athens_all_candidates.csv")

## Convert and reproject point CSV
athens_candidates <- st_as_sf(
  potential_facilities,
  coords = c("x", "y"),
  crs    = 4326             # WGS84
)

## turn vulnerability spatraster into raster layer
vulnindex_athens <- raster(athens_vulnerability_index)

## few adjustments are needed to ensure same crs and origin of the raster layers
crs(pop_athens_raster) <- "EPSG:4326"
crs(vulnindex_athens) <- "EPSG:4326"

pop_athens_final <- resample(pop_athens_raster, vulnindex_athens, method = "bilinear")

## run allocation function

#### SCENARIO 1 ####

## allocation
output_allocation_athens_s1 <- allocation_discrete(demand_raster = pop_athens_final, traveltime_raster=athens_cc_traveltime, bb_area = athens_districts, facilities=athens_cc, candidate=athens_candidates, n_fac = 252, objectiveshare = 0.8, weights=vulnindex_athens, objectiveminutes=15, exp_demand = 2, exp_weights = 1, dowscaling_model_type="lm", mode="walk", res_output=100, n_samples=1000, par=FALSE)

## plot
plot_athens_s1 <- allocation_plot_discrete(output_allocation_athens_s1, bb_area = athens_districts)
print(plot_athens_s1)

## save plot
dev.copy(png, "Data/allocation/athens_allocation_s1_plot.png", width = 800, height = 600)
dev.off()

## save dataframe with allocated facilities
write.csv(output_allocation_athens_s1[[1]], "Data/allocation/athens_allocation_s1_facilities.csv", row.names = FALSE)

## check stats as validation
athens_s1_cc <- bind_rows(athens_cc, output_allocation_athens_s1[[1]])
athens_cc_s1_traveltime <- traveltime(facilities=athens_s1_cc, bb_area=athens_districts, dowscaling_model_type="lm", mode="walk", res_output=100)
traveltime_stats(traveltime = athens_cc_s1_traveltime, demand_raster = pop_athens_final, breaks=c(5, 10, 15, 30), objectiveminutes=15)
# 
# weighted allocation:
# [1] "Iteration with 2 facilities."
# [1] "Coverage share attained: 0.873113150982062"
# [1] "Target coverage share of 0.8 attained with 2 facilities"
# non weighted 
# [1] "83.54 % of demand layer within the objectiveminutes threshold."


#### SCENARIO 2 #### 

## allocation
output_allocation_athens_s2 <- allocation_discrete(demand_raster = pop_athens_final, traveltime_raster=athens_cc_traveltime, bb_area = athens_districts, facilities=athens_cc, candidate=athens_candidates, n_fac = 252, objectiveshare = 0.99, weights=vulnindex_athens, objectiveminutes=15, exp_demand = 1, exp_weights = 1, dowscaling_model_type="lm", mode="walk", res_output=100, n_samples=1000, par=FALSE)

## plot
plot_athens_s2 <- allocation_plot_discrete(output_allocation_athens_s2, bb_area = athens_districts)
print(plot_athens_s2)

## save plot
dev.copy(png, "Data/allocation/athens_allocation_s2_plot.png", width = 800, height = 600)
dev.off()

## save dataframe with allocated facilities
write.csv(output_allocation_athens_s2[[1]], "Data/allocation/athens_allocation_s2_facilities.csv", row.names = FALSE)


#### SCENARIO 3 #### 

## allocation
output_allocation_athens_s3_1 <- allocation_discrete(demand_raster = pop_athens_final, traveltime_raster=athens_cc_traveltime, bb_area = athens_districts, facilities=athens_cc, candidate=athens_candidates, n_fac = 1, weights=vulnindex_athens, objectiveminutes=15, exp_demand = 3, exp_weights = 1, dowscaling_model_type="lm", mode="walk", res_output=100, n_samples=252, par=FALSE)

## plot
plot_athens_s3 <- allocation_plot_discrete(output_allocation_athens_s3, bb_area = athens_districts)
print(plot_athens_s3)

## save plot
dev.copy(png, "Data/allocation/athens_allocation_s3_plot.png", width = 800, height = 600)
dev.off()

## save dataframe with allocated facilities
write.csv(output_allocation_athens_s3[[1]], "Data/allocation/athens_allocation_s3_facilities.csv", row.names = FALSE)







