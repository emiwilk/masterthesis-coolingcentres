library(sf)
library(terra)
library(dplyr)  
library(ggplot2)
library(scales)
library(raster)

############## BOLOGNA DATA PREPARATION ############
## ensure targetcrs for all raster and sf objects is EPSG:4326
targetcrs <- "EPSG:4326"


####### BOUNDARY SHAPES #######

## read boundary shape file into an sf object
bologna_districts <- st_read("Data/Boundary Shapefiles/bologna_district_boundaries/quartieri.shp")

## see the first few rows and geometry type
print(head(bologna_districts))
st_geometry_type(bologna_districts)
st_crs(bologna_districts)

## Reproject the sf object to targetcrs EPSG:4326
bologna_districts_4326 <- st_transform(bologna_districts, crs = targetcrs)

## rename for ease of use
bologna_districts <- bologna_districts_4326
# check crs, if reprojection successful
st_crs(bologna_districts)
# shows EPSG:4326

## plot districts
plot(st_geometry(bologna_districts))

## for income analysis we also need zones
bologna_zones <- st_read("Data/Boundary Shapefiles/bologna_district_boundaries/zone.shp")

## see the first few rows and geometry type
print(head(bologna_zones))
st_geometry_type(bologna_zones)
st_crs(bologna_zones)

## Reproject the sf object to targetcrs EPSG:4326
bologna_zones_4326 <- st_transform(bologna_zones, crs = targetcrs)

## rename for ease of use
bologna_zones <- bologna_zones_4326
# check crs, if reprojection successful
st_crs(bologna_zones)
# shows EPSG:4326

## plot zones
plot(st_geometry(bologna_zones))


####### INCOME #######

## read csv for income in bologna
income_bologna <- read.csv("Data/socio-demographic - income/bologna_income.csv", stringsAsFactors = FALSE, header = TRUE, sep = ";")

## left‐join the income table onto the spatial (sf) object
income_bologna_shp <- bologna_zones %>%
  left_join(income_bologna, by = "NOMEZONA")

## plot with colour gradient for average income as visual control
ggplot(income_bologna_shp) +
  geom_sf(aes(fill = average.annual.income.brutto), colour = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", name = "Average Annual Income",breaks = seq( 0,
    max(income_bologna_shp$average.annual.income.brutto, na.rm = TRUE), by = 5000),
  labels = comma_format()) +
  labs(title = "Average Annual Income by District in Bologna") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

## add normalised index column for income > index is reversed, showing vulnerability due to low income
income_bologna_index <- income_bologna_shp %>%
  mutate(
    income.index = 1- (average.annual.income.brutto - min(average.annual.income.brutto, na.rm = TRUE)) /
      (max(average.annual.income.brutto, na.rm = TRUE) - min(average.annual.income.brutto, na.rm = TRUE))
  )

## Inspect the first few rows
head(income_bologna_index)

## sanity check: what’s the range?
range(income_bologna_index$income.index, na.rm = TRUE)

## plot income index
plot_income_bologna <-
  ggplot(income_bologna_index) +
  geom_sf(aes(fill = income.index), colour = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", 
                       name = "Income Vulnerability index",
                       direction = -1
  ) +
  labs(title = "Normalised Income Vulnerability Index by Zone in Bologna",
       subtitle = "1 = high vulnerability due to low income") +
  theme_minimal() +
  theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  axis.text = element_blank(),
  axis.ticks = element_blank()
)

# print plot
plot_income_bologna

## save plot for later
ggsave(
  filename = "Data/socio-demographic - income/bologna_income_index_map.png",  # can be .png, .pdf, .jpg, etc.
  plot     = plot_income_bologna,                           # omit if it’s the last plot you printed
  width    = 8,                           # in inches
  height   = 6,                           # in inches
  dpi      = 300                          # for raster formats
)

## save income index file
st_write(
  obj    = income_bologna_index,
  dsn    = "Data/socio-demographic - income/income_bologna_index.gpkg",
  layer  = "income_bologna_index",
  driver = "GPKG",
  delete_layer = TRUE  # overwrite if it exists
)

## save income in numbers file
st_write(
  obj    = income_bologna_shp,
  dsn    = "Data/socio-demographic - income/income_bologna_shp_minimal.gpkg",
  layer  = "income_bologna_shp_minimal",
  driver = "GPKG",
  delete_layer = TRUE  # overwrite if it exists
)

# read it later with
#income_bologna_index <- st_read("Data/income_bologna_index.gpkg", layer = "income_bologna_index")


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
bologna_3035 <- st_transform(bologna_districts, crs = 3035)

## Check they now match
st_crs(bologna_3035)
# should also report EPSG:3035 (ETRS89-extended / LAEA Europe)

## Convert to terra SpatVector if you need to mask/crop a terra raster
bologna_vect <- vect(bologna_3035)

## Crop Census Raster to bologna boundary
bologna_census_3035 <- st_intersection(census2021, bologna_3035)

## Reproject census raster to targetcrs EPSG:4326 
bologna_census <- st_transform(bologna_census_3035, targetcrs)
# Check successful transformation
st_crs(bologna_census)

## Check
plot(st_geometry(bologna_districts), border="black")
plot(st_geometry(bologna_census), col=NA, border="red", add=TRUE)

## Now we only keep the relevant data of census_bologna
age_bologna <- bologna_census [, c("GRD_ID", "T", "Y_GE65", "NUMQUART", "geom")]

## add columns for age ratio and age index based on the absolute numbers provided
age_bologna_index <- age_bologna %>%
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
range(age_bologna_index$age.ratio,  na.rm = TRUE)
range(age_bologna_index$age.index,  na.rm = TRUE)
head(age_bologna_index, 10)

## check numbers and validity
# Top 10 highest age.index
top10_age <- age_bologna_index %>%
  slice_max(order_by = age.index, n = 10)
  # Bottom 10 lowest age.index
bottom10_age <- age_bologna_index %>%
  slice_min(order_by = age.index, n = 10)
# Display
top10_age
bottom10_age

## plot age index raster
plot_age_bologna <- ggplot() +
  # plot the grid cells, no borders between cells
  geom_sf(
    data    = age_bologna_index,
    aes(fill = age.index),
    colour  = NA
  ) +
  # add district outlines
  geom_sf(
    data    = bologna_districts,
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
    title    = "Share of Population Aged >65 by 1 km Grid in Bologna (Index)",
    subtitle = "white: no population aged >65"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# print plot
plot_age_bologna

## we see white squares, those are squares with no population 
## darkest square has 80% residents over 65, because it is T=4 and >65=3

## Save files 
# save plot
ggsave(
  filename = "Data/socio-demographic - age/bologna_age_index_map.png",  # can be .png, .pdf, .jpg, etc.
  plot     = plot_age_bologna,                           # omit if it’s the last plot you printed
  width    = 8,                           # in inches
  height   = 6,                           # in inches
  dpi      = 300                          # for raster formats
)

# save age index file
st_write(
  obj    = age_bologna_index,
  dsn    = "Data/socio-demographic - age/age_bologna_index.gpkg",
  layer  = "age_bologna_index",
  driver = "GPKG",
  delete_layer = TRUE  # overwrite if it exists
)

# save age in numbers file, cropped to bologna
st_write(
  obj    = age_bologna,
  dsn    = "Data/socio-demographic - age/age_bologna.gpkg",
  layer  = "age_bologna",
  driver = "GPKG",
  delete_layer = TRUE  # overwrite if it exists
)


####### POPULATION DENSITY #######

## load global pop-density tif file with total population numbers
pop_globe <- rast("Data/demand - population density/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0.tif")

## inspect raster crs
crs(pop_globe)

## It is a large data set, so we crop it in its original crs before reprojecting
## Bring bologna districts into terra
bologna_vect <- vect(bologna_districts)

## Reproject to the raster’s World_Mollweide CRS
bologna_vect_moll <- project(bologna_vect, crs(pop_globe))

## Crop & mask global raster to bologna boundary
pop_crop   <- crop(pop_globe, ext(bologna_vect_moll))
pop_bologna <- mask(pop_crop, bologna_vect_moll)

# plot for first visual check
plot(
  pop_bologna,
  main   = "2025 Population Density – Bologna",
  legend = TRUE,
  plg    = list(title = "residents / 100 m²")
)

## now for better compatibility we reproject the raster into target crs
## Project the masked Bologna raster into that CRS
#    - method = "bilinear" is good for continuous data like density
pop_bologna_4326 <- project(
  x      = pop_bologna,
  y      = targetcrs,
  method = "bilinear"
)

## we crop and mask once more to receive a perfectly clean edge
## turn district boundary into terra object

## Crop to the bounding box of bologna
pop_bologna_4326_crop <- crop(
  x = pop_bologna_4326,
  y = ext(bologna_districts)
)

## Mask out everything outside the true district shapes
pop_bologna_4326_mask <- mask(
  x    = pop_bologna_4326_crop,
  mask = bologna_districts
)

## rename 
pop_bologna <- pop_bologna_4326_mask

## Plot to check perfectly cropped edges and save
png(
  filename = "Data/demand - population density/bologna_population_density_numbers.png", 
  width    = 1600,     # pixels
  height   = 1200,     # pixels
  res      = 300       # dpi
)
plot(
  pop_bologna,
  main   = "2025 Population Density – bologna",
  legend = TRUE,
  plg    = list(title = " residents
                / 100 m²")
)
plot(
  bologna_vect,
  add    = TRUE,
  border = "white",
  lwd    = 1
)
dev.off()

## normalise values onto 0 to 1 scale 
names(pop_bologna)
class(pop_bologna)

## compute the min and max
mm    <- minmax(pop_bologna)
minv  <- mm[1]
maxv  <- mm[2]

## linearly rescale: (x - min) / (max - min)
pop_bologna_index <- (pop_bologna - minv) / (maxv - minv)

## give it a name
names(pop_bologna_index) <- "pop_bologna_index"

## quick check on the new range
global(pop_bologna_index, fun = c("min","max"), na.rm = TRUE)
# should return 0 and 1

## plot for visual verification
## Create df for ggplot 
pop_bologna_df <- as.data.frame(
  pop_bologna_index,
  xy      = TRUE,
  na.rm   = TRUE    # <— this drops rows where the value is NA
)
names(pop_bologna_df)[3] <- "pop.index"

## Plot
plot_pop_bologna <- ggplot() +
  geom_raster(
    data = pop_bologna_df,
    aes(x = x, y = y, fill = pop.index)
  ) +
  geom_sf(
    data        = bologna_districts,
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
    title = "Normalised Population Density in Bologna"
  ) +
  theme_void() +
  theme(
    legend.title   = element_text(size = 10),
    legend.text    = element_text(size = 8),
    legend.key.size= unit(0.6, "lines")
  )

plot_pop_bologna

## save plot and files

## Save the plot
ggsave(
  filename = "Data/demand - population density/bologna_pop_density_map.png", 
  plot     = plot_pop_bologna, 
  width    = 8, 
  height   = 6, 
  dpi      = 300
)

## Write out the data.frame
write.csv(
  pop_bologna_df,
  file      = "Data/demand - population density/pop_bologna_df.csv",
  row.names = FALSE
)

## Save the SpatRaster
writeRaster(
  pop_bologna_index,
  "Data/demand - population density/pop_bologna_index.tif",
  filetype   = "GTiff",
  overwrite= TRUE
)

## save the total numbers as raster
writeRaster(
  pop_bologna,
  "Data/demand - population density/pop_bologna.tif",
  filetype   = "GTiff",
  overwrite= TRUE
)

####### HEAT STRESS #######

## Load the heat-stress raster
bologna_heat_rast <- rast(
  "Data/climate - moderate heat stress/impact_geo_curpol_bologna_urbclim-WBGT-dayover25_0.5_absolute_2030_0.001087076621267613.tif"
)

crs(bologna_heat_rast)

## Reproject & crop/mask to bologna
heat_crop   <- crop(bologna_heat_rast, ext(bologna_vect))
heat_bologna <- mask(heat_crop, bologna_vect)

## Plot raw days‐per‐year map
heat_bologna_df <- as.data.frame(heat_bologna, xy = TRUE, na.rm = TRUE)
names(heat_bologna_df)[3] <- "heatstress_days"

plot_bologna_heat <- ggplot() +
  geom_raster(data = heat_bologna_df, aes(x = x, y = y, fill = heatstress_days)) +
  geom_sf(data = bologna_districts, 
          fill = NA, colour = "black", size = 0.3, inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Days/year", na.value = "white") +
  labs(title = "Days a Year with Moderate Heat Stress in Bologna") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_bologna_heat) 
# the provided data set does not extend fully to the shape of bologna in the west
# those are areas with low heat stress so it does not affect the analysis significantly

#save as csv
write.csv(
  heat_bologna_df,
  file      = "Data/climate - moderate heat stress/bologna_heat_stress_df.csv",
  row.names = FALSE
)

## Save the SpatRaster
writeRaster(
  heat_bologna,
  "Data/climate - moderate heat stress/heat_bologna.tif",
  filetype   = "GTiff",
  overwrite= TRUE
)

## Normalize to [0,1]
mm       <- minmax(heat_bologna)    # c(min, max)
minv     <- mm[1]; maxv <- mm[2]
heat_index_bologna <- (heat_bologna - minv) / (maxv - minv)
names(heat_index_bologna) <- "heat_index_bologna"

## Plot indexed map
heat_index_bologna_df <- as.data.frame(heat_index_bologna, xy = TRUE, na.rm = TRUE)
names(heat_index_bologna_df)[3] <- "heat_index_bologna"

plot_heat_index_bologna <- ggplot() +
  geom_raster(data = heat_index_bologna_df, aes(x = x, y = y, fill = heat_index_bologna)) +
  geom_sf(data = st_transform(bologna_districts, crs(heat_index_bologna)), 
          fill = NA, colour = "black", size = 0.3, inherit.aes = FALSE) +
  scale_fill_gradientn(
    colours = c("yellow", "orange", "darkred"),
    name    = "Heat Stress Index"
  ) +
  labs(title = "Days a Year with Moderate Heat Stress in Bologna (Index)") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_heat_index_bologna)  # display it

## Save the indexed plot, data.frame, and raster
ggsave(
  filename = "Data/climate - moderate heat stress/bologna_heat_stress_index_map.png",
  plot     = plot_heat_index_bologna,
  width    = 8, height = 6, dpi = 300
)

write.csv(
  heat_index_bologna_df,
  file      = "Data/climate - moderate heat stress/bologna_heat_stress_index_df.csv",
  row.names = FALSE
)

## Save the SpatRaster
writeRaster(
  heat_index_bologna,
  "Data/climate - moderate heat stress/bologna_heat_stress_index.tif",
  filetype   = "GTiff",
  overwrite= TRUE
)

####### BUILDING DENSITY #######

## Load the building density raster
building_dens_globe <- rast(
  "Data/built environment - building density/GHS_building_density.tif"
)
crs(building_dens_globe)

## It is a large data set, so we crop it in its original crs before reprojecting
## Bring Bologna districts into terra
bologna_vect <- vect(bologna_districts)

## Reproject to the raster’s World_Mollweide CRS
bologna_vect_moll <- project(bologna_vect, crs(building_dens_globe))

## Crop & mask global raster to bologna boundary
building_dens_crop   <- crop(building_dens_globe, ext(bologna_vect_moll))
building_dens_mask <- mask(building_dens_crop, bologna_vect_moll)

# plot for first visual check
plot(
  building_dens_mask,
  main   = "2025 Building Density – Bologna",
  legend = TRUE,
  plg    = list(title = "building mass m³ / 100 m²")
)

## now for better compatibility we reproject the raster into target crs
## Project the masked Bologna raster into that CRS
#    - method = "bilinear" is good for continuous data like density
builddens_bologna <- project(
  x      = building_dens_mask,
  y      = targetcrs,
  method = "bilinear"
)

## we crop and mask once more to receive a perfectly clean edge
## turn district boundary into terra object

## Crop to the bounding box of Bologna
builddens_bologna_crop <- crop(
  x = builddens_bologna,
  y = ext(bologna_districts)
)

## Mask out everything outside the true district shapes
builddens_bologna_mask <- mask(
  x    = builddens_bologna_crop,
  mask = bologna_districts
)

## rename and check crs
builddens_bologna <- builddens_bologna_mask
crs(builddens_bologna)
# shows EPSG:4326

## Plot raw building mass (m³/100 m²)
builddens_bologna_df <- as.data.frame(builddens_bologna, xy = TRUE, na.rm = TRUE)
names(builddens_bologna_df)[3] <- "mass_m3"

## check extent of numbers with bottom10 and top10
# Top 10 highest building mass
top10_builddens <- builddens_bologna_df %>%
  slice_max(order_by = mass_m3, n = 10)
# Bottom 10 lowest age.index
bottom10_builddens <- builddens_bologna_df %>%
  slice_min(order_by = mass_m3, n = 10)
# display
top10_builddens
bottom10_builddens


## plot
plot_builddens_bologna_numbers <- ggplot() +
  geom_raster(data = builddens_bologna_df, aes(x = x, y = y, fill = mass_m3)) +
  geom_sf(
    data        = bologna_districts,
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
  labs(title = "Building Density in Bologna") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_builddens_bologna_numbers)

## Normalize to [0,1]
mm_bld_bologna   <- minmax(builddens_bologna)      # c(min, max)
min_bld_bologna  <- mm_bld_bologna[1];  max_bld_bologna <- mm_bld_bologna[2]
builddens_bologna_index  <- (builddens_bologna - min_bld_bologna) / (max_bld_bologna - min_bld_bologna)
names(builddens_bologna_index) <- "builddens_index_bologna"

## Plot indexed map
builddens_bologna_index_df <- as.data.frame(builddens_bologna_index, xy = TRUE, na.rm = TRUE)
names(builddens_bologna_index_df)[3] <- "builddens_index_bologna"

plot_builddens_bologna_index <- ggplot() +
  geom_raster(data = builddens_bologna_index_df, aes(x = x, y = y, fill = builddens_index_bologna)) +
  geom_sf(
    data        = bologna_districts,
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
  labs(title = "Building Density in Bologna (Index)") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_builddens_bologna_index)

## Save indexed plot, data.frame and raster
ggsave(
  filename = "Data/built environment - building density/bologna_building_density_index_map.png",
  plot     = plot_builddens_bologna_index,
  width    = 8, height = 6, dpi = 300
)

write.csv(
  builddens_bologna_index_df,
  file      = "Data/built environment - building density/bologna_building_density_index_df.csv",
  row.names = FALSE
)


writeRaster(
  builddens_bologna_index,
  "Data/built environment - building density/bologna_building_density_index.tif",
  filetype   = "GTiff",
  overwrite= TRUE
)

####### GREEN VEGETATION COVER #######

## Load the green‐cover raster
veg_rast <- rast(
  "Data/built environment - green land cover/Bologna/c_gls_FCOVER300-RT1_202507100000_GLOBE_OLCI_V1.1.1_ITH55_FCOVER.tif"
)
crs(veg_rast)

## Crop & mask to Bologna
veg_crop    <- crop(veg_rast,    ext(bologna_vect))
veg_bologna  <- mask(veg_crop,    bologna_vect)

## Convert raw fractions to a data.frame
veg_bologna_df <- as.data.frame(veg_bologna, xy = TRUE, na.rm = TRUE)
names(veg_bologna_df)[3] <- "fcover"  # fraction of green cover per 100 m²

## Plot raw green cover
plot_veg_bologna_raw <- ggplot(veg_bologna_df, aes(x = x, y = y, fill = fcover)) +
  geom_raster() +
  geom_sf(
    data        = bologna_districts,
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
  labs(title = "Green Vegetation Cover in Bologna") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_veg_bologna_raw)

## Normalize to [0,1]
mm_veg   <- minmax(veg_bologna)       # returns c(min, max)
veg_index_bologna  <- (veg_bologna - mm_veg[1]) / (mm_veg[2] - mm_veg[1])
names(veg_index_bologna) <- "veg_index_bologna"

## Data.frame for the indexed raster
veg_index_bologna_df <- as.data.frame(veg_index_bologna, xy = TRUE, na.rm = TRUE)
names(veg_index_bologna_df)[3] <- "veg_index_bologna"

## Plot the index map
plot_veg_index_bologna <- ggplot(veg_index_bologna_df, aes(x = x, y = y, fill = veg_index_bologna)) +
  geom_raster() +
  geom_sf(
    data        = bologna_districts,
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
  labs(title = "Green Vegetation Cover in Bologna (Index)") +
  coord_sf(datum = NA) +
  theme_void()

print(plot_veg_index_bologna)

## Save the indexed plot and data.frame
ggsave(
  filename = "Data/built environment - green land cover/vegetation_cover_index_bologna_map.png",
  plot     = plot_veg_index_bologna,
  width    = 8, height = 6, dpi = 300
)

write.csv(
  veg_index_bologna_df,
  file      = "Data/built environment - green land cover/vegetation_cover_index__bologna_df.csv",
  row.names = FALSE
)


## Reverse Index in veg_bologna_index to show heat vulnerability (high vegetation = low vulnerability)
veg_index_vuln_bologna <- 1 - veg_index_bologna

## Give it a clear layer name
names(veg_index_vuln_bologna) <- "veg_index_vuln_bologna"

## Quick check
minmax(veg_index_vuln_bologna)
# should return 0 and 1

## turn into dataframe
veg_vulnerability_index_bologna_df <- as.data.frame(veg_index_vuln_bologna, xy = TRUE, na.rm = TRUE)
names(veg_vulnerability_index_bologna_df)[3] <- "veg_index_vuln_bologna"

## Plot vulnerability (higher index = more vulnerable)
plot_veg_vulnerability_index_bologna <- ggplot() +
  geom_raster(
    data = veg_vulnerability_index_bologna_df,
    aes(x = x, y = y, fill = veg_index_vuln_bologna)
  ) +
  geom_sf(
    data        = bologna_districts,
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
    title = "Vulnerability Due To Low Green Vegetation Cover in Bologna",
    subtitle = "1 = high vulnerability due to low vegetation cover"
  ) +
  coord_sf(datum = NA) +
  theme_void()

print(plot_veg_vulnerability_index_bologna)

## Save the new data.frame, raster, and the plot
write.csv(
  veg_vulnerability_index_bologna_df,
  file      = "Data/built environment - green land cover/vegetation_vulnerability_index_bologna_df.csv",
  row.names = FALSE
)

ggsave(
  filename = "Data/built environment - green land cover/vegetation_vulnerability_index_bologna_map.png",
  plot     = plot_veg_vulnerability_index_bologna,
  width    = 8,
  height   = 6,
  dpi      = 300
)

writeRaster(
  veg_index_vuln_bologna,
  "Data/built environment - green land cover/vegetation_vulnerability_index_bologna.tif",
  filetype   = "GTiff",
  overwrite= TRUE
)

####### RISK INDEX #######
# objective of this section: create one risk index per city by
# 1: align all 6 indexed rasters into same grid resolution (crs is the same, but resolution not)
# 2: build arithmetic mean of the 6 rasters cell by cell (= vulnerability score for each cell)
# 3: plot the risk index into city map

## Use heat_index_bologna as the “template” for CRS, extent, origin & resolution
# it is original EPSG:4326 with around 100m res and hence the best template
template_vi <- heat_index_bologna

## List six input rasters (in any original CRS)
bologna_rasters_raw <- list(
  pop    = pop_bologna_index, 
  age    = age_bologna_index,
  income = income_bologna_index,
  heat   = heat_index_bologna,
  bld    = builddens_bologna_index,
  veg    = veg_index_vuln_bologna 
)

## inspect classes
sapply(bologna_rasters_raw, class)
# age_bologna_index and income_bologna_index are not yet formatted as SpatRaster, but as sf objects

# convert the income sf to a SpatVector
income_bologna_index_vect <- vect(income_bologna_index)  
# rasterize that vector by its index field
income_bologna_index_rast <- rasterize(
  x      = income_bologna_index_vect,
  y      = template_vi,
  field  = "income.index",
  fun    = "mean"            # in case of overlapping polygons
)
# give it a clear name
names(income_bologna_index_rast) <- "income_bologna_index"

# convert the age sf to a SpatVector
age_bologna_index_vect <- vect(age_bologna_index)  
# rasterize that vector by its index field
age_bologna_index_rast <- rasterize(
  x      = age_bologna_index_vect,
  y      = template_vi,
  field  = "age.index",
  fun    = "mean"            # in case of overlapping polygons
)
# give it a clear name
names(age_bologna_index_rast) <- "age_bologna_index"

# add income and age raster in return for the sf objects
bologna_rasters_raw$income <- income_bologna_index_rast
bologna_rasters_raw$age <- age_bologna_index_rast
#  double check classes
sapply(bologna_rasters_raw, class)
# all SpatRaster now

## Reproject & resample every raw raster onto the heat raster’s grid
bologna_rasters_final <- lapply(bologna_rasters_raw, function(r) {
  project(r, template_vi, method = "bilinear")
})

## Restore the names
names(bologna_rasters_final) <- names(bologna_rasters_raw)

## Quick sanity-check: confirm CRS and resolution match the template
sapply(bologna_rasters_final, function(x) {
  list(
    crs = crs(x),    
    res = res(x)    
  )
})

## Stack them for downstream cell-wise math
bologna_stack <- rast(bologna_rasters_final)

## Compute the arithmetic mean risk index in lon/lat
bologna_vulnerability_index <- app(
  bologna_stack,
  fun   = mean,
  na.rm = TRUE
)

names(bologna_vulnerability_index) <- "vulnerability_index"

## Crop the vulnerability raster to the Bologna bounding box once more for sharp edges
vuln_crop <- crop(
  x = bologna_vulnerability_index,
  y = ext(bologna_vect)
)

## Mask out everything outside the true district shapes
vuln_mask <- mask(
  x    = vuln_crop,
  mask = bologna_vect
)

## Assign back to the original name
bologna_vulnerability_index <- vuln_mask


## Save the SpatRaster
writeRaster(
  bologna_vulnerability_index,
  "Data/risk index/bologna_vulnerability_index.tif",
  filetype   = "GTiff",
  overwrite= TRUE
)

## Convert risk_index SpatRaster to a data.frame
bologna_vulnerability_index_df <- as.data.frame(
  bologna_vulnerability_index,
  xy    = TRUE,
  na.rm = TRUE
)

## Build the ggplot 
plot_vulnerability_index_bologna <- ggplot() +
  # raster layer
  geom_raster(
    data = bologna_vulnerability_index_df,
    aes(x = x, y = y, fill = vulnerability_index)
  ) +
  # district outlines
  geom_sf(
    data        = bologna_districts,
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
  labs(title = "Combined Heat Risk Index – Bologna") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.6, "lines")
  )

# display it
print(plot_vulnerability_index_bologna)

## Save the plot 
ggsave(
  filename = "Data/risk index/bologna_vulnerability_index_map.png",
  plot     = plot_vulnerability_index_bologna,
  width    = 8,
  height   = 6,
  dpi      = 300
)

## save data frame 
write.csv(bologna_vulnerability_index_df, "Data/risk index/bologna_vulnerability_index_df.csv", row.names = FALSE)

## inspect range of risk index by looking at lowest and highest values
# Get lowest 5 values
bologna_vul_low_5 <- sort(bologna_vulnerability_index_df$vulnerability_index)[1:5]
# Get highest 5 values
bologna_vul_high_5 <- sort(bologna_vulnerability_index_df$vulnerability_index, decreasing = TRUE)[1:5]
# Print results
bologna_vul_low_5
bologna_vul_high_5
# values range from 0.04 to 0.92

## Plot into histogram for visual index distribution
plot_bologna_vulnerability_distribution <- ggplot(bologna_vulnerability_index_df, aes(x = vulnerability_index)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 0.02, fill = "steelblue", color = "white") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), limits = c(0, 0.2), expand = c(0, 0)) +
  labs(
    title = "Distribution of Heat Risk Index Values in Bologna",
    x = "Heat Risk Index Value",
    y = "% of Grid Cells per Risk Value"
  ) +
  theme_minimal()

# display it
print(plot_bologna_vulnerability_distribution)

# save the plot
ggsave(
  filename = "Data/risk index/bologna_vulnerability_index_distribution.png",
  plot     = plot_bologna_vulnerability_distribution,
  width    = 8,
  height   = 6,
  dpi      = 300
)


####### EXISTING COOLING CENTRES ######

## read bologna existing cooling centres
bologna_cc_df <- read.csv("Data/existing cooling centres/bologna_existing_coolingcentres.csv", sep = ";")

## Convert and reproject point CSV
bologna_cc <- st_as_sf(
  bologna_cc_df,
  coords = c("lon", "lat"),
  crs    = 4326             # WGS84
)

## Use ggplot with vulnerability raster + district outlines + cc locations
plot_vulnerability_index_bologna_cc <- ggplot() +
  # raster layer
  geom_raster(
    data = bologna_vulnerability_index_df,
    aes(x = x, y = y, fill = vulnerability_index)
  ) +
  # district outlines
  geom_sf(
    data        = bologna_districts,
    fill        = NA,
    colour      = "black",
    size        = 0.3,
    inherit.aes = FALSE
  ) +
  # cooling centre points
  geom_sf(
    data        = bologna_cc,
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
  labs(title = "Existing Cooling Centres Against Heat Risk Index in Bologna") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.6, "lines")
  )

# display it
print(plot_vulnerability_index_bologna_cc)

## Save plot
ggsave(
  filename = "Data/risk index/bologna_vulnerability_index_cc.png",
  plot     = plot_vulnerability_index_bologna_cc,
  width    = 8,
  height   = 6,
  dpi      = 300
)


####### TRAVEL TIME #######
library(devtools)
install_github("https://github.com/giacfalk/locationallocation")
library(locationallocation)

## compute travel time (walking minutes) from each grid cell to existing cc
bologna_cc_traveltime <- traveltime(facilities=bologna_cc, bb_area=bologna_districts, dowscaling_model_type="lm", mode="walk", res_output=100)

## plot travel time
bologna_cc_traveltime_plot <- 
  traveltime_plot(traveltime=bologna_cc_traveltime,  bb_area=bologna_districts, facilities = bologna_cc)

print(bologna_cc_traveltime_plot)

# save travel time plot
dev.copy(png, "Data/travel time/bologna_traveltime_existingcc.png", width = 800, height = 600)
dev.off()

## calculate travel time thresholds
## as demand, the number of residents is extracted from the pop_bologna raster

## this requires a raster layer instead of SpatRaster
pop_bologna_raster <- raster(pop_bologna)

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
plot_traveltime_stats_bologna <- traveltime_stats_adjusted(traveltime = bologna_cc_traveltime, demand_raster = pop_bologna_raster, breaks=c(5, 10, 15, 30), objectiveminutes=15)

# save output
dev.copy(png, "Data/travel time/bologna_traveltime_existingcc_stats.png", width = 800, height = 600)
dev.off()


####### ALLOCATION PREP #######

## read potential cooling centres facilities
potential_facilities <- read.csv("Data/allocation/bologna_all_candidates.csv", sep = ";")

## Convert and reproject point CSV
bologna_candidates <- st_as_sf(
  potential_facilities,
  coords = c("x", "y"),
  crs    = 4326             # WGS84
)

## turn vulnerability spatraster into raster layer
vulnindex_bologna <- raster(bologna_vulnerability_index)

## few adjustments are needed to ensure same crs and origin of the raster layers
crs(pop_bologna_raster) <- "EPSG:4326"
crs(vulnindex_bologna) <- "EPSG:4326"

pop_bologna_final <- resample(pop_bologna_raster, vulnindex_bologna, method = "bilinear")

## run allocation function

#### SCENARIO 1 ####

## allocation
output_allocation_bologna_s1 <- allocation_discrete(demand_raster = pop_bologna_final, traveltime_raster=bologna_cc_traveltime, bb_area = bologna_districts, facilities=bologna_cc, candidate=bologna_candidates, n_fac = 254, objectiveshare = 0.8, weights=vulnindex_bologna, objectiveminutes=15, exp_demand = 2, exp_weights = 1, dowscaling_model_type="lm", mode="walk", res_output=100, n_samples=1000, par=FALSE)

## plot
plot_bologna_s1 <- allocation_plot_discrete(output_allocation_bologna_s1, bb_area = bologna_districts)
print(plot_bologna_s1)

## save plot
dev.copy(png, "Data/allocation/bologna_allocation_s1_plot.png", width = 800, height = 600)
dev.off()

## save dataframe with allocated facilities
write.csv(output_allocation_bologna_s1[[1]], "Data/allocation/bologna_allocation_s1_facilities.csv", row.names = FALSE)

## check stats as validation
bologna_s1_cc <- bind_rows(bologna_cc, output_allocation_bologna_s1[[1]])
bologna_cc_s1_traveltime <- traveltime(facilities=bologna_s1_cc, bb_area=bologna_districts, dowscaling_model_type="lm", mode="walk", res_output=100)
traveltime_stats(traveltime = bologna_cc_s1_traveltime, demand_raster = pop_bologna_final, breaks=c(5, 10, 15, 30), objectiveminutes=15)
# 
# weighted allocation:
# [1] "Iteration with 5 facilities."
# [1] "Coverage share attained: 0.859480204488913"
# [1] "77.03 % of demand layer within the objectiveminutes threshold."

#### SCENARIO 2 ### 

## allocation
output_allocation_bologna_s2 <- allocation_discrete(demand_raster = pop_bologna_final, traveltime_raster=bologna_cc_traveltime, bb_area = bologna_districts, facilities=bologna_cc, candidate=bologna_candidates, n_fac = 254, objectiveshare = 0.99, weights=vulnindex_bologna, objectiveminutes=15, exp_demand = 1, exp_weights = 1, dowscaling_model_type="lm", mode="walk", res_output=100, n_samples=1000, par=FALSE)

## plot
plot_bologna_s2 <- allocation_plot_discrete(output_allocation_bologna_s2, bb_area = bologna_districts)
print(plot_bologna_s2)

## save plot
dev.copy(png, "Data/allocation/bologna_allocation_s2_plot.png", width = 800, height = 600)
dev.off()

## save dataframe with allocated facilities
write.csv(output_allocation_bologna_s2[[1]], "Data/allocation/bologna_allocation_s2_facilities.csv", row.names = FALSE)


#### SCENARIO 3 ####

## allocation
output_allocation_bologna_s3 <- allocation_discrete(demand_raster = pop_bologna_final, traveltime_raster=bologna_cc_traveltime, bb_area = bologna_districts, facilities=bologna_cc, candidate=bologna_candidates, n_fac = 3, weights=vulnindex_bologna, objectiveminutes=15, exp_demand = 3, exp_weights = 1, dowscaling_model_type="lm", mode="walk", res_output=100, n_samples=10000, par=FALSE)

## plot
plot_bologna_s3 <- allocation_plot_discrete(output_allocation_bologna_s3, bb_area = bologna_districts)
print(plot_bologna_s3)

## save plot
dev.copy(png, "Data/allocation/bologna_allocation_s3_plot.png", width = 800, height = 600)
dev.off()

## save dataframe with allocated facilities
write.csv(output_allocation_bologna_s3[[1]], "Data/allocation/bologna_allocation_s3_facilities.csv", row.names = FALSE)







