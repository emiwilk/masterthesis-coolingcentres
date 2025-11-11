## Retrieving each city's boundary shape from Gisco Geodata Portal ##

# Install and load giscoR if not yet installed
install.packages("giscoR")  # only run once
library(giscoR)
library(sf)
library(dplyr)


# Download LAU (Local Administrative Units = communes/municipalities)
lau <- gisco_get_lau(
  year = "2021",        # Most recent available
  epsg = "4326",        # WGS84 projection
  cache = TRUE
)

# Preview structure
head(lau)


## Filter, plot, and save each citiy's shapefile from the downloaded data set ##

## 1 Vienna ##
# Filter for 1st city: Vienna — these are found in the LAU_NAME column (c)
vienna <- c("Wien")

vienna_boundary <- lau %>%
  filter(LAU_NAME %in% vienna)

# Plot
plot(st_geometry(vienna_boundary), main = "Vienna Boundary Shape")

# Save
st_write(vienna_boundary, "vienna_boundary.gpkg", delete_dsn = TRUE)


## 2 Barcelona ##
# Filter for 2nd city: Barcelona — these are found in the LAU_NAME column (c)
barcelona <- c("Barcelona")

barcelona_boundary <- lau %>%
  filter(LAU_NAME %in% barcelona)

# Plot
plot(st_geometry(barcelona_boundary), main = "Barcelona Boundary Shape")

# Save
st_write(barcelona_boundary, "barcelona_boundary.gpkg", delete_dsn = TRUE)


## 3 Athens ##
# Filter for 3rd city: Athens — these are found in the LAU_NAME column (c)
# the LAU data set uses the local names in national language, to find the right shape for Athens, we first filter for all athens related rows

# ensure system can read Greek letters
Sys.setlocale("LC_CTYPE", "Greek")

# ensure Greek letters remain readable and save as csv for better inspection
install.packages("readr")
library(readr)

# Convert to UTF-8 explicitly just in case
lau$LAU_NAME <- iconv(lau$LAU_NAME, from = "", to = "UTF-8")

# Drop geometry
lau_df <- st_drop_geometry(lau)

# Save using readr and write CSV with BOM to support UTF-8 in Excel
write.csv(lau_df, file = "lau_attributes_utf8_2.csv", row.names = FALSE, fileEncoding = "UTF-8-BOM")

# After inspection, I know I need to look for "Ψευδοκοινοτητα Αθηναιων" as the urban area of Athens
# Filter for rows that contain "Ψευδοκοινοτητα Αθηναιων"
athens_rows <- lau %>%
  filter(grepl("Ψευδοκοινοτητα Αθηναιων", LAU_NAME, ignore.case = FALSE))

# View matching rows
print(athens_rows)

# number of inhabitants and area in km2 of the city is aligned with my other data sources
# now we filter for the name in column c LAU_NAME as for the other cities before
athens <- c("Ψευδοκοινοτητα Αθηναιων")

athens_boundary <- lau %>%
  filter(LAU_NAME %in% athens)

# Plot
plot(st_geometry(athens_boundary), main = "Athens Boundary Shape")

# Save
st_write(athens_boundary, "athens_boundary.gpkg", delete_dsn = TRUE)


## 4 Bologna ##
# Filter for 4th city: Bologna — these are found in the LAU_NAME column (c)
bologna <- c("Bologna")

bologna_boundary <- lau %>%
  filter(LAU_NAME %in% bologna)

# Plot
plot(st_geometry(bologna_boundary), main = "Bologna Boundary Shape")

# Save
st_write(bologna_boundary, "bologna_boundary.gpkg", delete_dsn = TRUE)



#######
