library(sf)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tmap)

warsaw_data <- st_read("dzielnice_Warszawy.shp")

json_data <- fromJSON("api.um.warszawa.pl.customization")

# View the first few rows of the data
head(warsaw_data)

# Check the structure of the data
str(warsaw_data)

# Check the coordinate reference system (CRS)
st_crs(warsaw_data)

plot(warsaw_data)

bike_stations <- json_data$result$featureMemberList

# Extract the geometry and properties data
geometry_data <- bike_stations$geometry
properties_data <- bike_stations$properties

# Combine the coordinates and properties into a single data frame
bike_stations_df <- do.call(rbind, lapply(1:nrow(bike_stations), function(i) {
  # Extract coordinates
  coords <- geometry_data$coordinates[[i]]
  latitude <- as.numeric(coords$latitude)
  longitude <- as.numeric(coords$longitude)
  
  # Extract properties
  props <- properties_data[[i]]
  props_df <- data.frame(
    OBJECTID = props$value[props$key == "OBJECTID"],
    LOKALIZACJA = props$value[props$key == "LOKALIZACJA"],
    NR_STACJI = props$value[props$key == "NR_STACJI"],
    ROWERY = props$value[props$key == "ROWERY"],
    STOJAKI = props$value[props$key == "STOJAKI"],
    AKTU_DAN = props$value[props$key == "AKTU_DAN"]
  )
  
  # Combine coordinates and properties
  cbind(props_df, latitude, longitude)
}))

# View the resulting data frame
head(bike_stations_df)

bike_stations_sf <- st_as_sf(bike_stations_df, coords = c("longitude", "latitude"), crs = 4326)

# Check the CRS of warsaw_data
st_crs(warsaw_data)

# Transform bike_stations_sf to match the CRS of warsaw_data
bike_stations_sf <- st_transform(bike_stations_sf, st_crs(warsaw_data))

bike_stations_per_district <- st_join(warsaw_data, bike_stations_sf) %>%
  group_by(nazwa_dzie) %>%  # Group by district name
  summarise(total_stations = n())  # Count the number of stations per district

# View the result
head(bike_stations_per_district)

library(ggplot2)

ggplot() +
  geom_sf(data = bike_stations_per_district, aes(fill = total_stations)) +
  scale_fill_viridis_c(name = "Bike Stations") +
  labs(title = "Density of Bike Stations in Warsaw Districts") +
  theme_minimal()
