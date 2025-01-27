library(sf)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tmap)
library(dplyr)
library(tidyr)

warsaw_districts <- st_read("dzielnice_Warszawy.shp") #we might not need this but its neat for viz :)

tbus_data <- fromJSON("bus_tram_stops.customization")

metro_data <- fromJSON("metro_stops.customization")

properties1 <- read.csv('apartments_pl_2024_06.csv') #for now i load one month. If we have too little data, we can load more months and delete duplicate offers.

extract_row <- function(row) {
  row %>%
    pivot_wider(names_from = key, values_from = value)
}


#tbus
tbus_values <- tbus_data$result$values

tbus_clean <- tbus_values %>%
  lapply(extract_row) %>%
  bind_rows()

tbus_clean <- tbus_clean %>%
  mutate(
    szer_geo = as.numeric(szer_geo),
    dlug_geo = as.numeric(dlug_geo)
  )


#metro

feature_list <- metro_data$result$featureMemberList

coordinates <- feature_list$geometry$coordinates %>%
  bind_rows() %>%
  rename(latitude = latitude, longitude = longitude)

properties <- feature_list$properties %>%
  bind_rows() %>%
  rename(OBJECTID = value)

metro_df <- cbind(coordinates, properties)


####data prep

metro_df <- metro_df %>%
  mutate(transport_type = "M")

tbus_clean <- tbus_clean %>%
  mutate(transport_type = "T/B")

tbus_clean <- tbus_clean %>%
  mutate(szer_geo = as.numeric(szer_geo),
         dlug_geo = as.numeric(dlug_geo))

metro_df <- metro_df %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

# Combine tram/bus and metro data into one public transport dataset - idk if thats the right approach: i thought of adding weights for stops near to each other - basically take the centroid, leave only it in the dataset, and add the weight based on how many stops were replaced by that centroid
#so, what i mean is: if 5 stops are within a e.g. 50 meter radius from each other, we delete those 5, replace them with centroid coordinates, and give it weight = 5
public_transport <- bind_rows(
  tbus_clean %>% select(latitude = szer_geo, longitude = dlug_geo, transport_type),
  metro_df %>% select(latitude, longitude, transport_type)
)

properties_warsaw <- properties1 %>%
  filter(city == "warszawa")


# Function to calculate the minimum distance and transport type
library(geosphere)
calculate_min_distance_and_type <- function(prop_lat, prop_lon, transport_df) {
  distances <- distHaversine(
    c(prop_lon, prop_lat),
    transport_df %>% select(longitude, latitude)
  )
  min_index <- which.min(distances) # Index of the nearest transport location
  list(
    min_distance = min(distances), # Minimum distance
    transport_type = transport_df$transport_type[min_index] # Type of nearest transport
  )
}


properties_warsaw <- properties_warsaw %>%
  rowwise() %>%
  mutate(
    min_distance = calculate_min_distance_and_type(latitude, longitude, public_transport)$min_distance,
    nearest_transport_type = calculate_min_distance_and_type(latitude, longitude, public_transport)$transport_type
  ) %>%
  ungroup()

### it is here that i noticed - there are other distances in the dataset such as pharmacy distance or post office distance or centre distance - thats great! we can use that also
## but for now i will focus on just the transport

library(ggplot2)

ggplot(properties_warsaw, aes(x = min_distance, y = price/squareMeters)) +
  geom_point(alpha = 0.5) +
  labs(title = "Property Prices vs. Distance to Public Transport",
       x = "Distance to Nearest Public Transport (meters)",
       y = "Property Price") +
  theme_minimal()


correlation <- cor(properties_warsaw$min_distance, as.numeric(properties_warsaw$price)/properties_warsaw$squareMeters, method = "pearson")
print(paste("Correlation between distance and price:", correlation)) #there is *some* correlation

model <- lm(price/squareMeters ~ min_distance, data = properties_warsaw) #significant model, it does kind prove our thesis as is
summary(model)
# but lets improve this. If you have any ideas, feel free to share!
