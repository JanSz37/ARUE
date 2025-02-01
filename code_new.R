## Pakcages used
library(sf)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tmap)
library(tidyr)
library(geosphere)
library(corrplot)
library(gridExtra)
library(GGally)
library(lmtest)  
library(sandwich)  
library(stargazer)
library(spdep)
library(spatialreg)

# Data preparation

warsaw_districts <- st_read("dzielnice_Warszawy.shp") #for viz

tbus_data <- fromJSON("bus_tram_stops.customization") #self expanatory

metro_data <- fromJSON("metro_stops.customization") #same as above

properties1 <- read.csv('apartments_pl_2024_06.csv') #apartment prices with geolocation

# The first step involves loading and cleaning the public transport data, including bus/tram stops and metro stations. The data is transformed into a consistent format for further analysis.

extract_row <- function(row) {
  row %>%
    pivot_wider(names_from = key, values_from = value)
}

tbus_values <- tbus_data$result$values

tbus_clean <- tbus_values %>%
  lapply(extract_row) %>%
  bind_rows()

tbus_clean <- tbus_clean %>%
  mutate(
    szer_geo = as.numeric(szer_geo),
    dlug_geo = as.numeric(dlug_geo)
  )

feature_list <- metro_data$result$featureMemberList

coordinates <- feature_list$geometry$coordinates %>%
  bind_rows() %>%
  rename(latitude = latitude, longitude = longitude)

properties <- feature_list$properties %>%
  bind_rows() %>%
  rename(OBJECTID = value)

metro_df <- cbind(coordinates, properties)

metro_df <- metro_df %>%
  mutate(transport_type = "M")

tbus_clean <- tbus_clean %>%
  mutate(transport_type = "T/B") %>%
  mutate(szer_geo = as.numeric(szer_geo),
         dlug_geo = as.numeric(dlug_geo))

metro_df <- metro_df %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

public_transport <- bind_rows(
  tbus_clean %>% select(latitude = szer_geo, longitude = dlug_geo, transport_type),
  metro_df %>% select(latitude, longitude, transport_type)
)

# To analyze the spatial distribution of public transport points, clustering is performed. Points within a 500-meter radius are grouped into clusters, and centroids are calculated for each cluster. The weight of each centroid is determined by the number of points in its cluster.

public_transport_sf <- st_as_sf(public_transport, coords = c("longitude", "latitude"), crs = 4326)
public_transport_sf <- st_transform(public_transport_sf, crs = 32633)

distance_matrix <- st_distance(public_transport_sf)
radius <- 500
clusters <- list()

for (i in 1:nrow(public_transport_sf)) {
  nearby_points <- which(as.numeric(distance_matrix[i, ]) <= radius)
  clusters[[i]] <- nearby_points
}

clusters <- unique(clusters)

centroids <- lapply(clusters, function(cluster_indices) {
  cluster_points <- public_transport_sf[cluster_indices, ]
  centroid <- st_centroid(st_union(cluster_points))
  return(centroid)
})

centroids <- do.call(c, centroids)
centroids <- st_as_sf(centroids)
cluster_counts <- sapply(clusters, length)
centroids$weight <- cluster_counts

# The property data is filtered to include only listings in Warsaw. A function is created to calculate the minimum distance from each property to the nearest public transport point and identify the type of transport.

properties_warsaw <- properties1 %>%
  filter(city == "warszawa")

calculate_min_distance_and_type <- function(prop_lat, prop_lon, transport_df) {
  distances <- distHaversine(
    c(prop_lon, prop_lat),
    transport_df %>% select(longitude, latitude)
  )
  min_index <- which.min(distances)
  list(
    min_distance = min(distances),
    transport_type = transport_df$transport_type[min_index]
  )
}

properties_warsaw <- properties_warsaw %>%
  rowwise() %>%
  mutate(
    min_distance = calculate_min_distance_and_type(latitude, longitude, public_transport)$min_distance,
    nearest_transport_type = calculate_min_distance_and_type(latitude, longitude, public_transport)$transport_type
  ) %>%
  ungroup()

# To better capture the accessibility of properties to public transport, centroid-based scores are calculated. These scores incorporate both the distance to the nearest centroid and the weight of the centroid, 
# reflecting the density of transport points in the area. They are calculated as centroid weight divided by distance from property + 1. The 1 serves as a measure to prevent rapid score explosion, and thereby bias.

centroids_sf <- st_as_sf(centroids, coords = c("longitude", "latitude"), crs = 32633)
centroids_sf <- st_transform(centroids_sf, crs = 4326)
centroids_coords <- st_coordinates(centroids_sf)

centroids <- centroids %>%
  mutate(
    longitude = centroids_coords[, "X"],
    latitude = centroids_coords[, "Y"]
  ) %>%
  st_drop_geometry()

calculate_centroid_score <- function(prop_lat, prop_lon, centroids_df) {
  distances <- distHaversine(
    c(prop_lon, prop_lat),
    centroids_df %>% select(longitude, latitude)
  )
  scores <- centroids_df$weight / (distances + 1)
  max_score_index <- which.max(scores)
  list(
    centroid_score = max(scores),
    centroid_distance = distances[max_score_index],
    centroid_weight = centroids_df$weight[max_score_index]
  )
}

properties_warsaw <- properties_warsaw %>%
  rowwise() %>%
  mutate(
    centroid_score = calculate_centroid_score(latitude, longitude, centroids)$centroid_score,
    centroid_distance = calculate_centroid_score(latitude, longitude, centroids)$centroid_distance,
    centroid_weight = calculate_centroid_score(latitude, longitude, centroids)$centroid_weight
  ) %>%
  ungroup()



## Exploratory data analysis

warsaw_bbox <- st_bbox(warsaw_districts)
zoom_out_factor <- 1.05
expanded_bbox <- warsaw_bbox * c(1/zoom_out_factor, 1/zoom_out_factor, zoom_out_factor, zoom_out_factor)

tm_shape(warsaw_districts, bbox = expanded_bbox) +  
  tm_polygons(col = "lightgray", border.col = "white") +  
  tm_shape(public_transport_sf) +  
  tm_dots(col = "transport_type", palette = "Set1", size = 0.06, legend.show = FALSE) +  
  tm_layout(main.title = "Public Transport Points in Warsaw")

centroids_sf <- st_as_sf(centroids, coords = c("longitude", "latitude"), crs = 4326)
centroids_sf <- st_transform(centroids_sf, crs = st_crs(warsaw_districts))

ggplot() +
  geom_sf(data = warsaw_districts, fill = "lightgray", color = "black") +  
  geom_sf(data = centroids_sf, aes(size = weight), color = "red", alpha = 0.6) +  # Plot centroids
  scale_size_continuous(range = c(0.05, 3)) +  # Adjust the size of the centroids
  labs(title = "Cluster Centroids in Warsaw",
       size = "Centroid Weight") +
  theme_minimal()

properties_warsaw_sf <- st_as_sf(properties_warsaw, coords = c("longitude", "latitude"), crs = 4326)

properties_warsaw_sf <- st_transform(properties_warsaw_sf, crs = st_crs(warsaw_districts))

ggplot() +
  geom_sf(data = warsaw_districts, fill = "lightgray", color = "white") +  
  geom_sf(data = properties_warsaw_sf, aes(color = price), size = 1, alpha = 0.6) +  
  scale_color_viridis_c(
    option = "plasma", 
    name = "Price", 
    breaks = seq(0, 3000000, by = 1000000)  
  ) +
  labs(title = "Properties in Warsaw Colored by Price",
       subtitle = "Overlay on Warsaw Districts",
       caption = "Data: properties_warsaw") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = warsaw_districts, fill = "lightgray", color = "white") +  # Warsaw districts
  geom_sf(
    data = properties_warsaw_sf, 
    aes(
      color = centroid_score, 
      size = centroid_score,  
      alpha = centroid_score  
    )
  ) +
  scale_color_viridis_c(
    option = "plasma",  
    name = "Centroid Score", 
    rescaler = ~ scales::rescale_mid(.x, mid = median(properties_warsaw_sf$centroid_score))  # Emphasize higher values
  ) +
  scale_size_continuous(
    range = c(0.5, 3),  # Size range for points (smaller to larger)
    guide = "none"  # Hide size legend to avoid clutter
  ) +
  scale_alpha_continuous(
    range = c(0.3, 1),  # Alpha range (more transparent to more opaque)
    guide = "none"  # Hide alpha legend to avoid clutter
  ) +
  labs(
    title = "Properties in Warsaw Colored by Centroid Score",
    subtitle = "Higher scores are more visible",
    caption = "Data: properties_warsaw"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

properties_with_districts <- st_join(properties_warsaw_sf, warsaw_districts)
# Calculate average centroid score by district
avg_centroid_by_district <- properties_with_districts %>%
  group_by(nazwa_dzie) %>%  # Replace `district_name` with the actual column name for district names
  summarise(avg_centroid_score = mean(centroid_score, na.rm = TRUE)) %>%
  ungroup()
# Join average scores back to the districts shapefile
warsaw_districts_with_avg <- warsaw_districts %>%
  st_join(avg_centroid_by_district, by = "nazwa_dzie")  # Replace `district_name` with the actual column name

ggplot() +
  geom_sf(
    data = warsaw_districts_with_avg, 
    aes(fill = avg_centroid_score),  # Fill districts by average centroid score
    color = "white",  # District boundaries
    size = 0.2
  ) +
  scale_fill_viridis_c(
    option = "plasma",  # Use a high-contrast color scale
    name = "Avg. Centroid Score", 
    na.value = "lightgray"  # Color for districts with no data
  ) +
  labs(
    title = "Average Centroid Score by District in Warsaw",
    subtitle = "Higher scores indicate better access to public transport",
    caption = "Data: properties_warsaw"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Count properties per district
properties_per_district <- properties_warsaw_sf %>%
  st_join(warsaw_districts) %>%
  group_by(nazwa_dzie) %>%
  summarise(property_count = n()) %>%
  st_drop_geometry()

# Calculate area of each district in square kilometers
districts_with_area <- warsaw_districts %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1000000)

# Join counts with districts and calculate density
districts_with_density <- districts_with_area %>%
  left_join(properties_per_district, by = "nazwa_dzie") %>%
  mutate(density = property_count / area_km2)

# Create the map
ggplot(districts_with_density) +
  geom_sf(aes(fill = density), color = "white") +
  scale_fill_viridis_c(
    name = "Properties per km²",
    option = "plasma",
    direction = -1
  ) +
  theme_minimal() +
  labs(
    title = "Property Density in Warsaw Districts",
    subtitle = "Number of properties per square kilometer"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 8)
  )


## Correlation Analysis: Property Prices and Accessibility

properties_warsaw <- properties_warsaw %>%
  mutate(across(where(is.character), ~ ifelse(. == "", NA, .)))

# Remove rows where any of the specified columns have NA values
properties_warsaw <- properties_warsaw %>%
  filter(!is.na(clinicDistance) & 
           !is.na(postOfficeDistance) & 
           !is.na(kindergartenDistance) & 
           !is.na(restaurantDistance) & 
           !is.na(collegeDistance) & 
           !is.na(pharmacyDistance))

properties_warsaw <- properties_warsaw %>%
  drop_na(clinicDistance, postOfficeDistance, kindergartenDistance, 
          restaurantDistance, collegeDistance, pharmacyDistance)

# Check if missing values still exist
colSums(is.na(properties_warsaw))


ggplot(properties_warsaw, aes(x = min_distance, y = price/squareMeters)) +
  geom_point(alpha = 0.5) +
  labs(title = "Property Prices vs. Distance to Public Transport",
       subtitle = "Effect of Transport Accessibility on Price per Square Meter",
       x = "Distance (min) to Nearest Public Transport (meters)",
       y = "Property Price") +
  theme_minimal()

correlation <- cor(properties_warsaw$min_distance, as.numeric(properties_warsaw$price)/properties_warsaw$squareMeters, method = "pearson")
print(paste("Correlation between distance and price:", correlation))

ggplot(properties_warsaw, aes(x = centreDistance, y = price/squareMeters)) +
  geom_point(alpha = 0.5) +
  labs(title = "Property Prices vs. Distance to Center",
       subtitle = "Effect of Center Distance on Price per Square Meter",
       x = "Center Distance",
       y = "Property Price") +
  theme_minimal()


correlation <- cor(properties_warsaw$centreDistance, as.numeric(properties_warsaw$price)/properties_warsaw$squareMeters, method = "pearson")
print(paste("Correlation between Centre Distance and price:", correlation))


selected_vars <- properties_warsaw %>%
  select(price_per_m2 = price/squareMeters, centreDistance, min_distance, 
         centroid_score, restaurantDistance, clinicDistance, 
         postOfficeDistance, kindergartenDistance, pharmacyDistance) %>%
  drop_na()

cor_matrix <- cor(selected_vars, use = "pairwise.complete.obs", method = "pearson")

# Correlation matrix (heatmap)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", addCoef.col = "black", tl.col = "black",
         tl.srt = 45, number.cex = 0.8, tl.cex = 0.8)

selected_vars <- selected_vars %>%
  mutate(across(everything(), as.numeric))

set.seed(123)  
selected_sample <- selected_vars %>% sample_n(min(500, nrow(selected_vars)))

# Correlation graphs
ggpairs(selected_sample, 
        lower = list(continuous = wrap("smooth", method = "lm", color = "red")),
        upper = list(continuous = wrap("cor", method = "pearson", size = 4)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme_minimal()


## Modeling and Analysis Results

lm_linear_raw <- lm(price/squareMeters ~ centreDistance + min_distance + centroid_score + 
                      restaurantDistance + clinicDistance + postOfficeDistance + 
                      kindergartenDistance + pharmacyDistance, data = properties_warsaw)
lm_linear <- coeftest(lm_linear_raw, vcov = vcovHC(lm_linear_raw, type = "HC1"))

lm_linear


properties_warsaw <- properties_warsaw %>%
  mutate(centreDistance_2 = centreDistance^2)

lm_quad_raw <- lm(price/squareMeters ~ centreDistance + centreDistance_2 + min_distance + centroid_score+
                    restaurantDistance + clinicDistance + postOfficeDistance + 
                    kindergartenDistance + pharmacyDistance, data = properties_warsaw)
lm_quad <- coeftest(lm_quad_raw, vcov = vcovHC(lm_quad_raw, type = "HC1"))
lm_quad


lm_coord_raw <- lm(price/squareMeters ~ centreDistance + centreDistance_2 + min_distance + 
                     restaurantDistance + clinicDistance + postOfficeDistance + 
                     kindergartenDistance + pharmacyDistance + 
                     longitude + latitude, 
                   data = properties_warsaw)
lm_coord <- coeftest(lm_coord_raw, vcov = vcovHC(lm_coord_raw, type = "HC1"))
lm_coord

lm_lat_raw <- lm(price/squareMeters ~ centreDistance + centreDistance_2 + min_distance + 
                   restaurantDistance + clinicDistance + postOfficeDistance + 
                   kindergartenDistance + pharmacyDistance + latitude, 
                 data = properties_warsaw)
lm_lat <- coeftest(lm_lat_raw, vcov = vcovHC(lm_lat_raw, type = "HC1"))
lm_lat

results <- stargazer(
  lm_linear_raw, lm_quad_raw, lm_coord_raw, lm_lat_raw,
  se = list(
    sqrt(diag(vcovHC(lm_linear_raw, type="HC1"))),
    sqrt(diag(vcovHC(lm_quad_raw, type="HC1"))),
    sqrt(diag(vcovHC(lm_coord_raw, type="HC1"))),
    sqrt(diag(vcovHC(lm_lat_raw, type="HC1")))
  ),
  title = "Regression Output: Impact of Various Distances on Price per m²",
  type = "text", 
  star.cutoffs = c(0.10, 0.05, 0.01)
)



## **Spatial Autocorrelation Test (Moran’s I)**

# Before running spatial models, we check if property prices cluster geographically.
properties_warsaw <- properties_warsaw %>%
  distinct(longitude, latitude, .keep_all = TRUE)

# Create spatial coordinates
coords <- cbind(properties_warsaw$longitude, properties_warsaw$latitude)

# Define spatial neighbors based on the 4 nearest properties
neighbors <- knn2nb(knearneigh(coords, k = 15))

# Convert neighbors into spatial weights
weights <- nb2listw(neighbors, style = "W")

# Moran's I test for spatial autocorrelation
moran_test <- moran.test(properties_warsaw$price / properties_warsaw$squareMeters, listw = weights)
print(moran_test)


## **Spatial Error Model (SEM)**

spatial_model_sem <- errorsarlm(price/squareMeters ~ min_distance +centroid_score+ centreDistance + 
                                  restaurantDistance + clinicDistance + 
                                  postOfficeDistance+ kindergartenDistance+ pharmacyDistance, 
                                data = properties_warsaw, listw = weights)

summary(spatial_model_sem)








































