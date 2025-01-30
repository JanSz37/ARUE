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


######clustering
# Convert the dataframe to an sf object
public_transport_sf <- st_as_sf(public_transport, coords = c("longitude", "latitude"), crs = 4326)

# Transform to a projected coordinate system (e.g., UTM) for accurate distance measurements
public_transport_sf <- st_transform(public_transport_sf, crs = 32633)  # UTM zone 33N

# Calculate the distance matrix (in meters)
distance_matrix <- st_distance(public_transport_sf)


# Define the radius (500 meters)
radius <- 500

# Initialize a list to store cluster assignments
clusters <- list()

# Loop through each point and find nearby points
for (i in 1:nrow(public_transport_sf)) {
  nearby_points <- which(as.numeric(distance_matrix[i, ]) <= radius)
  clusters[[i]] <- nearby_points
}

# Remove duplicate clusters (optional, if you want unique clusters)
clusters <- unique(clusters)


# Calculate centroids for each cluster
centroids <- lapply(clusters, function(cluster_indices) {
  cluster_points <- public_transport_sf[cluster_indices, ]
  centroid <- st_centroid(st_union(cluster_points))
  return(centroid)
})

# Combine centroids into a single sf object
centroids <- do.call(c, centroids)
centroids <- st_as_sf(centroids)

# Count the number of points in each cluster
cluster_counts <- sapply(clusters, length)

# Add weights to the centroids
centroids$weight <- cluster_counts


###################




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



##########applying the same to centroids
# Convert the centroids dataframe to an sf object
centroids_sf <- st_as_sf(centroids, coords = c("longitude", "latitude"), crs = 32633)  # Assuming UTM zone 33N

centroids_sf <- st_transform(centroids_sf, crs = 4326)

centroids_coords <- st_coordinates(centroids_sf)

centroids <- centroids %>%
  mutate(
    longitude = centroids_coords[, "X"],
    latitude = centroids_coords[, "Y"]
  )

centroids <- centroids %>%
  st_drop_geometry()

# Function to calculate the centroid-based score
calculate_centroid_score <- function(prop_lat, prop_lon, centroids_df) {
  # Calculate distances to all centroids
  distances <- distHaversine(
    c(prop_lon, prop_lat),
    centroids_df %>% select(longitude, latitude)
  )
  
  # Calculate scores using the formula: weight / (distance + 1)
  scores <- centroids_df$weight / (distances + 1)
  
  # Find the maximum score (best centroid)
  max_score_index <- which.max(scores)
  
  # Return the maximum score, corresponding distance, and centroid weight
  list(
    centroid_score = max(scores),  # Best score
    centroid_distance = distances[max_score_index],  # Distance to the best centroid
    centroid_weight = centroids_df$weight[max_score_index]  # Weight of the best centroid
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



###modeling

library(corrplot)
cor(properties_warsaw[is.numeric(properties_warsaw)])

model <- lm(price/squareMeters ~ min_distance + centroid_score, data = properties_warsaw) #significant model, it does kind prove our thesis as is
summary(model)
# but lets improve this. If you have any ideas, feel free to share!



#####examining the density of properties in different districts
# Create points from properties data
properties_sf <- st_as_sf(properties_warsaw, 
                          coords = c("longitude", "latitude"), 
                          crs = 4326)

# Transform to the same CRS as warsaw_districts if needed
# Assuming warsaw_districts is in EPSG:2178 (common for Poland)
properties_sf <- st_transform(properties_sf, st_crs(warsaw_districts))

# Count properties per district
properties_per_district <- properties_sf %>%
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

# Print summary statistics
summary_stats <- districts_with_density %>%
  st_drop_geometry() %>%
  select(nazwa_dzie, property_count, area_km2, density) %>%
  arrange(desc(density))
print(summary_stats)
#########################################

########examining correlations##################

# Select only numeric columns
numeric_data <- properties_warsaw %>%
  select(where(is.numeric)) %>%
  # Remove any columns that are all NA or have zero variance
  select_if(function(x) !all(is.na(x)) && var(x, na.rm = TRUE) > 0)

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Create correlation plot
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.7,
         tl.cex = 0.7,
         diag = FALSE)

# Function to create scatter plots for highly correlated pairs
create_scatter_plots <- function(cor_matrix, threshold = 0.3) {
  # Get pairs of variables with correlation above threshold
  cor_pairs <- which(abs(cor_matrix) > threshold & upper.tri(cor_matrix), arr.ind = TRUE)
  
  # Create data frame of correlation pairs
  pairs_df <- data.frame(
    var1 = rownames(cor_matrix)[cor_pairs[,1]],
    var2 = colnames(cor_matrix)[cor_pairs[,2]],
    correlation = cor_matrix[cor_pairs]
  ) %>%
    arrange(desc(abs(correlation)))
  
  # Print top correlations
  print("Top correlations:")
  print(pairs_df)
  
  # Create scatter plots for top 5 correlations
  top_5_pairs <- head(pairs_df, 5)
  
  plots <- list()
  for(i in 1:nrow(top_5_pairs)) {
    pair <- top_5_pairs[i,]
    plot <- ggplot(numeric_data, aes_string(x = pair$var1, y = pair$var2)) +
      geom_point(alpha = 0.1) +
      geom_smooth(method = "lm", color = "red") +
      labs(title = sprintf("Correlation: %.2f", pair$correlation)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10))
    plots[[i]] <- plot
  }
  
  # Arrange plots in a grid
  gridExtra::grid.arrange(grobs = plots, ncol = 2)
}

# Create scatter plots for highly correlated variables
create_scatter_plots(cor_matrix, threshold = 0.3)

# Calculate summary statistics for price correlations
price_correlations <- data.frame(
  variable = names(numeric_data),
  correlation_with_price = cor(numeric_data$price, numeric_data, use = "pairwise.complete.obs")
) %>%
  arrange(desc(abs(correlation_with_price.price)))  # Note the .price suffix

# Print price correlations
print("Correlations with price:")
print(price_correlations)
