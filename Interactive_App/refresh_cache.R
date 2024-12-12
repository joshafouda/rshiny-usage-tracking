library(DBI)
library(dplyr)

# Load the caching functions
source("data_cache.R")

# Force refresh the cache
content_data <- fetch_and_cache_data(query_content, content_cache_path, refresh = TRUE)
usage_data <- fetch_and_cache_data(query_usage, usage_cache_path, refresh = TRUE)
users_data <- fetch_and_cache_data(query_users, users_cache_path, refresh = TRUE)

message("Cache refreshed successfully at ", Sys.time())
