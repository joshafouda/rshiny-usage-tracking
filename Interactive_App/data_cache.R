library(DBI)
library(dplyr)

# Define paths for cached data
cache_dir <- "cache/"
dir.create(cache_dir, showWarnings = FALSE)  # Ensure the cache directory exists
content_cache_path <- file.path(cache_dir, "content_data.rds")
usage_cache_path <- file.path(cache_dir, "usage_data.rds")
users_cache_path <- file.path(cache_dir, "users_data.rds")

# Function to fetch data from Snowflake and cache it
fetch_and_cache_data <- function(query_function, cache_path, refresh = FALSE) {
  if (!refresh && file.exists(cache_path)) {
    # Load data from cache
    message("Loading cached data: ", cache_path)
    readRDS(cache_path)
  } else {
    # Fetch data from Snowflake
    message("Fetching data from Snowflake: ", cache_path)
    data <- query_function()
    saveRDS(data, cache_path)  # Save to cache
    data
  }
}

# Snowflake connection (replace with your details)
db_connection <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "Snowflake",
  Server = "your_snowflake_server",
  Database = "your_database",
  Schema = "your_schema",
  UID = "your_username",
  PWD = "your_password"
)

# Query functions for each table
query_content <- function() {
  tbl(db_connection, "D_PST_CONTENT") %>% collect()
}

query_usage <- function() {
  tbl(db_connection, "D_PST_USAGE") %>% collect()
}

query_users <- function() {
  tbl(db_connection, "D_PST_USERS") %>% collect()
}

# Load data (from cache or database)
content_data <- fetch_and_cache_data(query_content, content_cache_path)
usage_data <- fetch_and_cache_data(query_usage, usage_cache_path)
users_data <- fetch_and_cache_data(query_users, users_cache_path)
