library(httr)
library(dplyr)
library(jsonlite)

# Set headers 
headers <- c(
  "Accept-Encoding" = "gzip",
  "User-Agent" = "StackEDA/1.0 (contact@email.com)"
)

# API key
api_key <- "6POx4rh4yVYDTF*ekYiO)g(("

api_url_base <- "https://api.stackexchange.com/2.3/search"
api_params <- list(
  key = api_key
)

# Initialize an empty dataframe
results_df <- data.frame()

# Set the number of pages
pages <- 7000

# Loop through pages and make API requests
for (page_num in 1:pages) {
  print(page_num)
  api_url = paste0("https://api.stackexchange.com/2.3/search?page=", page_num ,"&pagesize=100&order=desc&sort=activity&intitle=python&site=stackoverflow")
  
  response <- GET(api_url, query = params, headers = headers)
  print(response$status_code)
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text"))
    
    # Flatten the nested JSON structure
    items_df <- jsonlite::flatten(data$items)
    
    # Convert the list column to a delimited string
    items_df$tags <- sapply(items_df$tags, function(tag_list) paste(tag_list, collapse = "|"))
    
    # Append the current iteration data to the results_df
    results_df <- bind_rows(results_df, items_df)
    
  } else {
    print("Error: API call failed.")
  }
}
# Check the dimensions (shape) of the dataframe
dimensions <- dim(results_df)

# Print the number of rows and columns
cat("Number of rows:", dimensions[1], "\n")
cat("Number of columns:", dimensions[2], "\n")

# Select specific columns from the dataframe
df <- select(results_df, tags, question_id, title, is_answered, view_count, answer_count, score, last_activity_date, creation_date, last_edit_date, content_license, link)

# Using names() function
column_names <- names(df)
cat("Column Names :", column_names)

# Save the combined dataframe to CSV file
write.csv(df, file = "stackexchange_data.csv", row.names = FALSE)