rm(list = ls())

library(jsonlite)
library(dplyr)
library(readr)

# Load your JSON file (or string)
json_data <- fromJSON("equine_sequestration.json", simplifyVector = FALSE)

# Convert JSON to a raw character string
json_text <- toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)

# Perform find and replace
for (i in seq_len(nrow(ids))) {
  find_str <- ids$equine[i]
  replace_str <- ids$man[i]
  
  json_text <- gsub(find_str, replace_str, json_text, fixed = TRUE)
}

#find and replace for Hectares to Acres (only for converting into Isle of Man)
json_text<- gsub('"description": "Hectares \\(ha\\)"', '"description": "Acres (ac)"',json_text)
json_text<- gsub('"units": "ha"','"units": "ac"',json_text)

# Convert back to structured JSON
updated_json <- fromJSON(json_text, simplifyVector = FALSE)

# Optionally write it back to a file
write_json(updated_json, "man_sequestration.json", pretty = TRUE, auto_unbox = TRUE)
