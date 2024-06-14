if (!require(httr)) {
  install.packages("httr")
  library(httr)
}

if (!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

if (!require(tidyjson)) {
  install.packages("tidyjson")
  library(tidyjson)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if (!require(tibble)) {
  install.packages("tibble")
  library(tibble)
}


# Function to flatten and export attributes with subproperties
flatten_and_export <- function(df, attribute_list, tenant) {

  dir_path <- paste0('./', tenant) # ./etra

  print(file.exists(dir_path))

  if (!file.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(sprintf("Directory '%s' created successfully.\n", dir_path))
  }

  for (attr in attribute_list) {
    empty_df <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

    attr_df <- df %>%
      mutate(
             !!'refRoom' := .data[['refRoom']]$object,
             !!paste0(attr, "_value") := unlist(lapply(.data[[attr]]$value, unlist)),
             !!paste0(attr, "_observedAt") := .data[[attr]]$observedAt,
             !!paste0(attr, "_unitCode") := .data[[attr]]$unitCode) %>%
      select(
        'id',
        'refRoom',
        !!paste0(attr, "_value"),
        !!paste0(attr, "_observedAt"),
        !!paste0(attr, "_unitCode")
       )

    file_name <- paste0(attr, ".csv") # airTemperature.csv
 
    write.csv(attr_df, file.path(dir_path, file_name), row.names = FALSE)
    cat("Exported", file_name, "\n")
  }
}


# Example usage with your data frame 'df' and list of attributes
attribute_list <- c(
  "rssi", "airTemperature", "battery", "co2", "pm1", "pm10", 
  "pm25", "relativeHumidity", "barometricPressure", "eco2", 
  "formaldehyde", "light", "noiseLevel", "pn03", "pn05", 
  "pn10", "pn100", "pn25", "pn50", "sdCardPresence", "tvoc"
  )

url <- "https://orion.twinairdmp.online/ngsi-ld/v1/entities/?options=concise&type=hwsensors"
x_auth_token <- "HERE GOES X-AUTH-TOKEN. ASK ETRA OR THOWL FOR A TOKEN"
tenant <- "etra"

response <- GET(
  url,
  add_headers(
    "X-Auth-Token" = x_auth_token,
    "Content-Type" = "application/json",
    "NGSILD-Tenant" = tenant,
    "Link" = '<https://raw.githubusercontent.com/TwinAIR-Project/data-models/main/hwsensors/context.jsonld>; rel="http://www.w3.org/ns/json-ld#context"'
  )
)

# Check the response status
if (status_code(response) == 200) {
  content <- content(response, as = "text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(content)
  data <- as.data.frame(df)

  flatten_and_export(
    df, 
    attribute_list, 
    tenant
  )
} else {
  print(paste("Failed to fetch data. Status code:", status_code(response)))
  print(content(response, as = "text", encoding = "UTF-8"))
}
