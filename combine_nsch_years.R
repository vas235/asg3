library(data.table)
library(haven)
library(stringr)
library(jsonlite)
library(dplyr)
library(forcats)


# function to parse labels from the .csv file
# we stop stop at the string "writeplan"
parse_label_csv <- function(csv_file) {
  # read the .csv file
  labels_dt <- fread(csv_file, encoding = "UTF-8")
  
  # identify the row where "writeplan" appears
  stop_row <- which(labels_dt$variable == "writeplan")[1]
  
  # subset the data to only include rows up to "writeplan"
  labels_dt <- labels_dt[1:stop_row, ]
  
  # recode missing data labels (.m, .n, .l, .d) to 996, 997, 998, and 999
  labels_dt[value == ".m", value := 996]
  labels_dt[value == ".n", value := 997]
  labels_dt[value == ".l", value := 998]
  labels_dt[value == ".d", value := 999]
  
  # remove all types of apostrophes from the labels
  labels_dt[, desc := gsub("[‘’'`]", "", desc)]
  
  return(labels_dt)
}



# apply labels to each variable in the dataset using the .csv file
apply_labels_to_data_csv <- function(data, labels_dt) {
  if (is.null(labels_dt)) {
    return(data)  # if no labels, return the data as is
  }
  
  unique_variables <- unique(labels_dt$variable)
  
  for (var in unique_variables) {
    if (var %in% names(data)) {
      var_labels <- labels_dt[variable == var]
      
      # check if there are labels for values below 900 (indicating a categorical variable )
      has_labels_below_900 <- any(as.numeric(var_labels$value) < 900, na.rm = TRUE)
      
      if (has_labels_below_900) {
        # create a named vector for labels using numeric values
        label_map <- setNames(var_labels$desc, as.character(var_labels$value))  # make sure labels use character values for matching
        
        # convert the variable values to character for comparison with label_map
        data[[var]] <- as.character(data[[var]])
        
        # create the "_label" column with the mapped labels
        data[[paste0(var, "_label")]] <- ifelse(data[[var]] %in% names(label_map), label_map[data[[var]]], NA)
        
        # convert the original column back to numeric
        data[[var]] <- as.numeric(data[[var]])
        
      }
    }
  }
  
  return(data)
}


# set a directory containing the .csv files
csv_dir <- "/download-nsch-data"

# process each year's data
all_data_list <- list()

for (year in 2016:2023) {
  dta_file <- file.path(csv_dir, paste0("nsch_", year, "_topical.dta"))
  csv_file <- file.path(csv_dir, paste0("nsch_", year, "_topical.do.define.csv"))
  
  # load the .dta file using haven (this will automatically handle tagged NA values)
  raw_data <- read_dta(dta_file)
  
  # find and replace tagged NA values with numeric codes
  for (col in names(raw_data)) {
    raw_data[[col]][is_tagged_na(raw_data[[col]], "m")] <- 996
    raw_data[[col]][is_tagged_na(raw_data[[col]], "n")] <- 997
    raw_data[[col]][is_tagged_na(raw_data[[col]], "l")] <- 998
    raw_data[[col]][is_tagged_na(raw_data[[col]], "d")] <- 999
  }
  
  # handle stratum column
  if ("stratum" %in% names(raw_data)) {
    # convert to character to handle case-insensitive replacement
    raw_data$stratum <- as.character(raw_data$stratum)
    
    # reaplce 2A or 2a with 2
    raw_data$stratum[grepl("^2a?$", raw_data$stratum, ignore.case = TRUE)] <- "2"
    
    # convert the 'stratum' column to numeric
    raw_data$stratum <- as.numeric(raw_data$stratum)
  }
  
  
  # parse labels from the .csv file
  parsed_labels <- parse_label_csv(csv_file)
  
  # remove stratum entries from 2016's parsed labels
  if (year == 2016) {
    parsed_labels <- parsed_labels[variable != "stratum"]
  }
  
  # apply labels to the data
  labeled_data <- apply_labels_to_data_csv(raw_data, parsed_labels)
  
  # print the unique stratum values for the current year
  cat(paste("\n[DEBUG] Unique stratum values for year:", year, "\n"))
  print(unique(labeled_data$stratum))
  
  all_data_list[[as.character(year)]] <- data.table(labeled_data)
}


labels <- parsed_labels[variable == "a1_grade"]
print(labels)




apply_transformations <- function(data, transformations, year) {
  for (variable_name in names(transformations)) {
    details <- transformations[[variable_name]]
    
    if (year %in% details$years && variable_name %in% names(data)) {
      # access the "_label" column
      label_col <- paste0(variable_name, "_label")
      
      # apply JSON transformations
      for (i in seq_along(details$value)) {
        old_val <- details$value[i]
        new_val <- details$new_value[i]
        new_label <- details$new_label[i]
        
        # update values ( change raw data values if `old_val` and `new_val` are different)
        data[[variable_name]][data[[variable_name]] == as.numeric(old_val)] <- as.numeric(new_val)
        
        # Update labels: set new labels for updated values
        data[[label_col]][data[[variable_name]] == new_val] <- new_label
      }
    }
  }
  
  return(data)
}




# this is out config json file, this can be customized depending on the desired variables and the recoding that needs to be done
config <- fromJSON("variable-config-time-series.json")
desired_variables <- config$desired_variables

# this is where we process all the yearly data sets
processed_datasets <- lapply(names(all_data_list), function(year) {
  # access the original data table using the year name
  original_data <- all_data_list[[as.character(year)]]
  
  # apply transformations using the year
  transformed_data <- apply_transformations(original_data, config$transformations$transform, year)
  
  # subset to desired variables, including their matching _label columns if they exist
  label_columns <- paste0(desired_variables, "_label")
  existing_label_columns <- intersect(label_columns, names(transformed_data))
  
  # combine desired variables and the existing label columns for subsetting
  subset_columns <- c(desired_variables, existing_label_columns)
  
  subset_data <- transformed_data[, ..subset_columns]
  
  return(subset_data)
})


# concat all data into one data.table
combined_data <- rbindlist(processed_datasets, use.names = TRUE, fill = TRUE)


# function to replace missing numeric data codes with NA and handle factor conversion
final_conversion_to_factors <- function(data) {
  # set the missing data labels for categorical variables
  na_labels <- c("No valid response", "Not in universe", "Logical skip", "Suppressed for confidentiality")
  
  # set the missing numeric data codes
  na_numeric_codes <- c(996, 997, 998, 999)
  
  for (var in names(data)) {
    label_col <- paste0(var, "_label")
    
    print(var)
    
    # check if there is a corresponding "_label" column (meaning this is a categorical variable)
    if (label_col %in% names(data)) {
      
      
      # create a named vector to map values to labels
      value_label_map <- setNames(unique(data[[label_col]]), unique(data[[var]]))
      
      # sort the map by the numeric values
      values <- as.numeric(names(value_label_map))
      sorted_labels <- value_label_map[order(values)]
      sorted_values <- as.numeric(names(sorted_labels))
      
      if (var == "k2q35d"){
        print(sorted_values)
        print(str(sorted_labels))
        print(sorted_labels)
      }
      
      
      # convert the original column to a factor using the labels from the "_label" column
      data[[var]] <- factor(data[[var]], levels = order(sorted_values), labels = sorted_labels)
      
      # replace missing data labels with NA for categorical columns
      levels(data[[var]])[levels(data[[var]]) %in% na_labels] <- NA
      
      # remove the "_label" column as it is no longer needed
      data[[label_col]] <- NULL
    } else {
      # if no label column, treat as a numeric column
      if (is.numeric(data[[var]])) {
        # replace numeric missing data with NA
        data[[var]][data[[var]] %in% na_numeric_codes] <- NA
      }
    }
  }
  
  return(data)
}




# convert labeled vectors to factors and handle missing data
combined_data <- final_conversion_to_factors(combined_data)




# properly sets the fipsst codes to state names
fips_to_state <- setNames(
  c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
    "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", 
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
    "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
    "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
    "Wisconsin", "Wyoming"), 
  as.character(c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 
                 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 
                 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56))
)

# create a state category that has the state name instead of the code
combined_data$state <- fips_to_state[as.character(combined_data$fipsst)]

# convert 'state' to a factor
combined_data$state <- factor(combined_data$state)



# optionally, you can save the final output
saveRDS(combined_data, "combined-survey-data.rds")



