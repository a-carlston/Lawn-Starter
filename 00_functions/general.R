#' @export
install_packages <- function(packages, load_packages = FALSE) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  # Load all packages only if load_packages is TRUE
  if (load_packages) {
    invisible(lapply(packages, library, character.only = TRUE))
  }
}

#' @export
# Function to read all CSV/XLSX files from a folder and add a source file column
read_folder_files <- function(folder_path, file_type = "csv") {
  
  # Get list of files in the folder
  files <- list.files(path = folder_path, pattern = paste0("\\.", file_type, "$"), full.names = TRUE)
  
  # Read all files and add a column with the source file name
  data_list <- lapply(files, function(file) {
    if (file_type == "csv") {
      data <- suppressMessages(vroom::vroom(file))
    } else if (file_type == "xlsx") {
      data <- readxl::read_excel(file)
    }
    
    # Add source file column
    data <- dplyr::mutate(data, source_file = stringr::str_extract(basename(file), "^[^/]+"))
    
    return(data)
  })
  
  # Combine all data into one data frame
  combined_data <- dplyr::bind_rows(data_list)
  
  return(combined_data)
}


# data_wrangle ------------------------------------------------------------
#' @export
lag_by_weekdays <- function(df, column_name, years_back = 7, group_by_cols = c("Continent", "Country", "Brand")) {
  require(dplyr)
  require(lubridate)
  
  # Convert Date column to Date type if it isn't already
  df <- df |> mutate(Date = as.Date(Date))
  
  for (i in 1:years_back) {
    current_year <- lubridate::year(Sys.Date()) - i
    lag_column_name <- paste0("Lag_", column_name, "_", current_year)
    
    df <- df %>%
      group_by(across(all_of(group_by_cols))) %>%  # Dynamically group by specified columns
      mutate(
        # Calculate the lagged date by shifting `i` years back
        Lagged_Date = Date - years(i),
        
        # Adjust the lagged date to align with the same weekday in the previous year
        Lagged_Date = case_when(
          wday(Date) == wday(Lagged_Date) ~ Lagged_Date,
          TRUE ~ Lagged_Date + (wday(Date) - wday(Lagged_Date)) %% 7
        ),
        
        # Create the new lagged column using a safer matching approach
        !!lag_column_name := .data[[column_name]][match(Lagged_Date, Date)],
        
        # Handle NA values by using the last available value within the same group
        !!lag_column_name := ifelse(
          is.na(.data[[lag_column_name]]),
          lag(.data[[lag_column_name]], 1),
          .data[[lag_column_name]]
        )
      ) %>%
      ungroup()
  }
  
  return(df)
}