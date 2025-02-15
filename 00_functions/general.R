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
