
source("./00_functions/general.R", local = "general" <- new.env())

required_packages <- c("googlesheets4", "dplyr", "stringr", "tidyr", "vroom", "readxl")

# Check and Install only (do not load)
general$install_packages(required_packages, load_packages = FALSE)


# Read queue list from Google Sheets -----
master_roster <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Xtaag-X_RHIJMtZzLrKaf6Rj7hXzHN9qt3l5ifdvwGs/edit?gid=101567337#gid=101567337", sheet = "Employee_Roster") 

master_roster |>
  dplyr::filter(
    Employment_Status == "Active",
    Channel != "Back Office",
    Start_Date <= Sys.Date(),
    !grepl("vp|manager|director", Current_Role, ignore.case = TRUE),
    !grepl("Sales", Current_Role, ignore.case = TRUE)  # Filter out roles containing "Sales"
  ) |>
  tidyr::separate(Channel, into = c("Channel", "rest"), sep = ",", extra = "drop", fill = "right") |>
  dplyr::group_by(Channel, Current_Role) |>
  dplyr::summarise(Total = n(), .groups = "drop") |>
  View()



  
