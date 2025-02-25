
source("./00_functions/general.R", local = "general" <- new.env())

required_packages <- c("googlesheets4", "dplyr", "stringr", "tidyr", "vroom", "readxl")

# Check and Install only (do not load)
general$install_packages(required_packages, load_packages = FALSE)

# Read column mapping from Google Sheets ----

column_mapping <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1d8kMfDsCIEhRVAOtfK1b9i1O3Eq7mrFWxyDB2d8xGkE/edit?gid=1541747881#gid=1541747881", sheet = "AWS")
column_active <- column_mapping |> 
  dplyr::filter(active == "Yes")

# Read queue list from Google Sheets -----
list_of_queues <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1m2oXXN-ItJFuTOeH2eB-20xCI3rwXA4EO9-5DdCP6I8/edit?gid=0") |> 
  dplyr::mutate(queue = as.character(queue))

# Read and process raw data ----
raw_data <- general$read_folder_files(
  folder_path = "G:\\Shared drives\\WFM\\New_Data_Structure\\Raw_Data\\AWS\\Queue\\Day_New",
  file_type = "csv"
) 

raw_data <- raw_data |> 
    # Only rename columns that exist in both raw_data and column_mapping
    dplyr::rename_with(
      ~ setNames(
        column_mapping$new[column_mapping$original %in% names(raw_data)],
        column_mapping$original[column_mapping$original %in% names(raw_data)]
      )
    ) |>
  dplyr::mutate(interaction_date = as.Date(startinterval)) |> 
  dplyr::select(dplyr::all_of(column_active$new)) |> 
  dplyr::select(queue, interaction_date, dplyr::everything()) |> 
  dplyr::mutate(
    across(where(is.character) & !dplyr::all_of(c("queue")), ~ gsub("%", "", .x)),
    across(where(is.character) & !dplyr::all_of(c("queue")), ~ suppressWarnings(as.numeric(.x))),
    across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
    calls_offered_adjusted = dplyr::case_when(
      calls_incoming - calls_abandoned_30s < calls_handled_inbound ~ calls_handled_inbound, 
      TRUE ~ calls_incoming - calls_abandoned_30s
    ),
    channel = "Voice"
) 


# Create df ---- 
df <- raw_data |> 
  dplyr::left_join(list_of_queues |> dplyr::select(continent, country, brand, queue, category, channel), by = c("queue", "channel")) |> 
  dplyr::arrange(continent, country, brand, category, interaction_date) |> 
  dplyr::group_by(continent, country, brand, category, interaction_date) |> 
  dplyr::summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |> 
  dplyr::select(continent, country, brand, category, interaction_date, calls_offered_adjusted, calls_incoming, calls_handled_inbound, calls_abandoned, everything()) |> 
  dplyr::group_by(continent, country, brand, category) |>  
  dplyr::mutate(
    min_date = min(interaction_date, na.rm = TRUE), 
    max_date = max(interaction_date, na.rm = TRUE)
  ) |>  
  dplyr::ungroup() |>  
  tidyr::complete(
    interaction_date = seq.Date(min(min_date, na.rm = TRUE), max(max_date, na.rm = TRUE), by = "day"), 
    tidyr::nesting(continent, country, brand, category)
  ) |>  
  dplyr::select(-min_date, -max_date) |>  
  dplyr::mutate(
    channel = "Voice",
    across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) |>   # Fill missing numeric values with 0
  dplyr::filter(!is.na(continent))


df |> 
  dplyr::select(interaction_date, continent, country, brand, category, channel, calls_offered_adjusted) |> 
  writexl::write_xlsx(path = "00_data/test.xlsx")
























