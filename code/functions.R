import_posterior_files <- function(file_name, 
                                   save_path, 
                                   parameters,
                                   include_warmup = TRUE) {
  meta <- vroom::vroom(file.path(save_path, file_name),
                       delim = ",",
                       n_max = 47)
  is_warmup_saved <- stringr::str_extract(meta[[1]][9], "true") == "true"
  if (is.na(is_warmup_saved)) {
    is_warmup_saved <- stringr::str_extract(meta[[1]][9], "false") != "false"
  }
  
  if (include_warmup) {
    if (!is_warmup_saved) {
      message("Warmup is not saved. Ignoring include_warmup.")
    }
    df <- vroom::vroom(file.path(save_path, file_name),
                       delim = ",",
                       comment = "#",
                       col_select = vroom::starts_with(parameters))
  } else {
    num_warmup <- as.integer(stringr::str_extract(meta[[1]][8], "\\d+"))
    
    df <- vroom::vroom(file.path(save_path, file_name),
                       delim = ",",
                       comment = "#",
                       col_select = vroom::starts_with(parameters)) |> 
      slice(num_warmup + 1:n())
  }
  gc()
  return(df)
}

monitor_chains <- function(file_name, save_path, parameters) {
  df <- vroom::vroom(file.path(save_path, file_name),
                     delim = ",",
                     comment = "#",
                     col_select = all_of(parameters))
  gc()
  return(df)
}

map_year_id_to_year <- function(df) {
  df |> 
    mutate(year = case_match(year_id,
                             1 ~ 2013,
                             2 ~ 2017,
                             3 ~ 2021
                             )
           )
}
