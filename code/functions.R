import_posterior_files <- function(file_name, save_path, parameters) {
  df <- vroom(file.path(save_path, file_name),
              comment = "#",
              col_select = vroom::starts_with(parameters))
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
