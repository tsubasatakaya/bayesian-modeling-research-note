source("code/setup.R")
source("code/functions.R")

posterior_path <- file.path(output_path, "posterior_files")

#######################
# Interaction with year
#######################
all_files <- list.files(file.path(posterior_path, "fe_model"),
                        pattern = "\\.csv$")
files_to_read <- all_files[grep("^party_identification", all_files)]

beta_0_1_sample <- "beta_0.1"
posterior <- files_to_read |> 
  imap_dfr(~ monitor_chains(.x,
                            save_path = file.path(posterior_path, "fe_model"),
                            parameters = beta_0_1_sample) |> 
             mutate(chain = .y)
  ) |> 
  group_by(chain) |> 
  mutate(iter = 1:n()) |> 
  select(chain, iter, everything()) |> 
  pivot_longer(-c(chain, iter), names_to = "parameter")

ggplot(data = posterior) +
  geom_line(aes(x = iter, y = value, color = as.factor(chain))) +
  facet_wrap(~parameter, scales = "free")


#######################
# Varying slope by year
#######################
all_files <- list.files(file.path(posterior_path, "varying_slope_model"),
                        pattern = "\\.csv$")
files_to_read <- all_files[grep("^party_identification", all_files)]

beta_0_1_sample <- "beta_0.1"
posterior <- files_to_read |> 
  imap_dfr(~ monitor_chains(.x,
                            save_path = file.path(posterior_path, "varying_slope_model"),
                            parameters = beta_0_1_sample) |> 
             mutate(chain = .y)
  ) |> 
  group_by(chain) |> 
  mutate(iter = 1:n()) |> 
  select(chain, iter, everything()) |> 
  pivot_longer(-c(chain, iter), names_to = "parameter")

ggplot(data = posterior) +
  geom_line(aes(x = iter, y = value, color = as.factor(chain))) +
  facet_wrap(~parameter, scales = "free")
