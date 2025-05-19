################################################################################
# Setup
################################################################################
source("code/setup.R")
source("code/functions.R")

posterior_path <- file.path(output_path, "posterior_files")

states <- c(
  "Schleswig-Holstein",
  "Hamburg",
  "Lower Saxony",
  "Bremen",
  "North Rhine Westphalia",
  "Hesse",
  "Rhineland Palatinate",
  "Baden-Wuerttemberg",
  "Bavaria",
  "Saarland",
  "Berlin",
  "Brandenburg",
  "Mecklenburg-Vorpommern",
  "Saxony",
  "Saxony-Anhalt",
  "Thuringia"
)

################################################################################
# Prepare original data
################################################################################
base_data <- vroom(file.path(data_path, "base_data.csv"),
                   delim = ";") |> 
  mutate(state = factor(state, levels = states),
         year = factor(year, levels = c(2013, 2017, 2021))) |> 
  mutate(state_id = as.integer(state),
         year_id = as.integer(year),
         state_year_id = as.integer(factor(interaction(state, year))),
         gender = as.integer(factor(gender, levels = c("male", "female"))),
         education = as.integer(factor(education, levels = c("low", "middle", "high")))
  ) |> 
  filter(party_identification > 0)

state_year_data <- base_data |> 
  select(state, state_id, year, year_id, east, unemp_rate, gdp_per_capita) |> 
  distinct()

X <- model.matrix(~ 1 + gender + factor(education) + unemp + unemp_past + income,
                  data = base_data) |> 
  as.data.frame() |> 
  mutate(
    across(all_of(c("unemp_past", "income")), ~ (.- mean(.)) / sd(.)),
    across(-all_of(c("unemp_past", "income")), ~ . - mean(.))
  ) |> 
  as.matrix()
X <- X[,-1]
afd <- scale(base_data$party_identification_afd, scale = FALSE) |> 
  as.vector()

Z <- model.matrix(~ 1 + factor(year_id) + gdp_per_capita + unemp_rate,
                  data = state_year_data) |> 
  as.data.frame() |> 
  mutate(
    across(all_of(c("gdp_per_capita", "unemp_rate")), ~ (.- mean(.)) / sd(.)),
    across(-all_of(c("gdp_per_capita", "unemp_rate")), ~ . - mean(.))
  ) |> 
  as.matrix()
Z <- Z[,-1]
east <- scale(state_year_data$east, scale = FALSE) |> 
  as.vector()

################################################################################
# Prepare posterior
################################################################################
all_files <- list.files(file.path(posterior_path, "varying_slope_model"),
                        pattern = "\\.csv$")
files_to_read <- all_files[grep("^party_identification", all_files)]

# n_sample * n_state_year matrix
alpha_posterior <- files_to_read |> 
  map_dfr(~ import_posterior_files(.x,
                                   save_path = file.path(posterior_path, "varying_slope_model"),
                                   parameters = "alpha",
                                   include_warmup = FALSE)) |> 
  select(!any_of(dplyr::contains("raw")) & !any_of(dplyr::contains("mean"))) |> 
  as.matrix()

# n_sample * n_year matrix
delta_posterior <- files_to_read |> 
  map_dfr(~ import_posterior_files(.x,
                                   save_path = file.path(posterior_path, "varying_slope_model"),
                                   parameters = "delta",
                                   include_warmup = FALSE)) |> 
  select(!any_of(dplyr::contains("raw"))) |> 
  as.matrix()

# n_sample * n_covariate_X matrix
beta_0_posterior <- files_to_read |> 
  map_dfr(~ import_posterior_files(.x,
                                   save_path = file.path(posterior_path, "varying_slope_model"),
                                   parameters = "beta_0",
                                   include_warmup = FALSE)) |> 
  select(!any_of(dplyr::contains("raw")))


################################################################################
# Summarize posterior
################################################################################
#===================
# Posterior distribution of parameters
#===================
delta_posterior_long






#===================
# Population averaged predictions
#===================

data_new <- tibble(
  state_year_id = base_data$state_year_id,
  year_id = base_data$year_id,
  party_identification_afd = afd,
) |> 
  bind_cols(X)
afd_data_new <- data_new |> 
  filter(party_identification_afd == max(party_identification_afd))
non_afd_data_new <- data_new |> 
  filter(party_identification_afd == min(party_identification_afd))


### AfD supporters
# Subset relevant parameters
alpha_afd <- alpha_posterior[, afd_data_new$state_year_id] # n_sample * N_afd
delta_afd <- delta_posterior[, afd_data_new$year_id]  # n_sample * N_afd
linpred_afd <- t(alpha_afd) + 
  t(delta_afd) * afd_data_new$party_identification_afd +
  as.matrix(afd_data_new[,colnames(X)]) %*% t(beta_0_posterior)  # N_afd * n_sample

# Aggregate over observations by year_id
pred_afd <- data.table(year_id = afd_data_new$year_id)
draw_names <- paste0(seq_len(ncol(linpred_afd)))
pred_afd[, (draw_names) := as.data.table(linpred_afd)]
pred_afd <- pred_afd[, lapply(.SD, mean), by = year_id, .SDcols = draw_names]
pred_afd_long <- pred_afd |> 
  as_tibble() |> 
  pivot_longer(cols = !year_id,
               names_to = "draw",
               values_to = "y_pred") |> 
  mutate(group = "afd")

### Non-AfD supporters
# Subset relevant parameters
alpha_non_afd <- alpha_posterior[, non_afd_data_new$state_year_id] # n_sample * N_nonafd
delta_non_afd <- delta_posterior[, non_afd_data_new$year_id]  # n_sample * N_nonafd
linpred_non_afd <- t(alpha_non_afd) + 
  t(delta_non_afd) * non_afd_data_new$party_identification_afd +
  as.matrix(non_afd_data_new[,colnames(X)]) %*% t(beta_0_posterior)  # N_nonafd * n_sample

# Aggregate over observations by year_id
pred_non_afd <- data.table(year_id = non_afd_data_new$year_id)
pred_non_afd[, (draw_names) := as.data.table(linpred_non_afd)]
pred_non_afd <- pred_non_afd[, lapply(.SD, mean), by = year_id, .SDcols = draw_names]
pred_non_afd_long <- pred_non_afd |> 
  as_tibble() |> 
  pivot_longer(cols = !year_id,
               names_to = "draw",
               values_to = "y_pred") |> 
  mutate(group = "non-afd")

pop_pred_summary <- rbind(pred_afd_long, pred_non_afd_long) |> 
  summarize(lower = quantile(y_pred, 0.025),
            upper = quantile(y_pred, 0.975),
            point = mean(y_pred),
            .by = c("group", "year_id")) |> 
  mutate(year = case_match(
    year_id,
    1 ~ 2013,
    2 ~ 2017,
    3 ~ 2021
  ))

pop_pred_summary |> 
  mutate(year = factor(year)) |> 
  ggplot(aes(x = year, y = point, color = group)) +
  geom_pointrange(aes(x = year, y = point, ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.8))









