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

alpha_posterior <- files_to_read |> 
  map_dfr(~ import_posterior_files(.x,
                                   save_path = file.path(posterior_path, "varying_slope_model"),
                                   parameters = "alpha",
                                   include_warmup = FALSE)) |> 
  select(!any_of(dplyr::contains("raw")) & !any_of(dplyr::contains("mean"))) |> 
  pivot_longer(everything(), values_to = "alpha") |> 
  mutate(state_year_id = as.integer(str_extract(name, "\\d+"))) |> 
  mutate(draw = 1:n(), .by = state_year_id) |> 
  select(- name)

delta_posterior <- files_to_read |> 
  map_dfr(~ import_posterior_files(.x,
                                   save_path = file.path(posterior_path, "varying_slope_model"),
                                   parameters = "delta",
                                   include_warmup = FALSE)) |> 
  select(!any_of(dplyr::contains("raw"))) |> 
  pivot_longer(everything(), values_to = "delta") |> 
  mutate(year_id = as.integer(str_extract(name, "\\d+"))) |> 
  mutate(draw = 1:n(), .by = year_id) |> 
  select(- name)

beta_0_posterior <- files_to_read |> 
  map_dfr(~ import_posterior_files(.x,
                                   save_path = file.path(posterior_path, "varying_slope_model"),
                                   parameters = "beta_0",
                                   include_warmup = FALSE)) |> 
  select(!any_of(dplyr::contains("raw"))) |> 
  mutate(draw = 1:n())

combined_posterior <- alpha_posterior |> 
  left_join(delta_posterior, by = "draw", relationship = "many-to-many") |> 
  left_join(beta_0_posterior, by = "draw", relationship = "many-to-one")

################################################################################
# Summarize posterior
################################################################################
























