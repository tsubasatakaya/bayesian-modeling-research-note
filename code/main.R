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

base_data <- vroom(file.path(data_path, "base_data.csv"),
                   delim = ";") |> 
  mutate(state = factor(state, levels = states),
         year = factor(year, levels = c(2013, 2017, 2021))) |> 
  mutate(state_id = as.integer(state),
         year_id = as.integer(year),
         gender = as.integer(factor(gender, levels = c("male", "female"))),
         education = as.integer(factor(education, levels = c("low", "middle", "high")))
         ) |> 
  filter(party_identification > 0)


#######################
# Interaction with year
#######################
interaction_data <- base_data |> 
  mutate(east_year = as.integer(factor(interaction(east, year_id))),
         state_year_id = as.integer(factor(interaction(state, year)))
  )
state_year_data <- interaction_data |> 
  select(state, state_id, year, year_id, east, east_year, unemp_rate, gdp_per_capita) |> 
  distinct()

X <- model.matrix(~ -1 + party_identification_afd + gender +
                  factor(education) + unemp + unemp_past + income + factor(year_id),
                  data = interaction_data)
Z <- model.matrix(~ -1 + east + factor(east_year) + gdp_per_capita + unemp_rate +
                    factor(year_id),
                  data = state_year_data)

data_stan_int <- list(
  N = nrow(interaction_data),
  S = length(unique(state_year_data$state_id)),
  TT = length(unique(interaction_data$year_id)),
  ST = length(unique(interaction_data$state_year_id)),
  K = ncol(X),
  L = ncol(Z),
  ss = state_year_data$state_id,
  ti = interaction_data$year_id,
  ts = state_year_data$year_id,
  st = interaction_data$state_year_id,
  X = X,
  Z = Z,
  y = interaction_data$satis_demo
)

int_model <- "
data {
  int<lower=1> N;                   // Number of observations
  int<lower=1> S;                  // Number of states
  int<lower=1> TT;                  // Number of election years
  int<lower=1> ST;                 // Number of state-year combinations
  int<lower=1> K;                  // Number of individual-level covariates
  int<lower=1> L;                  // Number of state-level covariates
  array[ST] int<lower=1, upper=S> ss;  // State ID
  array[N] int<lower=1, upper=TT> ti;  // Year ID for individual-level data
  array[ST] int<lower=1, upper=TT> ts;  // Year ID for state-level data
  array[N]  int<lower=1, upper=ST> st;  // State-year ID
  matrix[N, K] X;                  // Individual-level covariates
  matrix[ST, L] Z;                      // State-level covariates
  array[N] int<lower=1> y;        // Outcome
}

parameters{
  vector[S] gamma_raw;   // standard normal raw state intercept
  vector[ST] alpha_raw;   // standard normal raw state-year intercept
  vector[ST] alpha;      // varying intercept for state-years
  vector[K] beta_0;      // slopes for individual-level covariates
  vector[L] beta_1;      // slopes for state-year level covariates
  vector[S] gamma;       // varying intercept for states
  vector[TT] phi_0;     // varying intercept for years (individual-level)
  vector[TT] phi_1;     // varying intercept for years (state-level)
  real mu_gamma;        // average across states
  
  real<lower=0> sigma_y;    // between-individual variation
  real<lower=0> sigma_alpha;  // between-state-year variation
  real<lower=0> sigma_gamma;  // between-state variation
}

transformed parameters {
  vector[S] gamma = mu_gamma + sigma_gamma * gamma_raw;
}

model {
  beta_0 ~ normal(0, 10);
  beta_1 ~ normal(0, 10);
  gamma ~ normal(mu_gamma, sigma_gamma);
  mu_gamma ~ normal(0, 10);
  phi_0 ~ normal(0, 10);
  phi_1 ~ normal(0, 10);
  alpha ~ normal(gamma[ss] + Z * beta_1 + phi_0[ts], sigma_alpha);
  
  // likelihood
  y ~ normal(alpha[st] + X * beta_0 + phi_1[ti], sigma_y);
}
"
# Save .stan file
write(int_model, file.path(stan_path, "int_model_party_identification.stan"))

# Compile model
int_model_compiled <- cmdstan_model(file.path(stan_path, "int_model_party_identification.stan"))

# sample from the posterior
fit <- int_model_compiled$sample(
  data = data_stan_int,
  seed = 1457L,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 1000,
  refresh = 50,
  output_dir = file.path(posterior_path, "int_model"),
  output_basename = "party_identification",
  save_warmup = TRUE,
  save_metric = TRUE,
  save_cmdstan_config = FALSE
)



















