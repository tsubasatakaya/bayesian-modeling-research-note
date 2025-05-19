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
         state_year_id = as.integer(factor(interaction(state, year))),
         gender = as.integer(factor(gender, levels = c("male", "female"))),
         education = as.integer(factor(education, levels = c("low", "middle", "high")))
         ) |> 
  filter(vote_int_second > 0)

state_year_data <- base_data |> 
  select(state, state_id, year, year_id, east, unemp_rate, gdp_per_capita) |> 
  distinct()


#######################
# Varying slope by year
#######################
X <- model.matrix(~ 1 + gender + factor(education) + unemp + unemp_past 
                  + person_econ_current,
                  data = base_data) |> 
  as.data.frame() |> 
  mutate(
    across(all_of(c("unemp_past", "person_econ_current")), ~ (.- mean(.)) / sd(.)),
    across(-all_of(c("unemp_past", "person_econ_current")), ~ . - mean(.))
  ) |> 
  as.matrix()
X <- X[,-1]
# afd <- scale(base_data$vote_int_second_afd, scale = FALSE) |> 
#   as.vector()
afd <- base_data$vote_int_second_afd

Z <- model.matrix(~ 1 + factor(year_id) + gdp_per_capita + unemp_rate,
                  data = state_year_data) |> 
  as.data.frame() |> 
  mutate(
    across(all_of(c("gdp_per_capita", "unemp_rate")), ~ (.- mean(.)) / sd(.)),
    across(-all_of(c("gdp_per_capita", "unemp_rate")), ~ . - mean(.))
  ) |> 
  as.matrix()
Z <- Z[,-1]
# east <- scale(state_year_data$east, scale = FALSE) |> 
#   as.vector()
east <- state_year_data$east

data_stan_year_slope <- list(
  N = nrow(base_data),
  S = length(unique(state_year_data$state_id)),
  ST = length(unique(base_data$state_year_id)),
  TT = length(unique(base_data$year_id)),
  K = ncol(X),
  L = ncol(Z),
  st = base_data$state_year_id,
  ss = state_year_data$state_id,
  ti = base_data$year_id,
  ts = state_year_data$year_id,
  X = X,
  afd = afd,
  Z = Z,
  east = east,
  y = base_data$satis_demo
)

year_slope_model <- "
data {
  int<lower=1> N;                         // Number of observations
  int<lower=1> S;                         // Number of states
  int<lower=1> ST;                        // Number of state-year combinations
  int<lower=1> TT;                        // Number of election years
  int<lower=1> K;                         // Number of individual-level covariates
  int<lower=1> L;                         // Number of state-level covariates
  array[N]  int<lower=1, upper=ST> st;    // State-year ID
  array[ST] int<lower=1, upper=S> ss;     // State ID
  array[N]  int<lower=1, upper=TT> ti;    // Year ID (individual-level)
  array[ST] int<lower=1, upper=TT> ts;    // Year ID (state-level)
  matrix[N, K] X;                         // Individual-level covariates
  vector[N] afd;                          // AfD support
  matrix[ST, L] Z;                        // State-level covariates
  vector[ST]  east;                       // East Germany dummy
  array[N] real y;                        // Outcome
}

parameters{
  vector[S] gamma_raw;                    // standard normal raw state intercept
  vector[ST] alpha_raw;                   // standard normal raw state-year intercept
  real mu_gamma;                          // average across states
  
  vector[K] beta_0;                       // slopes for individual-level covariates
  vector[L] beta_1;                       // slopes for state-year level covariates
  
  vector[TT] delta_raw;                   // standard normal raw AfD varying slope (by year)
  vector[TT] tau_raw;                     // standard normal raw East varying slope (by year)
  real mu_delta;                          // average slope AfD
  real mu_tau;                            // average slope East
  
  real<lower=0> sigma_y;                  // between-individual variation
  real<lower=0> sigma_alpha;              // between-state-year variation
  real<lower=0> sigma_gamma;              // between-state variation
  real<lower=0> sigma_delta;              // between-year variation for AfD slope
  real<lower=0> sigma_tau;                // between-year variation for East slope
}

transformed parameters {
  // non-centered parameterization
  vector[TT] delta = mu_delta + sigma_delta * delta_raw;
  vector[TT] tau = mu_tau + sigma_tau * tau_raw;
  vector[S] gamma = mu_gamma + sigma_gamma * gamma_raw;   
  
  vector[ST] alpha_mean = gamma[ss] + east .* tau[ts] + Z * beta_1;
  vector[ST] alpha = alpha_mean + sigma_alpha * alpha_raw;  
}

model {
  gamma_raw ~ std_normal();
  alpha_raw ~ std_normal();
  delta_raw ~ std_normal();
  tau_raw ~ std_normal();
  delta ~ normal(0, 10);
  beta_0 ~ normal(0, 10);
  beta_1 ~ normal(0, 10);
  mu_gamma ~ normal(0, 10);
  mu_delta ~ normal(0, 10);
  mu_tau ~ normal(0, 10);
  sigma_y ~ normal(0, 10);
  sigma_gamma ~ normal(0, 2);
  sigma_alpha ~ normal(0, 2);
  sigma_delta ~ normal(0, 2);
  sigma_tau ~ normal(0, 2);
  
  // likelihood
  y ~ normal(alpha[st] + afd .* delta[ti] + X * beta_0, sigma_y);
}
"
# Save .stan file
write(year_slope_model, file.path(stan_path, "year_slope_model_vote_intention_second.stan"))

# Compile model
year_slope_model_compiled <- cmdstan_model(file.path(stan_path, 
                                                     "year_slope_model_vote_intention_second.stan"))

# sample from the posterior
fit <- year_slope_model_compiled$sample(
  data = data_stan_year_slope,
  seed = 1457L,
  parallel_chains = 4,
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 1000,
  refresh = 50,
  adapt_delta = 0.98,
  output_dir = file.path(posterior_path, "varying_slope_model"),
  output_basename = "vote_intention_second",
  save_warmup = TRUE,
  save_metric = FALSE,
  save_cmdstan_config = FALSE
)
fit$save_object(file.path(output_path, "models", "year_slope_vote_intention_second_fitted.RDS"))








