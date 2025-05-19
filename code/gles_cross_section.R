source("code/setup.R")
source("gles_cross_section/column_mapping.R")

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

years <- c(2013, 2017, 2021)
all_data <- list(
  read_dta("gles_cross_section/ZA5700_en_v2-0-2.dta") |> 
    select(all_of(unname(mapping_2013))) |> 
    rename(all_of(mapping_2013)),
  read_dta("gles_cross_section/ZA6800_en_v5-1-0.dta") |> 
    select(all_of(unname(mapping_2017))) |> 
    rename(all_of(mapping_2017)),
  read_dta("gles_cross_section/ZA7700_v3-1-0_en.dta") |> 
    select(all_of(unname(mapping_2021))) |> 
    rename(all_of(mapping_2021))
)


vars_to_check <- c(
  "state",
  "year",
  "east_west_1",
  "satis_demo",
  "person_econ_current",
  "school_certificate",
  "employment",
  "unemp_past",
  "income"
)


gles_data <- tibble()
for (i in seq_along(years)) {
  df <- all_data[[i]] |> 
    filter(if_all(all_of(vars_to_check), ~. >= 0)) |>
    filter(str_to_lower(as.character(haven::as_factor(gender))) %in% c("male", "female")) |>
    mutate(state = as.character(haven::as_factor(state)),
           gender = factor(as.integer(haven::as_factor(gender)),
                           labels = c("male", "female"))
           )|> 
    mutate(vote_int_first_label = as.character(haven::as_factor(vote_int_first)),
           .after = vote_int_first
           ) |> 
    mutate(vote_int_second_label = as.character(haven::as_factor(vote_int_second)),
           .after = vote_int_second) |> 
    mutate(party_identification_label = as.character(haven::as_factor(party_identification)),
           .after = party_identification
           ) |> 
    mutate(age = as.integer(year - as.numeric(year_birth)),
           .before = year_birth) |> 
    filter(!is.na(age)) |> 
    select(-year_birth)
  
  df <- df |> 
    mutate(across(c(east_west_1, satis_demo, vote_int_first, vote_int_second, 
                    party_identification, person_econ_current, 
                    school_certificate,employment, unemp_past, income), as.integer))
  
  if ("east_west_2" %in% colnames(df)) {
    df <-  df |> 
      filter(east_west_2 > 0) |> 
      mutate(east_west_2 = as.integer(east_west_2))
  }
    
  print(nrow(df))
  gles_data <- bind_rows(df, gles_data)
}

cleaned_gles_data <- gles_data |> 
  mutate(state = case_match(
    state,
    c("Lower-Saxony", "Lower Saxony") ~ "Lower Saxony",
    c("Rhineland-Palatinate", "Rhineland Palatinate") ~ "Rhineland Palatinate",
    c("North Rhine-Westfalia", "North Rhine Westphalia") ~ "North Rhine Westphalia",
    .default = state
  )) |> 
  mutate(state = factor(state, levels = states)) |> 
  mutate(vote_int_first_afd = ifelse(vote_int_first_label == "AfD", 1, 0),
         vote_int_second_afd = ifelse(vote_int_second_label == "AfD", 1, 0),
         party_identification_afd = ifelse(party_identification_label == "AfD", 1, 0),
         education = case_when(school_certificate %in% c(1, 2) ~ "low",
                               school_certificate %in% c(3, 4, 6) ~ "middle",
                               school_certificate == 5 ~ "high",
                               .default = NA),
         unemp = ifelse(employment == 7, 1, 0),
         east = ifelse(east_west_1 == 1, 1, 0),
         ) |> 
  mutate(education = factor(education, levels = c("low", "middle", "high")),
         satis_demo = 6 - satis_demo) |> # reverse order s.t. 1 = not satisfied, 5 = very satisfied
  select(year, ID, state, east, gender, age, satis_demo,
         vote_int_first, vote_int_first_afd, vote_int_second, vote_int_second_afd, 
         party_identification, party_identification_afd,
         person_econ_current, education, unemp, unemp_past,
         income) |> 
  drop_na() |> 
  arrange(year, ID)

state_de_to_en <- c(
  "Sachsen-Anhalt" = "Saxony-Anhalt",
  "Niedersachsen" = "Lower Saxony",
  "Nordrhein-Westfalen" = "North Rhine Westphalia",
  "Sachsen" = "Saxony",
  "Baden-Württemberg" = "Baden-Wuerttemberg",
  "Hessen" = "Hesse",
  "Bayern" = "Bavaria",
  "Rheinland-Pfalz" = "Rhineland Palatinate",
  "Thüringen" = "Thuringia"
)


gdp_data <- vroom("destatis/82111-0010_en_flat.csv", 
                  delim = ";", col_select = c("time", "1_variable_attribute_label",
                                              "value", "value_unit")) |> 
  filter(time %in% c(2013, 2017, 2021)) |> 
  rename("year" = time,
         "state" = `1_variable_attribute_label`,
         "gdp" = value,
         "gdp_unit" = value_unit
         ) |> 
  mutate(state = recode(state, !!!state_de_to_en)) |> 
  mutate(state = factor(state, levels = states))

pop_data <- vroom("destatis/12411-0010_en_flat.csv",
                  delim = ";", col_select = c("time", "1_variable_attribute_label",
                                              "value")) |> 
  mutate(year = year(ymd(time)), .before = time) |> 
  filter(year %in% c(2013, 2017, 2021)) |> 
  rename("state" = `1_variable_attribute_label`,
         "pop" = value) |> 
  select(-time) |> 
  mutate(state = recode(state, !!!state_de_to_en)) |> 
  mutate(state = factor(state, levels = states))

unemp_data <- vroom("destatis/13211-0007_en_flat.csv",
                    delim = ";", col_select = c("time", "1_variable_attribute_label",
                                                "value", "value_unit",
                                                "value_variable_label")) |> 
  filter(time %in% c(2013, 2017, 2021)) |> 
  filter(value_variable_label == "Unemployment as percent. of civilian labour force") |> 
  filter(value_unit == "percent") |> 
  rename("year" = time,
         "state" = `1_variable_attribute_label`,
         "unemp_rate" = value) |> 
  select(-c(value_unit, value_variable_label)) |> 
  mutate(state = recode(state, !!!state_de_to_en)) |> 
  mutate(state = factor(state, levels = states))

state_level_data <- gdp_data |> 
  full_join(pop_data, by = c("state", "year"), relationship = "one-to-one") |> 
  full_join(unemp_data, by = c("state", "year"), relationship = "one-to-one") |> 
  mutate(gdp_per_capita = gdp / pop * 10^2)  # unit: EUR 10K


merged_data <- cleaned_gles_data |> 
  left_join(state_level_data, 
            by = c("state", "year"),
            relationship = "many-to-one"
            )
vroom_write(merged_data, "data/base_data.csv", delim = ";")




