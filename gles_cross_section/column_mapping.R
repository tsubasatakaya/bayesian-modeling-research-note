mapping_2009 <- c(
  "year" = "year",
  "ID" = "vw_lfdn",
  "state" = "land",
  "east_west_1" = "ostwest", # all of Berlin = East
  "east_west_2" = "ostwest2",  # classification of Berlin
  "gender" = "q1",
  "age" = "q1a",
  "satis_demo" = "q5",
  "vote_int_party" = "q11a",
  "person_econ_current" = "q18",
  "position_socioecon" = "q63",
  "position_liber_autho" = "q64",
  "party_identification" = "q139a",
  "year_birth" = "d201b",
  "school_certificate" = "d206",
  "employment" = "d209",
  "unemp_past" = "d212",
  "fear_job_loss" = "d226",
  "income" = "d270b"
)

mapping_2013 <- c(
  "year" = "year",
  "ID" = "lfdn",
  "state" = "bl",
  "east_west_1" = "ostwest", # all of Berlin = East
  "gender" = "q1",
  "year_birth" = "q2c",
  "satis_demo" = "q6",
  "vote_int_party" = "q11aa",
  "person_econ_current" = "q17",
  # "position_socioecon" = "q67",
  # "position_liber_autho" = "q68",
  "party_identification" = "q119a",
  "school_certificate" = "q163",
  "employment" = "q165",
  "unemp_past" = "q173a",
  "income" = "q215"
)


mapping_2017 <- c(
  "year" = "year",
  "ID" = "lfdn",
  "state" = "bula",
  "east_west_1" = "ostwest", # all of Berlin = East
  "east_west_2" = "ostwest2",  # classification of Berlin
  "gender" = "q1",
  "year_birth" = "q2a",
  "satis_demo" = "q6",
  "vote_int_party" = "q11aa",
  "person_econ_current" = "q15",
  # "position_socioecon" = "q59",
  # "position_liber_autho" = "q60",
  "party_identification" = "q99a",
  "school_certificate" = "q136",
  "employment" = "q138",
  "unemp_past" = "q146a",
  "income" = "q192"
)

mapping_2021 <- c(
  "year" = "intyear",
  "ID" = "lfdn",
  "state" = "bula",
  "east_west_1" = "ostwest", # all of Berlin = East
  "east_west_2" = "ostwest2",  # classification of Berlin
  "gender" = "d1",
  "year_birth" = "d2a",
  "satis_demo" = "q4",
  "vote_int_party" = "q8aa",
  "person_econ_current" = "q13",
  "party_identification" = "q75a",
  "school_certificate" = "d7",
  "employment" = "d9",
  "unemp_past" = "d17a",
  "income" = "d63"
)
