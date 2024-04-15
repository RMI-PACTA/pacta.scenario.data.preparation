scenario_regions <- readr::read_csv(
  here::here("data-raw", "scenario_regions.csv"),
  col_types = "ccci",
  na = ""
)
