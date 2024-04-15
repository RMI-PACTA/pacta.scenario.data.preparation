raw_data_env <- new.env()

source(file = "data-raw/prepare_scenario_regions.R", local = raw_data_env)
source(file = "data-raw/prepare_scenario_source_pacta_geography_bridge.R", local = raw_data_env)

source(file = "data-raw/prepare_geco_2022_geography_bridge.R", local = raw_data_env)
source(file = "data-raw/prepare_geco_2022_technology_bridge.R", local = raw_data_env)

source(file = "data-raw/prepare_geco_2023_geography_bridge.R", local = raw_data_env)
source(file = "data-raw/prepare_geco_2023_technology_bridge.R", local = raw_data_env)

source(file = "data-raw/prepare_isf_2021_geography_bridge.R", local = raw_data_env)
source(file = "data-raw/prepare_isf_2021_technology_bridge.R", local = raw_data_env)

source(file = "data-raw/prepare_isf_2023_geography_bridge.R", local = raw_data_env)
source(file = "data-raw/prepare_isf_2023_technology_bridge.R", local = raw_data_env)

source(file = "data-raw/prepare_weo_2022_geography_bridge.R", local = raw_data_env)
source(file = "data-raw/prepare_weo_2022_technology_bridge.R", local = raw_data_env)

source(file = "data-raw/prepare_weo_2023_geography_bridge.R", local = raw_data_env)
source(file = "data-raw/prepare_weo_2023_technology_bridge.R", local = raw_data_env)

save(list = names(raw_data_env), file = "R/sysdata.rda", envir = raw_data_env)
