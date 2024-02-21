test_technology_bridge_geco <- fake_technology_bridge(
  TechnologyName = c("Electric", "Steel", "Coal"),
  TechnologyAll = c("Electric", "Steel", "Coal")
)

test_that("with known inputs, has expected output", {
  out <- prepare_geco_2022_scenario(
    technology_bridge = test_technology_bridge_geco,
    geco_2022_automotive_raw = fake_geco_2022_automotive_raw(),
    geco_2022_aviation_raw = fake_geco_2022_aviation_raw(),
    geco_2022_steel_raw = fake_geco_2022_steel_raw(),
    geco_2022_fossil_fuels_15c_raw = fake_geco_2022_fossil_fuels_raw(
      Scenario = "1.5C"
    ),
    geco_2022_fossil_fuels_ndc_raw = fake_geco_2022_fossil_fuels_raw(
      Scenario = "NDC"
    ),
    geco_2022_fossil_fuels_ref_raw = fake_geco_2022_fossil_fuels_raw(
      Scenario = "Reference"
    ),
    geco_2022_power_15c_raw = fake_geco_2022_power_raw(
      Scenario = "1.5C"
    ),
    geco_2022_power_ndc_raw = fake_geco_2022_power_raw(
      Scenario = "NDC"
    ),
    geco_2022_power_ref_raw = fake_geco_2022_power_raw(
      Scenario = "Reference"
    )
  )

  expect_snapshot(out)
})

test_that("with bad input, errors gracefully", {

  bad_input_prepare_geco_2022_scenario <- function(
    # this just makes the test writing a little more concise
    technology_bridge = fake_technology_bridge(),
    geco_2022_automotive_raw = fake_geco_2022_automotive_raw(),
    geco_2022_aviation_raw = fake_geco_2022_aviation_raw(),
    geco_2022_steel_raw = fake_geco_2022_steel_raw(),
    geco_2022_fossil_fuels_15c_raw = fake_geco_2022_fossil_fuels_raw(
      Scenario = "1.5C"
    ),
    geco_2022_fossil_fuels_ndc_raw = fake_geco_2022_fossil_fuels_raw(
      Scenario = "NDC"
    ),
    geco_2022_fossil_fuels_ref_raw = fake_geco_2022_fossil_fuels_raw(
      Scenario = "Reference"
    ),
    geco_2022_power_15c_raw = fake_geco_2022_power_raw(
      Scenario = "1.5C"
    ),
    geco_2022_power_ndc_raw = fake_geco_2022_power_raw(
      Scenario = "NDC"
    ),
    geco_2022_power_ref_raw = fake_geco_2022_power_raw(
      Scenario = "Reference"
    )
  ) {
    prepare_geco_2022_scenario(
      technology_bridge = technology_bridge,
      geco_2022_automotive_raw = geco_2022_automotive_raw,
      geco_2022_aviation_raw = geco_2022_aviation_raw,
      geco_2022_steel_raw = geco_2022_steel_raw,
      geco_2022_fossil_fuels_15c_raw = geco_2022_fossil_fuels_15c_raw,
      geco_2022_fossil_fuels_ndc_raw = geco_2022_fossil_fuels_ndc_raw,
      geco_2022_fossil_fuels_ref_raw = geco_2022_fossil_fuels_ref_raw,
      geco_2022_power_15c_raw = geco_2022_power_15c_raw,
      geco_2022_power_ndc_raw = geco_2022_power_ndc_raw,
      geco_2022_power_ref_raw = geco_2022_power_ref_raw
    )
  }

  expect_error(
    bad_input_prepare_geco_2022_scenario(technology_bridge = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_automotive_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_aviation_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_steel_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_fossil_fuels_15c_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_fossil_fuels_ndc_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_fossil_fuels_ref_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_power_15c_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_power_ndc_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
  expect_error(
    bad_input_prepare_geco_2022_scenario(geco_2022_power_ref_raw = "bad"),
    "data.frame.*not.*TRUE"
  )
})
