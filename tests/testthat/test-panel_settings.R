test_that("default settings follow the recommended reaction layout", {
  reactions <- default_panel_settings()$pool_reactions

  expect_equal(unname(reactions[c("D1", "D1.1", "1A", "R1", "R1.1", "R1.2", "1B", "5")]), rep("1", 8))
  expect_equal(unname(reactions[c("R2", "R2.1", "2")]), rep("2", 3))
  expect_equal(unname(reactions[c("M1", "M1.1", "M1.addon")]), rep("1", 3))
  expect_equal(unname(reactions[c("M2", "M2.1")]), rep("2", 2))
})

test_that("default settings exclude the species targets", {
  excluded <- default_panel_settings()$excluded_targets

  expect_true(all(c(
    "Pf3D7_13_v3-1041624-1041829", "PvP01_12_v1-1185007-1185184",
    "PKNH_MIT_v2-0002298-0002488", "PKNH_12_v2-198869-199113-1AB"
  ) %in% excluded))
})

test_that("panel_settings extends a base", {
  settings <- panel_settings(c(R1.2 = "3", custom = "A"), excluded_targets = "extra", base = default_panel_settings())

  expect_equal(settings$pool_reactions[["R1.2"]], "3")
  expect_equal(settings$pool_reactions[["custom"]], "A")
  expect_equal(settings$pool_reactions[["D1"]], "1")
  expect_true("extra" %in% settings$excluded_targets)
  expect_true("Pf3D7_13_v3-1041624-1041829" %in% settings$excluded_targets)
})

test_that("panel_settings validates its input", {
  expect_error(panel_settings(c("1", "2")), "named vector")
  expect_error(panel_settings(c(a = "1", a = "2")), "Duplicate pools")
  expect_error(panel_settings(c(a = NA)), "must have a reaction")
  expect_error(panel_settings(base = list()), "`base` must be created")
})

test_that("assign_reactions splits shared pools and counts targets in each reaction", {
  panel_reactions <- assign_reactions(fixture_panel())

  shared <- panel_reactions[panel_reactions$target_name == "targetD", ]
  expect_setequal(shared$reaction, c("1", "2"))
  expect_equal(panel_reactions$reaction[panel_reactions$target_name == "targetA"], "1")
  expect_equal(panel_reactions$reaction[panel_reactions$target_name == "targetE"], "2")
  expect_equal(nrow(panel_reactions), nrow(fixture_panel()) + 1)
})

test_that("targets in several pools of the same reaction are counted once", {
  panel <- fixture_panel()
  panel$pool[1] <- "D1.1,R1.2"

  panel_reactions <- assign_reactions(panel)

  expect_equal(sum(panel_reactions$target_name == "targetA"), 1)
})

test_that("assign_reactions errors on pools without a reaction", {
  panel <- fixture_panel()
  panel$pool[1] <- "AMPLseq"

  expect_error(assign_reactions(panel), "No reaction is defined for pool\\(s\\): AMPLseq")
  expect_no_error(assign_reactions(panel, panel_settings(c(AMPLseq = "3"), base = default_panel_settings())))
})

test_that("assign_reactions requires a pool column", {
  expect_error(assign_reactions(fixture_panel()[, -7]), "no `pool` column")
})

test_that("summarise_pools counts targets per pool", {
  pools <- summarise_pools(fixture_panel())

  expect_equal(pools$n_targets[pools$pool == "R1.2"], 3)
  expect_equal(pools$n_targets[pools$pool == "R2.1"], 3)
  expect_equal(pools$reaction[pools$pool == "R2.1"], "2")
})
