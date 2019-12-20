library(testthat)
library(depth)

context("depth")

test_that("depth implementation is correct", {
  data_depth <- data.frame(z1 = c(-2, -0.5, 0.5, 2), z2 = 0)
  halfspaces_depth <- train_depth(expand.grid(data_depth), n_halfspace = 1000,
                                  scope = 2, seed = 1)
  expect_equivalent(evaluate_depth(data_depth, halfspaces = halfspaces_depth,
                                   metric = "depth") / 16,
                    apply(data_depth, 1, depth, x = data_depth))
})

context("mass")

test_that("mass implementation is correct", {
  span <- c(-2, -0.5, 0.5, 2)
  data_mass <- data.frame(z1 = span, z2 = span, z3 = span)
  halfspaces_mass <- train_depth(expand.grid(data_mass), n_halfspace = 1000,
                                 scope = 1, seed = 1)
  expect_equivalent(evaluate_depth(data_mass, halfspaces = halfspaces_mass,
                                   metric = "mass"),
                    c(0.65, 0.8, 0.8, 0.65),
                    tolerance = 5e-2)
})

