test_that("edit_spatial_points validates input", {

  # Test non-sf input
  expect_error(
    edit_spatial_points(data.frame(x = 1:3, y = 1:3)),
    "Input must be an sf object"
  )

  # Test non-POINT geometry
  library(sf)
  lines <- st_sf(
    id = 1,
    geometry = st_sfc(st_linestring(matrix(1:4, 2, 2)))
  )
  expect_error(
    edit_spatial_points(lines),
    "only works with POINT"
  )
})

test_that("compare_edits works correctly", {
  library(sf)

  original <- st_sf(
    id = 1:2,
    geometry = st_sfc(
      st_point(c(0, 0)),
      st_point(c(1, 1))
    ),
    crs = 4326
  )

  edited <- original
  st_geometry(edited)[1] <- st_sfc(st_point(c(0.1, 0.1)), crs = 4326)

  comparison <- compare_edits(original, edited, id_column = "id")

  expect_s3_class(comparison, "data.frame")
  expect_equal(nrow(comparison), 2)
  expect_true("Modified" %in% names(comparison))
  expect_true(comparison$Modified[1])
  expect_false(comparison$Modified[2])
})
