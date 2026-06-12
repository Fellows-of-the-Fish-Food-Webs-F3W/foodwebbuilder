test_that("flatten_foodweb returns the expected structure", {

  foodweb <- matrix(
    c(0, 1,
      1, 0),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("prey1", "prey2"),
      c("cons1", "cons2")
    )
  )

  flat <- flatten_foodweb(foodweb)

  expect_s3_class(flat, "data.frame")

  expect_equal(
    names(flat),
    c("prey", "consumer", "interaction")
  )

  expect_equal(nrow(flat), length(foodweb))
})

test_that("flatten_foodweb preserves ordering and values", {

  foodweb <- matrix(
    c(0, 1,
      1, 0),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("prey1", "prey2"),
      c("cons1", "cons2")
    )
  )

  flat <- flatten_foodweb(foodweb)

  expected <- data.frame(
    prey = c("prey1", "prey1", "prey2", "prey2"),
    consumer = c("cons1", "cons2", "cons1", "cons2"),
    interaction = c(0, 1, 1, 0)
  )

  expect_equal(flat, expected)
})

test_that("unflatten_foodweb reconstructs the original matrix", {

  foodweb <- matrix(
    c(0, 1,
      1, 0),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("prey1", "prey2"),
      c("cons1", "cons2")
    )
  )

  flat <- flatten_foodweb(foodweb)

  rebuilt <- unflatten_foodweb(flat)

  expect_equal(rebuilt, foodweb)
})

test_that("flatten and unflatten are inverse operations", {

  set.seed(123)

  foodweb <- matrix(
    sample(0:1, 9, replace = TRUE),
    nrow = 3,
    dimnames = list(
      c("a", "b", "c"),
      c("x", "y", "z")
    )
  )

  rebuilt <- unflatten_foodweb(
    flatten_foodweb(foodweb)
  )

  expect_equal(rebuilt, foodweb)
})

test_that("functions work with non-binary values", {

  foodweb <- matrix(
    c(0.2, -1.5,
      3.4, 0),
    nrow = 2,
    dimnames = list(
      c("p1", "p2"),
      c("c1", "c2")
    )
  )

  rebuilt <- unflatten_foodweb(
    flatten_foodweb(foodweb)
  )

  expect_equal(rebuilt, foodweb)
})

test_that("functions handle a 1x1 matrix", {

  foodweb <- matrix(
    1,
    nrow = 1,
    dimnames = list(
      "prey1",
      "cons1"
    )
  )

  flat <- flatten_foodweb(foodweb)

  rebuilt <- unflatten_foodweb(flat)

  expect_equal(rebuilt, foodweb)
})

test_that("flatten_foodweb errors when rownames are missing", {

  foodweb <- matrix(
    1:4,
    nrow = 2
  )

  expect_error(
    flatten_foodweb(foodweb)
  )
})

test_that("unflatten_foodweb errors when required columns are missing", {

  bad_df <- data.frame(
    a = 1:3,
    b = 4:6
  )

  expect_error(
    unflatten_foodweb(bad_df)
  )
})

test_that("flatten_foodweb_list returns a data frame", {
  
  fw1 <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(
      c("a", "b"),
      c("A", "B")
    )
  )
  
  fw2 <- matrix(
    5:8,
    nrow = 2,
    dimnames = list(
      c("a", "b"),
      c("A", "B")
    )
  )
  
  fw_list <- list(
    site1 = fw1,
    site2 = fw2
  )
  
  out <- flatten_foodweb_list(fw_list)
  
  expect_s3_class(out, "data.frame")
})

test_that("flatten_foodweb_list returns expected columns", {
  
  fw <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(
      c("a", "b"),
      c("A", "B")
    )
  )
  
  out <- flatten_foodweb_list(list(site1 = fw))
  
  expect_equal(
    names(out),
    c("prey", "consumer", "interaction", "operation_id")
  )
})

test_that("flatten_foodweb_list preserves interactions", {
  
  fw <- matrix(
    c(1, 2,
      3, 4),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("a", "b"),
      c("A", "B")
    )
  )
  
  out <- flatten_foodweb_list(list(site1 = fw))
  
  expect_equal(
    out$interaction,
    as.vector(t(fw))
  )
})

test_that("flatten_foodweb_list preserves operation ids", {
  
  fw1 <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(
      c("a", "b"),
      c("A", "B")
    )
  )
  
  fw2 <- matrix(
    5:8,
    nrow = 2,
    dimnames = list(
      c("a", "b"),
      c("A", "B")
    )
  )
  
  out <- flatten_foodweb_list(
    list(
      operation_1 = fw1,
      operation_2 = fw2
    )
  )
  
  expect_equal(
    unique(out$operation_id),
    c("operation_1", "operation_2")
  )
})