# define necessary arguments
count <- 15300
popn <- c(1500000, 550000, 120000)
std_rt <- c(0.0012, 0.0036, 0.048)

test_that("function works when all arguments are valid", {
  expect_equal(get_smr(count, popn, std_rt),
    count / sum(popn * std_rt),
    tolerance = 1e-6
  )
  expect_equal(
    get_smr(count, popn, std_rt, output_type = "counts"),
    popn * std_rt
  )
  expect_equal(get_smr(count, popn, std_rt, percent = TRUE),
    count / sum(popn * std_rt) * 100,
    tolerance = 1e-6
  )
})

# define a character vector
char_vec <- c("apple", "banana", "carrot")

test_that("function throws an error when the input array isn't numeric", {
  expect_error(get_smr(count, popn = char_vec, std_rt),
    "`popn` must be a numeric vector"
  )
  expect_error(
    get_smr(count, popn, std_rt = char_vec),
    "`std_rt` must be a numeric vector"
  )
})

test_that("function throws an error when `count` isn't a numeric scalar", {
  expect_error(get_smr("apple", popn, std_rt), "`count` must be numeric")
  expect_error(get_smr(char_vec, popn, std_rt), "`count` must be numeric")
  expect_error(get_smr(c(15300, 147000), popn, std_rt), "`count` must be a scalar")
})

test_that("function throws an error when the lengths of input arrays don't match", {
  expect_error(
    get_smr(count, popn, std_rt[1:2]),
    "lengths of `popn` and `std_rt` must match"
  )
})

test_that("function throws an error when `output_type` isn't one of 'ratio' or 'counts'", {
  expect_error(
    get_smr(count, popn, std_rt, output_type = "rate"),
    '`output_type` must be either "ratio" or "counts"'
  )
})

test_that("function throws an error when `percent` isn't logical", {
  expect_error(
    get_smr(count, popn, std_rt, percent = "rate"),
    "`percent` must be one of TRUE or FALSE"
  )
  expect_error(
    get_smr(count, popn, std_rt, percent = NA),
    "`percent` must be one of TRUE or FALSE"
  )
})

# define input vectors with NA
popn_na <- c(1500000, 550000, NA)
std_rt_na <- c(0.0012, 0.0036, NA)

test_that("function throws an error when `output_type` is 'ratio' and input vectors have NA values", {
  expect_error(
    get_smr(count, popn_na, std_rt, output_type = "ratio"),
    "when calculating the SMR, `popn` should not have any NA values"
  )
  expect_error(
    get_smr(count, popn, std_rt_na, output_type = "ratio"),
    "when calculating the SMR, `std_rt` should not have any NA values"
  )
})

test_that("function raises a warning when `output_type` is 'counts' and input vectors have NA values", {
  expect_warning(
    get_smr(count, popn_na, std_rt, output_type = "counts"),
    "one or more elements in `popn` are NA"
  )
  expect_equal(
    suppressWarnings(get_smr(count, popn_na, std_rt, output_type = "counts")),
    popn_na * std_rt,
    tolerance = 1e-6
  )
  expect_warning(
    get_smr(count, popn, std_rt_na, output_type = "counts"),
    "one or more elements in `std_rt` are NA"
  )
  expect_equal(
    suppressWarnings(get_smr(count, popn, std_rt_na, output_type = "counts")),
    popn * std_rt_na,
    tolerance = 1e-6
  )
})
