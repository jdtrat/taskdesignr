test_that("get individual string lengths are appropriate", {

  val <- purrr::map(1:100,
             ~ purrr::map_chr(.x = 1:100,
                            ~ get_individual_string(
                              length = .x,
                              digits = TRUE,
                              upperalpha = TRUE,
                              loweralpha = TRUE
                            )))

  purrr::walk(.x = val,
              ~expect_equal(nchar(.x), seq(1, 100, by = 1))
  )

})

test_that("check_unique() works", {

  check_not_unique <- function(...) {
    not_unique_vec <- rand_str(n = 1000,
                                      length = 3,
                                      unique = FALSE)
    return(sum(duplicated(not_unique_vec)))
  }

  check_unique <- function(...) {
    unique_vec <- rand_str(n = 1000,
                                  length = 3,
                                  unique = TRUE,
                                  ...)

    return(sum(duplicated(unique_vec)))
  }

  unique <- sum(purrr::map_dbl(1:100,
                               ~check_unique()))

  not_unique <- mean(purrr::map_dbl(1:100,
                                    ~check_not_unique()))

  expect_equal(unique, 0)
  expect_gt(not_unique, 0)

})
