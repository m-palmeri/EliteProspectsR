
test_that("Ralph Kreuger (alive with player page)", {
  staff_info <- get_staff_info("https://www.eliteprospects.com/staff/539/ralph-krueger")

  expect_equal(staff_info$staff_id, 539)
  expect_false(is.na(staff_info$player_id))
  expect_false(is.na(staff_info$player_link))
})


test_that("Dick Irvin (dead with player page)", {
  staff_info <- get_staff_info("https://www.eliteprospects.com/staff/6265/dick-irvin")

  expect_equal(staff_info$staff_id, 6265)
  expect_equal(staff_info$staff_name, "Dick Irvin")
  expect_false(is.na(staff_info$player_id))
  expect_false(is.na(staff_info$player_link))
})


test_that("Stan Kroenke (no player page)", {
  staff_info <- get_staff_info("https://www.eliteprospects.com/staff/1642/stan-kroenke")

  expect_equal(staff_info$staff_id, 1642)
  expect_equal(staff_info$place_of_birth, "Columbia, MO, USA")
  expect_na(staff_info$player_id)
  expect_na(staff_info$player_link)
})
