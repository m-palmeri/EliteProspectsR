
test_that("Vegas Golden Knights 2019-2020", {
  staff_list <- get_team_staff("https://www.eliteprospects.com/team/22211/vegas-golden-knights/2019-2020")

  expect_false(any(is.na(staff_list$role)))

  expect_true("Peter DeBoer" %in% staff_list$staff_name)
  expect_equal(staff_list$role[staff_list$staff_name == "Peter DeBoer"], "Head Coach")

  expect_true("Bill Foley" %in% staff_list$staff_name)
  expect_setequal(staff_list$role[staff_list$staff_name == "Bill Foley"],
                  c("CEO", "Chairman", "Franchise Owner"))

  expect_true("Tom Cruz" %in% staff_list$staff_name)
  expect_equal(staff_list$role[staff_list$staff_name == "Tom Cruz"], "Video Coach")
})


test_that("Lloydminster Bobcats 2017-2018", {
  staff_list <- get_team_staff("https://www.eliteprospects.com/team/3292/lloydminster-bobcats/2017-2018")

  expect_false(any(is.na(staff_list$role)))

  expect_true("Brent Mohrbutter" %in% staff_list$staff_name)
  expect_equal(staff_list$role[staff_list$staff_name == "Brent Mohrbutter"], "President")

  expect_true("Travis Clayton" %in% staff_list$staff_name)
  expect_equal(staff_list$role[staff_list$staff_name == "Travis Clayton"], "GM/Head Coach")
})
