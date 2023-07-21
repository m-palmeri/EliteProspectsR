
test_that("Barry Trotz staff history", {
  staff_history <- get_staff_history("https://www.eliteprospects.com/staff/332/barry-trotz")

  expect_false(any(is.na(staff_history$season)))

  expect_equal(staff_history$team[staff_history$season == "2017-18"],
               "Washington Capitals")
  expect_equal(staff_history$league[staff_history$season == "1986-87"],
               "MJHL")
  expect_equal(staff_history$role[staff_history$season == "1998-99"],
               "Head Coach")
})


test_that("Patrick Roy staff history", {
  staff_history <- get_staff_history("https://www.eliteprospects.com/staff/499/patrick-roy")

  expect_false(any(is.na(staff_history$season)))

  expect_equal(staff_history$team[staff_history$season == "1998-99"],
               "Québec Remparts")
  expect_equal(staff_history$team[staff_history$season == "2016-17"],
               "Not active as staff")
  expect_setequal(staff_history$role[staff_history$season == "2015-16"],
                  c("Head Coach", "VP. of Hockey Operations"))
})


test_that("Ralph Krueger staff history", {
  staff_history <- get_staff_history("https://www.eliteprospects.com/staff/539/ralph-krueger")

  expect_false(any(is.na(staff_history$season)))

  expect_equal(staff_history$team[staff_history$season == "2009-10"],
               "Switzerland")
  expect_equal(staff_history$role[staff_history$season == "2013-14"],
               "Team Consultant")
  expect_equal(staff_history$team[staff_history$season == "2019-20"],
               "Buffalo Sabres")
})
