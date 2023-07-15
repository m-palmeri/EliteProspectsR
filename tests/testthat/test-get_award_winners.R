
test_that("Vezina Award", {
  awards <- get_award_winners("https://www.eliteprospects.com/awards/nhl?name=NHL%20Best%20Goalie%20(Vezina%20Trophy)")

  expect_equal(awards$player[awards$season == "2017-2018"], "Pekka Rinne")
  expect_equal(awards$player[awards$season == "2012-2013"], "Sergei Bobrovski")
  expect_equal(awards$player[awards$season == "1978-1979"], c("Ken Dryden", "Michel Larocque"))
})


test_that("Allsvenskan Best Junior Award", {
  awards <- get_award_winners("https://www.eliteprospects.com/awards/hockeyallsvenskan?name=HockeyAllsvenskan%20Best%20Junior%20(Guldgallret)")

  expect_equal(awards$player[awards$season == "2022-2023"], "Carl Lindbom")
  expect_equal(awards$player[awards$season == "2017-2018"], "Jacob Olofsson")
  expect_equal(awards$player[awards$season == "2012-2013"], "Filip Forsberg")
})

test_that("Red Tilson Award", {
  awards <- get_award_winners("https://www.eliteprospects.com/awards/ohl?name=OHL%20Most%20Outstanding%20Player%20(Red%20Tilson%20Trophy)")

  expect_equal(awards$player[awards$season == "2019-2020"], "Marco Rossi")
  expect_equal(awards$player[awards$season == "2010-2011"], "Ryan Ellis")
  expect_equal(awards$player[awards$season == "1982-1983"], "Doug Gilmour")
})
