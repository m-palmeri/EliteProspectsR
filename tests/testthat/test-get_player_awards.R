
test_that("Connor McDavid Awards", {
  awards <- get_player_awards("https://www.eliteprospects.com/player/183442/connor-mcdavid")

  expect_equal(awards[awards$season == "2016-2017", "award"],
               c("NHL All-Star Game", "NHL First All-Star Team",
                 "NHL Most Assists", "NHL Most Points", "NHL Most Valuable Player",
                 "NHL Most Valuable Player Finalist", "NHL MVP Selected by NHLPA"))

  expect_equal(awards[awards$season == "2019-2020", "award"],
               c("NHL All-Star Game"))

})


test_that("Nicholas West Awards", {
  awards <- get_player_awards("https://www.eliteprospects.com/player/289172/nicholas-west")

  expect_equal(names(awards), c("season", "award", "award_link"))
  expect_equal(nrow(awards), 0)
})


test_that("Filip Forsberg Awards", {
  awards <- get_player_awards("https://www.eliteprospects.com/player/29626/filip-forsberg")

  expect_equal(awards[awards$season == "2009-2010", "award"],
               c("J20 SM Gold Medal"))

  expect_equal(awards[awards$season == "2013-2014", "award"],
               c("U20 WJC All-Star Team", "U20 WJC Best Forward",
               "U20 WJC Most Valuable Player", "U20 WJC Silver Medal"))
})


test_that("John Blum Awards", {
  awards <- get_player_awards("https://www.eliteprospects.com/player/68038/john-blum")

  comp <- load_test_rds("john_blum_awards.rds")

  expect_equal(awards, comp)
})


test_that("Pekka Rinne Awards", {
  awards <- get_player_awards("https://www.eliteprospects.com/player/4282/pekka-rinne")

  expect_equal(awards[awards$season == "2005-2006", "award"],
               c("AHL All-Star Game"))

  expect_equal(awards[awards$season == "2013-2014", "award"],
               c("World Championship All-Star Team", "World Championship Most Valuable Player",
               "World Championship Silver Medal", "World Championship Top 3 Player on Team"))

  expect_equal(awards[awards$season == "2017-2018", "award"],
               c("Finnish Player of the Year", "NHL All-Star Game", "NHL Best Goalie",
               "NHL Best Goalie Finalist", "NHL First All-Star Team", "NHL Most Shutouts"))
})


test_that("Tommy Green Awards", {
  awards <- get_player_awards("https://www.eliteprospects.com/player/96540/tommy-green")

  expect_equal(names(awards), c("season", "award", "award_link"))
  expect_equal(nrow(awards), 0)
})
