
test_that("Nashville Predators Picks", {
  draft_picks <- get_team_picks("https://www.eliteprospects.com/team/65/nashville-predators")

  expect_false(any(is.na(draft_picks)))

  expect_length(draft_picks$player[draft_picks$year == 2022], 6)

  expect_setequal(draft_picks$player[draft_picks$year == 2010],
                  c("Austin Watson", "Taylor Aronson", "Patrick Cehlin",
                    "Anthony Bitetto", "David Elsner", "Joonas Rask"))
})


test_that("Niagara Icedogs Picks", {
  draft_picks <- get_team_picks("https://www.eliteprospects.com/team/2231/niagara-icedogs")

  expect_false(any(is.na(draft_picks)))

  expect_length(draft_picks$player[draft_picks$year == 2012], 14)

  expect_true("Logan Brown" %in% draft_picks$player)
  expect_true("Josh Norris" %in% draft_picks$player)
  expect_true("DOugie Hamilton" %in% draft_picks$player)
})
