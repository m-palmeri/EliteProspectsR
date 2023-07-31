
### league_names_crosswalk             -----

#' Eliteprospects league list
#'
#'
#'
#' @format ## `league_names_crosswalk`
#' A data frame with 482 rows and 3 columns:
#' \describe{
#'   \item{full_name}{full name of the league (National Hockey League, etc.)}
#'   \item{abbreviation}{abbreviation of the league (NHL, etc.)}
#'   \item{link_component}{link embedded variable to demark particular league}
#' }
#' @source <https://www.eliteprospects.com/leagues>
"league_names_crosswalk"


### draft_names_crosswalk              -----

#' Eliteprospects draft list
#'
#' A crosswalk of league drafts to the link suffixes for easy website creation
#' and lookup
#'
#' @format ## `draft_names_crosswalk`
#' A data frame with 41 rows and 2 columns:
#' \describe{
#'   \item{draft_name}{official name of the draft}
#'   \item{link_component}{link embedded variable to demark particular draft}
#' }
#' @source <https://www.eliteprospects.com/draft/nhl-entry-draft>
"draft_names_crosswalk"
