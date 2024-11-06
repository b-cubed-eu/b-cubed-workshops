#' Make bread
#'
#' @param grains Amount of grains (numeric).
#' @param yeast Amount of yeast (numeric).
#' @param water Amount of water (numeric).
#' @param salt Amount of salt (numeric).
#'
#' @return Amount of bread.
#' @export
#'
#' @examples
#' make_bread(
#'   grains = 1,
#'   yeast = 2,
#'   water = 3,
#'   salt = 4
#' )
make_bread <- function(grains, yeast, water, salt) {
  # Code to generate `bread`.
  # The code here can be easy (easy bread recipes do exist)
  # or quite complex (complex bread recipes do exist too)
  bread <- grains + yeast + water + salt
  return(bread)
}


#' Make focaccia
#'
#' @param grains Amount of grains (numeric).
#' @param yeast Amount of yeast (numeric).
#' @param water Amount of water (numeric).
#' @param salt Aùount of salt (numeric).
#'
#' @return Amount of focaccia (numeric).
#' @export
#'
#' @examples
#' make_focaccia(
#'   grains = 1,
#'   yeast = 2,
#'   water = 3,
#'   salt = 4
#' )
make_focaccia <- function(grains, yeast, water, salt) {
  # Code to generate `focaccia`
  focaccia <- grains + 1.5 * sqrt(yeast) + 0.7 * water^2 + 2 * salt
  return(focaccia)
}
