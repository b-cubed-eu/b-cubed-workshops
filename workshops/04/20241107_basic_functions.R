## Basic functions

make_bread <- function(grains, yeast, water, salt) {
  # Code to generate `bread`.
  # The code here can be easy (easy bread recipes do exist)
  # or quite complex (complex bread recipes do exist too)
  bread <- grains + yeast + water + salt
  return(bread)
}

make_focaccia <- function(grains, yeast, water, salt) {
  # Code to generate `focaccia`
  focaccia <- grains + 1.5 * sqrt(yeast) + 0.7 * water^2 + 2 * salt
  return(focaccia)
}

