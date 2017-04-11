#' @import ensurer
is.scalar <-
  function(.x)
    length(.x) == 1

#' @import ensurer
type_numeric_scalar <-
  ensurer::ensures_that(is.numeric,
               is.scalar)

type_logical_scalar <-
  ensures_that(is.logical,
               is.scalar)

type_character_scalar <-
  ensures_that(is.character,
               is.scalar)

type_integer_scalar <-
  ensures_that(is.integer,
               is.scalar)