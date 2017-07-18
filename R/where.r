#' Multiple ifelse clauses
#'
#' This is a version of ifelse that allows to check against multiple conditions
#' or alternatively a vectorized version of the `when` function in purrr.
#'
#' @param .x Vector of values to match against
#' @param ... Either formulas with a condition as LHS (optional) and an action
#'   as RHS. Or named arguments that define additional values to match against,
#'   i.e. that can be used in the formulas just like .x.
#'
#' @return The value resulting from the first valid condition *for the given
#'   vector element* is returned. Formulas without LHS or unnamed values are
#'   used as default.
#'
#' @examples
#' where(1:10,
#'       y = 10:1,
#'       .x <= 4 ~ y,
#'       .x <= 6 ~ 11:20,
#'       ~ -.x)
where <-
  function (.x, ...)
  {
    dots <- list(...)
    names <- names(dots)

    ##### check for named arguments #####
    #
    # Named arguments can be used in the matching conditions
    named <-
      if (is.null(names)) {
        rep(FALSE, length(dots))
      } else {
        names != ""
      }

    if (sum(!named) == 0)
      stop("At least one matching condition is needed.", call. = FALSE)

    is.formula <- vapply(
      dots,
      function(dot)
        identical(class(dot), "formula"),
      logical(1L)
    )

    env <- new.env(parent = parent.frame())
    env$.x <- .x

    if (sum(named) > 0)
      for (i in which(named))
        env[[names[i]]] <- dots[[i]]


    ind <- seq_len(length(.x))
    result <- logical(length(.x))

    for (i in which(!named))
    {
      if (length(ind) < 1)
        break

      if (is.formula[i])
      {
        action <- length(dots[[i]])

        if (action == 2)
        {
          result[ind] <-
            rep(
              eval(dots[[i]][[action]], env, env),
              length.out = length(.x)
            )[ind]
          break
        } else {
          trues <- which(eval(dots[[i]][[2]], env, env)[ind])

          if(length(trues) < 1)
            next

          result[ind[trues]] <-
            rep(
              eval(dots[[i]][[action]], env, env),
              length.out = length(.x)
            )[ind[trues]]

          ind <- ind[-trues]
        }

      } else {
        result[ind] <- dots[[i]]
        break
      }
    }

    result
  }
