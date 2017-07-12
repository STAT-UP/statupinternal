#' Restrict Variables to a Local Block
#'
#' More to come
#'
#' @param ... Object definitions and the main expression.
#' @param .expr Expression to evaluate. By default the last element of ...
#' @param .exports List of objects to export. By default taken from everything but the last element of ...
#' @param .conditions List of conditions to check. Usually defined in ...
#' @param .env Environment. Where to provide the objects. By default the
#'             environment where the function was executed.
#' @param .quiet Logical. Should the exports be printed to stdout?
#'
#' @examples
#'
#' # This provides X and Y. The variables a and beta are not exported
#' # as they are not specified in ...
#' provide(
#'   X = list(dim(X) == c(100,4),
#'            is.numeric(X)),
#'   Y,
#'   NROW(Y)==NROW(X),
#'   {
#'     a <- 4
#'
#'     X <- matrix(rnorm(100*a), ncol=a)
#'     beta <- rnorm(a)
#'
#'     Y <- X %*% beta + rnorm(100)
#'   }
#' )
#'
#' @export
provide <-
  function(...,
           .expr = NULL,
           .exports = NULL,
           .conditions = NULL,
           .env = parent.frame(),
           .quiet = FALSE)
  {
    env <- new.env()
    parent <- .env

    ##### Find and Evaluate .expr #####
    .dots <- match.call(expand.dots = FALSE)$...

    stopifnot(!is.null(.expr) || length(.dots) > 0)

    if(is.null(.expr))
    {
      .expr <- .dots[[length(.dots)]]
      .dots <- .dots[-length(.dots)]
    }

    eval(.expr, env, parent)

    ##### Find .conditions and .exports #####
    if(length(.dots) > 0)
    {
      .conditions <-
        c(
          .conditions,
          Filter(x = .dots,
                 f = is.call)
        )

      names <-
        Filter(x = .dots,
               f = function(x)
                 is.character(x) || is.symbol(x)) %>%
        lapply(as.character) %>%
        unlist

      .exports <-
        c(
          names,
          names(.conditions)
        )
    }


    .exports <- unique(.exports)
    .exports <- .exports[.exports != ""]

    if(length(.exports) == 0)
      .exports <- ls(env)

    stopifnot(is.character(.exports))


    ##### Test .conditions #####

    if(length(.conditions) > 0)
    {
      # conditions can be iterative so we need a loop
      for(i in seq_along(.conditions))
      {
        if(isTRUE(.conditions[[i]][[1]] == as.symbol("list")))
        {
          eval(call("do.call",
                    what = "stopifnot",
                    args = .conditions[[i]]),
               env)
        } else {
          eval(call("stopifnot",
                    .conditions[[i]]),
               env)
        }
      }
    }

    ##### Export .exports #####

    lapply(
      .exports,
      function(var)
        assign(var,
               get(var, envir = env),
               envir = parent)
    )

    if(!isTRUE(.quiet))
      message("Providing variables:\n",
              paste(sort(.exports),
                    collapse = ", "))

    ##### Return Vector with Names of Exported Objects #####
    invisible(list(provided = .exports,
                   all.objects = ls(env)))
  }



