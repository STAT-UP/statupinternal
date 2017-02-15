#' Install (If Necessary) and Load Multiple Packages at Once
#'
#' Convenience function that for a list of packages looks if they are installed,
#' installs them if they are not and then loads them.
#'
#' @param ... Packages. Quoted or unquoted.
#' @param .packages Character vector or list of packages. Mainly useful for
#'   automated use in functions. Packages in .packages will be loaded after
#'   those in ...
#'
#' @return TRUE if packages were installed, FALSE otherwise. There are no other
#'   sanity checks at the moment.
#'
#' @examples
#' loadorinstall(dplyr, data.table, magrittr)
#'
#' @export
load_or_install <-
  function(..., .packages = NULL)
  {
    if(!is.list(.packages))
      .packages <- as.list(.packages)

    .packages <-
      c(eval(substitute(alist(...))), .packages)

    install.counter <- 0

    for(pkg in .packages)
    {
      if(!is.character(pkg))
        pkg <- deparse(pkg)

      if(!require(pkg, character.only = TRUE))
      {
        utils::install.packages(pkg)
        install.counter <- install.counter + 1
      }

      library(pkg, character.only = TRUE)
    }

    invisible(install.counter > 0)
  }
