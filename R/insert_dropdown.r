#' Insert the Necessary Code to Turn Tabs into Dropdowns
#'
#' RMarkdown allows you to turn subsections into tabs by putting `{.tabset}`
#' behind the title of the parent section. With this code you can write
#' `{.tabset .tabset-dropdown}` to get a dropdown menus instead of tabs.
#'
#' To use this, either create a chunk (with `results = "asis"`) that calls
#' `insert_dropdown_html`, or put `insert_dropdown_location()` as `after_body`
#' include in the RMarkdown.
#'
#' \preformatted{```{r, results = 'asis'}
#' statupinternal::insert_dropdown_html()
#' ```}
#'
#' or
#'
#' \preformatted{---
#' title: Dropdowns
#' output:
#'   html_document:
#'     includes:
#'       after_body: `statupinternal::insert_dropdown_location()`
#' ---
#'
#' # Header1 {.tabset .tabset-dropdown}
#'
#' ## Dropdown Entry 1
#' ...
#'
#' ## Dropdown Entry 2
#' ...
#' }
#'
#' @export
insert_dropdown_location <- function() {
  x <- system.file("data", "html_document", "tabset-dropdown.html", package = "statupinternal")

  stopifnot(stringr::str_length(x) > 0)

  x
}

#' @rdname insert_dropdown_location
#' @import magrittr
#' @export
insert_dropdown_html <- function() {
  x <- system.file("data", "html_document", "tabset-dropdown.html", package = "statupinternal")

  stopifnot(stringr::str_length(x) > 0)

  x %>%
    readLines(encoding = "UTF-8") %>%
    cat(sep = "\n")
}
