#' Insert the Necessary Code to Turn Tabs into Dropdowns
#'
#' RMarkdown allows you to turn subsections into tabs by putting `{.tabset}`
#' behind the title of the parent section. With this code you can write
#' `{.tabset .tabset-dropdown}` to get a dropdown menus instead of tabs.
#'
#' To use this, either create a chunk (with `results = "asis"`) that calls
#' `insert_dropdown_html`, or put the value of `insert_dropdown_location()` as
#' `after_body` include in the RMarkdown.
#'
#' \preformatted{---
#' title: Dropdowns
#' output: html_document
#' ---
#'
#' # Header1 {.tabset .tabset-dropdown}
#'
#' ## Dropdown Entry 1
#' ...
#'
#' ## Dropdown Entry 2
#' ...
#'
#' ```{r, results = 'asis', echo = FALSE}
#' statupinternal::insert_dropdown_html()
#' ```
#' }
#'
#' @import magrittr
#' @export
insert_dropdown_html <- function() {
  x <- system.file("data", "html_document", "tabset-dropdown.html", package = "statupinternal")

  stopifnot(stringr::str_length(x) > 0)

  x %>%
    readLines(encoding = "UTF-8") %>%
    cat(sep = "\n")
}

#' @rdname insert_dropdown_html
#' @export
insert_dropdown_location <- function() {
  x <- system.file("data", "html_document", "tabset-dropdown.html", package = "statupinternal")

  stopifnot(stringr::str_length(x) > 0)

  x
}
