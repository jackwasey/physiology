#' French to diameter
#'
#' Convert French size of a catheter to diameter in mm
#'
#' @param x Size in French units, or mm
#' @export
french_to_diameter_mm <- function(x)
  x / 3

#' @rdname french_to_diameter_mm
diameter_mm_to_french <- function(x)
  x * 3

generate_med_conv <- function() {
  loadNamespace("xml2")
  loadNamespace("rvest")
  whole_page <- xml2::read_html("http://www.amamanualofstyle.com/page/si-conversion-calculator")
  med_conv <- rvest::html_table(rvest::html_nodes(whole_page, ".siTable"), fill = TRUE, header = TRUE)[[2]][-5]
  saveRDS(med_conv, file = "data/med_conv.rda", compress = "xz")
}
