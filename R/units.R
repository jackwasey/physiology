#' French to diameter
#'
#' Convert French size of a catheter to diameter in mm. Currently accepts or
#' returns non-integer French values
#'
#' @param x Size in French units, or mm
#' @export
french_to_diameter_mm <- function(x) {
  stopifnot(x >= 0)
  x * 3
}

#' @rdname french_to_diameter_mm
diameter_mm_to_french <- function(x) {
  stopifnot(x >= 0)
  x / 3
}

generate_med_conv <- function() {
  loadNamespace("xml2")
  loadNamespace("rvest")
  whole_page <- xml2::read_html(encoding = "latin1",
    "http://www.amamanualofstyle.com/page/si-conversion-calculator")
  med_conv <- rvest::html_table(
    rvest::html_nodes(whole_page, ".siTable"),
    fill = TRUE, header = TRUE)[[2]][-5]
  Encoding(med_conv$Analyte) <- "UTF-8"
  med_conv <- lapply(med_conv, function(x) if (x == "" || x == " ") NA else x)
  med_conv <- as.data.frame(med_conv[-c(7, 9)]) # do i want factors?
  names(med_conv) <- c("analyte", "specimen", "ref.conventional",
                       "unit.conventional", "factor", "ref.si",
                       "unit.si")
  med_conv <- iconv(med_conv, from = "latin1", to = "UTF-8")
  save(med_conv, file = "data/med_conv.rda", compress = "xz")
}

#' Conversion factors and reference ranges for lab values
#'
#' A few hundred lab values with _Conventional_ (US) and SI units, with a
#' scaling factor and reference ranges.
#'
#' @format data frame in a tidy format, with the following columns, with example
#'  values:
#'  - Analyte _Carbon dioxide, _Pco2_
#'  - Specimen _Arterial blood_
#'  - Reference Range, Conventional Unit _35-45_
#'  - Conventional Unit _mm Hg_
#'  - Conversion Factor (Multiply by) _0.133_
#'  - Reference Range, SI Unit _47.-5.9_
#'  - SI Unit _kPa_
#'  @source \url{http://www.amamanualofstyle.com/page/si-conversion-calculator}
"med_conv"
