#' Launch pedatric anesthesia calculator shiny application
#' @importFrom shiny runApp
#' @export
peds_anes_calc <- function(...) {
  pac_dir <- system.file("peds_anes_calc",
                          package = "physiology",
                          mustWork = TRUE)
  shiny::runApp(pac_dir, display.mode = "normal")
}
