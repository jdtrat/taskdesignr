#' Generate presentation assets
#'
#' @keywords internal
#' @noRd
generate_presentation_assets <- function() {
  if (!dir.exists(here::here("inst"))) {
    dir.create(here::here("inst"))
    dir.create(here::here("inst/presentation_assets"))
  }
  if (dir.exists(here::here("inst")) && !dir.exists(here::here("inst/presentation_assets"))) {
    dir.create(here::here("inst/presentation_assets"))
  }

  params <- list(.x = paste0("Presenting ", seq(1:5), " ",
                             paste0(sample(LETTERS, 3), collapse = "")),
                 .y = c("#63B8FF", "#FF6A6A", "#E9967A", "#4EEE94", "#EEDC82"),
                 .z = seq(1:5))

  purrr::pwalk(.l = params, function(.x, .y, .z) {
    magick::image_blank(width = "400", height = "400", color = .y) %>%
      magick::image_annotate(text = .x, gravity = "center", size = "48") %>%
      magick::image_write(path = here::here(paste0("inst/presentation_assets/present_", .z, ".png")), format = "png")
  })

}

.onLoad <- function(libname, pkgname) {

  if (!dir.exists(here::here("inst/presentation_assets"))) {
    generate_presentation_assets()
  }

  shiny::addResourcePath(prefix = "presentation_assets",
                  directoryPath = system.file(
                    "presentation_assets",
                    package = "taskdesignr"
                  )
  )
}


.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath(prefix = "presentation_assets")
}
