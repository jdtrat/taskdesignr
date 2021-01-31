#' Define a task run
#'
#' @param trial_temps The trial templates
#' @param num_trials The number of trials in a run
#'
#' @return
#' @export
#'
#' @examples
multiply_trials <- function(trial_temps, num_trials) {

  purrr::set_names(1:num_trials, ~ paste0("trial_", .x)) %>%
    purrr::map(~trial_temps)

}

add_trial_id <- function() {

}

add_screen_id <- function() {

}

#
# mlps <- multiply_trials(trial_temp, 5)
#
#
# un_mlps <- unlist(mlps)
#
# un_mlps$trial_1.screen_1.screen
#
#
# un_mlps[[4]]
# names(un_mlps)[1]
#
#
#
# ids <- names(un_mlps)
#
#
# screen_html <- function(trial_list, id_list, trial_number) {
#
#   shiny::tagList(
#     shiny::div(
#       class = class,
#       id = id,
#       body
#     )
#   )
#
# }
#
# screen_html()
#
#
#
# shiny::tagList(shiny::div(class = eval(unlisted_run$trial_1.screen_1.screen_type),
#                           id = names(unlisted_run$trial_1.screen_1.screen),
#                           eval(unlisted_run$trial_1.screen_2.screen)))
#
#
#
#
# # purrr::map(b, ~ map(.x, ~tagList(div(class = .x$screen_type, .x))))
# #
# #
# # map(run ~ map(.x, ~.x$screen_type))
# #
# #
# # map(b, ~ map2(.x, .y = 1:length(.x), ~tagList(div(class = .y, .x))))
# #
# #
# # purrr::map(run, ~ map(.x, ~ map(.x$screen_type, ~ print(.x))))
# #
# #
# #
# #
# # #
# # #
# #
# #
# # screen_names <- purrr::map_chr(trial_temp, ~.x$screen_type)
# #
# # id_grid <- expand.grid(1:3, screen_names)
# #
# # ids <- paste("trial",
# #              id_grid$Var1,
# #              id_grid$Var2,
# #              sep = "-")
# # # Create IDs/classes for each trial/presentation type.
# # # Class should be the screen_type and id should be the class-trial number,
# # # e.g. id = "trial-1-choice".
# # purrr::map(b, ~ purrr::map2(.x, .y = 1:length(.x), ~shiny::tagList(shiny::div(class = .y, .x))))
# #
# # #
# # #get the UI code for each screen in each trial
# # b <- purrr::map(run, ~ purrr::map(.x, ~.x$screen))
# # #
# # map(b, ~ pmap(list(.x = rep(.x,3), .y = ids, .z = rep(screen_names, 3)), ~tagList(div(class = .z,
# #                                                                           id = .y,
# #                                                                           .x))))
# #
# #
# #
# #testing cross stuff
# #
# # b[[1]][1]
# #
# # map2(b, .y = 1:3, ~print(.x[[.y]]))
# #
# #
# # for (i in 1:3) {
# #   map(b, ~print(paste("Number", i, .x[[i]])))
# # }
# #
# # pmap(list(.x = b,
# #           .y = ids,))
# #
# #
# #
# #
# #
# #
# # params <- NULL
# # params$presentation <- fs::dir_ls(here::here("inst/presentation_assets/"))
# #
# # purrr::set_names(1:num_trials, ~ paste0("trial_", .x)) %>%
# #   purrr::map(~trial_temps)
# #
