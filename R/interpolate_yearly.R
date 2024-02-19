#' Interpolate values in a dataset, by year.
#'
#' @param data An input dataset. Must contain the columns `year` and `value`.
#' @param ... Other grouping variables. `value` will be interpolated for each
#'   group.
#'
#' @return A dataset with the column `value` interpolated linearly against the
#'   column `year`.
interpolate_yearly <- function(data, ...) {


  data <- dplyr::group_by(data, ...)
  data <- tidyr::complete(data, year = tidyr::full_seq(.data$year, 1))
  data <- dplyr::mutate(
    data,
    value = zoo::na.approx(.data$value, .data$year, na.rm = FALSE)
  )

  dplyr::ungroup(data)
}
