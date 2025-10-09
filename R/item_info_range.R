#' Get Item Information Across Theta Range
#'
#' @param results A grm_analysis object
#' @param theta_range Numeric vector of theta values (default seq(-3,3,0.1))
#' @return Data.frame with information for each item at each theta level
#' @export
get_item_info_range <- function(results, theta_range = seq(-3, 3, 0.1)) {
  requireNamespace("mirt")
  fit <- results$fit
  n_items <- mirt::extract.mirt(fit, 'nitems')
  item_names <- colnames(mirt::extract.mirt(fit, 'data'))
  info_matrix <- sapply(1:n_items, function(i) {
    sapply(theta_range, function(theta) .calc_item_info(fit, i, theta))
  })
  colnames(info_matrix) <- item_names
  info_df <- as.data.frame(info_matrix)
  info_df$theta <- theta_range
  return(info_df)
}
