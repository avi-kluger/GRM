#' Extract Best Performing Items from GRM Analysis
#'
#' @param results A grm_analysis object from run_grm()
#' @param item_text Optional named vector or data.frame with item text/labels
#' @param low_theta Theta value for "low" ability (default = -2)
#' @param high_theta Theta value for "high" ability (default = 2)
#' @return A list of class 'best_items' with best items by criteria
#' @export
extract_best_items <- function(results, item_text = NULL, low_theta = -2, high_theta = 2) {
  requireNamespace("mirt")
  params <- results$parameters$items
  discrimination <- params[, "a"]
  names(discrimination) <- rownames(params)
  best_disc_idx <- which.max(discrimination)
  best_disc_item <- names(discrimination)[best_disc_idx]
  best_disc_value <- discrimination[best_disc_idx]
  fit <- results$fit
  n_items <- mirt::extract.mirt(fit, 'nitems')
  item_names <- rownames(params)
  info_low <- sapply(1:n_items, function(i) .calc_item_info(fit, i, low_theta))
  names(info_low) <- item_names
  best_low_idx <- which.max(info_low)
  best_low_item <- names(info_low)[best_low_idx]
  best_low_value <- info_low[best_low_idx]
  info_high <- sapply(1:n_items, function(i) .calc_item_info(fit, i, high_theta))
  names(info_high) <- item_names
  best_high_idx <- which.max(info_high)
  best_high_item <- names(info_high)[best_high_idx]
  best_high_value <- info_high[best_high_idx]
  get_item_text <- function(item_name) {
    if (is.null(item_text)) return(item_name)
    if (is.data.frame(item_text)) {
      idx <- which(item_text[, 1] == item_name)
      if (length(idx) > 0) return(as.character(item_text[idx, 2]))
    } else if (is.vector(item_text) && item_name %in% names(item_text)) {
      return(item_text[item_name])
    }
    return(item_name)
  }
  output <- list(
    best_discrimination = list(item = best_disc_item, discrimination = best_disc_value, text = get_item_text(best_disc_item)),
    best_low_theta = list(item = best_low_item, theta = low_theta, information = best_low_value, discrimination = discrimination[best_low_item], text = get_item_text(best_low_item)),
    best_high_theta = list(item = best_high_item, theta = high_theta, information = best_high_value, discrimination = discrimination[best_high_item], text = get_item_text(best_high_item)),
    all_discrimination = sort(discrimination, decreasing = TRUE),
    all_discrimination_with_text = sapply(names(sort(discrimination, decreasing = TRUE)), get_item_text),
    theta_range = c(low_theta, high_theta)
  )
  class(output) <- c("best_items", "list")
  return(output)
}