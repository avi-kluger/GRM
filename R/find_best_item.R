#' Find Best Item at Specific Theta Level
#'
#' @param results A grm_analysis object
#' @param theta Numeric target theta value
#' @param item_text Optional item text labels
#' @return List with best item information
#' @export
find_best_item_at_theta <- function(results, theta, item_text = NULL) {
  requireNamespace("mirt")
  fit <- results$fit
  n_items <- mirt::extract.mirt(fit, 'nitems')
  item_names <- colnames(mirt::extract.mirt(fit, 'data'))
  info <- sapply(1:n_items, function(i) .calc_item_info(fit, i, theta))
  names(info) <- item_names
  best_idx <- which.max(info)
  best_item <- names(info)[best_idx]
  params <- results$parameters$items
  disc <- params[best_item, "a"]
  item_label <- best_item
  if (!is.null(item_text)) {
    if (is.data.frame(item_text)) {
      idx <- which(item_text[, 1] == best_item)
      if (length(idx) > 0) item_label <- as.character(item_text[idx, 2])
    } else if (best_item %in% names(item_text)) {
      item_label <- item_text[best_item]
    }
  }
  return(list(theta = theta, item = best_item, information = info[best_idx], discrimination = disc, text = item_label))
}