#' Plot Best Items' Information Functions
#'
#' @param results A grm_analysis object
#' @param best_items Output from extract_best_items()
#' @return ggplot object
#' @importFrom ggplot2 ggplot aes geom_line geom_vline labs theme_minimal theme
#' @export
plot_best_items <- function(results, best_items) {
  requireNamespace("ggplot2")
  requireNamespace("mirt")
  items_to_plot <- unique(c(best_items$best_discrimination$item, best_items$best_low_theta$item, best_items$best_high_theta$item))
  fit <- results$fit
  item_names <- colnames(mirt::extract.mirt(fit, 'data'))
  item_indices <- which(item_names %in% items_to_plot)
  theta_seq <- seq(-3, 3, 0.1)
  info_data <- do.call(rbind, lapply(item_indices, function(idx) {
    info_vals <- sapply(theta_seq, function(t) .calc_item_info(fit, idx, t))
    data.frame(theta = theta_seq, information = info_vals, item = item_names[idx])
  }))
  p <- ggplot2::ggplot(info_data, ggplot2::aes(x = theta, y = information, color = item)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_vline(xintercept = best_items$best_low_theta$theta, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = best_items$best_high_theta$theta, linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(title = "Information Functions of Best Performing Items", x = "Theta (\u03B8)", y = "Information", color = "Item") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
  return(p)
}