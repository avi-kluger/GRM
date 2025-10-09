# ============================================================================
# HELPER FUNCTIONS FOR GRM PACKAGE
# ============================================================================

#' Manual Item Information Calculation (Internal)
#'
#' @param fit A mirt model object
#' @param item_num Item number
#' @param theta Theta value
#' @keywords internal
.calc_item_info <- function(fit, item_num, theta) {
  item <- mirt::extract.item(fit, item_num)
  probs <- mirt::probtrace(item, matrix(theta))
  
  h <- 0.001
  probs_plus <- mirt::probtrace(item, matrix(theta + h))
  probs_minus <- mirt::probtrace(item, matrix(theta - h))
  deriv <- (probs_plus - probs_minus) / (2 * h)
  
  info <- sum(deriv^2 / probs, na.rm = TRUE)
  return(info)
}

#' Extract Best Performing Items from GRM Analysis
#'
#' @param results A grm_analysis object from run_grm()
#' @param item_text Optional named vector or dataframe with item text/labels
#' @param low_theta Theta value for "low" ability (default = -2)
#' @param high_theta Theta value for "high" ability (default = 2)
#'
#' @return A list with best items for different criteria
#' @export
extract_best_items <- function(results, 
                               item_text = NULL,
                               low_theta = -2,
                               high_theta = 2) {
  
  params <- results$parameters$items
  discrimination <- params[, "a"]
  names(discrimination) <- rownames(params)
  
  # Best overall discrimination
  best_disc_idx <- which.max(discrimination)
  best_disc_item <- names(discrimination)[best_disc_idx]
  best_disc_value <- discrimination[best_disc_idx]
  
  # Calculate information at specific theta levels
  fit <- results$fit
  n_items <- mirt::extract.mirt(fit, 'nitems')
  item_names <- rownames(params)
  
  # Low theta
  info_low <- sapply(1:n_items, function(i) .calc_item_info(fit, i, low_theta))
  names(info_low) <- item_names
  best_low_idx <- which.max(info_low)
  best_low_item <- names(info_low)[best_low_idx]
  best_low_value <- info_low[best_low_idx]
  
  # High theta
  info_high <- sapply(1:n_items, function(i) .calc_item_info(fit, i, high_theta))
  names(info_high) <- item_names
  best_high_idx <- which.max(info_high)
  best_high_item <- names(info_high)[best_high_idx]
  best_high_value <- info_high[best_high_idx]
  
  # Get item text helper
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
    best_discrimination = list(
      item = best_disc_item,
      discrimination = best_disc_value,
      text = get_item_text(best_disc_item)
    ),
    best_low_theta = list(
      item = best_low_item,
      theta = low_theta,
      information = best_low_value,
      discrimination = discrimination[best_low_item],
      text = get_item_text(best_low_item)
    ),
    best_high_theta = list(
      item = best_high_item,
      theta = high_theta,
      information = best_high_value,
      discrimination = discrimination[best_high_item],
      text = get_item_text(best_high_item)
    ),
    all_discrimination = sort(discrimination, decreasing = TRUE),
    all_discrimination_with_text = sapply(
      names(sort(discrimination, decreasing = TRUE)), 
      get_item_text
    ),
    theta_range = c(low_theta, high_theta)
  )
  
  class(output) <- c("best_items", "list")
  return(output)
}

#' Get Item Information Across Theta Range
#'
#' @param results A grm_analysis object from run_grm()
#' @param theta_range Vector of theta values (default = seq(-3, 3, 0.1))
#'
#' @return Dataframe with information for each item at each theta level
#' @export
get_item_info_range <- function(results, theta_range = seq(-3, 3, 0.1)) {
  
  fit <- results$fit
  n_items <- mirt::extract.mirt(fit, 'nitems')
  item_names <- colnames(mirt::extract.mirt(fit, 'data'))
  
  # Calculate information for each theta value
  info_matrix <- sapply(1:n_items, function(i) {
    sapply(theta_range, function(theta) .calc_item_info(fit, i, theta))
  })
  
  colnames(info_matrix) <- item_names
  
  # Convert to dataframe
  info_df <- as.data.frame(info_matrix)
  info_df$theta <- theta_range
  
  return(info_df)
}

#' Plot Best Items' Information Functions
#'
#' @param results A grm_analysis object from run_grm()
#' @param best_items Output from extract_best_items()
#'
#' @return ggplot object
#' @importFrom ggplot2 ggplot aes geom_line geom_vline labs theme_minimal theme
#' @export
plot_best_items <- function(results, best_items) {
  
  # Get unique best items
  items_to_plot <- unique(c(
    best_items$best_discrimination$item,
    best_items$best_low_theta$item,
    best_items$best_high_theta$item
  ))
  
  fit <- results$fit
  item_names <- colnames(mirt::extract.mirt(fit, 'data'))
  item_indices <- which(item_names %in% items_to_plot)
  
  # Calculate information across theta range
  theta_seq <- seq(-3, 3, 0.1)
  info_data <- do.call(rbind, lapply(item_indices, function(idx) {
    info_vals <- sapply(theta_seq, function(t) .calc_item_info(fit, idx, t))
    data.frame(
      theta = theta_seq,
      information = info_vals,
      item = item_names[idx]
    )
  }))
  
  # Create plot
  p <- ggplot2::ggplot(info_data, ggplot2::aes(x = theta, y = information, color = item)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = best_items$best_low_theta$theta, 
               linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = best_items$best_high_theta$theta, 
               linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(
      title = "Information Functions of Best Performing Items",
      x = "Theta",
      y = "Information",
      color = "Item"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

#' Extract item labels from data column attributes
#'
#' @param data A dataframe with item columns that may have "label" attributes
#' @return A named vector of item labels
#' @export
extract_item_labels <- function(data) {
  item_labels <- sapply(names(data), function(col) {
    label <- attr(data[[col]], "label")
    if (is.null(label) || label == "") col else label
  })
  return(item_labels)
}

#' Calculate empirical reliability
#'
#' @param theta Matrix with factor scores and SEs
#' @return Empirical reliability estimate
#' @importFrom stats var
#' @export
empirical_rxx <- function(theta) {
  if (is.null(theta) || ncol(theta) < 2) return(NA)
  
  tryCatch({
    scores <- theta[, 1]
    ses <- theta[, 2]
    
    # Empirical reliability = 1 - (average SE^2 / observed variance)
    emp_rel <- 1 - mean(ses^2, na.rm = TRUE) / stats::var(scores, na.rm = TRUE)
    return(max(0, emp_rel)) # Ensure non-negative
  }, error = function(e) {
    warning("Could not calculate empirical reliability: ", e$message)
    return(NA)
  })
}

#' Calculate marginal reliability
#'
#' @param fit A mirt model object
#' @return Marginal reliability estimate
#' @export
marginal_rxx <- function(fit) {
  tryCatch({
    mirt::marginal_rxx(fit)
  }, error = function(e) {
    warning("Could not calculate marginal reliability: ", e$message)
    return(NA)
  })
}

# ============================================================================
# PRINT METHODS
# ============================================================================

#' Print method for grm_analysis objects
#' @param x A grm_analysis object
#' @param ... Additional arguments
#' @method print grm_analysis
#' @export
print.grm_analysis <- function(x, ...) {
  cat("\n=== GRM Analysis Results ===\n\n")
  
  cat("Sample:\n")
  cat(sprintf("  Items: %d | Participants: %d\n\n", 
              x$n_items, x$n_participants))
  
  cat("Factor Structure:\n")
  cat(sprintf("  Omega total: %.3f\n", x$reliability$omega))
  cat(sprintf("  Omega hierarchical: %.3f\n", x$reliability$omega_h))
  if (x$reliability$omega_h > 0.70) {
    cat("  [OK] Strong general factor - unidimensional assumption supported\n\n")
  } else if (x$reliability$omega_h > 0.50) {
    cat("  [MODERATE] Moderate general factor - consider multidimensional model\n\n")
  } else {
    cat("  [WARNING] Weak general factor - multidimensional structure likely\n\n")
  }

  cat("Dimensionality:\n")
  if (!is.null(x$efa$Vaccounted)) {
    cat(sprintf("  Variance explained: %.1f%%\n", 
                x$efa$Vaccounted[2, 1] * 100))
  }
  cat(sprintf("  Parallel analysis suggests: %d factor(s)\n\n", 
              x$parallel_analysis$nfact))
  
  if (!is.null(x$model_fit)) {
    cat("Model Fit:\n")
    cat(sprintf("  M2(df=%d) = %.2f, p = %.4f\n",
                x$model_fit$df, x$model_fit$M2, x$model_fit$p))
    cat(sprintf("  RMSEA = %.3f [%.3f, %.3f]\n",
                x$model_fit$RMSEA, x$model_fit$RMSEA_5, x$model_fit$RMSEA_95))
    cat(sprintf("  CFI = %.3f | TLI = %.3f\n\n",
                x$model_fit$CFI, x$model_fit$TLI))
  }
  
  cat("Reliability:\n")
  cat(sprintf("  Empirical: %.3f | Marginal: %.3f\n",
              x$reliability$empirical, x$reliability$marginal))
  cat(sprintf("  Alpha: %.3f\n\n", x$reliability$alpha))
  
  cat("\n")
}

#' Print method for best_items objects
#' @param x A best_items object
#' @param ... Additional arguments
#' @method print best_items
#' @export
print.best_items <- function(x, ...) {
  cat("\n=== Best Performing Items ===\n\n")
  
  cat("BEST OVERALL DISCRIMINATION\n")
  cat(sprintf("  Item: %s\n", x$best_discrimination$item))
  cat(sprintf("  Discrimination (a): %.3f\n", x$best_discrimination$discrimination))
  cat(sprintf("  Text: %s\n\n", x$best_discrimination$text))
  
  cat(sprintf("BEST FOR LOW ABILITY (theta = %.1f)\n", x$best_low_theta$theta))
  cat(sprintf("  Item: %s\n", x$best_low_theta$item))
  cat(sprintf("  Information: %.3f\n", x$best_low_theta$information))
  cat(sprintf("  Discrimination (a): %.3f\n", x$best_low_theta$discrimination))
  cat(sprintf("  Text: %s\n\n", x$best_low_theta$text))
  
  cat(sprintf("BEST FOR HIGH ABILITY (theta = %.1f)\n", x$best_high_theta$theta))
  cat(sprintf("  Item: %s\n", x$best_high_theta$item))
  cat(sprintf("  Information: %.3f\n", x$best_high_theta$information))
  cat(sprintf("  Discrimination (a): %.3f\n", x$best_high_theta$discrimination))
  cat(sprintf("  Text: %s\n\n", x$best_high_theta$text))
  
  cat("TOP 5 ITEMS BY DISCRIMINATION\n")
  top5 <- head(x$all_discrimination, 5)
  top5_text <- head(x$all_discrimination_with_text, 5)
  for (i in 1:length(top5)) {
    cat(sprintf("  %d. %s (%.3f): %s\n", i, names(top5)[i], top5[i], top5_text[i]))
  }
  cat("\n")
}