#' @export
print.grm_analysis <- function(x, ...) {
  cat("\n=== GRM Analysis Results ===\n\n")
  cat(sprintf("  Items: %d | Participants: %d\n\n", x$n_items, x$n_participants))
  cat(sprintf("  Omega total: %.3f | Omega hierarchical: %.3f | Alpha: %.3f\n\n", x$reliability$omega, x$reliability$omega_h, x$reliability$alpha))
  cat(sprintf("  Variance explained (1st factor): %.1f%%\n\n", x$efa$Vaccounted[2, 1] * 100))
  if (!is.null(x$model_fit)) {
    cat(sprintf("  M2(df=%d) = %.2f, p = %.4f\n", x$model_fit$df, x$model_fit$M2, x$model_fit$p))
    cat(sprintf("  RMSEA = %.3f [%.3f, %.3f] | CFI = %.3f | TLI = %.3f\n\n", x$model_fit$RMSEA, x$model_fit$RMSEA_5, x$model_fit$RMSEA_95, x$model_fit$CFI, x$model_fit$TLI))
  }
  cat(sprintf("  Empirical reliability: %.3f | Marginal reliability: %.3f\n\n", x$reliability$empirical, x$reliability$marginal))
}
#' @export
print.best_items <- function(x, ...) {
  cat("\n=== Best Performing Items ===\n\n")
  cat("BEST OVERALL DISCRIMINATION\n")
  cat(sprintf("  Item: %s\n  Discrimination: %.3f\n  Text: %s\n\n", x$best_discrimination$item, x$best_discrimination$discrimination, x$best_discrimination$text))
  cat(sprintf("BEST FOR LOW ABILITY (\u03B8 = %.1f)\n  Item: %s\n  Information: %.3f\n  Discrimination: %.3f\n  Text: %s\n\n", x$best_low_theta$theta, x$best_low_theta$item, x$best_low_theta$information, x$best_low_theta$discrimination, x$best_low_theta$text))
  cat(sprintf("BEST FOR HIGH ABILITY (\u03B8 = %.1f)\n  Item: %s\n  Information: %.3f\n  Discrimination: %.3f\n  Text: %s\n\n", x$best_high_theta$theta, x$best_high_theta$item, x$best_high_theta$information, x$best_high_theta$discrimination, x$best_high_theta$text))
  cat("TOP 5 ITEMS BY DISCRIMINATION\n")
  top5 <- head(x$all_discrimination, 5)
  top5_text <- head(x$all_discrimination_with_text, 5)
  for (i in seq_along(top5)) {
    cat(sprintf("  %d. %s (%.3f): %s\n", i, names(top5)[i], top5[i], top5_text[i]))
  }
  cat("\n")
}
