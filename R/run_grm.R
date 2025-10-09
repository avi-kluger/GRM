#' Perform Complete Graded Response Model Analysis
#'
#' A comprehensive wrapper function that fits a Graded Response Model (GRM) using
#' the mirt package and conducts a full psychometric analysis including
#' dimensionality assessment, reliability analysis, model fit evaluation,
#' and diagnostic plotting with detailed item analysis and recommendations.
#'
#' @param data A data.frame or matrix containing polytomous item responses
#' @param n_factors Integer specifying the number of factors to extract (default = 1)
#' @param save_plots Logical indicating whether to save diagnostic plots (default = TRUE)
#' @param output_dir Character string specifying the directory for plots (default = "grm_output")
#' @param display_plots Logical indicating whether to display plots (default = TRUE)
#' @param auto_display Logical indicating whether to display results (default = TRUE)
#' @return A list of class 'grm_analysis' containing all analysis results
#' @export
#' @importFrom stats cor var quantile
#' @importFrom utils capture.output
#' @importFrom grDevices png dev.off
#' @importFrom mirt mirt coef M2 itemfit residuals fscores extract.mirt marginal_rxx
#' @importFrom graphics abline plot
#' @import psych
#' @import ggmirt
#' @importFrom ggplot2 ggplot aes geom_line geom_vline labs theme_minimal theme element_text scale_color_manual scale_x_continuous annotate ggsave
run_grm <- function(data, 
                    n_factors = 1, 
                    save_plots = TRUE, 
                    output_dir = "grm_output",
                    display_plots = TRUE,
                    auto_display = TRUE) {
  
  # Load required packages quietly
  suppressPackageStartupMessages({
    requireNamespace("mirt")
    requireNamespace("psych")
    requireNamespace("ggmirt")
    requireNamespace("ggplot2")
  })
  
  if (save_plots) dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  results <- list(
    n_items = ncol(data),
    n_participants = nrow(data)
  )
  
  message("\n=== GRM Analysis ===")
  message(sprintf("Items: %d | Participants: %d", ncol(data), nrow(data)))
  
  # EXTRACT ITEM LABELS FIRST AND DISPLAY
  results$item_labels <- extract_item_labels(data)
  
  if (auto_display && !is.null(results$item_labels)) {
    message("\n--- ITEM LABELS ---")
    for (i in seq_along(results$item_labels)) {
      message(sprintf("%d. %s: %s", i, names(results$item_labels)[i], results$item_labels[i]))
    }
    message("")
  }
  
  # OMEGA RELIABILITY & FACTOR STRUCTURE - ALWAYS DEFAULT (1 factor)
  message("0. Initial Reliability & Factor Structure Check")
  
  # Always run default omega (1 factor) as requested
  omega_results <- tryCatch({
    psych::omega(data, plot = FALSE)  # Default omega only
  }, error = function(e) {
    warning("Omega analysis failed: ", e$message)
    list(omega.tot = NA, omega_h = NA, alpha = NA)
  })
  
  results$omega_detailed <- omega_results
  
  message(sprintf("   Omega total: %.3f", omega_results$omega.tot))
  message(sprintf("   Omega hierarchical: %.3f", omega_results$omega_h))
  message(sprintf("   Alpha: %.3f", omega_results$alpha))
  
  # Check if multidimensional structure is likely based on omega_h
  multidimensional_likely <- !is.na(omega_results$omega_h) && omega_results$omega_h <= 0.50
  
  if (multidimensional_likely) {
    message("\n\033[31m*** RED WARNING: MULTIDIMENSIONAL STRUCTURE LIKELY ***\033[39m")
    message("\033[31mOmega hierarchical <= 0.50 suggests multidimensional structure.\033[39m")
    message("\033[31mItem analyses below should be interpreted with CAUTION!\033[39m")
    message("\033[31mConsider running multidimensional models for proper analysis.\033[39m\n")
  }
  
  # Interpret omega hierarchical
  if (!is.na(omega_results$omega_h)) {
    if (omega_results$omega_h > 0.70) {
      message("   [OK] Strong general factor (omega_h > 0.70) - unidimensional assumption supported\n")
    } else if (omega_results$omega_h > 0.50) {
      message("   [MODERATE] Moderate general factor (omega_h > 0.50) - consider multidimensional model\n")
    } else {
      message("   [WARNING] Weak general factor (omega_h <= 0.50) - multidimensional structure likely\n")
    }
  }
  
  # DIMENSIONALITY
  message("1. Dimensionality Assessment")
  cor_matrix <- stats::cor(data, method = "spearman", use = "pairwise.complete.obs")
  efa <- psych::fa(cor_matrix, nfactors = n_factors, fm = "minres")
  results$efa <- efa
  
  if (!is.null(efa$Vaccounted)) {
    message(sprintf("   Variance explained: %.1f%%", efa$Vaccounted[2, 1] * 100))
  }
  
  pa <- suppressMessages(psych::fa.parallel(data, fm = "minres", fa = "pc", plot = FALSE))
  results$parallel_analysis <- pa
  message(sprintf("   Suggested factors: %d\n", pa$nfact))
  
  # GRM MODEL
  message("2. Fitting GRM Model")
  fit <- mirt::mirt(data = data, model = n_factors, itemtype = "graded", 
                    SE = TRUE, verbose = FALSE)
  results$fit <- fit
  results$parameters <- mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)
  message("   Model fitted successfully\n")
  
  # MODEL FIT
  message("3. Model Fit")
  model_fit <- tryCatch(mirt::M2(fit, type = "C2"), error = function(e) { warning("M2 failed: ", e$message); NULL })
  results$model_fit <- model_fit
  
  if (!is.null(model_fit)) {
    message(sprintf("   M2 = %.2f, df = %d, p = %.4f",
                    model_fit$M2, model_fit$df, model_fit$p))
    message(sprintf("   RMSEA = %.3f [%.3f, %.3f]",
                    model_fit$RMSEA, model_fit$RMSEA_5, model_fit$RMSEA_95))
    message(sprintf("   CFI = %.3f | TLI = %.3f\n",
                    model_fit$CFI, model_fit$TLI))
  }
  
  # ITEM FIT
  item_fit <- tryCatch(mirt::itemfit(fit), error = function(e) { warning(e$message); NULL })
  results$item_fit <- item_fit
  if (!is.null(item_fit)) {
    n_misfit <- sum(item_fit$p.S_X2 < 0.05, na.rm = TRUE)
    message(sprintf("   Items with poor fit: %d/%d\n", n_misfit, ncol(data)))
  }
  
  # LOCAL DEPENDENCY
  message("4. Local Dependency")
  q3 <- tryCatch(mirt::residuals(fit, type = "Q3"), error = function(e) { warning(e$message); NULL })
  results$q3 <- q3
  if (!is.null(q3)) {
    n_high <- sum(abs(q3[upper.tri(q3)]) > 0.2, na.rm = TRUE)
    message(sprintf("   Item pairs with |Q3| > 0.2: %d\n", n_high))
  }
  
  # RELIABILITY
  message("5. IRT-based Reliability")
  theta <- mirt::fscores(fit, full.scores.SE = TRUE)
  results$theta <- theta
  
  emp_rel <- empirical_rxx(theta)
  marg_rel <- if (n_factors == 1) marginal_rxx(fit) else NA
  
  results$reliability <- list(
    empirical = emp_rel,
    marginal = marg_rel,
    omega = omega_results$omega.tot,
    omega_h = omega_results$omega_h,
    alpha = omega_results$alpha
  )
  
  if (n_factors == 1) {
    message(sprintf("   Empirical: %.3f | Marginal: %.3f", emp_rel, marg_rel))
  } else {
    message(sprintf("   Empirical: %.3f | Marginal: Not available (multidimensional)", emp_rel))
  }
  message(sprintf("   Omega: %.3f | Alpha: %.3f\n", 
                  omega_results$omega.tot, omega_results$alpha))
  
  # DETAILED ITEM ANALYSIS WITH LABELS AND RECOMMENDATIONS
  if (auto_display && n_factors == 1) {
    message("6. Detailed Item Analysis with Recommendations")
    
    # Get item parameters and information
    params <- results$parameters$items
    discrimination <- params[, "a"]
    names(discrimination) <- rownames(params)
    
    # Calculate information at theta = -2, 0, 2
    item_info_m2 <- sapply(1:ncol(data), function(i) .calc_item_info(fit, i, -2))
    item_info_0 <- sapply(1:ncol(data), function(i) .calc_item_info(fit, i, 0))
    item_info_p2 <- sapply(1:ncol(data), function(i) .calc_item_info(fit, i, 2))
    
    names(item_info_m2) <- names(discrimination)
    names(item_info_0) <- names(discrimination)
    names(item_info_p2) <- names(discrimination)
    
    # Create comprehensive item analysis data frame
    item_analysis <- data.frame(
      Item = names(discrimination),
      Label = results$item_labels[names(discrimination)],
      Discrimination = discrimination,
      Info_theta_m2 = item_info_m2,
      Info_theta_0 = item_info_0,
      Info_theta_p2 = item_info_p2,
      stringsAsFactors = FALSE
    )
    
    # Add item fit information if available - handle missing columns safely
    if (!is.null(item_fit) && "p.S_X2" %in% colnames(item_fit)) {
      item_analysis$Item_Fit_p <- item_fit$p.S_X2[match(item_analysis$Item, rownames(item_fit))]
      item_analysis$Poor_Fit <- item_analysis$Item_Fit_p < 0.05
    } else {
      item_analysis$Item_Fit_p <- NA
      item_analysis$Poor_Fit <- FALSE
    }
    
    results$item_analysis <- item_analysis
    
    # DISCRIMINATION ANALYSIS
    message("\n--- ITEM DISCRIMINATION ANALYSIS ---")
    
    # Sort by discrimination (descending)
    disc_sorted <- item_analysis[order(item_analysis$Discrimination, decreasing = TRUE), ]
    
    # Categorize discrimination levels
    good_disc <- disc_sorted[disc_sorted$Discrimination >= 1.5, ]
    avg_disc <- disc_sorted[disc_sorted$Discrimination >= 1.0 & disc_sorted$Discrimination < 1.5, ]
    poor_disc <- disc_sorted[disc_sorted$Discrimination < 1.0, ]
    
    if (nrow(good_disc) > 0) {
      message("\n** GOOD DISCRIMINATION (a >= 1.5) **")
      for (i in 1:nrow(good_disc)) {
        message(sprintf("%d. %s (%s): a = %.3f", 
                        i, good_disc$Item[i], good_disc$Label[i], good_disc$Discrimination[i]))
      }
    }
    
    if (nrow(avg_disc) > 0) {
      message("\n** AVERAGE DISCRIMINATION (1.0 <= a < 1.5) **")
      for (i in 1:nrow(avg_disc)) {
        message(sprintf("%d. %s (%s): a = %.3f", 
                        i, avg_disc$Item[i], avg_disc$Label[i], avg_disc$Discrimination[i]))
      }
    }
    
    if (nrow(poor_disc) > 0) {
      message("\n** POOR DISCRIMINATION (a < 1.0) **")
      for (i in 1:nrow(poor_disc)) {
        message(sprintf("%d. %s (%s): a = %.3f", 
                        i, poor_disc$Item[i], poor_disc$Label[i], poor_disc$Discrimination[i]))
      }
    }
    
    # INFORMATION ANALYSIS AT DIFFERENT THETA LEVELS
    message("\n--- ITEM INFORMATION ANALYSIS ---")
    
    # Information at theta = -2 (Low ability)
    message("\n** INFORMATION AT THETA = -2 (Low Ability) **")
    info_m2_sorted <- item_analysis[order(item_analysis$Info_theta_m2, decreasing = TRUE), ]
    for (i in 1:min(5, nrow(info_m2_sorted))) {
      message(sprintf("%d. %s (%s): Info = %.3f", 
                      i, info_m2_sorted$Item[i], info_m2_sorted$Label[i], info_m2_sorted$Info_theta_m2[i]))
    }
    
    # Information at theta = 0 (Average ability)
    message("\n** INFORMATION AT THETA = 0 (Average Ability) **")
    info_0_sorted <- item_analysis[order(item_analysis$Info_theta_0, decreasing = TRUE), ]
    for (i in 1:min(5, nrow(info_0_sorted))) {
      message(sprintf("%d. %s (%s): Info = %.3f", 
                      i, info_0_sorted$Item[i], info_0_sorted$Label[i], info_0_sorted$Info_theta_0[i]))
    }
    
    # Information at theta = 2 (High ability)
    message("\n** INFORMATION AT THETA = 2 (High Ability) **")
    info_p2_sorted <- item_analysis[order(item_analysis$Info_theta_p2, decreasing = TRUE), ]
    for (i in 1:min(5, nrow(info_p2_sorted))) {
      message(sprintf("%d. %s (%s): Info = %.3f", 
                      i, info_p2_sorted$Item[i], info_p2_sorted$Label[i], info_p2_sorted$Info_theta_p2[i]))
    }
    
    # COMPREHENSIVE ITEM RECOMMENDATIONS - with safer indexing
    message("\n--- PSYCHOMETRIC RECOMMENDATIONS ---")
    
    # Calculate thresholds safely
    info_75th <- quantile(item_analysis$Info_theta_0, 0.75, na.rm = TRUE)
    info_25th <- quantile(item_analysis$Info_theta_0, 0.25, na.rm = TRUE)
    
    # Determine items to retain (excellent items) - safer indexing
    excellent_criteria <- (item_analysis$Discrimination >= 1.5) & 
      (!item_analysis$Poor_Fit | is.na(item_analysis$Poor_Fit)) & 
      (item_analysis$Info_theta_0 >= info_75th)
    excellent_items <- item_analysis[which(excellent_criteria), ]
    
    # Determine items to discard (poor items) - safer indexing  
    poor_criteria <- (item_analysis$Discrimination < 1.0) | 
      (item_analysis$Poor_Fit & !is.na(item_analysis$Poor_Fit)) | 
      (item_analysis$Info_theta_0 <= info_25th)
    poor_items <- item_analysis[which(poor_criteria), ]
    
    # Items to retain
    if (nrow(excellent_items) > 0) {
      message("\n** ITEMS TO RETAIN (Excellent Psychometric Properties) **")
      excellent_sorted <- excellent_items[order(excellent_items$Discrimination, decreasing = TRUE), ]
      for (i in 1:nrow(excellent_sorted)) {
        message(sprintf("%d. %s (%s): a = %.3f, Info@theta=0 = %.3f, Fit: %s", 
                        i, excellent_sorted$Item[i], excellent_sorted$Label[i], 
                        excellent_sorted$Discrimination[i], excellent_sorted$Info_theta_0[i],
                        ifelse(!is.na(excellent_sorted$Poor_Fit[i]) && excellent_sorted$Poor_Fit[i], "Poor", "Good")))
      }
      results$items_to_retain <- excellent_sorted
    } else {
      message("\n** ITEMS TO RETAIN **")
      message("No items meet excellent criteria. Consider reviewing discrimination thresholds.")
      results$items_to_retain <- data.frame()
    }
    
    # Items to discard
    if (nrow(poor_items) > 0) {
      message("\n** ITEMS TO DISCARD (Poor Psychometric Properties) **")
      poor_sorted <- poor_items[order(poor_items$Discrimination, decreasing = FALSE), ]
      for (i in 1:nrow(poor_sorted)) {
        reasons <- c()
        if (poor_sorted$Discrimination[i] < 1.0) reasons <- c(reasons, "Low discrimination")
        # Safer check for Poor_Fit - handle NA values
        if (!is.na(poor_sorted$Poor_Fit[i]) && poor_sorted$Poor_Fit[i]) reasons <- c(reasons, "Poor model fit")
        if (poor_sorted$Info_theta_0[i] <= info_25th) {
          reasons <- c(reasons, "Low information")
        }
        
        message(sprintf("%d. %s (%s): a = %.3f, Reasons: %s", 
                        i, poor_sorted$Item[i], poor_sorted$Label[i], 
                        poor_sorted$Discrimination[i], paste(reasons, collapse = ", ")))
      }
      results$items_to_discard <- poor_sorted
    } else {
      message("\n** ITEMS TO DISCARD **")
      message("No items meet discard criteria. All items have acceptable properties.")
      results$items_to_discard <- data.frame()
    }
    
  } else if (auto_display && n_factors > 1) {
    # For multidimensional models, skip detailed item analysis but provide warning
    message("6. Item Analysis Skipped")
    message("   Detailed item analysis not available for multidimensional models")
    message("   Use unidimensional model (n_factors = 1) for item-level recommendations")
  }
  
  # PLOTTING (only for unidimensional models)
  if (n_factors == 1) {
    message("\n7. Saving and Displaying Plots")
    # Store plots for viewer display
    plots_list <- list()

    tryCatch({
      p1 <- ggmirt::tracePlot(fit, title = "Item Probability Functions") + 
        ggplot2::labs(color = "Response Categories")
      plots_list$trace_plot <- p1
      if (save_plots) ggplot2::ggsave(file.path(output_dir, "trace_plot.png"), p1, width = 12, height = 8)
      if (display_plots) print(p1)
    }, error = function(e) message("   Trace plot skipped: ", e$message))

    tryCatch({
      p2 <- ggmirt::itemInfoPlot(fit, facet = TRUE, title = "Item Information")
      plots_list$item_info_plot <- p2
      if (save_plots) ggplot2::ggsave(file.path(output_dir, "item_info.png"), p2, width = 14, height = 10)
      if (display_plots) print(p2)
    }, error = function(e) message("   Item info plot skipped: ", e$message))

    tryCatch({
      p3 <- ggmirt::testInfoPlot(fit, title = "Test Information")
      plots_list$test_info_plot <- p3
      if (save_plots) ggplot2::ggsave(file.path(output_dir, "test_info.png"), p3, width = 10, height = 6)
      if (display_plots) print(p3)
    }, error = function(e) message("   Test info plot skipped: ", e$message))

    tryCatch({
      p4 <- ggmirt::conRelPlot(fit, title = "Conditional Reliability")
      plots_list$reliability_plot <- p4
      if (save_plots) ggplot2::ggsave(file.path(output_dir, "reliability.png"), p4, width = 10, height = 6)
      if (display_plots) print(p4)
    }, error = function(e) message("   Reliability plot skipped: ", e$message))
    # Best items plot - display last to ensure it shows in viewer
    if (exists('item_analysis') && !is.null(item_analysis) && nrow(item_analysis) > 0) {
      tryCatch({
        p5 <- create_best_items_plot(item_analysis, results$item_labels)
        plots_list$best_items_plot <- p5
        if (save_plots) ggplot2::ggsave(file.path(output_dir, "best_items_plot.png"), p5, width = 12, height = 8)
        if (display_plots) print(p5)
      }, error = function(e) message("   Best items plot skipped: ", e$message))
    }

    # Store plots for return
    results$plots <- plots_list

    if (save_plots) {
      message(sprintf("   Plots saved to %s/", output_dir))
    }
  } else {
    message("\n7. Plots Skipped for Multidimensional Model")
    message("   Standard ggmirt plots not supported for multidimensional models")
  }

  message("\n=== Analysis Complete ===\n")

  class(results) <- c("grm_analysis", "list")
  return(results)
}

#' Create Best Items Visualization Plot
#' @param item_analysis Data frame with item analysis results
#' @param item_labels Named vector of item labels
#' @return ggplot object
create_best_items_plot <- function(item_analysis, item_labels) {
  
  # Get the fitted model from the parent environment
  fit <- get('fit', envir = parent.frame())
  
  # Find the three best items at each theta level
  best_at_m2 <- item_analysis$Item[which.max(item_analysis$Info_theta_m2)]
  best_at_0 <- item_analysis$Item[which(item_analysis$Info_theta_0 == max(item_analysis$Info_theta_0[item_analysis$Item != best_at_m2]))]
  best_at_p2 <- item_analysis$Item[which(item_analysis$Info_theta_p2 == max(item_analysis$Info_theta_p2[!item_analysis$Item %in% c(best_at_m2, best_at_0)]))]
  
  # Create theta range for plotting
  theta_range <- seq(-3, 3, length.out = 200)
  
  # Calculate actual item information curves
  plot_data <- data.frame()
  selected_items <- c(best_at_m2, best_at_0, best_at_p2)
  
  for(i in 1:length(selected_items)) {
    item_name <- selected_items[i]
    item_idx <- match(item_name, item_analysis$Item)
    item_label <- paste0(item_name, ': ', item_analysis$Label[item_idx])
    
    # Calculate information using mirt
    info_values <- sapply(theta_range, function(theta) {
      tryCatch({
        mirt::iteminfo(mirt::extract.item(fit, item_idx), Theta = matrix(theta))
      }, error = function(e) 0)
    })
    
    item_data <- data.frame(
      Theta = theta_range,
      Information = as.numeric(info_values),
      Item = item_name,
      Label = item_label
    )
    plot_data <- rbind(plot_data, item_data)
  }
  
  # Create the plot
  ggplot2::ggplot(plot_data, ggplot2::aes(x = Theta, y = Information, color = Label)) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::geom_vline(xintercept = c(-2, 0, 2), linetype = 'dashed', alpha = 0.7, color = 'gray50') +
    ggplot2::labs(title = 'Item Information Functions: Best Items at Different Ability Levels',
                  subtitle = 'Actual information curves from fitted GRM model',
                  x = 'Ability Level (θ)',
                  y = 'Item Information',
                  color = 'Items') +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), limits = c(-3, 3))
}

#' Extract item labels from data
#' @param data The data frame with item responses
#' @return Named vector of item labels
extract_item_labels <- function(data) {
  labels <- attr(data, 'variable.labels')
  if (is.null(labels)) {
    labels <- colnames(data)
    names(labels) <- colnames(data)
  }
  return(labels)
}

#' Calculate item information at specific theta
#' @param fit Fitted mirt model
#' @param item_index Item index
#' @param theta Ability level
#' @return Item information value
.calc_item_info <- function(fit, item_index, theta) {
  tryCatch({
    mirt::iteminfo(mirt::extract.item(fit, item_index), Theta = matrix(theta))
  }, error = function(e) 0)
}

#' Calculate empirical reliability
#' @param theta_scores Matrix of theta scores and standard errors
#' @return Empirical reliability coefficient
empirical_rxx <- function(theta_scores) {
  if (ncol(theta_scores) < 2) return(NA)
  theta <- theta_scores[, 1]
  se <- theta_scores[, 2]
  var_theta <- var(theta, na.rm = TRUE)
  mean_se2 <- mean(se^2, na.rm = TRUE)
  return(var_theta / (var_theta + mean_se2))
}
