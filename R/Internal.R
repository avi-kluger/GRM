#' Internal: Numeric item information (robust across mirt versions)
#'
#' @param fit A mirt model object
#' @param item_num Integer index of the item
#' @param theta Numeric theta value
#' @return Numeric information value for the item at theta
#' @keywords internal
.calc_item_info <- function(fit, item_num, theta) {
  item <- extract.item(fit, item_num)
  probs <- probtrace(item, matrix(theta))
  h <- 1e-3
  probs_plus <- probtrace(item, matrix(theta + h))
  probs_minus <- probtrace(item, matrix(theta - h))
  deriv <- (probs_plus - probs_minus) / (2 * h)
  info <- sum((deriv^2) / probs, na.rm = TRUE)
  return(info)
}