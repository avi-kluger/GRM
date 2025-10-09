#' Extract item labels from data column attributes
#'
#' @param data A data.frame with item columns that may have "label" attributes
#' @return Named character vector of item labels
#' @export
extract_item_labels <- function(data) {
  item_labels <- sapply(names(data), function(col) {
    label <- attr(data[[col]], "label")
    if (is.null(label) || identical(label, "")) col else label
  }, USE.NAMES = TRUE)
  return(item_labels)
}