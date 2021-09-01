#' Computes Descriptive Statistics
#'
#' @param data input dataset
#' @param var1 name of variable 1 (unquoted)
#' @param var2 name of variable 2 (unquoted)
#'
#' @return a data frame with descriptive statistics including average, min-max and standard dev
#' @export
#'
#' @examples
#' compute_ds(data = faithful,var1 = eruptions, var2 = waiting)
#'
#' @importFrom rlang .data
compute_ds <- function(data, var1, var2){

  x = data %>% dplyr::pull({{var1}})
  y = data %>% dplyr::pull({{var2}})

  # compute and store statistics
  average <-  c(mean(x,na.rm=TRUE), mean(y,na.rm=TRUE))
  minimum <- c(min(x,na.rm=TRUE),min(y,na.rm=TRUE))
  maximum <- c(max(x,na.rm=TRUE),max(y,na.rm=TRUE))
  stdev <- c(stats::sd(x,na.rm=TRUE),stats::sd(y,na.rm=TRUE))

  dataSummary <- rbind(average,minimum,maximum,stdev)
  colnames(dataSummary) <- colnames(data)
  return(dataSummary)
}
