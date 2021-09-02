#' Computes Descriptive Statistics
#'
#' @param data input data set
#' @param var1 name of variable 1 (in quotes)
#' @param var2 name of variable 2 (in quotes)
#'
#' @return a data frame containing descriptive statistics
#' @export
#'
#' @examples
#' compute_ds_another_way(data = faithful,var1 = 'eruptions', var2 = 'waiting')
#'
#' @importFrom rlang .data
compute_ds_another_way <- function(data,var1,var2){
  # compute and store statistics
  average <-  c(mean(data[[var1]],na.rm=TRUE), mean(data[[var2]],na.rm=TRUE))
  minimum <- c(min(data[[var1]],na.rm=TRUE),min(data[[var2]],na.rm=TRUE))
  maximum <- c(max(data[[var1]],na.rm=TRUE),max(data[[var2]],na.rm=TRUE))
  stdev <- c(stats::sd(data[[var1]],na.rm=TRUE),stats::sd(data[[var2]],na.rm=TRUE))

  dataSummary <- rbind(average,minimum,maximum,stdev)
  colnames(dataSummary) <- colnames(data)
  return(dataSummary)
}
