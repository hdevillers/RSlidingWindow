#' RSlidingWindow
#'
#' A fast and simple sliding window tool for R.
#'
#' @param x A numeric vector.
#' @param length.sw An integer, length of the windows.
#' @param by.sw An integer, steps between each window [default: 1].
#' @param method.sw A string, name of the method to use (see details) [default: "mean"].
#' @param keep.cut A boolean, indicating if the last window must be kept if it is uncompleted [default: TRUE].
#' @export
RSlidingWindow <- function(x,
                           length.sw,
                           by.sw = 1,
                           method.sw = "mean",
                           keep.cut = TRUE)
{
  # Control method
  methods.sw <- c("mean")
  if( !(method.sw %in% methods.sw) )
    stop(paste("Unknown method '", method.sw, "'.", sep=""))

  # Run SW
  output <- switch (method.sw,
    mean = sliding_window_mean(x, length.sw, by.sw, keep.cut)
  )

  return(output)
}
