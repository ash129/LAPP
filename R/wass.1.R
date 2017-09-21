#' Calculate the first Wasserstein metric (Earth Mover's Distance) of two samples
#'
#' \code{wass.1()} takes in two numeric vectors and returns a single numeric distance metric. The
#' metric is equivalent to the sum of the absolute difference in area between the empirical distribution functions
#' of the two samples. This total area is found by calculating left Riemann sums.
#'
#' @param x the first sample
#'
#' @param y the second sample
#'
#' @return the first Wasserstein distance
#'
#' @export
#'
wass.1= function(x, y){

  # sorted data
  x= sort(x)
  y= sort(y)

  # empirical density functions
  ex= ecdf(x)
  ey= ecdf(y)

  # number of items in each list
  x.len= length(x)
  y.len= length(y)

  # find bin boundaries
  x.bin= unique(x)
  y.bin= unique(y)
  tot.bins= sort(unique(c(x.bin, y.bin)))

  # total distance
  D= 0

  # calculate using left Riemann sums
  # check for at least one bin
  if(length(tot.bins) > 1){

    # calculated rectangular area of each bin
    for( ind in 1:(length(tot.bins) - 1)){
      # bin width
      left.bin= tot.bins[ind]
      right.bin= tot.bins[ind + 1]
      width= right.bin - left.bin

      # bin height
      x.val= ex(left.bin)
      y.val= ey(left.bin)
      height= abs(x.val - y.val)

      D= D + abs(width * height)
    }

  }

  return(D)
}
