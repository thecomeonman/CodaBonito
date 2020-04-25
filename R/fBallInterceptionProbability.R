#' Pitch control 
#'
#' @param tti_sigma
#' @param time_to_intercept
#' @param Time
#' @examples
#' @export
fBallInterceptionProbability = function(
    tti_sigma,
    time_to_intercept,
    Time
) {

    # probability of a player arriving at target location at time 'T' given their expected time_to_intercept (time of arrival), as described in Spearman 2018
    f = 1/(1. + exp( -pi/sqrt(3.0)/tti_sigma * (Time - time_to_intercept) ) )
    return (f)

}