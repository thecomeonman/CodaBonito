#' Pitch control support function
#'
#' @param reaction_time
#' @param VelocityX
#' @param VelocityY
#' @param position_x
#' @param position_y
#' @param vmax
#' @param target_x
#' @param target_y
#' @examples
#' @export
fSimpleTimeToIntercept = function(
    reaction_time,
    VelocityX,
    VelocityY,
    position_x,
    position_y,
    vmax,
    target_x,
    target_y
) {

    position = cbind(position_x, position_y)
    velocity = cbind(VelocityX, VelocityY)
    target = cbind(target_x, target_y)

    # Time to intercept assumes that the player continues moving at current velocity for 'reaction_time' seconds
    # and then runs at full speed to the target position.
    reaction = position + ( velocity * reaction_time )

    time_to_intercept = reaction_time + ( rowSums( ( target - reaction ) ^ 2 ) ^ 0.5 ) / vmax
    # time_to_intercept = reaction_time + ( ( ( ( ( r_final[1] - reaction[, 1] ) ^ 2 ) + ( ( r_final[2] - reaction[, 2] ) ^ 2 ) ) ^ 0.5 ) ) / vmax

    return ( time_to_intercept )

}
