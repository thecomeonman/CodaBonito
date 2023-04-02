#' Likelihood of number of success of an arbitrary set of probabilities
#'
#' Is just binomial distributions under the hood
#'
#' @param probs a vector of the probabilities of all the independent events
#' @param n_buckets the number of buckets to break the 0 to 1 probability
#' space in and combine events falling within that bucket to speed up the
#' processing.
#'
#' @import data.table
#' @examples
#' dt_success_probs = get_likelihood(
#'    # probs = runif(100),
#'    probs = rep(.5,100),
#'    n_buckets = Inf
#' )
#' dt_success_probs[,plot(successes,pdf)]
#'
#' @export
get_likelihood = function(
   probs,
   n_buckets = Inf
) {

   if ( n_buckets >= length(probs) ) {

      dt_probs = data.table(
         prob = probs,
         count = 1
      )

   } else {

      dt_probs = data.table(
         bucket = cut(probs,seq(0,1,1/n_buckets)),
         prob = probs
      )

      dt_probs = dt_probs[, list(prob = mean(prob), count = .N),bucket]

   }

   dt_probs[, bucket_id := .I]
   dt_probs = dt_probs[, list(successes = 0:count), list(bucket_id,prob,count)]

   dt_probs[, count_probability := (prob^successes)*((1-prob)^(count-successes))]
   dt_probs[, ways_to_choose := choose(count, successes)]
   dt_probs[, count_probability := count_probability * ways_to_choose]

   dt_success_probs = data.table(
      successes = 0,
      pdf = 1,
      k = 'k'
   )

   for ( bucket_id_counter in dt_probs[, unique(bucket_id)] ) {

      dt_success_probs = merge(
         dt_success_probs,
         dt_probs[bucket_id_counter == bucket_id, list(k = 'k',successes, count_probability)],
         'k',
         allow.cartesian = T
      )

      dt_success_probs[, successes := successes.x + successes.y]
      dt_success_probs[, pdf := pdf * count_probability]
      # dt_success_probs[, c('successes.x','successes.y','count_probability') := NULL]
      dt_success_probs = dt_success_probs[,
         list(pdf = sum(pdf)),
         list(k,successes)
      ]

   }

   dt_success_probs[, k := NULL]
   dt_success_probs[, success_pct := successes/length(probs)]
   dt_success_probs[, cdf := cumsum(pdf)]

   dt_success_probs

}
