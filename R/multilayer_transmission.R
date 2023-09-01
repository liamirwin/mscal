#' Compute Multilayer Transmission using Fresnel Equations
#'
#' This function calculates the transmission coefficient for a multilayer material
#' based on the Fresnel equations. It uses the function `fresnel_transmission` to calculate
#' the transmission for each layer and multiplies them together.
#'
#' @param phi The initial angle of incidence in radians. It's the angle between the incident ray and the surface normal of the first layer.
#' @param n A numeric vector representing the refractive indices of each layer in the multilayer material, starting with the outermost layer.
#' @param polarization A numeric vector of length 2 containing the weights of s-polarization (Rs) and p-polarization (Rp).
#'
#' @return The total Fresnel transmission coefficient T, calculated for the multilayer material.
#' @export
#'
#' @examples
#' multilayer_transmission(0.5, c(1, 1.5, 1.2), c(0.5, 0.5))
multilayer_transmission <- function(phi, n, polarization) {
  # Initialize transmission coefficient
  T = 1.0

  # Effective angle of incidence
  phi_eff = phi

  # Loop through each layer
  for (i in 1:(length(n) - 1)) {
    # Refractive indices of current and next layer
    n1 = n[i]
    n2 = n[i + 1]

    # Adjust the effective angle of incidence based on the current refractive index
    phi_eff = asin(sin(phi_eff) / n1)

    # Calculate the Fresnel transmission for the current layer and multiply it with the total transmission
    T = T * fresnel_transmission(phi_eff, n1, n2, polarization)
  }

  return(T)
}
