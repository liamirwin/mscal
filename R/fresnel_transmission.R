#' Fresnel Transmission for Downwelling Light Sensor (DLS)
#'
#' This function computes the Fresnel transmission for the Downwelling Light Sensor (DLS)
#' using the Fresnel equations for reflection and transmission of an electromagnetic wave
#' when it crosses the boundary between two media with different refractive indices.
#' This correction adjusts for the DLS reflecting, rather than measuring, some of the irradiance that hits it.
#' The function is based on the equations from the MicaSense GitHub repository.
#'
#' @param phi The angle of incidence in radians. It's the angle between the incident ray and the surface normal.
#' @param n1 The refractive index of the first medium (typically air).
#' @param n2 The refractive index of the second medium (typically the material of the DLS sensor).
#' @param polarization A numeric vector of length 2 containing the weights of s-polarization (Rs) and p-polarization (Rp).
#'
#' @return The Fresnel transmission coefficient T, clamped between 0 and 1.
#' @export
#'
#' @examples
#' fresnel_transmission(0.5, 1, 1.5, c(0.5, 0.5))
fresnel_transmission <- function(phi, n1 = 1, n2 = 1.5, polarization = c(0.5, 0.5)) {
  # Fresnel equations
  f1 = cos(phi)
  f2 = sqrt(1 - (n1 / n2 * sin(phi))^2)

  # Reflectance for s-polarization and p-polarization
  Rs = ((n1 * f1 - n2 * f2) / (n1 * f1 + n2 * f2))^2
  Rp = ((n1 * f2 - n2 * f1) / (n1 * f2 + n2 * f1))^2

  # Transmission
  T = 1 - polarization[1] * Rs - polarization[2] * Rp
  T = pmin(pmax(T, 0), 1)  # Clamp the value between 0 and 1

  return(T)
}
