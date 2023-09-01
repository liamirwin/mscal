#' Compute Sun-Sensor Angle
#'
#' This function calculates the angle between the sun and the Downwelling Light Sensor (DLS)
#' based on the sun's position and the orientation of the drone.
#'
#' @param SolarElevation Numeric. The elevation angle of the sun above the horizon.
#' @param SolarAzimuth Numeric. The azimuth angle of the sun.
#' @param Roll Numeric. The roll angle of the drone.
#' @param Pitch Numeric. The pitch angle of the drone.
#' @param Yaw Numeric. The yaw angle of the drone.
#'
#' @return Numeric. The angle between the sun and the sensor in radians.
#'
#' @export
#' @examples
#' angle = compute_sun_angle(40, 120, 0.2, 0.1, 0.3)
compute_sun_angle = function(SolarElevation, SolarAzimuth, Roll, Pitch, Yaw) {

  # Define the orientation vector of the DLS in body coordinates
  # DLS pointing down?
  ori = c(0, 0, -1)

  # Convert to numeric
  SolarElevation = as.numeric(SolarElevation)
  SolarAzimuth = as.numeric(SolarAzimuth)
  Roll = as.numeric(Roll)
  Pitch = as.numeric(Pitch)
  Yaw = as.numeric(Yaw)

  # Convert sun azimuth and elevation to NED (north east down) coordinates
  elements = c(
    cos(SolarAzimuth) * cos(SolarElevation),
    sin(SolarAzimuth) * cos(SolarElevation),
    -sin(SolarElevation))

  # transpose vector to matrix
  nSun = t(matrix(elements, ncol = 3))

  # Convert the sensor orientation angles (Roll, Pitch, Yaw) to a 3x3 rotation matrix
  c1 = cos(-Yaw)
  s1 = sin(-Yaw)
  c2 = cos(-Pitch)
  s2 = sin(-Pitch)
  c3 = cos(-Roll)
  s3 = sin(-Roll)
  Ryaw = matrix(c(c1, s1, 0, -s1, c1, 0, 0, 0, 1), ncol = 3, byrow = TRUE)
  Rpitch = matrix(c(c2, 0, -s2, 0, 1, 0, s2, 0, c2), ncol = 3, byrow = TRUE)
  Rroll = matrix(c(1, 0, 0, 0, c3, s3, 0, -s3, c3), ncol = 3, byrow = TRUE)
  # take the dot product of the three matrices
  R_sensor = Ryaw %*% Rpitch %*% Rroll

  # Compute the orientation vector of the sensor in NED coordinates
  # as the dot product of sensor orientation and sensor position relative to the drone
  nSensor = R_sensor %*% ori

  # Compute the angle between the sensor and the sun
  angle = acos(sum(nSun * nSensor))
  return(angle) # in radians
}
