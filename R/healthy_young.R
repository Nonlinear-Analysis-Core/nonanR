#' Gaitprint data for a healthy young person
#'
#' A subset of data from the NONAN Gaitprint data repository. The age range for healthy young people is 19-34 years old. 
#'
#' @format ## `healthy_young`
#' A data frame with 10,000 rows and 18 columns:
#' \describe{
#'   \item{time}{Time in seconds. Data is sampled at 200Hz}
#'   \item{Pelvis pitch (deg)}{Pelvis angle in reference to the direction of heading}
#'   \item{Head pitch (deg)}{Head angle in reference to the direction of heading}
#'   \item{Thigh pitch LT (deg)}{Left thigh angle in reference to the direction of heading}
#'   \item{Shank pitch LT (deg)}{Left shank angle in reference to the direction of heading}
#'   \item{Contact LT}{Pre calculated heel contact events for the left leg. 0 means the foot is in the air and 1000 indicates contact with the ground}
#'   \item{Foot pitch LT (deg)}{Left foot angle in reference to the direction of heading}
#'   \item{Thigh pitch RT (deg)}{Right thigh angle in reference to the direction of heading}
#'   \item{Shank pitch RT (deg)}{Right shank angle in reference to the direction of heading}
#'   \item{Contact RT}{Pre calculated heel contact events for the right leg. 0 means the foot is in the air and 1000 indicates contact with the ground}
#'   \item{Foot pitch RT (deg)}{Right foot angle in reference to the direction of heading}
#'   \item{Lumbar Flexion (deg)}{Lumbar spine angle in reference to the direction of heading}
#'   \item{Hip Flexion LT (deg)}{Left hip angle in reference to the direction of heading}
#'   \item{Knee Flexion LT (deg)}{Left knee angle in reference to the direction of heading}
#'   \item{Ankle Dorsiflexion LT (deg)}{Left ankle angle in reference to the direction of heading}
#'   \item{Hip Flexion RT (deg)}{Right hip angle in reference to the direction of heading}
#'   \item{Knee Flexion RT (deg)}{Right knee angle in reference to the direction of heading}
#'   \item{Ankle Dorsiflexion RT (deg)}{Right ankle angle in reference to the direction of heading}
#'   ...
#' }
#' @source <https://springernature.figshare.com/collections/NONAN_GaitPrint_An_IMU_gait_database_of_healthy_young_adults/6415061/1>
"healthy_young"