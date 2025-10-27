#' Internal imports and globals
#'
#' Import lightweight symbols and declare data-masked variables to keep R CMD check quiet.
#'
#' @importFrom stats median var quantile acf na.pass approx time mad
#' @importFrom feasts STL
#' @importFrom fabletools model components 
#' @importFrom dplyr %>% group_by summarise arrange transmute filter mutate count
#' @importFrom dplyr n_distinct bind_rows left_join rename group_modify ungroup select tibble across
#' @importFrom tidyr unnest
#' @importFrom utils head
#'
#' @keywords internal
"_PACKAGE"

## declare columns used in tidy evaluation to silence R CMD check 
utils::globalVariables(c(
  "id","time","tmin","tmax","grid","value","lo","hi","n",
  "time_start","time_end","issue_type","severity","score","details","idx",
  "step","rule","params","changed_count"
))