#' Find TF2 Map.
#'
#' This function searches for a map using a regex pattern.
#'
#' This function searches for map by a regex pattern, and returns matching maps with the number of kills recorded for each map.
#' Maps with fewer kills than the threshold set are automatically filtered out. The threshold is 1 by default.
#'
#' @param pattern A regex pattern to be used to search for maps
#' @param minkills The threshold for minimum number of kills a map must have to be included
#' @return A dataframe of matching map names and their associated kill counts
#' @examples
#' findmap("plr_")
#' findmap("ctf_", 900)
#' findmap("event$")
#' findmap("(oo).*\\1")
#' @export

findmap <- function(pattern, minkills = 1){
  url <- "http://heatmaps.tf/data/maps.json"
  tf <- GET(url)
  tf_list <- content(tf)
  tf_df <- data.frame(matrix(unlist(tf_list), ncol = 2, byrow = T))
  colnames(tf_df) <- c("map", "kills")
  tf_df$kills <- as.numeric(tf_df$kills)

  tf_df[str_detect(tf_df[,1], pattern), ] %>%
    dplyr::filter(kills >= minkills) %>%
    arrange(desc(kills))
}
