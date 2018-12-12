#' Find Weapon Elevation Statistics.
#'
#' This function shows how elevation affects kills.
#'
#' This function shows the distribution of elevation differences between killers and victims, by the map, killers' weapon, and optionally the enemy class.
#' If no enemy class is specified, then the function will use kills against all enemy classes.
#' Elevation is defined such that it is positive if the killer is on higher ground than the victim.
#' Function may take several seconds to run depending on map.
#'
#' @param map The map name, typically beginning with "ctf_", "cp_", "pl_", "arena_", "plr_", or "koth_"
#' @param enemy_class (Optional) One of scout, sniper, soldier, demoman, medic, heavy, pyro, spy, or engineer
#' @param weapon An integer referring to the weapon from the TF2 item index at https://wiki.alliedmods.net/Team_Fortress_2_Item_Definition_Indexes
#' @return A kernel density estimate of probability of kills by elevation.
#' @examples
#' elevation("pl_goldrush", 13) ## scattergun (short range)
#' elevation("pl_goldrush", 14) ## sniper rifle (long range)
#' elevation("pl_goldrush", 14, "sniper") ## against enemy snipers
#' elevation("ctf_doublecross", 15150) ## returns message if no kills for the parameters entered
#' @export
elevation <- function(map, weapon, enemy_class=NULL){

  fields <- c("killer_z", "victim_z", "killer_weapon")
  n <- length(fields)

  url <- str_c("http://heatmaps.tf/data/kills/", map, ".json?", "&victim_class=", enemy_class, "&fields=", str_c(fields, collapse = ","))

  tf <- GET(url)
  tf_list <- content(tf)$kills
  tf_df <- data.frame(matrix(unlist(tf_list), ncol = n, byrow = T))
  colnames(tf_df) <- fields
  tf_df <- tf_df %>%
    dplyr::filter(killer_weapon == weapon) %>%
    mutate(elevation = killer_z - victim_z)

  if (nrow(tf_df) == 0) message("There are no kills on this map for that weapon")

  e_class <- ifelse(is.null(enemy_class), "all classe", enemy_class)
  graph_title <- str_c("Kills of ", e_class, "s in ", map, " using weapon ", weapon)

  plot <- ggplot(tf_df, aes(elevation)) +
    geom_density(adjust = 2) +
    ggtitle(graph_title) +
    xlim(-400,400)
  suppressWarnings(print(plot))
}
