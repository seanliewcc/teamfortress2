#' Find Weapon Range Statistics.
#'
#' This function shows how weapon range affects kills.
#'
#' This function shows the distribution of kill ranges, by the map, killers' weapon, and optionally the enemy class.
#' If no enemy class is specified, then the function will use kills against all enemy classes.
#' Range can be set to either 2D (only x-y) or 3D distance.
#' Function may take several seconds to run depending on the map.
#'
#' @param map The map name, typically beginning with "ctf_", "cp_", "pl_", "arena_", "plr_", or "koth_"
#' @param enemy_class (Optional) One of scout, sniper, soldier, demoman, medic, heavy, pyro, spy, or engineer
#' @param weapon An integer referring to the weapon from the TF2 item index at https://wiki.alliedmods.net/Team_Fortress_2_Item_Definition_Indexes
#' @param distance Either "2D" or "3D" (default)
#' @return A kernel density estimate of probability of kills by range.
#' @examples
#' weprange("pl_goldrush", 21) ## flamethrower (short range)
#' weprange("pl_goldrush", 56) ## huntsman (long range)
#' weprange("pl_goldrush", 56, enemy_class="sniper", distance = "2D")
#' weprange("ctf_doublecross", 15150) ## message if no kills for the parameters entered
#' @export
weprange <- function(map, weapon, enemy_class=NULL, distance = "3D"){

  fields <- c("killer_x", "killer_y", "killer_z", "victim_x", "victim_y", "victim_z", "killer_weapon")
  n <- length(fields)

  url <- str_c("http://heatmaps.tf/data/kills/", map, ".json?", "&victim_class=", enemy_class, "&fields=", str_c(fields, collapse = ","))

  tf <- GET(url)
  tf_list <- content(tf)$kills
  tf_df <- data.frame(matrix(unlist(tf_list), ncol = n, byrow = T))
  colnames(tf_df) <- fields
  tf_df <- tf_df %>%
    dplyr::filter(killer_weapon == weapon) %>%
    mutate(distance_2D = sqrt((killer_x - victim_x)^2 + (killer_y - victim_y)^2),
           distance_3D = sqrt((killer_x - victim_x)^2 + (killer_y - victim_y)^2 + (killer_z - victim_z)^2))

  if (nrow(tf_df) == 0) message("There are no kills on this map for that weapon")

  e_class <- ifelse(is.null(enemy_class), "all classe", enemy_class)
  graph_title <- str_c("Kills of ", e_class, "s in ", map, " using weapon ", weapon)

  if (distance == "2D") {
    plot <- ggplot(tf_df, aes(distance_2D))}
  else {
    plot <- ggplot(tf_df, aes(distance_3D))}
  plot +
    geom_density(adjust = 2) +
    ggtitle(graph_title)
}
