#' Find Best Character Class.
#'
#' This function helps players choose the best character class to KILL a given enemy.
#'
#' This function helps players find the best class for killing a given enemy class.
#' This is a function of map, enemy’s class, and optionally team.
#' For asymmetric maps (pl_ and some cp_), BLU represents the attacking team and RED the defending team.
#'
#' @param map The map name, typically beginning with "ctf_", "cp_", "pl_", "arena_", "plr_", or "koth_"
#' @param enemy_class One of scout, sniper, soldier, demoman, medic, heavy, pyro, spy, or engineer
#' @param team (Optional) BLU (offence) or RED (defence)
#' @return A bar chart displaying the number of kills each class made for the given map and enemy class.
#' @import stringr
#' @import httr
#' @import dplyr
#' @import ggplot2
#' @examples
#' charclass("koth_viaduct", enemy_class = "sniper")
#' charclass("pl_goldrush", enemy_class = "spy", team = "RED")
#' @export
charclass <- function(map, enemy_class, team = "all"){
  if (team == "all") {
    url <- str_c("http://heatmaps.tf/data/kills/", map, ".json?", "victim_class=", enemy_class, "&fields=killer_class")
  }
  else {
    url <- str_c("http://heatmaps.tf/data/kills/", map, ".json?", "victim_class=", enemy_class, "&killer_team=", team, "&fields=killer_class")
  }
  tf <- GET(url)
  tf_list <- content(tf)$kills %>% unlist()
  classes <- tf_list %>%
    subset(tf_list > 0) %>%
    factor(levels = 1:9, labels = c("scout", "sniper", "soldier", "demoman", "medic", "heavy", "pyro", "spy", "engineer"))
  props <- table(classes) %>%
    data.frame() %>%
    arrange(desc(Freq))

  graph_title <- str_c("Kills of ", enemy_class, "s in ", map)

  ggplot(props, aes(x = classes, y = Freq)) +
    geom_bar(stat = 'identity') +
    ggtitle(graph_title)
}
