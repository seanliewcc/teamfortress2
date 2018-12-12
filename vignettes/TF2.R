## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE----------------------------------------------------------
library(teamfortress2)

## ------------------------------------------------------------------------
findmap("hightower")

## ------------------------------------------------------------------------
findmap("event$", 500)

## ------------------------------------------------------------------------
elevation("pl_goldrush", 56) ## huntsman (short range)
elevation("pl_goldrush", 14) ## sniper rifle (long range)

## ------------------------------------------------------------------------
elevation("pl_goldrush", 127, "spy") ## The Direct Hit (rocket launcher)
elevation("pl_goldrush", 127, "sniper")

## ------------------------------------------------------------------------
weprange("pl_goldrush", 19) ## grenade launcher
weprange("pl_goldrush", 20) ## stickybomb launcher

## ------------------------------------------------------------------------
weprange("pl_goldrush", 416, distance = "2D") ## The market gardener
weprange("pl_goldrush", 416, distance = "3D")

## ------------------------------------------------------------------------
charclass("pl_goldrush", enemy_class = "engineer", team = "BLU")
charclass("pl_goldrush", enemy_class = "engineer", team = "RED")

