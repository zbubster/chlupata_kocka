library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- MENU (statické) ---
menu_df <- data.frame(
  item  = c("Pivo", "Víno", "Limo", "Panák", "Čaj", "Káva"),
  cena  = c(35, 50, 25, 60, 25, 30),
  stringsAsFactors = FALSE
)

menu_choices <- setNames(menu_df$cena, paste0(menu_df$item, " (", menu_df$cena, " Kč)"))

# --- HOSTS (výchozí seznam, dynamické) ---
hosts_init <- data.frame(
  host = c("Bavíš", "Fijala", "Okamúra", "Rakušák", "Konečníková", "Hříbek", "Otoman", "Šlacha"),
  stringsAsFactors = FALSE
)

source("scripts/ui.R")
source("scripts/server.R")
shinyApp(ui, server)
