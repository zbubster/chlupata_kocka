## menu and guests

# --- MENU ---
menu_df <- data.frame(
  item  = menu,
  cena  = price,
  stringsAsFactors = FALSE
)
menu_choices <- setNames(menu_df$item, paste0(menu_df$item, " (", menu_df$cena, " Kč)"))

# --- GUESTS ---
# this is a default df, updated guest list is stored in temp/hosts.rds
hosts_init <- data.frame(
  host = guests,
  stringsAsFactors = FALSE
)
