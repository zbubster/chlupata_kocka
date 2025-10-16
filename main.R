#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
#
#             Chlupatá kočka
#
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Pro správné použití programu nahraďtě níže v sekce CONFIG seznam hostů reálnými jmény. Dále upravte položky, které budou k dostání a jejich cenu (na pořadí záleží a zároveň length(menu) == length(price)!!). Následně stačí spustit sekci RUN a poslední příkaz, který otevře okno programu. V případě uzavření okna jsou zaznamenané změny uloženy v adresáři temp, a zároveň při opakovaném spuštění aplikace se opět načtou.

# CONFIG
guests <- c("Bavíš", "Fijala", "Okamúra", "Rakušák", "Konečníková", "Hříbek", "Otoman", "Šlacha")
#guests <- c()
menu <- c("Pivo", "Víno", "Limo", "Panák", "Čaj", "Káva")
price <- c(35, 50, 25, 60, 25, 30)

# RUN
source("scripts/knihovnik.R"); knihovnik(shiny, ggplot2, dplyr, tidyr, shinyalert)
source("scripts/menu_guests.R")
source("scripts/ui.R")
source("scripts/server.R")

shinyApp(ui, server)
