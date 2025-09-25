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

ui <- fluidPage(
  titlePanel("Chlupatá kočka"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Objednávka:"),
      selectInput("host", "Vyber hosta:", choices = hosts_init$host),
      selectInput("item", "Položka:", choices = menu_choices),
      numericInput("qty", "Množství:", value = 1, min = 0.1, step = 1),
      actionButton("add", "Přidat na účet", class = "btn-primary"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
      tags$h4("Přidat účet"),
      fluidRow(
        column(8, textInput("new_host", "Jméno:", value = "")),
        column(8, actionButton("add_host", "Přidat"))
      ),
      tags$h3("DANGER ZONE", style = "color: red; font-weight: bold"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
      tags$h4("Storno"),
      actionButton("undo", "Storno poslední objednávky", class = "btn-warning"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
      tags$h4("Export"),
      downloadButton("export", "Export historie"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
      tags$h4("Reset"),
      actionButton("reset", "Vynulovat vše", class = "btn-danger")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Účty (total)",
                 tableOutput("ucty")),
        tabPanel("Historie objednávek",
                 tableOutput("history")),
        tabPanel("Summary",
                 tableOutput("summary_totals")),
        tabPanel("Graf",
                 plotOutput("ucty_plot", height = "700px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- Stav aplikace ---
  # Historie objednávek (Time, Host, Item, Qty, Price, Total)
  orders <- reactiveVal({
    data.frame(
      Time  = as.POSIXct(character()),
      Host  = character(),
      Item  = character(),
      Qty   = integer(),
      Price = numeric(),
      Total = numeric(),
      stringsAsFactors = FALSE
    )
  })
  
  # Kumulativní graf útraty jednotlivých hostů v čase
  output$ucty_plot <- renderPlot({
    od <- orders()
    if (nrow(od) == 0) return(NULL)
    
    # všichni hosté (včetně těch bez objednávek)
    all_hosts <- hosts_df()$host
    
    # časové uzly = všechny momenty objednávek
    time_points <- sort(unique(od$Time))
    
    # kumulativní suma na úrovni (Host, Time) pro skutečné objednávky
    base <- od %>%
      arrange(Time) %>%
      group_by(Host) %>%
      mutate(CumTotal = cumsum(Total)) %>%
      ungroup() %>%
      select(Time, Host, CumTotal)
    
    # kompletní mřížka: každý host × každý časový bod
    grid <- expand.grid(Time = time_points, Host = all_hosts, KEEP.OUT.ATTRS = FALSE) %>%
      as_tibble() %>%
      left_join(base, by = c("Time", "Host")) %>%
      group_by(Host) %>%
      arrange(Time, .by_group = TRUE) %>%
      tidyr::fill(CumTotal, .direction = "down") %>%   # carry-forward
      mutate(CumTotal = ifelse(is.na(CumTotal), 0, CumTotal)) %>%
      ungroup()
    
    # Pokud je jen 1 časový bod, ukážeme body; jinak čáry + body
    if (length(time_points) == 1) {
      ggplot(grid, aes(x = Time, y = CumTotal, color = Host, group = Host)) +
        geom_point(size = 2) +
        labs(x = "Čas", y = "Kumulativní útrata (Kč)", color = "Host") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")
    } else {
      ggplot(grid, aes(x = Time, y = CumTotal, color = Host, group = Host)) +
        geom_line(linewidth = 1) +                 # lineární průběh
        geom_point(size = 1.6) +                   # volitelně body v uzlech
        labs(x = "Čas", y = "Kumulativní útrata (Kč)", color = "Host") +
        scale_x_datetime(date_labels = "%H:%M\n%d.%m.") +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        )
    }
  })
  
  # --- Stav: seznam hostů jako samostatná df ---
  hosts_df <- reactiveVal(hosts_init)
  
  # --- Perzistence při startu ---
  if (file.exists("orders.rds")) orders(readRDS("orders.rds"))
  if (file.exists("hosts.rds"))  hosts_df(readRDS("hosts.rds"))
  
  # --- Reaktivní update selectInput host při změně hosts_df ---
  observe({
    h <- hosts_df()$host
    sel <- if (!is.null(input$host) && input$host %in% h) input$host else head(h, 1)
    updateSelectInput(session, "host", choices = h, selected = sel)
  })
  
  # --- Přidání nového hosta (bez duplikátů, case-insensitive) ---
  observeEvent(input$add_host, {
    nm <- trimws(input$new_host)
    if (!nzchar(nm)) return()
    d  <- hosts_df()
    exists_ci <- tolower(nm) %in% tolower(d$host)
    if (!exists_ci) {
      d <- rbind(d, data.frame(host = nm, stringsAsFactors = FALSE))
      hosts_df(d)
      updateTextInput(session, "new_host", value = "")
      updateSelectInput(session, "host", choices = d$host, selected = nm)
    } else {
      # jen vyber existující (přes přesné jméno, pokud se liší jen velikostí písmen)
      pick <- d$host[match(tolower(nm), tolower(d$host))]
      updateSelectInput(session, "host", choices = d$host, selected = pick)
      updateTextInput(session, "new_host", value = "")
    }
  })
  
  # --- Přidání objednávky ---
  observeEvent(input$add, {
    req(input$host, input$item, input$qty)
    qty <- as.integer(input$qty)
    if (is.na(qty) || qty < 1) return(NULL)
    
    price <- as.numeric(input$item)
    item_name <- menu_df$item[match(price, menu_df$cena)]
    if (is.na(item_name)) item_name <- "Neznámá položka"
    
    # jistota, že host je v hosts_df (kdyby přišel odjinud)
    if (!(input$host %in% hosts_df()$host)) {
      hosts_df(rbind(hosts_df(), data.frame(host = input$host, stringsAsFactors = FALSE)))
    }
    
    new_row <- data.frame(
      Time  = Sys.time(),
      Host  = input$host,
      Item  = item_name,
      Qty   = qty,
      Price = price,
      Total = qty * price,
      stringsAsFactors = FALSE
    )
    orders(rbind(orders(), new_row))
  })
  
  # --- Perzistence: ukládej na disk po změně ---
  observeEvent(orders(),  ignoreInit = TRUE, { saveRDS(orders(), "orders.rds") })
  observeEvent(hosts_df(), ignoreInit = TRUE, { saveRDS(hosts_df(), "hosts.rds")  })
  
  # --- Storno poslední objednávky ---
  observeEvent(input$undo, {
    od <- orders()
    if (nrow(od) > 0) orders(od[-nrow(od), , drop = FALSE])
  })
  
  # --- Reset (vymaže jen objednávky; hosty nechá) ---
  observeEvent(input$reset, {
    orders(data.frame(
      Time  = as.POSIXct(character()),
      Host  = character(),
      Item  = character(),
      Qty   = integer(),
      Price = numeric(),
      Total = numeric(),
      stringsAsFactors = FALSE
    ))
  })
  
  # --- Výstupy ---
  output$ucty <- renderTable({
    od <- orders()
    h  <- hosts_df()$host
    if (nrow(od) == 0) {
      data.frame(Host = h, Castka = 0, check.names = FALSE)
    } else {
      agg <- aggregate(Total ~ Host, od, sum)
      # doplň i hosty bez objednávky
      miss <- setdiff(h, agg$Host)
      if (length(miss)) {
        agg <- rbind(agg, data.frame(Host = miss, Total = 0))
      }
      agg[order(agg$Host), ]
    }
  }, digits = 0)
  
  output$history <- renderTable({
    od <- orders()
    if (nrow(od) == 0) return(od)
    od[order(od$Time), ]
  }, digits = 0)
  
  output$summary_totals <- renderTable({
    od <- orders()
    data.frame(
      Metrika = c("Počet objednávek", "Počet hostů", "Celkem Kč"),
      Hodnota = c(nrow(od), length(hosts_df()$host), if (nrow(od)) sum(od$Total) else 0)
    )
  }, digits = 0)
  
  # Export CSV (historie)
  output$export <- downloadHandler(
    filename = function() paste0("historie_objednavek_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv"),
    content  = function(file) write.csv(orders(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
}

shinyApp(ui, server)
