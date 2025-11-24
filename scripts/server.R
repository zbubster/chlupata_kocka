## server for shiny

server <- function(input, output, session) {
  
  # --- State of application ---
  # History (Time, guest, Item, Qty, Price, Total)
  orders <- reactiveVal({
    data.frame(
      Time  = as.POSIXct(character()),
      guest  = character(),
      Item  = character(),
      Qty   = numeric(),
      Price = numeric(),
      Total = numeric(),
      stringsAsFactors = FALSE
    )
  })
  
  # Cumulative graph
  output$ucty_plot <- renderPlot({
    od <- orders()
    if (nrow(od) == 0) return(NULL)
    
    # all guests (tab free guests included)
    all_guests <- guests_df()$guest
    
    # time nods = times all separate orders
    time_points <- sort(unique(od$Time))
    
    # kumulativní suma na úrovni (guest, Time) pro skutečné objednávky
    base <- od %>%
      arrange(Time) %>%
      group_by(guest) %>%
      mutate(CumTotal = cumsum(Total)) %>%
      ungroup() %>%
      select(Time, guest, CumTotal)
    
    # kompletní mřížka: každý guest × každý časový bod
    grid <- expand.grid(Time = time_points, guest = all_guests, KEEP.OUT.ATTRS = FALSE) %>%
      as_tibble() %>%
      left_join(base, by = c("Time", "guest")) %>%
      group_by(guest) %>%
      arrange(Time, .by_group = TRUE) %>%
      tidyr::fill(CumTotal, .direction = "down") %>%   # carry-forward
      mutate(CumTotal = ifelse(is.na(CumTotal), 0, CumTotal)) %>%
      ungroup()
    
    # Pokud je jen 1 časový bod, ukážeme body; jinak čáry + body
    if (length(time_points) == 1) {
      ggplot(grid, aes(x = Time, y = CumTotal, color = guest, group = guest)) +
        geom_point(size = 2) +
        labs(x = "Čas", y = "Kumulativní útrata (Kč)", color = "guest") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")
    } else {
      ggplot(grid, aes(x = Time, y = CumTotal, color = guest, group = guest)) +
        geom_line(linewidth = 1) +                 # lineární průběh
        geom_point(size = 1.6) +                   # volitelně body v uzlech
        labs(x = "Čas", y = "Kumulativní útrata (Kč)", color = "guest") +
        scale_x_datetime(date_labels = "%H:%M\n%d.%m.") +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        )
    }
  })
  
  # --- guests list as selfstanding df ---
  guests_df <- reactiveVal(guests_init)
  
  # --- persistence ---
  if (file.exists("temp/orders.rds")) orders(readRDS("temp/orders.rds"))
  if (file.exists("temp/guests.rds"))  guests_df(readRDS("temp/guests.rds"))
  
  # --- Reaktivní update selectInput guest při změně guests_df ---
  observe({
    h <- guests_df()$guest
    sel <- if (!is.null(input$guest) && input$guest %in% h) input$guest else head(h, 1)
    updateSelectInput(session, "guest", choices = h, selected = sel)
  })
  
  # --- Add new guest (case-insensitive) ---
  observeEvent(input$add_guest, {
    nm <- trimws(input$new_guest)
    if (!nzchar(nm)) return()
    d  <- guests_df()
    exists_ci <- tolower(nm) %in% tolower(d$guest)
    if (!exists_ci) {
      d <- rbind(d, data.frame(guest = nm, stringsAsFactors = FALSE))
      guests_df(d)
      updateTextInput(session, "new_guest", value = "")
      updateSelectInput(session, "guest", choices = d$guest, selected = nm)
    } else {
      pick <- d$guest[match(tolower(nm), tolower(d$guest))]
      updateSelectInput(session, "guest", choices = d$guest, selected = pick)
      updateTextInput(session, "new_guest", value = "")
    }
  })
  
  # --- order INPUT ---
  observeEvent(input$add, {
    req(input$guest, input$item, input$qty)
    qty <- as.numeric(input$qty)
    if (is.na(qty) || qty < 0.1) return(NULL)
    
    item_name <- input$item
    price <- menu_df$cena[match(item_name, menu_df$item)]
    if (is.na(price)) price <- 0
    
    # make sure if guest is in guests
    if (!(input$guest %in% guests_df()$guest)) {
      guests_df(rbind(guests_df(), data.frame(guest = input$guest, stringsAsFactors = FALSE)))
    }
    
    new_row <- data.frame(
      Time  = Sys.time(),
      guest  = input$guest,
      Item  = item_name,
      Qty   = qty,
      Price = price,
      Total = round(qty * price, 2),
      stringsAsFactors = FALSE
    )
    orders(rbind(orders(), new_row))

    # reset qty after input confirmed
    updateNumericInput(session, "qty", value = 1)
  })
  
  # --- Perzistence: save to local disc after each change ---
  if (!dir.exists("temp")) dir.create("temp")
  observeEvent(orders(),  ignoreInit = TRUE, { saveRDS(orders(), "temp/orders.rds") })
  observeEvent(guests_df(), ignoreInit = TRUE, { saveRDS(guests_df(), "temp/guests.rds")  })
  
  # --- Redo the last order ---
  observeEvent(input$undo, ignoreInit = TRUE, {
    shinyalert(
      title = "Undo the last input?",
      text  = "Changes made by the last action will be reset..",
      type  = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "YES, redo.",
      cancelButtonText  = "NO, keep it.",
      callbackR = function(ok){
        if (isTRUE(ok)) {
          isolate({
            # deletion itself
            observeEvent(input$undo, {
              od <- orders()
              if (nrow(od) > 0) orders(od[-nrow(od), , drop = FALSE])
            })
          })
          showNotification("Last input deleted.", type = "message")
        }
      }
    )
  })
  
  # --- Reset (delete all tabs; keep guests) ---
  observeEvent(input$reset, ignoreInit = TRUE, {
    shinyalert(
      title = "Do you want to RESET whole app?",
      text  = "Sets all tabs to zero. Irreversible!",
      type  = "error",
      showCancelButton = TRUE,
      confirmButtonText = "YES, reset.",
      cancelButtonText  = "NO",
      callbackR = function(ok){
        if (isTRUE(ok)) {
          isolate({
            # RESET
            observeEvent(input$reset, {
              orders(data.frame(
                Time  = as.POSIXct(character()),
                guest  = character(),
                Item  = character(),
                Qty   = integer(),
                Price = numeric(),
                Total = numeric(),
                stringsAsFactors = FALSE
              ))
            })
          })
          showNotification("All data lost, did you really want to do that?.", type = "warning")
        }
      }
    )
  })
  
  # --- OUTPUTS ---
  output$ucty <- renderTable({
    od <- orders()
    h  <- guests_df()$guest
    if (nrow(od) == 0) {
      data.frame(guest = h, Castka = 0, check.names = FALSE)
    } else {
      agg <- aggregate(Total ~ guest, od, sum)
      # doplň i guesty bez objednávky
      miss <- setdiff(h, agg$guest)
      if (length(miss)) {
        agg <- rbind(agg, data.frame(guest = miss, Total = 0))
      }
      agg[order(agg$guest), ]
    }
  }, digits = 2)
  
  output$history <- renderTable({
    od <- orders()
    if (nrow(od) == 0) return(od)
    od[order(od$Time), ]
  }, digits = 2)
  
  output$summary_totals <- renderTable({
    od <- orders()
    data.frame(
      Metrika = c("Počet objednávek", "Počet guestů", "Celkem Kč"),
      Hodnota = c(nrow(od), length(guests_df()$guest), if (nrow(od)) sum(od$Total) else 0)
    )
  }, digits = 2)
  
  # Export CSV (historie účtů chronologicky)
  output$export <- downloadHandler(
    filename = function() paste0("historie_objednavek_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv"),
    content  = function(file) write.csv(orders(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
}
