ui <- fluidPage(
  titlePanel("GATO ESTÚPIDO"),
  
  sidebarLayout(
    sidebarPanel(
      # --- bez objednávky ---
      tags$h4("Add guest"),
      fluidRow(
        column(8, textInput("new_guest", "Name:", value = "")),
        column(8, actionButton("add_guest", "Confirm"))
      ),
      tags$h3("DANGER ZONE", style = "color: red; font-weight: bold"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
      tags$h4("Storno"),
      actionButton("undo", "Undo the last action", class = "btn-warning"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
      tags$h4("Export"),
      downloadButton("export", "Data export"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
      tags$h4("Reset"),
      actionButton("reset", "Delete all", class = "btn-danger")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Order",
          
          # --- schované původní selectInputy, které pořád živí input$guest a input$item ---
          tags$head(
            tags$style(HTML("
              .hidden-inputs { display: none; }
            "))
          ),
          
          div(
            class = "hidden-inputs",
            selectInput("guest", "Select guest:", choices = guests_init$guest),
            selectInput("item",  "Item:",         choices = menu_choices)
          ),
          
          # --- viditelné UI ovládání ---
          tags$h3("Order"),
          
          tags$h4("Select guest"),
          uiOutput("guest_buttons"),
          
          tags$hr(),
          
          tags$h4("Select item"),
          uiOutput("item_buttons"),
          
          tags$hr(),
          
          tags$h5(textOutput("order_preview")),
          
          fluidRow(
            column(
              4,
              numericInput("qty", "Quantity:", value = 1, min = 0.1, step = 0.1)
            ),
            column(
              4,
              actionButton("add", "Add to the tab", class = "btn-primary", width = "100%")
            )
          )
        ),
        
        tabPanel("Tabs (total)",    tableOutput("ucty")),
        tabPanel("Orders history",  tableOutput("history")),
        tabPanel("Summary",         tableOutput("summary_totals")),
        tabPanel("Graph",           plotOutput("ucty_plot", height = "700px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- Stav aplikace: historie objednávek ---
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
  
  # --- Seznam guestů jako samostatný df ---
  guests_df <- reactiveVal(guests_init)
  
  # --- Persistence (load) ---
  if (file.exists("temp/orders.rds")) orders(readRDS("temp/orders.rds"))
  if (file.exists("temp/guests.rds"))  guests_df(readRDS("temp/guests.rds"))
  
  # --- Cumulative graph ---
  output$ucty_plot <- renderPlot({
    od <- orders()
    if (nrow(od) == 0) return(NULL)
    
    all_guests  <- guests_df()$guest
    time_points <- sort(unique(od$Time))
    
    base <- od %>%
      arrange(Time) %>%
      group_by(guest) %>%
      mutate(CumTotal = cumsum(Total)) %>%
      ungroup() %>%
      select(Time, guest, CumTotal)
    
    grid <- expand.grid(Time = time_points, guest = all_guests, KEEP.OUT.ATTRS = FALSE) %>%
      as_tibble() %>%
      left_join(base, by = c("Time", "guest")) %>%
      group_by(guest) %>%
      arrange(Time, .by_group = TRUE) %>%
      tidyr::fill(CumTotal, .direction = "down") %>%
      mutate(CumTotal = ifelse(is.na(CumTotal), 0, CumTotal)) %>%
      ungroup()
    
    if (length(time_points) == 1) {
      ggplot(grid, aes(x = Time, y = CumTotal, color = guest, group = guest)) +
        geom_point(size = 2) +
        labs(x = "Čas", y = "Kumulativní útrata (Kč)", color = "guest") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")
    } else {
      ggplot(grid, aes(x = Time, y = CumTotal, color = guest, group = guest)) +
        geom_line(linewidth = 1) +
        geom_point(size = 1.6) +
        labs(x = "Čas", y = "Kumulativní útrata (Kč)", color = "guest") +
        scale_x_datetime(date_labels = "%H:%M\n%d.%m.") +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        )
    }
  })
  
  # --- Udržování selectInput("guest") v synchronizaci s guests_df ---
  observe({
    h <- guests_df()$guest
    if (length(h) == 0) return()
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
  
  # =====================================================================
  #  TLAČÍTKA PRO VÝBĚR GUESTA A ITEMU (bez zvýraznění)
  # =====================================================================
  
  # --- Guest buttons ---
  output$guest_buttons <- renderUI({
    guests <- guests_df()$guest
    if (length(guests) == 0) {
      return(tags$em("No guests yet – add one in the sidebar."))
    }
    
    btns <- lapply(seq_along(guests), function(i) {
      g <- guests[i]
      actionButton(
        inputId = paste0("guest_btn_", i),
        label   = g,
        class   = "btn btn-outline-primary"
      )
    })
    
    do.call(flowLayout, btns)
  })
  
  observe({
    guests <- guests_df()$guest
    lapply(seq_along(guests), function(i) {
      g  <- guests[i]
      id <- paste0("guest_btn_", i)
      
      observeEvent(input[[id]], {
        updateSelectInput(session, "guest", selected = g)
      }, ignoreInit = TRUE)
    })
  })
  
  # --- Item buttons ---
  output$item_buttons <- renderUI({
    items <- menu_df$item
    if (length(items) == 0) {
      return(tags$em("Menu is empty."))
    }
    
    btns <- lapply(seq_along(items), function(i) {
      it    <- items[i]
      label <- paste0(it, " (", menu_df$cena[i], " Kč)")
      
      actionButton(
        inputId = paste0("item_btn_", i),
        label   = label,
        class   = "btn btn-outline-secondary"
      )
    })
    
    do.call(flowLayout, btns)
  })
  
  observe({
    items <- menu_df$item
    lapply(seq_along(items), function(i) {
      it <- items[i]
      id <- paste0("item_btn_", i)
      
      observeEvent(input[[id]], {
        updateSelectInput(session, "item", selected = it)
      }, ignoreInit = TRUE)
    })
  })
  
  # =====================================================================
  #  ORDER PREVIEW ŘÁDEK (guest, item, qty)
  # =====================================================================
  
  output$order_preview <- renderText({
    g  <- input$guest
    it <- input$item
    q  <- input$qty
    
    if (is.null(g) || g == "" || is.null(it) || it == "" || is.null(q) || is.na(q)) {
      return("Order: (nothing selected yet)")
    }
    
    paste0("Order: ", g, " – ", it, " – qty: ", q)
  })
  
  # =====================================================================
  #  ORDER INPUT
  # =====================================================================
  
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
    
    # reset qty
    updateNumericInput(session, "qty", value = 1)
    
    # reset výběr guest & item na první hodnotu (možné i nechat, když bys chtěl)
    h <- guests_df()$guest
    if (length(h) > 0) {
      updateSelectInput(session, "guest", selected = h[1])
    }
    itms <- menu_df$item
    if (length(itms) > 0) {
      updateSelectInput(session, "item", selected = itms[1])
    }
  })
  
  # --- Perzistence: save to local disc after each change ---
  if (!dir.exists("temp")) dir.create("temp")
  observeEvent(orders(),    ignoreInit = TRUE, { saveRDS(orders(),   "temp/orders.rds") })
  observeEvent(guests_df(), ignoreInit = TRUE, { saveRDS(guests_df(), "temp/guests.rds") })
  
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
  
  # --- Export CSV ---
  output$export <- downloadHandler(
    filename = function() paste0("historie_objednavek_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv"),
    content  = function(file) write.csv(orders(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
}

