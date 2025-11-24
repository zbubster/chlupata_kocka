## ui for shiny

ui <- fluidPage(
  titlePanel("GATO ESTÚPIDO"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Order:"),
      selectInput("guest", "Select guest:", choices = guests_init$guest),
      selectInput("item", "Item:", choices = menu_choices),
      numericInput("qty", "Quantity:", value = 1, min = 0.1, step = 1),
      actionButton("add", "Add to the tab", class = "btn-primary"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
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
        tabPanel("Tabs (total)",
                 tableOutput("ucty")),
        tabPanel("Orders history",
                 tableOutput("history")),
        tabPanel("Summary",
                 tableOutput("summary_totals")),
        tabPanel("Graph",
                 plotOutput("ucty_plot", height = "700px"))
      )
    )
  )
)

