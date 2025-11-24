## ui for shiny

ui <- fluidPage(
  titlePanel("Chlupatá kočka"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Objednávka:"),
      selectInput("guest", "Vyber guesta:", choices = guests_init$guest),
      selectInput("item", "Položka:", choices = menu_choices),
      numericInput("qty", "Množství:", value = 1, min = 0.1, step = 1),
      actionButton("add", "Přidat na účet", class = "btn-primary"),
      tags$hr("#-#-#-#-#-#-#-#-#-#-#-#"),
      tags$h4("Přidat účet"),
      fluidRow(
        column(8, textInput("new_guest", "Jméno:", value = "")),
        column(8, actionButton("add_guest", "Přidat"))
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

