library(shiny)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  div(
    style = "max-width: 1000px; margin: 0 auto; padding-top: 30px;",
    
    h1("Lobbying from Within"),
    h4("Interactive companion to the book"),
    
    p(
      "This website provides interactive visualisations and analyses accompanying the chapters of ",
      tags$em("Lobbying from Within: Interest Groups in the Swiss Parliament"),
      "."
    ),
    
    p(
      "Each chapter app allows readers to explore the underlying data and patterns in greater detail."
    ),
    
    hr(),
    
    layout_column_wrap(
      width = 1/2,
      

      card(
        card_header("Chapter 5"),
        card_body(
          p("MPs’ ties to interest groups over time, by parliamentary party group."),
          tags$a(
            href = "https://019dbfcb-8314-5a8b-3e17-49c1b303b983.share.connect.posit.cloud/",
            target = "_blank",
            class = "btn btn-primary",
            "Open Chapter 5 app"
          )
        )
      )
    ),
    
    hr(),
    
    p(
      style = "color: #666666;",
      "Author: Steven Eichenberger"
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
