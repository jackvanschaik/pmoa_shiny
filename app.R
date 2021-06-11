# Imports ######################################################################
library(shiny)
library(shinydashboard)
library(readr)
library(reactable)
library(stringr)
library(dplyr)

# Preprocessing ################################################################
pmoa_dict <- read_csv("pmoa_dict.csv")
hgnc_2 <- readRDS("hgnc_2.rds")
hgnc_res <<- hgnc_2

# UI Definition ################################################################
ui <- dashboardPage(
    dashboardHeader(title = "Gene Webpage"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Gene Search", tabName = "search", icon = icon("search")),
            menuItem("Plots", tabName = "plot", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "search",
                    fluidRow(
                        box(width = 12,
                            textInput("srch_txt", "Find a gene", value = ""),
                            checkboxInput("srch_pain", "Pain genes only"),
                            textOutput("txt_out"),
                            reactableOutput("tbl_out"),
                            verbatimTextOutput("selected")
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            reactableOutput("res_out")
                        )
                    )
            ),
            tabItem(tabName = "plot",
                    fluidRow(
                        box(

                        )
                    )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    selected <- reactive(getReactableState("tbl_out", "selected"))

    output$selected <- renderPrint({
        sel <- selected()
        if (is.null(sel)) {
            txt <- ""
        }
        else {
            symbol <- hgnc_res$gene_name[sel]
            cat(sel)
            txt <- symbol
        }
        print(txt)
    })

    output$res_out <- renderReactable({
        sel <- selected()
        res <- NULL
        if (!is.null(sel)) {
            symbol <- hgnc_res$gene_name[sel]
            res <- filter(pmoa_dict, tolower(symbol) == token)
        }
        reactable(res)
    })

    output$tbl_out <- renderReactable({
        q <- input$srch_txt

        q_reg <- paste0("(?i)", tolower(q))

        # do the search
        hgnc_res <<- filter(hgnc_2, gene_name == toupper(q) | str_detect(long_name, q_reg))
        if (input$srch_pain == TRUE) {
            hgnc_res <<- filter(hgnc_res, pain_flag == TRUE)
        }
        reactable(hgnc_res, selection = "single", onClick = "select",
                  defaultPageSize = 4)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
