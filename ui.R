library(shiny)

shinyUI(
    pageWithSidebar(
        headerPanel('Predict Age of Abalone'),
        sidebarPanel(
            h3('Inputs'),
            radioButtons("infantNonInfant", label = "Infant/Non-infant",
                         choices = list("Infant" = 1, "Non-Infant" = 2), selected = 1),
            numericInput('meanSize','Mean Size',NULL,min=0.1, max=1, step=0.1),
            numericInput('shuckedWeight','Shucked Weight',NULL,min=0.1, max=1.5, step=0.1),
            numericInput('shellWeight','Shell Weight',NULL,min=0.1, max=1.1, step=0.1),
            actionButton('goButton','Go!')
        ),
        mainPanel(
            h4('You entered'),
            verbatimTextOutput("inputInfant"),
            verbatimTextOutput("inputMeanSize"),
            verbatimTextOutput("inputShuckedWeight"),
            verbatimTextOutput("inputShellWeight"),
            h4('Which resulted in a prediction of '),
            verbatimTextOutput("prediction"),
            h4('Your fortune'),
            verbatimTextOutput("fortune"),
            plotOutput("newRings")
        )
    )
)
