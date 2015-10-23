library(shiny)

shinyUI(
    pageWithSidebar(
        headerPanel('Predict Age of Abalone'),
        sidebarPanel(
            img(src="AbaloneMeatByJackLikins.jpg",height=175,width=245),
            a("Documentation", href="doc.html", target="_blank"),
            h3('Inputs'),
            radioButtons("infantNonInfant", label = "Infant/Not-Infant",
                         choices = list("Infant" = 1, "Non-Infant" = 2), selected = 1),
            numericInput('meanSize','Mean Size (0.1 to 1)',NULL,min=0.1, max=1, step=0.1),
            numericInput('shuckedWeight','Shucked Weight (0.1 to 1.5)',NULL,min=0.1, max=1.5, step=0.1),
            numericInput('shellWeight','Shell Weight (0.1 to 1.1)',NULL,min=0.1, max=1.1, step=0.1),
            actionButton('goButton','Go!')
        ),
        mainPanel(
            h4('Predicted age'),
            verbatimTextOutput("prediction"),
            h4('Your fortune'),
            verbatimTextOutput("fortune"),
            h4('Your abalone'),
            plotOutput("newRings")
        )
    )
)
