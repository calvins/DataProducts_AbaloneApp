# Code that you put before shinyServer in the server.R function gets called once when you do runApp()
# 
#load packages
library(shiny)
library(MASS)
library(rms)
library(plotrix)

# vector of fortunes to use
fortunes <- c('Today it\'s up to you to create the peacefulness you long for.','A friend asks only for your time not your money.','If you refuse to accept anything but the best, you very often get it.','A smile is your passport into the hearts of others.',
       'A good way to keep healthy is to eat more Chinese food.','Your high-minded principles spell success.','Hard work pays off in the future, laziness pays off now.',
       'Change can hurt, but it leads a path to something better.','Enjoy the good luck a companion brings you.')

# load data and process
abnames = c('sex','length','diameter','height','weight.w','weight.s','weight.v','weight.sh','rings')
abalone = read.csv("data/abalone.data", header = F , sep = ',', col.names = abnames)
abalone$height[abalone$height==0] = NA
abalone$sex = as.character(abalone$sex)
abalone$sex[abalone$sex != 'I'] = 'K'
abalone$sex = as.factor(abalone$sex)

abalone$weight.diff = abalone$weight.w - 
    (abalone$weight.v + abalone$weight.s + abalone$weight.sh)
abalone$height[2052] = 0.130
abalone$weight.mean1 = (abalone$weight.s*abalone$weight.v*abalone$weight.sh)^(1/3)
abalone$weight.mean2 = (abalone$weight.w*abalone$weight.s*abalone$weight.sh*abalone$weight.v)^(1/4)
abalone$weight.norm1 = sqrt(abalone$weight.s^2 + abalone$weight.v^2 + abalone$weight.sh^2)
abalone$weight.norm2 = sqrt(abalone$weight.w^2 + abalone$weight.s^2 + abalone$weight.v^2 + abalone$weight.sh^2)
abalone$size.norm = sqrt(abalone$length^2 + abalone$diameter^2 + abalone$height^2) # Norm of vectors
abalone$size.mean = (abalone$length*abalone$diameter*abalone$height)^(1/3)         # Geometric Mean

# best fit multiple regression model
abfit7.2 = lm(log(rings) ~ sex + log(size.mean) + weight.s + weight.sh, data = abalone)

# Test case
# model log of rings
# sex redefined as infant/non-infant sex = K
# size.mean = mean of interaction term of length, diameter, and height meanSize 0.38
# shuckedWeight 0.45
# shell weight 0.35
# exp(p) 12.47249

# prediction function
predictAbaloneAge <- function(sex,meanSize,shuckedWeight,shellWeight) {
    if (sex == 1) {
        sexInput = "I"    
    }
    if (sex == 2) {
        sexInput = "K"    
    }
    p <- predict(abfit7.2, newdata = data.frame(sex = sexInput, size.mean = meanSize, weight.s = shuckedWeight, weight.sh = shellWeight))
    result <- round(exp(p),0)
    result
}

# get fortune function
getFortune <- function () {

    return(cat(fortunes[sample(1:9,1)]))

}

# drawRings function
drawRings <- function(rings) {
  # set up a blank plot
  x<-c(1,2,3,4,5,6,7); y<-c(8.3,10.3,19.0,16,0,15.6,19.8)
  plot(x,y,type="n",axes=FALSE,xlab="",ylab="")

  ringRadius <- 2.5
  ringColor <- rings
  for (i in 1:rings) {
    ringRadius <- ringRadius - 0.02
    ringColor <- ringColor - 1
    draw.circle(4,10,ringRadius,border="blue",col=ringColor)
  }
  draw.circle(4,10,ringRadius-.05,border="blue",col=1)
}

# Code inside the unnamed function of shinyServer(function(input, output){ but not in a reactive statement 
#   will run once for every new user (or page refresh)
# 
#   Code in reactive functions of shinyServer get run repeatedly as needed when new values are entered
# (reactive functions are those like render*)

pp <- reactiveValues()

shinyServer(
    function(input, output) {

        predictionReactive <- eventReactive(input$goButton, {
            pp$a <- predictAbaloneAge(input$infantNonInfant,input$meanSize,input$shuckedWeight,input$shellWeight)

        })
        
        fortuneReactive <- eventReactive(input$goButton, {
            getFortune()    
        })
        
        newRingsReactive <- eventReactive(input$goButton, {
            
            drawRings(isolate(pp$a))
        })
        
        output$inputInfant <- renderText({input$infantNonInfant})
        output$inputMeanSize <- renderText({input$meanSize})
        output$inputShuckedWeight <- renderText({input$shuckedWeight})
        output$inputShellWeight <- renderText({input$shellWeight})
        output$prediction <- renderText({
            predictionReactive()
        })
        
        output$fortune <- renderPrint({
            fortuneReactive()
        })
        
        output$newRings <- renderPlot({
            newRingsReactive()
        },width=640,height=640)
    }
)
