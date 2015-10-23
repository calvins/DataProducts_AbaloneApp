#load packages
library(shiny)
library(MASS)
library(rms)
library(plotrix)

# vector of fortunes to use
fortunes <- c(
    "Today it\'s up to you to create the peacefulness you long for.",
    "A friend asks only for your time not your money.",
    "If you refuse to accept anything but the best, you very often get it.",
    "A smile is your passport into the hearts of others.",
    "A good way to keep healthy is to eat more Chinese food.",
    "Your high-minded principles spell success.",
    "Hard work pays off in the future, laziness pays off now.",
    "Change can hurt, but it leads a path to something better.",
    "Enjoy the good luck a companion brings you.",
    "People are naturally attracted to you.",
    "Hidden in a valley beside an open stream- This will be the type of place where you will find your dream.",
    "A chance meeting opens new doors to success and friendship.",
    "You learn from your mistakes... You will learn a lot today.",
    "If you have something good in your life, don\'t let it go!",
    "What ever you\'re goal is in life, embrace it visualize it, and for it will be yours.",
    "Your shoes will make you happy today.",
    "You cannot love life until you live the life you love.",
    "Be on the lookout for coming events; They cast their shadows beforehand.",
    "Land is always on the mind of a flying bird.",
    "The man or woman you desire feels the same about you.",
    "Meeting adversity well is the source of your strength.",
    "A dream you have will come true.",
    "Our deeds determine us, as much as we determine our deeds.",
    "Never give up. You\'re not a failure if you don\'t give up.",
    "You will become great if you believe in yourself.",
    "There is no greater pleasure than seeing your loved ones prosper.",
    "You will marry your lover.",
    "A very attractive person has a message for you.",
    "You already know the answer to the questions lingering inside your head.",
    "It is now, and in this world, that we must live.",
    "You must try, or hate yourself for not trying.",
    "You can make your own happiness.",
    "The greatest risk is not taking one.",
    "The love of your life is stepping into your planet this summer.",
    "Love can last a lifetime, if you want it to.",
    "Adversity is the parent of virtue.",
    "Serious trouble will bypass you.",
    "A short stranger will soon enter your life with blessings to share.",
    "Now is the time to try something new.",
    "Wealth awaits you very soon.",
    "If you feel you are right, stand firmly by your convictions.",
    "If winter comes, can spring be far behind?",
    "Keep your eye out for someone special.",
    "You are very talented in many ways.",
    "A stranger, is a friend you have not spoken to yet.",
    "A new voyage will fill your life with untold memories.",
    "You will travel to many exotic places in your lifetime.",
    "Your ability for accomplishment will follow with success.",
    "Nothing astonishes men so much as common sense and plain dealing.",
    "Its amazing how much good you can do if you dont care who gets the credit.",
    "Everyone agrees. You are the best.",
    "LIFE CONSIST NOT IN HOLDING GOOD CARDS, BUT IN PLAYING THOSE YOU HOLD WELL.",
    "Jealousy doesn\'t open doors, it closes them!",
    "It\'s better to be alone sometimes.",
    "When fear hurts you, conquer it and defeat it!",
    "Let the deeds speak.",
    "You will be called in to fulfill a position of high honor and responsibility.",
    "The man on the top of the mountain did not fall there.",
    "You will conquer obstacles to achieve success.",
    "Joys are often the shadows, cast by sorrows.",
    "Fortune favors the brave.",
    "An upward movement initiated in time can counteract fate.",
    "A journey of a thousand miles begins with a single step.",
    "Sometimes you just need to lay on the floor.",
    "Never give up. Always find a reason to keep trying.",
    "If you have something worth fighting for, then fight for it.",
    "Stop wishing. Start doing.",
    "Accept your past without regrets. Handle your present with confidence. Face your future without fear.",
    "Stay true to those who would do the same for you.",
    "Ask yourself if what you are doing today is getting you closer to where you want to be tomorrow.",
    "Happiness is an activity.",
    "Help is always needed but not always appreciated. Stay true to your heart and help those in need weather they appreciate it or not.",
    "Hone your competitive instincts.",
    "Finish your work on hand don\'t be greedy.",
    "For success today, look first to yourself.",
    "Your fortune is as sweet as a cookie.",
    "Integrity is the essence of everything successful.",
    "If you\'re happy, you\'re successful.",
    "You will always be surrounded by true friends",
    "Believing that you are beautiful will make you appear beautiful to others around you.",
    "Happinees comes from a good life.",
    "Before trying to please others think of what makes you happy.",
    "When hungry, order more Chinese food.",
    "Your golden opportunity is coming shortly.",
    "For hate is never conquered by hate. Hate is conquered by love .",
    "You will make many changes before settling down happily.",
    "A man is born to live and not prepare to live.",
    "You cannot become rich except by enriching others.",
    "Don\'t pursue happiness - create it.",
    "You will be successful in love.",
    "All your fingers can\'t be of the same length.",
    "Wise sayings often fall on barren ground, but a kind word is never thrown away.",
    "A lifetime of happiness is in store for you.",
    "It is very possible that you will achieve greatness in your lifetime.",
    "Be tactful; overlook your own opportunity.",
    "You are the controller of your destiny.",
    "Everything happens for a reson.",
    "How can you have a beutiful ending without making beautiful mistakes.",
    "You can open doors with your charm and patience.",
    "Welcome the change coming into your life."
)

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

    return(cat(fortunes[sample(1:100,1)]))

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

# place to hold predicted age to pass to draw rings
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
        },width=550,height=550)
    }
)
