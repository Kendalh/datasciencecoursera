library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Hypothesis Test Explanation"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("sigma", "Sigma", min = 1, max = 10, value = 4),
            sliderInput("mua", "Mean", min = -3, max = 3, value = 1, step = 0.5),
            sliderInput("alpha", "Alpha", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
            numericInput("n", "Sample Size", value = 100)
        ),

        mainPanel(
            h3("The Null Hypothesis is assuming the population follows the Gaussian distribution centered at zero.
               The observations are simulated with sample mean and sigma as inputed from the sidebar, 
               can we reject the null hypothesis and accept the alternative as the distribution is not centered at zero?"),
            p("Red curve is the distribution for the null hypothesis; 
              Blue curve is the distrbution for the observations. " ),
            plotOutput("plot"),
            p("The test statistics is ", textOutput("statistic", inline = TRUE)),
            p("Given the alpha value, the two-sided test score is ", textOutput("zscore", inline = TRUE)),
            p("The confidence interval is (", textOutput("cinterval", inline = TRUE), ")"),
            p(textOutput("result")),
            p("The power of this test is ", textOutput("power", inline = TRUE)),
            
        )
    )
))
