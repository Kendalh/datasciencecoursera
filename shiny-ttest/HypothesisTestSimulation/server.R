library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    mu0 <- 0
    mua <- reactive({input$mua})
    sigma <- reactive({input$sigma})
    alpha <- reactive({input$alpha})
    n <- reactive({input$n})
    sd <- reactive({
        input$sigma / sqrt(input$n)
    })
    
    output$plot <- renderPlot({
        # dt
        #custom0 <- function(x) { dt(x - mu0, n-1) }
        #custom1 <- function(x) { dt(x - mua, n-1) }
        
        g <- ggplot(data.frame(mu = c(-4, 4)), aes(x = mu))
        g <- g + stat_function(fun = dnorm, geom = "line", 
                              args = list(mean = mu0, sd = sd()), 
                              size = 1, col = "red")
        g <- g + stat_function(fun = dnorm, geom = "line", 
                              args = list(mean = mua(), sd = sd()), 
                              size = 1, col = "blue")
        z <- zscore() * sd()
        xitc <- ifelse(mua() > 0, mu0 + z, mu0 - z)
        g = g + geom_vline(xintercept = xitc, size = 2)
        g
    })
    
    tstatistic <- reactive({
        abs(mua() - mu0) / sd()
    })
    
    zscore <- reactive({
        qnorm(1 - alpha()/2)
    })
    
    output$statistic <- renderText({
        tstatistic()
    })
    
    output$zscore <- renderText({
        zscore()
    })
    
    output$result <- renderText({
        x <- ifelse(tstatistic() > zscore(), "greater", "smaller")
        y <- ifelse(tstatistic() > zscore(), "reject", "failed to reject")
        paste("As the test statistic is", x, "than the quantile score, we", y, "the Null hypothesis.")
    })
    
    output$cinterval <- renderText({
        mua() + c(-1,1) * zscore() * sd()
    })
    
    output$power <- renderText({
        pnorm(mu0 + zscore() * sd(), mean = mua(), sd = sd(), lower.tail = FALSE)
    })
})
