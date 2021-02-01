#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


## UI SIDE
ui <- fluidPage(
    
    # Application title
    navbarPage("Chaos Game",
                tabsetPanel(
                   tabPanel("Initial Sequence",
                            sidebarLayout(
                                position = "right",
                                sidebarPanel( 
                                    selectizeInput('shape', label = "Shape", choices = c(
                                        'Triangle' = 'triangle',
                                        'Square' = 'square',
                                        'Pentagon' = 'pentagon'
                                    )),
                                    sliderInput("points",
                                                "Number of points:",
                                                min = 1,
                                                max = 100,
                                                value = 1,
                                                step = 1,
                                                animate=animationOptions(interval = 400)
                                    ),
                                    sliderInput("fraction",
                                                "Distance between points:",
                                                min = 0.01,
                                                max = 0.99,
                                                value = 0.5),
                                    actionButton("reset", label = "Reset"),
                                    checkboxInput("consecdifferent", "Consecutives are different", FALSE),
                                    checkboxInput("clockwisedifferent", "Clockwise are different", FALSE)
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel( 
                                    # textOutput("points.number.output"),
                                    # textOutput("points.fraction.output"),
                                    div(
                                        plotOutput("chaosGame1", width = "500px", height = "500px")
                                        
                                    )
                                )
                            )),
                   tabPanel("Mid Sequence",
                            sidebarLayout(
                                position = "right",
                                sidebarPanel( 
                                    selectizeInput('shape', label = "Shape", choices = c(
                                        'Triangle' = 'triangle',
                                        'Square' = 'square',
                                        'Pentagon' = 'pentagon'
                                    )),
                                    sliderInput("points",
                                                "Number of points:",
                                                min = 100,
                                                max = 5000,
                                                value = 100,
                                                step = 25,
                                                animate=animationOptions(interval = 400)
                                    ),
                                    sliderInput("fraction",
                                                "Distance between points:",
                                                min = 0.01,
                                                max = 0.99,
                                                value = 0.5),
                                    actionButton("reset", label = "Reset"),
                                    checkboxInput("consecdifferent", "Consecutives are different", FALSE),
                                    checkboxInput("clockwisedifferent", "Clockwise are different", FALSE)
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel( 
                                    # textOutput("points.number.output"),
                                    # textOutput("points.fraction.output"),
                                    div(
                                        plotOutput("chaosGame2", width = "500px", height = "500px")
                                        
                                    )
                                )
                            )),
                   tabPanel("Complete Sequence",
                            sidebarLayout(
                                position = "right",
                                sidebarPanel( 
                                    selectizeInput('shape', label = "Shape", choices = c(
                                        'Triangle' = 'triangle',
                                        'Square' = 'square',
                                        'Pentagon' = 'pentagon'
                                    )),
                                    sliderInput("points",
                                                "Number of points:",
                                                min = 5000,
                                                max = 50000,
                                                value = 5000,
                                                step = 1000,
                                                animate=animationOptions(interval = 400)
                                    ),
                                    sliderInput("fraction",
                                                "Distance between points:",
                                                min = 0.01,
                                                max = 0.99,
                                                value = 0.5),
                                    actionButton("reset", label = "Reset"),
                                    checkboxInput("consecdifferent", "Consecutives are different", FALSE),
                                    checkboxInput("clockwisedifferent", "Clockwise are different", FALSE)
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel( 
                                    # textOutput("points.number.output"),
                                    # textOutput("points.fraction.output"),
                                    div(
                                        plotOutput("chaosGame3", width = "500px", height = "500px")
                                        
                                    )
                                )
                            ))
               )),
    
    
)

## SERVER SIDE
myRngConsec <- function (myLength = 50000, numb = c(1,3)) {
    y <- rep(NA, myLength)
    prev = 0
    for(i in 1:myLength){
        y[i] = sample(setdiff(numb, prev),1)
        prev = y[i]
    }
    return (y)
}

myRngClockwise<- function (myLength = 50000, numb = c(1,3)) {
    y <- rep(NA, myLength)
    prev = sample(numb, 1)
    y[1] = prev
    for(i in 2:myLength){
        diff = (1 + prev) %% length(numb) 
        if (diff == 0) {
            diff = length(numb)
        }
        y[i] = sample(setdiff(numb, diff),1)
        prev = y[i]
    }
    return (y)
}

## Generation of points
## Generating triangle
generate.triangle <- function (dist, consec, clock) {
    maxLength <- 50000
    # points <- floor (runif (maxLength, 1, 4))
    # generated the random vertices the points will point towards
    vertices <- matrix(NA, ncol = 3, nrow = 3) #reprezenting the vertices and their coords
    vertices[1,] <- c(1, 0, 0)
    vertices[2,] <- c(2, 1, 0)
    vertices[3,] <- c(3, 0.5, sqrt(3)/2) # h = sqrt(0.75) = sqrt(3/4)
    rvertices <- c()
    if (consec == TRUE) {
        rvertices <- myRngConsec(maxLength, c(1:3))
    } else if (clock == TRUE) {
        rvertices <- myRngClockwise(maxLength, c(1:3))
    } else {
        rvertices <- sample(1:3, maxLength, replace = TRUE)
    }
    points <- matrix(NA, ncol = 2, nrow = maxLength)
    colnames(points) <- c("x", "y")
    # letting the first point be completely outside the attractor (IFS will bring it inside eventually)
    points[1,] <- c(runif(1), runif(1)) # generating first random point
    
    for (i in 1:(maxLength - 1)) {
        vert.index <- which(vertices[,1] == rvertices[i]) # getting the vertice of index rvertice[i]
        x.vertice <- vertices[vert.index, 2]
        y.vertice <- vertices[vert.index, 3]
        
        x.new <- points[i, 1] + (x.vertice - points[i, 1]) * dist # add to previous point x
        y.new <- points[i, 2] + (y.vertice - points[i, 2]) * dist # the fraction dist of the distance towards vertice
        
        points [i + 1, ] <- c(x.new, y.new)
    }
    return (list (vertices, rvertices, points))
}

## Generating square

generate.square <- function (dist, consec, clock) {
    maxLength <- 50000
    # generated the random vertices the points will point towards
    vertices <- matrix(NA, ncol = 3, nrow = 4) #reprezenting the vertices and their coords
    vertices[1,] <- c(1, 0, 0)
    vertices[2,] <- c(2, 1.0, 0)
    vertices[3,] <- c(3, 1.0, 1.0)
    vertices[4,] <- c(4, 0, 1.0) 
    rvertices <- c()
    if (consec == TRUE) {
        rvertices <- myRngConsec(maxLength, c(1:4))
    } else if (clock == TRUE) {
        rvertices <- myRngClockwise(maxLength, c(1:4))
    } else {
        rvertices <- floor (runif (maxLength, 1, 5))
        
    }
    points <- matrix(NA, ncol = 2, nrow = maxLength)
    colnames(points) <- c("x", "y")
    # letting the first point be completely outside the attractor (IFS will bring it inside eventually)
    points[1,] <- c(runif(1), runif(1)) # generating first random point
    
    for (i in 1:(maxLength - 1)) {
        vert.index <- which(vertices[,1] == rvertices[i]) 
        x.vertice <- vertices[vert.index, 2]
        y.vertice <- vertices[vert.index, 3]
        
        x.new <- points[i, 1] + (x.vertice - points[i, 1]) * dist # add to previous point x
        y.new <- points[i, 2] + (y.vertice - points[i, 2]) * dist # the fraction dist of the distance towards vertice
        
        points [i + 1, ] <- c(x.new, y.new)
    }
    return (list (vertices, rvertices, points))
}

generate.pentagon <- function (dist, consec, clock) {
    maxLength <- 50000
    # generated the random vertices the points will point towards
    vertices <- matrix(NA, ncol = 3, nrow = 5) #reprezenting the vertices and their coords
    vertices[1,] <- c(1, 0.1875, 0) # length of edge ~= 0.625; 0.3125
    vertices[2,] <- c(2, 1 - 0.1875, 0) # secret distance: 0.1275
    vertices[3,] <- c(3, 1.0, 0.6118) 
    vertices[4,] <- c(4, 0.5, 1)
    vertices[5,] <- c(5, 0, 0.6118)
    rvertices <- c()
    if (consec == TRUE) {
        rvertices <- myRngConsec(maxLength, c(1:5))
    } else if (clock == TRUE) {
        rvertices <- myRngClockwise(maxLength, c(1:5))
    } else {
        rvertices <- floor (runif (maxLength, 1, 6))
        
    }
    points <- matrix(NA, ncol = 2, nrow = maxLength)
    colnames(points) <- c("x", "y")
    # letting the first point be completely outside the attractor (IFS will bring it inside eventually)
    points[1,] <- c(runif(1), runif(1)) # generating first random point
    
    for (i in 1:(maxLength - 1)) {
        vert.index <- which(vertices[,1] == rvertices[i]) 
        x.vertice <- vertices[vert.index, 2]
        y.vertice <- vertices[vert.index, 3]
        
        x.new <- points[i, 1] + (x.vertice - points[i, 1]) * dist # add to previous point x
        y.new <- points[i, 2] + (y.vertice - points[i, 2]) * dist # the fraction dist of the distance towards vertice
        
        points [i + 1, ] <- c(x.new, y.new)
    }
    return (list (vertices, rvertices, points))
}
# Define server logic required to draw a histogram
server <- function(input, output) {

    data.list <- reactive({
        if (input$shape == "triangle") {
            return (generate.triangle(input$fraction * (input$reset >= 0), input$consecdifferent, input$clockwisedifferent ))
        }
        if (input$shape == "square") {
            return (generate.square(input$fraction * (input$reset >= 0), input$consecdifferent, input$clockwisedifferent ))
        }
        if (input$shape == "pentagon") {
            return (generate.pentagon(input$fraction * (input$reset >= 0), input$consecdifferent, input$clockwisedifferent ))
        }
    })
    
    output$chaosGame1 <- renderPlot ({
        vertices  <- data.list()[[1]]
        rvertices <- data.list()[[2]]
        coords    <- data.list()[[3]]
        par(mar=c(0.5,0.5,0.5,0.5))
        plot(0,0,xlim=c(0,1),ylim=c(0,1),col=0,
             yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
        points(coords[1:input$points,1],coords[1:input$points,2],pch=20, cex=0.4,col="black")  
        points(vertices[,2],vertices[,3],pch=20,cex=2,col="red")
        
    })
    output$chaosGame2 <- renderPlot ({
        vertices  <- data.list()[[1]]
        rvertices <- data.list()[[2]]
        coords    <- data.list()[[3]]
        par(mar=c(0.5,0.5,0.5,0.5))
        plot(0,0,xlim=c(0,1),ylim=c(0,1),col=0,
             yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
        points(coords[1:input$points,1],coords[1:input$points,2],pch=20, cex=0.4,col="black")  
        points(vertices[,2],vertices[,3],pch=20,cex=2,col="red")
        
    })
    output$chaosGame3 <- renderPlot ({
        vertices  <- data.list()[[1]]
        rvertices <- data.list()[[2]]
        coords    <- data.list()[[3]]
        par(mar=c(0.5,0.5,0.5,0.5))
        plot(0,0,xlim=c(0,1),ylim=c(0,1),col=0,
             yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
        points(coords[1:input$points,1],coords[1:input$points,2],pch=20, cex=0.4,col="black")  
        points(vertices[,2],vertices[,3],pch=20,cex=2,col="red")
        
    })
    # output$points.number.output <- renderText({
    #     paste("Running with ", input$points , " points")
    # })
    # output$points.fraction.output <- renderText({
    #     paste("Button value " , input$fraction)
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
