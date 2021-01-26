#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chaos Game"),

    # Sidebar with a slider input for number of bins 
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
                        max = 100000,
                        value = 30),
            sliderInput("fraction",
                        "Distance between points:",
                        min = 0.01,
                        max = 0.99,
                        value = 0.5),
            actionButton("reset", label = "Reset")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("points.number.output"),
            textOutput("points.fraction.output"),
            plotOutput("chaosGame")
        )
    )
)

## Generation of points
## Generating triangle
generate.triangle <- function (dist) {
    maxLength <- 100000
    # points <- floor (runif (maxLength, 1, 4))
    # generated the random vertices the points will point towards
    vertices <- matrix(NA, ncol = 3, nrow = 3) #reprezenting the vertices and their coords
    vertices[1,] <- c(1, 0, 0)
    vertices[2,] <- c(2, 1, 0)
    vertices[3,] <- c(3, 0.5, sqrt(3)/2) # h = sqrt(0.75) = sqrt(3/4)
    rvertices <- sample(1:3, maxLength, replace = TRUE)
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

generate.square <- function (dist) {
    maxLength <- 100000
    # generated the random vertices the points will point towards
    vertices <- matrix(NA, ncol = 3, nrow = 4) #reprezenting the vertices and their coords
    vertices[1,] <- c(1, 0, 0)
    vertices[2,] <- c(2, 1.0, 0)
    vertices[3,] <- c(3, 0, 1.0) 
    vertices[4,] <- c(4, 1.0, 1.0)
    rvertices <- floor (runif (maxLength, 1, 5))
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

generate.pentagon <- function (dist) {
    maxLength <- 100000
    # generated the random vertices the points will point towards
    vertices <- matrix(NA, ncol = 3, nrow = 5) #reprezenting the vertices and their coords
    vertices[1,] <- c(1, 0.1875, 0) # length of edge ~= 0.625; 0.3125
    vertices[2,] <- c(2, 1 - 0.1875, 0) # secret distance: 0.1275
    vertices[3,] <- c(3, 1.0, 0.6118) 
    vertices[4,] <- c(4, 0, 0.6118)
    vertices[5,] <- c(5, 0.5, 1)
    rvertices <- floor (runif (maxLength, 1, 6))
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
            return (generate.triangle(input$fraction * (input$reset >= 0)))
        }
        if (input$shape == "square") {
            return (generate.square(input$fraction * (input$reset >= 0)))
        }
        if (input$shape == "pentagon") {
            return (generate.pentagon(input$fraction * (input$reset >= 0)))
        }
    })
    
    output$chaosGame <- renderPlot ({
        vertices  <- data.list()[[1]]
        rvertices <- data.list()[[2]]
        coords    <- data.list()[[3]]
        par(mar=c(0.5,0.5,0.5,0.5))
        plot(0,0,xlim=c(0,1),ylim=c(0,1),col=0,
             yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
        points(coords[1:input$points,1],coords[1:input$points,2],pch=20, cex=0.5,col="blue")  
        points(vertices[,2],vertices[,3],pch=20,cex=2,col="red")
        
    })
    output$points.number.output <- renderText({
        paste("Running with ", input$points , " points")
    })
    output$points.fraction.output <- renderText({
        paste("Button value " , input$fraction)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
