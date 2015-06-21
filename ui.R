library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Energy Star Model Validation"),
    sidebarPanel(
        selectInput("DataSet",label =h3("Please select data set"),
                    choices = list("Office" =  1, "Grocery" = 2)),
        sliderInput(inputId = "training_percent",label = "Training set proportion",value = 0.5,min = 0.5,max = 0.9,step = 0.1)
        ),
    mainPanel(
        h1("Introduction"),
        p("This app apply Energy Star's building benmarching score model to actual building energy consumption data
          and give user a brief understanding of how the model Energy Star's use to predict building consumption energy can be
          so unreliable."),
        h1("Data Source"),
        p("Building energy consumption data comes from U.S. Energy Information Administration. http://www.eia.gov/consumption/commercial/"),
        p("If you are interested in Energy Star's model, please go to http://aceee.org/files/proceedings/2014/data/papers/3-725.pdf#page=1"),
        h1("What does this app do?"),
        p("We partition the data set into training set and testing set in a proportion specified by user. 
          Then we fit linear model in training set using methodology similar to what Energy Star uses. In the end we apply this
          model to testing set then calculate the R square in testing set. This process is repeated 1000 times with training set and
          testing set resample. As you might see, for a lot of time, R squares in testing set are negative, which signaling
          that the model is worse than fitting a constant. Try grocery data, they are the worst."),
        p("Please wait for a few second before the graphs show up."),
        h1("R square histogram"),
        plotOutput('r2hist')
    )
))