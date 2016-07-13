shinyUI(navbarPage("Visualizing 2014 SF Crime Data",
                   tabPanel('Univariate Exploration',
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("vars",label = strong("Choose Variable"),
                                            choices = list('Type of Crime' = 'Category','Month','Time of Day' = 'Times','Police District' = 'PdDistrict'),
                                            selected = 'Category')
                                , width = 3),
                              mainPanel(
                              	h3('Graphical Summary'),
                                plotOutput('plot1',  width = "100%"),
                                h3('Numerical Summary'),'\n',
                                textOutput('sum11'),'\n',
                                textOutput('sum12')
                              )
                              
                            )),
                   tabPanel('Bivariate Exploration',
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("vars1",label = strong("Choose First Variable"),
                                            choices = list('Type of Crime' = 'Category','Month','Time of Day' = 'Times','Police District' = 'PdDistrict'),
                                            selected = 'Category'),
                                selectInput("vars2",label = strong("Choose Second Variable"),
                                            choices = list('Type of Crime' = 'Category','Month','Time of Day' = 'Times','Police District' = 'PdDistrict'),
                                            selected = 'Month')
                              , width = 3),
                              mainPanel(
                              	h3('Graphical Summary'),
                                plotOutput('plot2',  width = "100%")
                              # textOutput('sum21')
                              )
                              
                            ))

  
  
  ))