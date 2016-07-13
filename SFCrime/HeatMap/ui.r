# ui.R

#paste("list('All Crimes' = 'ALL CRIMES',",paste(paste("'",levels(df.sf_crime$Category),"' = '",levels(df.sf_crime$Category),"'",sep=''),collapse=','), ")",sep='')

shinyUI(fluidPage(
  titlePanel("San Francisco Crime Data"),
  
  sidebarLayout(
    sidebarPanel(
    
      helpText("Select different categories to explore the data"),
    
      selectInput("crime", label = "Crime Category", 
        choices = list('All Crimes' = 'ALL CRIMES','ASSAULT' = 'ASSAULT','BURGLARY' = 'BURGLARY','DRUG/NARCOTIC' = 'DRUG/NARCOTIC','DRUNKENNESS' = 'DRUNKENNESS','FORGERY/COUNTERFEITING' = 'FORGERY/COUNTERFEITING','FRAUD' = 'FRAUD','LARCENY/THEFT' = 'LARCENY/THEFT','MISSING PERSON' = 'MISSING PERSON','NON-CRIMINAL' = 'NON-CRIMINAL','OTHER OFFENSES' = 'OTHER OFFENSES','ROBBERY' = 'ROBBERY','SECONDARY CODES' = 'SECONDARY CODES','SEX OFFENSES, FORCIBLE' = 'SEX OFFENSES, FORCIBLE','STOLEN PROPERTY' = 'STOLEN PROPERTY','SUSPICIOUS OCC' = 'SUSPICIOUS OCC','TRESPASS' = 'TRESPASS','VANDALISM' = 'VANDALISM','VEHICLE THEFT' = 'VEHICLE THEFT','WARRANTS' = 'WARRANTS','WEAPON LAWS' = 'WEAPON LAWS'),
        selected = 'ALL CRIMES'),
        
        dateRangeInput("dates", 
                       "Date range",
        start = "2014-01-01", 
        end = "2014-12-31",min = "2014-01-01",max="2014-12-31",format = "mm/dd/yyyy"),
                       
      
      selectInput("day", label = "Day of the week", 
        choices = list("All days" = "All days", "Monday" = "Monday", "Tuesday" = "Tuesday", "Wednesday" = "Wednesday", "Thursday" = "Thursday", "Friday" = "Friday", "Saturday" = "Saturday", "Sunday" = "Sunday"), selected = "All days"),
                       
    
     
      sliderInput("time", label = "Time of day (24 h)",
        min = 0, max = 24, value = c(0,24)),
        
        
      selectInput("district", label = "Police District", 
        choices = list("All districts" = "All districts" ,'BAYVIEW' = 'BAYVIEW','CENTRAL' = 'CENTRAL','INGLESIDE' = 'INGLESIDE','MISSION' = 'MISSION','NORTHERN' = 'NORTHERN','PARK' = 'PARK','RICHMOND' = 'RICHMOND','SOUTHERN' = 'SOUTHERN','TARAVAL' = 'TARAVAL','TENDERLOIN' = 'TENDERLOIN'), selected = "All districts" )   
   
     ), 
      mainPanel(
        plotOutput("map")
        )
  )))