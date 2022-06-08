library(plotly)
library(readxl)
GSSData <- read_excel("GSSData.xlsx")

#x and y axis labels

x <- list(title = "Years")
y <- list(title = "Happiness State By Percent")

x2 <- list(title = "Happiness State")
y2 <- list(title = "Number of Responses")

#data

data <- as.data.frame(GSSData)
row.names(data) <- c("Very Happy","Pretty Happy","Not Too Happy")

years <- as.numeric(colnames(data))
state <- rownames(data)

vhap <- data[1,]
phap <- data[2,]
nthap <- data[3,]

linedata <- t(t(data)/colSums(data))
linedata <- round(linedata*100, digits=1)


#-------------------------------------------------------------------------
#Begin Shiny

#UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Happiness Levels Since 1972"),
  p("(Data is from the General Social Survey)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "select_year",
                  label = "Choose a year to construct a bar chart",
                  choices = years,
                  selected = 1972
                  
      )                
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      plotlyOutput("distHist"),
    )
  )
)

# Interactions
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    # draw the line plot
    
    data <- data.frame(years, linedata)
    fig <- plot_ly(
      data, 
      x = ~years,
      y = ~linedata[1,],
      name = 'Very Happy',
      type = 'scatter',
      mode = 'lines')
    fig <- fig %>% add_trace(
      y = ~linedata[2,], 
      name = 'Pretty happy', 
      type = 'scatter', 
      mode = 'lines')
    fig <- fig %>% add_trace(
      y=~linedata[3,], 
      name = 'Not too happy', 
      type = 'scatter', 
      mode = 'lines')
    fig <- fig %>% layout(xaxis = x, yaxis = y, title = "Happiness States Over Time")
  })
  
  output$distHist <- renderPlotly({
    if(input$select_year == 1972){
      bar = plot_ly(x = state, y = data[1:3,1], name = '1972', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2, 
                     title = "Comparison Between Happiness States in 1972")}
    
    else if(input$select_year == 1973){
      bar = plot_ly(x = state, y = data[1:3,2], name = '1973', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1973")}
    
    else if(input$select_year == 1974){
      bar = plot_ly(x = state, y = data[1:3,3], name = '1974', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1974")}
    
    else if(input$select_year == 1975){
      bar = plot_ly(x = state, y = data[1:3,4], name = '1975', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1975")}
    
    else if(input$select_year == 1976){
      bar = plot_ly(x = state, y = data[1:3,5], name = '1976', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1976")}
    
    else if(input$select_year == 1977){
      bar = plot_ly(x = state, y = data[1:3,6], name = '1977', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1977")}
    
    else if(input$select_year == 1978){
      bar = plot_ly(x = state, y = data[1:3,7], name = '1978', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1978")}
    
    else if(input$select_year == 1980){
      bar = plot_ly(x = state, y = data[1:3,8], name = '1980', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1980")}
    
    else if(input$select_year == 1982){
      bar = plot_ly(x = state, y = data[1:3,9], name = '1982', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1982")}
    
    else if(input$select_year == 1983){
      bar = plot_ly(x = state, y = data[1:3,10], name = '1983', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1983")}
    
    else if(input$select_year == 1984){
      bar = plot_ly(x = state, y = data[1:3,11], name = '1984', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1984")}
    
    else if(input$select_year == 1985){
      bar = plot_ly(x = state, y = data[1:3,12], name = '1985', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1985")}
    
    else if(input$select_year == 1986){
      bar = plot_ly(x = state, y = data[1:3,13], name = '1986', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1986")}
    
    else if(input$select_year == 1987){
      bar = plot_ly(x = state, y = data[1:3,14], name = '1987', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1987")}
    
    else if(input$select_year == 1988){
      bar = plot_ly(x = state, y = data[1:3,15], name = '1988', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1988")}
    
    else if(input$select_year == 1989){
      bar = plot_ly(x = state, y = data[1:3,16], name = '1989', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1989")}
    
    else if(input$select_year == 1990){
      bar = plot_ly(x = state, y = data[1:3,17], name = '1990', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1990")}
    
    else if(input$select_year == 1991){
      bar = plot_ly(x = state, y = data[1:3,18], name = '1991', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1991")}
    
    else if(input$select_year == 1993){
      bar = plot_ly(x = state, y = data[1:3,19], name = '1993', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1993")}
    
    else if(input$select_year == 1994){
      bar = plot_ly(x = state, y = data[1:3,20], name = '1994', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1994")}
    
    else if(input$select_year == 1996){
      bar = plot_ly(x = state, y = data[1:3,21], name = '1996', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1996")}
    
    else if(input$select_year == 1998){
      bar = plot_ly(x = state, y = data[1:3,22], name = '1998', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 1998")}
    
    else if(input$select_year == 2000){
      bar = plot_ly(x = state, y = data[1:3,23], name = '2000', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2000")}
    
    else if(input$select_year == 2002){
      bar = plot_ly(x = state, y = data[1:3,24], name = '2002', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2002")}
    
    else if(input$select_year == 2004){
      bar = plot_ly(x = state, y = data[1:3,25], name = '2004', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2004")}
    
    else if(input$select_year == 2006){
      bar = plot_ly(x = state, y = data[1:3,26], name = '2006', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2006")}
    
    else if(input$select_year == 2008){
      bar = plot_ly(x = state, y = data[1:3,27], name = '2008', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2008")}
    
    else if(input$select_year == 2010){
      bar = plot_ly(x = state, y = data[1:3,28], name = '2010', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2010")}
    
    else if(input$select_year == 2012){
      bar = plot_ly(x = state, y = data[1:3,29], name = '2012', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2012")}
    
    else if(input$select_year == 2014){
      bar = plot_ly(x = state, y = data[1:3,30], name = '2014', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2014")}
    
    else if(input$select_year == 2016){
      bar = plot_ly(x = state, y = data[1:3,31], name = '2016', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2016")}
    
    else if(input$select_year == 2018){
      bar = plot_ly(x = state, y = data[1:3,32], name = '2018', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2018")}
    
    else if(input$select_year == 2021){
      bar = plot_ly(x = state, y = data[1:3,33], name = '2021', type = 'bar',
                    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c')))
      bar %>% layout(xaxis = x2, yaxis = y2,
                     title = "Comparison Between Happiness States in 2021")}
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
