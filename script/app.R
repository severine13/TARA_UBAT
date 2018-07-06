#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(

  
   # Application title
   titlePanel("Fast observation of UBAT dataset"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput("file1",label="Load a file from Tara Expeditions"),
         sliderInput("span",label="Span",min=0,max=0.2,value=0.1),
        # selectInput("variable1",label="Variable 1 to plot",choices=c("Raw_Amp"="Raw_Amp","Log_Amp"="Log_Amp","Prog_Amp"="Prog_Amp","HighV"="HighV")),
        # selectInput("variable2",label="Variable 2 to plot",choices=c("Raw_Amp"=Raw_Amp,"Log_Amp"=Log_Amp,"Prog_Amp"=Prog_Amp,"HighV"=HighV)),
         numericInput("start","start",value=0),
         numericInput("end","end",value=17)
          ),
      
      # Show a plot of the generated distribution
      mainPanel(
       #  plotOutput("plot1"),
        # plotOutput("plot2"),
         plotOutput("plot3"),
         tableOutput("contents")
      ))
)



# Define server logic required to draw a histogram
server <- function(input, output){
  # observe({
  #   
  # file1=input$file1
  # data1 = read.csv(file1$datapath)
  # })
  # 
  # output$plot <- renderPlot({
  #   plot(data1[,1],data1[,2])
  # })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
        if (is.null(inFile))
      return(NULL)
        read.csv(inFile$datapath)
  })
  # 
  
  
  
  # output$plot1 <- renderPlot({
  #   file1=input$file1
  #   data1 = read.table(file1$datapath,skip=2)
  #   names(data1)=c("Time_s","Raw_Amp","Log_Amp","Prog_Amp","HighV")
  #   ggplot(data1,aes(x=Time_s,y=Raw_Amp))+geom_line()+geom_smooth(span=0.01)
  #   
  # })
  # output$plot2 <- renderPlot({
  #   file1=input$file1
  #   data1 = read.table(file1$datapath,skip=2)
  #   names(data1)=c("Time_s","Raw_Amp","Log_Amp","Prog_Amp","HighV")
  #   ggplot(data1,aes(x=Time_s,y=Log_Amp))+geom_line()+geom_smooth(span=0.01)
  # })
  output$plot3 <- renderPlot({
    
    
     file1=input$file1
     data1 = read.csv(file1$datapath)
    
    # recuperer toutes les raw dans une matrice
    raw0_59 = data1[11:70]
    vect_raw = unlist(raw0_59)
    # longueur des donnees
    nr = nrow(data1)
    nc = 60
    mat_raw = matrix(vect_raw,nrow=nr,ncol=nc)
    # verif table(dat$raw_0-mat_raw[,1]) --> OK
    
    # vecteur temporel consecutif OK
    mat_vec=""
    mat_vec$Biolum = as.vector(t(mat_raw))
    # matrix en photons.s-1
    ## cal = rep(dat$cal_striing,length.out=length(mat_vec))
    ## vect_cal = cal*mat_vec
    
    
    # recreer le data.frame
    mat_vec$UBAT=rep(data1[,1],each=60)
    mat_vec$nbr_min=rep(data1[,2],each=60)
    mat_vec$cal=rep(data1[,3],each=60)
    mat_vec$v2=rep(data1[,4],each=60)
    mat_vec$v3=rep(data1[,5],each=60)
    mat_vec$v4=rep(data1[,6],each=60)
    mat_vec$v5=rep(data1[,7],each=60)
    mat_vec$v6=rep(data1[,8],each=60)
    mat_vec$v7=rep(data1[,9],each=60)
    mat_vec$v8=rep(data1[,10],each=60)
    
    mat_vec$cal = mat_vec$cal*mat_vec$Biolum
    mat_vec=as.data.frame(mat_vec)
    # plot data
    ggplot(mat_vec,aes(y=Biolum,x=nbr_min))+geom_line()+ggtitle(files[i])+scale_y_continuous(limits=c(0,30000))+ylab("Bioluminescence (AU)")+
      xlab("Time (s)")
    
    
    
    
  #  file1=input$file1
  #  data1 = read.table(file1$datapath,sep=",")
  #  names(data1)=c("UBAT","line","Time_s","Raw_Amp","Log_Amp","Prog_Amp","HighV")
  #  ggplot(data1,aes(x=Time_s,y=Prog_Amp))+geom_line()+geom_smooth(span=input$span)+geom_line(aes(y=Log_Amp),linetype = "dashed",colour="red")+xlab("Time (s)")+xlim(input$start,input$end)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
