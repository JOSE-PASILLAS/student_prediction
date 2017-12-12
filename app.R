library(networkD3)
library(plyr)
# Define UI ----
ui <- fluidPage(
  titlePanel("Escolar"),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  
                  
                  # selectInput("select", h3("Reporte"),
                  #             choices = list("Genero - Unidad" = 1,
                  #                            "Estado - Unidad" = 2,
                  #                            "Prediccion simple" = 3
                  # 
                  #             ), selected = 1)),
                  
                  selectInput(inputId = "reports",
                              label = "Choose a Report",
                              choices = c("Genero", "Estado", "Prediccion"))),
                
                
                mainPanel(
                  textOutput("selected_var"),
                  sankeyNetworkOutput("plot"),
                  tableOutput("results")
                  # tableOutput("results"),
                )
  )
)

options(shiny.sanitize.errors = FALSE)

# Define server logic ----
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$reports,
           "Genero" = 1,
           "Estado" = 2,
           "Prediccion" = 3)
  })
  
  
  # output$selected_var <- renderText({ 
  #   paste("You have selected", input$select)
  # })
  
  escolar_raw <- read.csv("a911.csv",strip.white=TRUE)
  poblacional <- read.csv("poblacion.csv")
  #setwd("~/uaz/escolar")
  #write.csv(payroll,"payroll.csv",row.names = FALSE)
  # Load the data from .csv file
  m2017<-read.csv("matricula_2017.csv")
  m2017 <- m2017[,c("area",
                    "unidad", 
                    #"nomunidad",
                    #"numunidad",
                    #"nomunidad",
                    "programa",
                    #"nomfuncion",
                    "nivel",
                    "rango_edad",
                    "edo_911",
                    "semestre",
                    "municipio_nacimiento",
                    "estado_nacimiento",
                    "genero",
                    "ciclo"
  )]
  
  m2017 <- m2017[m2017$edo_911=='NUEVO INGRESO',]
  
  output$plot <- renderSankeyNetwork({
    
    if (datasetInput()==1){
      m_grouped<-ddply(m2017,.(genero,unidad),c("nrow"))
      m_grouped$nrow<- as.numeric(m_grouped$nrow)
      
      nodes = data.frame("name" = c(c("HOMBRE","MUJER"),levels(m_grouped$unidad)))
      
      links <- data.frame(source=m_grouped$genero,target=m_grouped$unidad,value=m_grouped$nrow)
      
      links$source <- as.numeric(links$source )-1
      links$target <- as.numeric(links$target)+1
      
      
      sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize= 12, nodeWidth = 30)
    }
    else if (datasetInput()==2){
      m_grouped<-ddply(m2017,.(estado_nacimiento,unidad),c("nrow"))
      m_grouped$nrow<- as.numeric(m_grouped$nrow)
      m_grouped <- m_grouped[m_grouped$nrow>=5,]
      
      nodes = data.frame("name" = c(levels(droplevels(m_grouped$estado_nacimiento)),
                                    levels(droplevels(m_grouped$unidad))),
                         indice=seq(1, nlevels(droplevels(m_grouped$unidad))+
                                      nlevels(droplevels(m_grouped$estado_nacimiento))))
      
      links <- data.frame(source=m_grouped$estado_nacimiento,
                          target=m_grouped$unidad,value=m_grouped$nrow)
      links <- links[order(links$source),]
      
      links$source<- nodes$indice[match(links$source,nodes$name)]-1
      links$target<- nodes$indice[match(links$target,nodes$name)]-1
      
      sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize= 9, nodeWidth = 30)
    }
    else {
      escolar <- escolar_raw[,c("unidad", 
                                "edo_911",
                                "ciclo")]
      escolar <- escolar[escolar$edo_911=='NUEVO INGRESO',]
      escolar$ciclo<-as.character(escolar$ciclo)
      escolar$ciclo[escolar$ciclo=="2008-2009"]<-2008
      escolar$ciclo[escolar$ciclo=="2009-2010"]<-2009
      escolar$ciclo[escolar$ciclo=="2010-2011"]<-2010
      escolar$ciclo[escolar$ciclo=="2011-2012"]<-2011
      escolar$ciclo[escolar$ciclo=="2012-2013"]<-2012
      escolar$ciclo[escolar$ciclo=="2013-2014"]<-2013
      escolar$ciclo[escolar$ciclo=="2014-2015"]<-2014
      escolar$ciclo[escolar$ciclo=="2015-2016"]<-2015
      escolar$ciclo[escolar$ciclo=="2016-2017"]<-2016
      escolar$ciclo<-as.numeric(escolar$ciclo)
      
      escolar$poblacion <- poblacional$poblacion[match(escolar$ciclo,poblacional$year)]
      e_grouped<-ddply(escolar,.(unidad,ciclo,poblacion),c("nrow"))
      train <- e_grouped[e_grouped$ciclo!=2016,]
      test <- e_grouped[e_grouped$ciclo==2016,]
      model <-lm(nrow~ciclo+poblacion+unidad,data=e_grouped)
      prediction <-predict(model,test)
      error<-abs(test$nrow-prediction)
      results <- data.frame(Unidad=test$unidad,ciclo=2016,Proyeccion=prediction, Real=test$nrow, error=error)
      output$results <- renderTable(results)
       
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
