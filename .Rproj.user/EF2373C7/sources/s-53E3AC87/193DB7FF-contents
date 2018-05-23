## app.R ##
library(shinydashboard)
#staplr for pdf form fill up (must install some software so not shiny)

rollDice<-function(dice=6,nbr=1,sum=F){
  res<-sample(1:dice, nbr, replace=T)
  if(sum) res <-sum(res)
  return(res)
}

occupation<-read.csv2(file = "Occupation.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Character Generator"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stats", tabName = "Stats", icon = icon("dashboard")),
      menuItem("Job", tabName = "Job", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Stats",
              
              
              box(title = "Input",width = 12,collapsible = T,
                  fluidRow(
                    column(width = 4,textInput(inputId = "DiceFOR",value = as.character(rollDice(nbr = 3,dice = 6,sum = T)),label = "FOR")),
                    column(width = 4,textInput(inputId = "DiceCON",value = as.character(rollDice(nbr = 3,dice = 6,sum = T)),label = "CON")),
                    column(width = 4, textInput(inputId = "DiceDEX",value = as.character(rollDice(nbr = 3,dice = 6,sum = T)),label = "DEX"))
                  ),
                  
                  fluidRow(
                    column(width = 4,textInput(inputId = "DiceAPP",value = as.character(rollDice(nbr = 3,dice = 6,sum = T)),label = "APP")),
                    column(width = 4,textInput(inputId = "DicePOU",value = as.character(rollDice(nbr = 3,dice = 6,sum = T)),label = "POU")),
                    column(width = 4, textInput(inputId = "DiceChance",value = as.character(rollDice(nbr = 3,dice = 6,sum = T)),label = "Chance"))
                  ),
                  fluidRow(
                    column(width = 4,textInput(inputId = "DiceTAI",value = as.character(rollDice(nbr = 2,dice = 6,sum = T)),label = "TAI")),
                    column(width = 4,textInput(inputId = "DiceINT",value = as.character(rollDice(nbr = 2,dice = 6,sum = T)),label = "INT")),
                    column(width = 4, textInput(inputId = "DiceEDU",value = as.character(rollDice(nbr = 2,dice = 6,sum = T)),label = "EDU"))
                  ),
                  actionButton(inputId = "reroll",label = "Reroll")
              ),
              box(title = "Age",width = 6,collapsible = T,
                  textInput(inputId = "Age",label = "Age",value = "25"),
                  htmlOutput("AgeDisplay"),
                  numericInput("ageFOR", "FOR", 0, min = 0, max = 100),
                  numericInput("ageDEX", "DEX", 0, min = 0, max = 100),
                  numericInput("ageCON", "CON", 0, min = 0, max = 100),
                  numericInput("ageEDU", "EDU points from tests :", 0, min = 0, max = 100)
              ),
              
              box(title = "Values",width = 6,collapsible = T,
                  fluidRow(
                    column(width = 5,tableOutput("statsTable")),
                    column(width = 3,tableOutput("Derive")),
                    column(width = 3,tableOutput("DerivePhy"))
                  )
              ),
              actionButton(inputId = "DEBUG",label = "DEBUG")
              
      ),
      
      # Second tab content
      tabItem(tabName = "Job",
              box(title = "Points",width = 12,
                  fluidRow(
                    column(width = 6,selectInput(inputId = "Occupation",label = "Occupation",choices = occupation$Occupation)),
                    column(width = 6,numericInput(inputId = "Credit",label = "Credit",value = 10,min = 0,max = 90)),
                    column(width = 6,htmlOutput(outputId = "InterestPoint")),
                    column(width = 6,htmlOutput(outputId = "OccupationPoint"))
                  )
              ),
              box(title = "Credit",width=12,
                  fluidRow(
                    column(width = 6,selectInput(inputId = "Period",label = "Period",choices = c("Classic","Modern"))),
                    column(width = 6,tableOutput("CreditTable"))
                  )
              )
      )
    )
  )
)

server <- function(input, output,session) {
  
  observeEvent(eventExpr = input$reroll,{
    updateTextInput(session = session,inputId = "DiceFOR",value =as.character(rollDice(nbr = 3,dice = 6,sum = T)) )
    updateTextInput(session = session,inputId = "DiceCON",value =as.character(rollDice(nbr = 3,dice = 6,sum = T)) )
    updateTextInput(session = session,inputId = "DiceDEX",value =as.character(rollDice(nbr = 3,dice = 6,sum = T)) )
    updateTextInput(session = session,inputId = "DiceAPP",value =as.character(rollDice(nbr = 3,dice = 6,sum = T)) )
    updateTextInput(session = session,inputId = "DicePOU",value =as.character(rollDice(nbr = 3,dice = 6,sum = T)) )
    updateTextInput(session = session,inputId = "DiceTAI",value =as.character(rollDice(nbr = 2,dice = 6,sum = T)+5) )
    updateTextInput(session = session,inputId = "DiceINT",value =as.character(rollDice(nbr = 2,dice = 6,sum = T)+5) )
    updateTextInput(session = session,inputId = "DiceEDU",value =as.character(rollDice(nbr = 2,dice = 6,sum = T)+5) )
    updateTextInput(session = session,inputId = "DiceChance",value =as.character(rollDice(nbr = 3,dice = 6,sum = T)) )
  })
  
  output$statsTable<-renderTable(expr = {
    Stats<-c(FOR=floor(as.integer(input$DiceFOR)*5),CON=floor(as.integer(input$DiceCON)*5),DEX=floor(as.integer(input$DiceDEX)*5),
             TAI=floor(as.integer(input$DiceTAI)*5),INT=floor(as.integer(input$DiceINT)*5),EDU=floor(as.integer(input$DiceEDU)*5),
             APP=floor(as.integer(input$DiceAPP)*5),POU=floor(as.integer(input$DicePOU)*5),Chance=floor(as.integer(input$DiceChance)*5))
    appAgeMod<-ceiling(as.numeric(as.character(cut(as.numeric(input$Age), breaks=c(0, 25, 40, 50, 60, 70,80,750),labels = c(0,-0.1,-5,-10,-15,-20,-25)))))
    
    Stats["APP"]<-Stats["APP"]+appAgeMod
    Stats["FOR"]<-Stats["FOR"]-input$ageFOR
    Stats["DEX"]<-Stats["DEX"]-input$ageDEX
    Stats["CON"]<-Stats["CON"]-input$ageCON
    Stats["EDU"]<-Stats["EDU"]-input$ageEDU
    res<-data.frame(Stats=Stats,Half=floor(Stats/2),Fifth=floor(Stats/5))
    rownames(res)<-c("FOR","CON","DEX","TAI","INT","EDU","APP","POU","Chance")
    res
  },rownames = T,digits = 0,bordered = T,striped = T)
  
  output$Derive<-renderTable({
    res<-data.frame()
    res<-rbind(res,SAN=floor(as.integer(input$DicePOU)*5)) #SAN
    res<-rbind(res,PDV=floor(((floor(as.integer(input$DiceCON)*5)-input$ageCON)+floor(as.integer(input$DiceTAI)*5))/10))#PDV
    res<-rbind(res,SAN=floor(as.integer(input$DicePOU)))#MAG
    
    if(floor(as.integer(input$DiceFOR)*5)-input$ageFOR<floor(as.integer(input$DiceTAI)*5)){
      if(floor(as.integer(input$DiceDEX)*5)-input$ageDEX<floor(as.integer(input$DiceTAI)*5)){
        MVT<-7
      }else{
        MVT<-8
      }
    }else{
      if(floor(as.integer(input$DiceDEX)*5)-input$ageDEX>floor(as.integer(input$DiceTAI)*5)){
        MVT<-9
      }else{
        MVT<-8
      }
    }
    
    res<-rbind(res,MVT)#MVT
    rownames(res)<-c("SAN","PDV","PDM","MVT")
    colnames(res)<-c("Derived")
    res
  },rownames = T,striped = T,bordered = T,digits = 0)
  
  output$DerivePhy<-renderTable({
    
    if((floor(as.integer(input$DiceFOR)*5)-input$ageFOR+floor(as.integer(input$DiceTAI)*5))<65){
      CAR<- "-2"
    }else{
      CAR<-as.character(ceiling(((floor(as.integer(input$DiceFOR)*5)-input$ageFOR+floor(as.integer(input$DiceTAI)*5))-125)/40))
    }
    
    IMP<-as.character(cut(as.numeric(CAR), breaks=c(-2, -1, 0, 1, 2, 3, 4, 5,6),labels = c("-2","-1","0","1D4","1D6","2D6","3D6","4D6"),right = F))
    if(is.na(IMP)) IMP<-"5D6"
    res<-data.frame(CAR,stringsAsFactors = F) #CAR
    res<-rbind(res,IMP)#IMP
    res<-rbind(res,as.character(floor(((as.integer(input$DiceDEX)*5)-input$ageDEX)/2)))#MAG
    rownames(res)<-c("Carrure","Impact","Esquive")
    colnames(res)<-c("Derived")
    res
  },rownames = T,striped = T,bordered = T,digits = 0)
  
  output$AgeDisplay<-renderText({
    AgeMod<-ceiling(as.numeric(as.character(cut(as.numeric(input$Age), breaks=c(0, 25, 40, 50, 60, 70,80,750),labels = c(0,-5,-5.1,-10,-20,-40,-80)))))
    ExpTest<-ceiling(as.numeric(as.character(cut(as.numeric(input$Age), breaks=c(0, 25, 40, 50, 60, 70,80,750),labels = c(0,1,2,3,4,5,6)))))
    
    AgeMod<-AgeMod+input$ageFOR+input$ageDEX+input$ageCON
    paste("<b>Total points to remove:",-AgeMod,"</b></br> </br>",
          "<b>Experience Test:",ExpTest,"</b></br> </br>")
  })
  output$InterestPoint<-renderText({
    paste("<b>Interest Points </b> :",floor(as.integer(input$DiceINT)*5)*2)
  })
  output$OccupationPoint<-renderText({
    points<-(floor(as.integer(input$DiceEDU)*5)-input$ageEDU)*2
    job<-occupation[occupation$Occupation==input$Occupation,]
    JobCategory<-as.character(job$Skill1)
    switch (JobCategory,
            "EDU" = points<-points+2*(floor(as.integer(input$DiceEDU)*5)-input$ageEDU),
            "FOR" = points<-points+2*(floor(as.integer(input$DiceFOR)*5)-input$ageFOR),
            "INT" = points<-points+2*(floor(as.integer(input$DiceINT)*5)),
            "POU" = points<-points+2*(floor(as.integer(input$DicePOU)*5)),
            "APP" = points<-points+2*(floor(as.integer(input$DiceAPP)*5)-input$ageAPP),
            "DEX" = points<-points+2*(floor(as.integer(input$DiceDEX)*5)-input$ageDEX)
    )
    points1<-points
    JobCategory<-as.character(job$Skill2)
    points<-(floor(as.integer(input$DiceEDU)*5)-input$ageEDU)*2
    if(!is.na(JobCategory)){
      switch (JobCategory,
              "EDU" = points<-points+2*(floor(as.integer(input$DiceEDU)*5)-input$ageEDU),
              "FOR" = points<-points+2*(floor(as.integer(input$DiceFOR)*5)-input$ageFOR),
              "INT" = points<-points+2*(floor(as.integer(input$DiceINT)*5)),
              "POU" = points<-points+2*(floor(as.integer(input$DicePOU)*5)),
              "APP" = points<-points+2*(floor(as.integer(input$DiceAPP)*5)-input$ageAPP),
              "DEX" = points<-points+2*(floor(as.integer(input$DiceDEX)*5)-input$ageDEX)
      )

    }
    if(points1>points) points<-points1
    points<-points-input$Credit
    paste("<b>Occupation Points </b> :",points)
  })
  
  output$CreditTable<-renderTable({

    if(input$Period=="Classic"){
      if(input$Credit<1){
        esp<-0.5
        cap<-0
        cur<-0.5
      }else if(input$Credit<10){
        esp<-input$Credit
        cap<-input$Credit*10
        cur<-2
      }else if(input$Credit<50){
        esp<-input$Credit*2
        cap<-input$Credit*50
        cur<-10
      }else if(input$Credit<90){
        esp<-input$Credit*5
        cap<-input$Credit*500
        cur<-50
      }else if(input$Credit<99){
        esp<-input$Credit*20
        cap<-input$Credit*2000
        cur<-250
      }else{
        cap<-5000000
        esp<-50000
        cur<-5000
      }
    }else{
      if(input$Credit<1){
        esp<-10
        cap<-0
        cur<-10
      }else if(input$Credit<10){
        esp<-input$Credit*20
        cap<-input$Credit*200
        cur<-40
      }else if(input$Credit<50){
        esp<-input$Credit*40
        cap<-input$Credit*1000
        cur<-200
      }else if(input$Credit<90){
        esp<-input$Credit*100
        cap<-input$Credit*10000
        cur<-1000
      }else if(input$Credit<99){
        esp<-input$Credit*400
        cap<-input$Credit*40000
        cur<-100000
      }else{
        cap<-1000000
        esp<-100000000
        cur<-100000
      }
    }
    res<-data.frame(Current=cur,Cash=esp,Capital=cap)
    res
  },digits = 1)
  observeEvent(eventExpr = input$DEBUG,handlerExpr = browser())
}

shinyApp(ui, server)