## app.R ##
require(dashboardthemes)
require(shinydashboard)
require(stringr)
require(DT)
require(staplr)
require(hms)
require(animation) #to load pdf reader software
#Pictures 
#Skills check
#get("value", eval(as.symbol(""))))
#test<-"ORI_0"
#fieldsbase[[test]]$value<-"88"


rollDice<-function(dice=6,nbr=1,sum=F){
  res<-sample(1:dice, nbr, replace=T)
  if(sum) res <-sum(res)
  return(res)
}

occupation<-read.csv2(file = "Occupation.csv",encoding = "ANSI") #HERE
names<-read.csv2(file = "Names.csv",encoding = "ANSI")
attache<-read.csv2(file = "Attaches.csv",encoding = "ANSI")
armes<-read.csv2(file = "Armes.csv",encoding = "ANSI")
fieldsbase<-get_fields(input_filepath = "fichebase.pdf")
codepdf<-read.csv2(file = "CodePDF.csv",encoding = "ANSI")
fieldsDone<-NULL

fichesession<-{
  time<-Sys.time()
  time<-str_remove_all(as.hms(time),pattern = ":")
  paste("fiche_",time,".pdf",sep = "")
}


ui <- dashboardPage(
  dashboardHeader(title = "Character Generator",
                  tags$li(class = "dropdown",
                  tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
                  tags$li(class = "dropdown", actionLink("CreatePDF", "Generate PDF")))
  ),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stats", tabName = "Stats", icon = icon("calculator")),
      menuItem("Job", tabName = "Job", icon = icon("user-md")),
      menuItem("Personal", tabName = "Personal", icon = icon("user")),
      menuItem("Equipment", tabName = "Equipment", icon = icon("bomb"))#,
      #menuItem("Generator", tabName = "Generator", icon = icon("cogs"))
    )
  ),
  ## Body content
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "Stats",
              tags$style(".topimg {
                            margin-left:-60px;
                         margin-right:-60px;
                         margin-top:-30px;
                         margin-bottom:10px;
                         }"),
                  div(class="topimg",img(src="Ecran2.jpg", width="100%")),
              
              box(status="primary",title = "Input",width = 12,collapsible = T,
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
              box(status="primary",title = "Age",width = 6,collapsible = T,
                  textInput(inputId = "Age",label = "Age",value = "25"),
                  htmlOutput("AgeDisplay"),
                  numericInput("ageFOR", "FOR", 0, min = 0, max = 100),
                  numericInput("ageDEX", "DEX", 0, min = 0, max = 100),
                  numericInput("ageCON", "CON", 0, min = 0, max = 100),
                  numericInput("ageEDU", "EDU points from tests :", 0, min = 0, max = 100)
              ),
              
              box(status="primary",title = "Values",width = 6,collapsible = T,
                  fluidRow(
                    column(width = 5,tableOutput("statsTable")),
                    column(width = 3,tableOutput("Derive")),
                    column(width = 3,tableOutput("DerivePhy"))
                  )
              )#,
              #actionButton(inputId = "DEBUG",label = "DEBUG")
              
      ),
      
      # Second tab content
      tabItem(tabName = "Job",
              tags$style(".topimg {
                            margin-left:-60px;
                         margin-right:-60px;
                         margin-top:-30px;
                         margin-bottom:10px;
                         }"),
              div(class="topimg",img(src="EcranV62.jpg", width="100%")),
              box(status="primary",title = "Points",width = 12,
                  fluidRow(
                    column(width = 3, checkboxInput("Recommanded","Show Only Recommanded",value = F)),
                    column(width = 4,selectInput(inputId = "Occupation",label = "Occupation",choices = occupation$Occupation)),
                    column(width = 5,tableOutput(outputId = "TotalPoints"),align="center")
                  )
              ),              box(status="primary",title = "Skills",width=12,
                                  fluidRow(
                                    column(width = 12,tableOutput(outputId = "SkillTableValue"),align="center"),
                                    
                                    column(width = 4,htmlOutput(outputId = "SkillJoker")),
                                    column(width = 4,checkboxInput(inputId = "SkillOnly",label = "Only Profession Skill",value = T)),
                                    column(width = 4,htmlOutput(outputId = "SkillSocial")),
                                    column(width = 6,selectInput(inputId = "SkillChoose",label = "Skill to affect value",choices = "Placeholder" )),
                                    column(width = 6,numericInput(inputId = "SkillValue",label = "Skill Value",value = 0,min = 0,max = 100)),
                                    column(width = 3,actionButton(inputId = "SkillValueSave",label = "Validate")),
                                    column(width = 3,actionButton(inputId = "SkillReset",label = "Reset all"))
                                  )

              ),
              box(status="primary",title = "Credit",width=12,
                  fluidRow(
                    column(width = 4,selectInput(inputId = "Period",label = "Period",choices = c("Classic","Modern"))),
                    column(width = 4,numericInput(inputId = "Credit",label = "Credit",value = 10,min = 0,max = 90)),
                    column(width = 4,tableOutput("CreditTable"))
                  )
              )

              
      ),
      
      tabItem(tabName="Personal",
              tags$style(".topimg {
                            margin-left:-60px;
                         margin-right:-60px;
                         margin-top:-30px;
                         margin-bottom:10px;
                         }"),
                  div(class="topimg",img(src="map.jpg", width="100%")),
               box(status="primary",title = "Name",width = 12,
                   fluidRow(
                     column(width = 4,selectInput(inputId = "Gender",label = "Prefered Gender",choices = c("Male","Female","Random"))),
                     column(width = 4,textInput(inputId = "FirstName",label = "First Name",placeholder = "Jane")),
                     column(width = 4,textInput(inputId = "LastName",label = "Last Name",placeholder = "Smith")),
                     column(width = 3,actionButton(inputId = "RollName",label = "Random Name"))
                   )
                   ),
              box(status="primary",title = "Particular",width = 12,
                  fluidRow(
                    column(width = 4,textInput(inputId = "Croyance",label = "Believes",placeholder = "God")),
                    column(width = 4,textInput(inputId = "Personne",label = "Important person",placeholder = "Jane Smith")),
                    column(width = 4,textInput(inputId = "PersonReason",label = "Why person important",placeholder = "Sister")),
                    column(width = 4,textInput(inputId = "Place",label = "Important place",placeholder = "Home")),
                    column(width = 4,textInput(inputId = "Object",label = "Important object",placeholder ="Necklace")),
                    column(width = 4,textInput(inputId = "Characteristic",label = "Characteristic",placeholder="Happy")),
                    column(width = 1,actionButton(inputId = "RandomCharacteristics",label = "Randomize"))
                  )
              )
               
               ),
      tabItem(tabName = "Equipment",
              tags$style(".topimg {
                            margin-left:-60px;
                         margin-right:-60px;
                         margin-top:-30px;
                         margin-bottom:10px;
                         }"),
                  div(class="topimg",img(src="Stand.jpg", width="100%")),
              
                box(status="primary",title = "Weapons",width=12,
                    fluidRow(
                      column(width = 6, selectInput(inputId = "WeaponPeriod",label = "Period",choices = c("all","1890s", "1920s","present","rare"),selected = "all")),
                      column(width = 6, selectInput(inputId = "WeaponCategory",label = "Category",choices =unique(armes$Category))),
                      column(width = 12,DTOutput(outputId = "WeaponTable"))
                      )
                      )
              ) #,
      # tabItem(tabName = "Generator",
      #         actionButton(inputId = "CreatePDF",label = "Generate PDF")
      #         )
     
      
    )
  )
)

server <- function(input, output,session) {
  fields<-fieldsbase
  
  output$SkillJoker<-renderText({
    input$SkillValueSave
    input$SkillReset
    social<-fieldsDone
    social<-social[!(social %in% str_to_title(strsplit(as.character(occupation[occupation$Occupation==input$Occupation,"Skills"]),", ")[[1]]))]
    left<-occupation[occupation$Occupation==input$Occupation,"Joker"]-length(social)
    paste("<br><b>Joker Skills left </b>:",left,"<br><br>")
  })
  
  output$SkillSocial<-renderText({
    input$SkillValueSave
    input$SkillReset
    social<-fieldsDone[fieldsDone %in% c("Baratin","Persuasion","Charme","Intimidation")]
    social<-social[!(social %in% str_to_title(strsplit(as.character(occupation[occupation$Occupation==input$Occupation,"Skills"]),", ")[[1]]))]
    left<-occupation[occupation$Occupation==input$Occupation,"Sociale"]-length(social)
    paste("<br><b>Social Skills left </b>:",left,"<br><br>")
  })
  
  observeEvent(input$SkillReset,{
    
    fields<<-fieldsbase
    fieldsDone<<-NULL
    showModal(modalDialog(title = "Confirmation",easyClose = T,footer=NULL,
                          "All skills have been reset"
                          ))
  })
  
  observeEvent(input$SkillValueSave,{
    
    skillcode<-as.character(codepdf[codepdf$Name==input$SkillChoose,"Code"])
    if(length(skillcode)==0 || is.null(skillcode)){
      showModal(modalDialog(title = "Error",easyClose = T,footer = NULL,
              "This skill isn't found"
                            
      ))
    }else{
      fields[[skillcode]]$value<<-as.character(input$SkillValue)
      fieldsDone<<-c(fieldsDone,input$SkillChoose)
      fieldsDone<<-fieldsDone[!duplicated(fieldsDone)] 
      showModal(modalDialog(title = "Confirmation",easyClose = T,footer = NULL,
                            paste(input$SkillChoose,"has been set to",input$SkillValue)
                            
      ))
    }
    

  })
  
  
  output$pdfviewer <- renderText({
    return(paste('<iframe style="height:600px; width:100%" src="', fichesession, '"></iframe>', sep = ""))
  })
  
  
  output$Character.pdf <- downloadHandler(
    filename = function() {
      paste("fiche", "pdf", sep=".")
    },
    
    content = function(file) {
      file.copy(fichesession, file)
    },
    contentType = "pdf"
  )
  
  observeEvent(eventExpr = input$CreatePDF,{
    
    Stats<-c(FOR=floor(as.integer(input$DiceFOR)*5),CON=floor(as.integer(input$DiceCON)*5),DEX=floor(as.integer(input$DiceDEX)*5),
             TAI=floor(as.integer(input$DiceTAI)*5),INT=floor(as.integer(input$DiceINT)*5),EDU=floor(as.integer(input$DiceEDU)*5),
             APP=floor(as.integer(input$DiceAPP)*5),POU=floor(as.integer(input$DicePOU)*5),Chance=floor(as.integer(input$DiceChance)*5))
    appAgeMod<-ceiling(as.numeric(as.character(cut(as.numeric(input$Age), breaks=c(0, 25, 40, 50, 60, 70,80,750),labels = c(0,-0.1,-5,-10,-15,-20,-25)))))
    
    Stats["APP"]<-Stats["APP"]+appAgeMod
    Stats["FOR"]<-Stats["FOR"]-input$ageFOR
    Stats["DEX"]<-Stats["DEX"]-input$ageDEX
    Stats["CON"]<-Stats["CON"]-input$ageCON
    Stats["EDU"]<-Stats["EDU"]+input$ageEDU
    
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
    
    if((floor(as.integer(input$DiceFOR)*5)-input$ageFOR+floor(as.integer(input$DiceTAI)*5))<65){
      CAR<- "-2"
    }else{
      CAR<-as.character(ceiling(((floor(as.integer(input$DiceFOR)*5)-input$ageFOR+floor(as.integer(input$DiceTAI)*5))-125)/40))
    }
    
    IMP<-as.character(cut(as.numeric(CAR), breaks=c(-2, -1, 0, 1, 2, 3, 4, 5,6),labels = c("-2","-1","0","1D4","1D6","2D6","3D6","4D6"),right = F))
    ESQ<-as.character(floor(((as.integer(input$DiceDEX)*5)-input$ageDEX)/2))
    
    fields<-fields
    fields$Nom$value<-paste(input$FirstName,input$LastName)
    fields$Occupation$value<-input$Occupation
    fields$Sexe$value<-input$Gender
    # fields$`Esp&#232;ces`$value
    # fields$capital$value
    # fields$depencesCourantes$value
    fields$POU_0$value<-Stats["POU"] #_1 _2
    fields$CON_0$value<-Stats["CON"]
    fields$APP_0$value<-Stats["APP"]
    fields$EDU_0$value<-Stats["EDU"]
    fields$FOR_0$value<-Stats["FOR"]
    fields$TAI_0$value<-Stats["TAI"]
    fields$INT_0$value<-Stats["INT"]
    fields$DEX_0$value<-Stats["DEX"]
    fields$CRE_0$value<-input$Credit
    fields$age$value<-input$Age
    fields$MVT$value<-MVT
    fields$pv_max$value<-res[2,]
    fields$pm_max$value<-res[3,]
    fields$sm_initial$value<-res[1,]
    fields$sm_max$value<-res[1,]
    fields$CHANCE$value<-Stats["Chance"]
    fields$impact$value<-IMP
    fields$carrure$value<-CAR
    fields$ESQ$value<-ESQ
  
    showModal(modalDialog(
      title = "",footer = NULL,
      "Your character is being created.",
      "Please wait ..."
    ))
    set_fields(input_filepath = "fichebase.pdf",output_filepath = fichesession,fields = fields)
    removeModal()
    showModal(modalDialog(
      title = "PDF Generation",
      "Your character sheet has been created",
      downloadLink('Character.pdf', 'Download'),footer = actionButton(inputId = "PDFClose",label = "Dismiss")
    ))
    
  })
  observeEvent(eventExpr = input$PDFClose,{
    file.remove(fichesession)
    fichesession<-{
      time<-Sys.time()
      time<-str_remove_all(as.hms(time),pattern = ":")
      paste("fiche_",time,".pdf",sep = "")
    }
    removeModal()
  })
  output$WeaponTable<-renderDT({
    
    temp<-armes[armes$Category==input$WeaponCategory,]
    res<-temp[sapply(X =temp$Era,FUN = grepl,pattern=input$WeaponPeriod ),]
    res<-rbind(res,temp[temp$Era=="all",])
    datatable(res[!duplicated(res),], extensions = 'FixedColumns',
                  options = list(
                    scrollX = TRUE,
                    fixedColumns = TRUE
                  ))
  })
  
  observeEvent(eventExpr = input$RandomCharacteristics,{
    dice<-rollDice(dice =10,nbr = 6 )
    updateTextInput(session=session,inputId = "Croyance",label = "Believes",value = attache[dice[1],"Croyance"])
    updateTextInput(session=session,inputId = "Personne",label = "Important person",value = attache[dice[2],"Personne"])
    updateTextInput(session=session,inputId = "PersonReason",label = "Why person important",value = attache[dice[3],"Raison"])
    updateTextInput(session=session,inputId = "Place",label = "Important place",value = attache[dice[4],"Lieux"])
    updateTextInput(session=session,inputId = "Object",label = "Important object",value =attache[dice[5],"Bien"])
    updateTextInput(session=session,inputId = "Characteristic",label = "Characteristic",value=attache[dice[6],"Traits"])
  })
  
  observeEvent(eventExpr = input$RollName,{
    dice<-rollDice(dice = 100,nbr = 2)
    if(input$Gender == "Male"){
      updateTextInput(session=session,inputId = "FirstName",label = "First Name",value = names[dice[1],"Masculin"])
    }else if(input$Gender == "Female"){
      updateTextInput(session=session,inputId = "FirstName",label = "First Name",value = names[dice[1],"Feminin"])
    }else{
      if(rollDice(dice = 2,nbr = 1)==1){
        updateTextInput(session=session,inputId = "FirstName",label = "First Name",value = names[dice[1],"Feminin"])
      }else{
        updateTextInput(session=session,inputId = "FirstName",label = "First Name",value = names[dice[1],"Masculin"])
      }
    }

    updateTextInput(session=session,inputId = "LastName",label = "Last Name",value =names[dice[2],"Famille"])
  })
  
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
    Stats["EDU"]<-Stats["EDU"]+input$ageEDU
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
    res<-rbind(res,as.character(floor(((as.integer(input$DiceDEX)*5)-input$ageDEX)/2)))#DEX
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
    points<-(floor(as.integer(input$DiceEDU)*5)+input$ageEDU)*2
    job<-occupation[occupation$Occupation==input$Occupation,]
    JobCategory<-as.character(job$Skill1)
    appAgeMod<-ceiling(as.numeric(as.character(cut(as.numeric(input$Age), breaks=c(0, 25, 40, 50, 60, 70,80,750),labels = c(0,-0.1,-5,-10,-15,-20,-25)))))
    switch (JobCategory,
            "EDU" = points<-points+2*(floor(as.integer(input$DiceEDU)*5)+input$ageEDU),
            "FOR" = points<-points+2*(floor(as.integer(input$DiceFOR)*5)-input$ageFOR),
            "INT" = points<-points+2*(floor(as.integer(input$DiceINT)*5)),
            "POU" = points<-points+2*(floor(as.integer(input$DicePOU)*5)),
            "APP" = points<-points+2*(floor(as.integer(input$DiceAPP)*5)+appAgeMod),
            "DEX" = points<-points+2*(floor(as.integer(input$DiceDEX)*5)-input$ageDEX)
    )
    points1<-points
    JobCategory<-as.character(job$Skill2)
    points<-(floor(as.integer(input$DiceEDU)*5)+input$ageEDU)*2
    if(!is.na(JobCategory)){
      switch (JobCategory,
              "EDU" = points<-points+2*(floor(as.integer(input$DiceEDU)*5)+input$ageEDU),
              "FOR" = points<-points+2*(floor(as.integer(input$DiceFOR)*5)-input$ageFOR),
              "INT" = points<-points+2*(floor(as.integer(input$DiceINT)*5)),
              "POU" = points<-points+2*(floor(as.integer(input$DicePOU)*5)),
              "APP" = points<-points+2*(floor(as.integer(input$DiceAPP)*5)-input$ageAPP),
              "DEX" = points<-points+2*(floor(as.integer(input$DiceDEX)*5)-input$ageDEX)
      )

    }
    if(points1>points) points<-points1
    #points<-points-input$Credit
    paste("<b>Occupation Points </b> :",points)
  })
  
  output$TotalPoints<-renderTable({
    input$SkillValueSave
    input$Credit
    input$SkillReset
    points<-(floor(as.integer(input$DiceEDU)*5)+input$ageEDU)*2
    job<-occupation[occupation$Occupation==input$Occupation,]
    JobCategory<-as.character(job$Skill1)
    appAgeMod<-ceiling(as.numeric(as.character(cut(as.numeric(input$Age), breaks=c(0, 25, 40, 50, 60, 70,80,750),labels = c(0,-0.1,-5,-10,-15,-20,-25)))))
    switch (JobCategory,
            "EDU" = points<-points+2*(floor(as.integer(input$DiceEDU)*5)+input$ageEDU),
            "FOR" = points<-points+2*(floor(as.integer(input$DiceFOR)*5)-input$ageFOR),
            "INT" = points<-points+2*(floor(as.integer(input$DiceINT)*5)),
            "POU" = points<-points+2*(floor(as.integer(input$DicePOU)*5)),
            "APP" = points<-points+2*(floor(as.integer(input$DiceAPP)*5)+appAgeMod),
            "DEX" = points<-points+2*(floor(as.integer(input$DiceDEX)*5)-input$ageDEX)
    )
    points1<-points
    JobCategory<-as.character(job$Skill2)
    points<-(floor(as.integer(input$DiceEDU)*5)+input$ageEDU)*2
    if(!is.na(JobCategory)){
      switch (JobCategory,
              "EDU" = points<-points+2*(floor(as.integer(input$DiceEDU)*5)+input$ageEDU),
              "FOR" = points<-points+2*(floor(as.integer(input$DiceFOR)*5)-input$ageFOR),
              "INT" = points<-points+2*(floor(as.integer(input$DiceINT)*5)),
              "POU" = points<-points+2*(floor(as.integer(input$DicePOU)*5)),
              "APP" = points<-points+2*(floor(as.integer(input$DiceAPP)*5)-input$ageAPP),
              "DEX" = points<-points+2*(floor(as.integer(input$DiceDEX)*5)-input$ageDEX)
      )
      
    }
    if(points1>points) points<-points1
    temp<-fieldsDone
    codes<-as.character(sapply(X = fieldsDone,FUN = function(x) codepdf[codepdf$Name==x,"Code"]))
    val<-sapply(X=codes,FUN=function(x) fields[[x]]$value)
    temp<-cbind(temp,val)
    temp<-as.data.frame(temp)
    
   temp<- temp[order(temp$temp),] 
    table<-data.frame(Occupation_Point=points,Interest_Point=floor(as.integer(input$DiceINT)*5)*2,Total_points=points+floor(as.integer(input$DiceINT)*5)*2-input$Credit -sum(as.numeric(as.character(temp$val))))
colnames(table)<-c("Occupation Points","Interest Points", "Total Points")
table
  },colnames = T)
  
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
  
  # output$SkillTable<-renderTable(expr = {
  #   temp<-input$SkillValueSave
  #   temp<-str_to_title(strsplit(as.character(occupation[occupation$Occupation==input$Occupation,"Skills"]),", ")[[1]])
  #   
  #   temp<-temp[!(temp %in% fieldsDone)]
  #   
  #   },colnames = F)
  
  output$SkillTableValue<-renderTable(expr = {
    input$SkillValueSave
    input$SkillReset
    temp<-fieldsDone
    codes<-as.character(sapply(X = fieldsDone,FUN = function(x) codepdf[codepdf$Name==x,"Code"]))
    val<-sapply(X=codes,FUN=function(x) fields[[x]]$value)
    temp<-cbind(temp,val)
    temp<-as.data.frame(temp)
    
    temp[order(temp$temp),] 
    
  },colnames = F)
  
  
  observeEvent({input$Occupation
    input$SkillOnly},{
    if(input$SkillOnly){
      updateSelectInput(session = session,inputId ='SkillChoose',choices ={str_to_title(strsplit(as.character(occupation[occupation$Occupation==input$Occupation,"Skills"]),", ")[[1]])}  ) 
    }else{
      updateSelectInput(session = session,inputId ='SkillChoose',choices ={str_to_title(codepdf$Name)}  ) 
    }
    
  })
  
  
  observeEvent(input$Recommanded,{
    if(input$Recommanded){
      updateSelectInput(session=session,inputId = "Occupation",label = "Occupation",choices = isolate(Recommanded())$Occupation)
    }else{
      updateSelectInput(session=session,inputId = "Occupation",label = "Occupation",choices = occupation$Occupation)
    }
  })

  Recommanded<-reactive({
    temp<-NULL
      FOR<-(floor(as.integer(input$DiceFOR)*5)-input$ageFOR)
      DEX<-(floor(as.integer(input$DiceDEX)*5)-input$ageDEX)
      EDU<-(floor(as.integer(input$DiceEDU)*5)+input$ageEDU)
      POU<-(floor(as.integer(input$DicePOU)*5))
      appAgeMod<-ceiling(as.numeric(as.character(cut(as.numeric(input$Age), breaks=c(0, 25, 40, 50, 60, 70,80,750),labels = c(0,-0.1,-5,-10,-15,-20,-25)))))
      APP<-(floor(as.integer(input$DiceAPP)*5)+appAgeMod)
      
      maxs<-which( c(FOR,DEX,EDU,POU,APP) == max(c(FOR,DEX,EDU,POU,APP)) )
      if(1 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill1=="FOR",])
      if(1 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill2=="FOR",])  
      if(2 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill1=="DEX",])
      if(2 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill2=="DEX",])
      if(3 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill1=="EDU",])
      if(3 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill2=="EDU",])
      if(4 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill1=="POU",])
      if(4 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill2=="POU",])
      if(5 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill1=="APP",])
      if(5 %in% maxs) temp<-rbind(temp,occupation[occupation$Skill2=="APP",])
      temp[!is.na(temp$Occupation),]
    
  })
  
  observeEvent(eventExpr = input$DEBUG,handlerExpr = browser())
}

shinyApp(ui, server)