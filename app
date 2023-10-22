# dane

#fire <- read.csv("C:/Users/k_far/Downloads/Algerian_forest_fires_dataset.csv", skip = 1)
fire <- read.csv("https://raw.githubusercontent.com/kfaryn/kfaryn.github.io/main/docs/Algerian_forest_fires_dataset.csv", skip = 1)
fire <- as.data.frame(fire)

# przeksztalcenia danych

# dodanie zmiennej region
fire[,"Region"] <- NA
fire$Region[1:122] <- "Bejaia"                    # wiersze do 122 to region Bejaia
fire$Region[123:nrow(fire)] <- "Sidi-Bel Abbes"   # wiersze od 123 do końca to region Sidi-Bel Abbes
fire$Region <- as.factor(fire$Region)             # jest to zmienna czynnikowa

# poprawienie zmiennej klasy oznaczajace wystapienie pozaru lub nie
fire$Classes <- fire$Classes....                  
for (i in 1:nrow(fire)){
  if (substr(fire$Classes[i], 1, 4) == "fire") {
    fire$Classes[i] <- "fire"
  } else {
    fire$Classes[i] <- "not fire"
  }
}
fire$Classes <- as.factor(fire$Classes)

#dodanie zmiennej bedacej pelna data
fire$date <- as.Date(paste0(fire$day,"/",fire$month,"/",fire$year),"%d/%m/%Y")

# dodanie zmiennej bedacej nazwa miesiaca obserwacji
fire$mc <- factor(fire$month, 
                  levels=6:9, 
                  labels=c("Czerwiec", "Lipiec","Sierpień", "Wrzesień"))

#wywolanie ostatecznego zbioru
fire = subset(fire, select = -Classes.... )
#View(fire)

# podglad danych
summary(fire)
#View(fire)
#str(fire)

# wczytanie pakietów
library(ggplot2)
library(gridExtra)
library(shiny)
library(corrplot)

ui <- fluidPage(
  HTML('<center><img src="https://kfaryn.github.io/strona/img/algieria3.PNG" width="800" height="323"></center>'),
  navlistPanel(
    tabPanel("Klimatogramy",
             titlePanel("Klimatogram dla regionu"),
             sidebarLayout(sidebarPanel(
               selectInput(
                 "selectedRegion",
                 "Wybierz region dla klimatogramu",
                 choices = levels(fire$Region),
                 selected = NULL
               ),
               sliderInput("grubs", "Ustaw szerokość słupków:", 0, 1, 0.6),
               sliderInput("grubl", "Ustaw grubość linii:", 0.5, 5, 2)
             ),
             
             mainPanel(plotOutput("klimatogram")))),
    tabPanel("Szeregi czasowe", 
             titlePanel("Szeregi czasowe zmiennych"),
             sidebarLayout(sidebarPanel(
               selectInput(
                 "selectedCol",
                 "Wybierz zmienną osi Y",
                 choices = colnames(fire[4:13]),
                 selected = colnames(fire)[16]
               ),
               dateRangeInput("dates", "Wybierz zakres dat:",
                              start = "2012-06-01",
                              end   = "2012-09-30"),
               selectInput(
                 "selectedColorA",
                 "Wybierz kolor linii dla regionu Bejaia",
                 choices = c("blue","red", "pink", "yellow", "green", "orange", "brown", "black", "grey", "violet", "white"),
                 selected = "blue"
               ),
               selectInput(
                 "selectedColorB",
                 "Wybierz kolor linii dla regionu Sidi-Bel Abbes",
                 choices = c("blue","red", "pink", "yellow", "green", "orange", "brown", "black", "grey", "violet", "white"),
                 selected = "red"
               )
             ),
             mainPanel(plotOutput("szeregi")))),
    tabPanel("Wykresy dotyczące pożarów",
             titlePanel("Wykresy dotyczące pożarów"),
             fluidRow(column(8,plotOutput("wykpoz"), offset = 2)),
             fluidRow(column(8,plotOutput("wykpoz2"), offset = 2))
    ),
    tabPanel( "Pożar/brak pożaru w regionie",
              titlePanel("Wystąpienie pożaru lub jego brak w zależności od wartości zmiennych dla każdego z Regionów"),
              sidebarLayout(sidebarPanel(
                selectInput(
                  "selectedY",
                  "Wybierz zmienną dla osi Y",
                  choices = colnames(fire[4:13]),
                  selected = NULL
                ),
                selectInput(
                  "selectedX",
                  "Wybierz zmienną dla osi X",
                  choices = colnames(fire[4:13]),
                  selected = NULL
                ),
                selectInput(
                  "punktkol",
                  "Wybierz kolor punktów",
                  choices = c("blue","red", "pink", "yellow", "green", "orange", "brown", "black", "grey", "violet", "white"),
                  selected = "black"
                ),
                sliderInput("kszt", "Ustal kształt punktów:", 1, 25, 19),
                sliderInput("rozm", "Ustal rozmiar punktów:", 0, 5, 1)
              ),
              mainPanel(plotOutput("pozar")))),
    tabPanel("Wykresy słupkowe opadów",
             titlePanel("Miesięczne sumy opadow dla regionów"),
             sidebarLayout(sidebarPanel(
               selectInput(
                 "selColA",
                 "Wybierz kolor słupka dla regionu Bejaia",
                 choices = c("blue","red", "pink", "yellow", "green", "orange", "brown", "black", "grey", "violet", "white"),
                 selected = "blue"
               ),
               selectInput(
                 "selColB",
                 "Wybierz kolor słupka dla regionu Sidi-Bel Abbes",
                 choices = c("blue","red", "pink", "yellow", "green", "orange", "brown", "black", "grey", "violet", "white"),
                 selected = "red"
               ),
               sliderInput("szer", "Ustal szerokość słupków", 0, 1, 0.6),
               selectInput(
                 "seltyp",
                 "Wybierz sposób prezentacji opadów",
                 choices = c("mc","date"),
                 selected = "mc"
               )
             ),
             mainPanel(plotOutput("sumopad")))),
    tabPanel("Wykresy pudełkowe",
             titlePanel("Wykresy pudełkowe dla zestawu zmiennych"),
             sidebarLayout(sidebarPanel(
               selectInput(
                 "selY",
                 "Wybierz zmienną dla osi Y",
                 choices = colnames(fire[4:13]),
                 selected = NULL
               ),
               selectInput(
                 "selX",
                 "Wybierz zmienną dla osi X",
                 choices = colnames(fire[c(14,15,17)]),
                 selected = NULL
               ),
               selectInput(
                 "selectedFactor",
                 "Wybierz zmienną czynnikowa",
                 choices = colnames(fire[c(14,15,17)]),
                 selected = NULL
               )
             ),
             mainPanel(plotOutput("pudelko")))),
    tabPanel("Korelacja zmiennych",
             titlePanel("Korelacja zmiennych"),
             sidebarLayout(sidebarPanel(
               selectInput(
                 "selMet",
                 "Wybierz sposob prezentacji korelacji",
                 choices = c("number","color", "pie", "square", "ellipse"),
                 selected = "number"
               )
             ),
             mainPanel(plotOutput("korelacja")))),
  )
)


server <- function(input, output) {
  output$klimatogram <- renderPlot({
    region <- input$selectedRegion
    grubs <- input$grubs
    grubl <- input$grubl
    kl <-
      ggplot(fire[fire$Region == region,], aes(x= mc)) 
    kl <- kl + geom_bar(aes(y=Rain), stat = "identity", fill = "steelblue", width = grubs) +
      stat_summary(aes(y = Temperature,group=1), fun=mean, colour="red", geom="line",group=1, lwd = grubl) +
      scale_y_continuous(name = "Suma opadów (w mm)",
                         sec.axis = sec_axis( trans=~., name="Średnia temperatura (w °C)")
      ) +
      scale_x_discrete(name="Miesiąc") +
      labs(title=paste0("Klimatogram dla ", region)) +
      theme( axis.title.y = element_text(hjust=1))
    kl
  })
  output$szeregi <- renderPlot({
    y_axis <- input$selectedCol
    kola <- input$selectedColorA
    kolb <- input$selectedColorB
    gg <-
      ggplot(fire, aes_string(x = "date", y = y_axis, shape = "Region")) 
    gg <- gg + geom_line(aes(colour = Region)) + 
      scale_x_date(limits = as.Date(c(input$dates[1],input$dates[2]))) +
      scale_color_manual(values = c(kola, kolb))
    gg
  })
  output$pozar <- renderPlot({
    x <- input$selectedX
    y <- input$selectedY
    col <- input$punktkol
    kszt <- input$kszt
    rozm <- input$rozm
    gg2 <- ggplot(fire, aes_string(x = x, y=y)) 
    gg2 <- gg2 + geom_point(color = col, shape = kszt, size = rozm)+ facet_grid(Classes ~ Region) 
    gg2
  })
  output$pudelko <- renderPlot({
    x <- input$selX
    y <- input$selY
    fact <- input$selectedFactor
    pud <- ggplot(fire, aes_string(x = x, y = y)) 
    pud <- pud + geom_boxplot(aes_string(fill = fact))
    pud
  })
  output$sumopad <- renderPlot({
    kolaa <- input$selColA
    kolbb <- input$selColB
    szer <- input$szer
    typ <- input$seltyp
    ggplot(fire) + geom_bar(aes_string(x=typ,y="Rain", fill = "Region"), stat = "identity",
                 position="dodge", width = szer) +         
      scale_fill_manual(values=c(kolaa, kolbb))
  })
  output$korelacja <- renderPlot({
    met <- input$selMet
    corrplot(cor(fire[,4:13]), method = met)
  })
  output$wykpoz <- renderPlot({
    ggplot(fire, aes(y=Classes, x= date)) + geom_point(aes(color = Region), size=2, alpha =0.5)
  })
  output$wykpoz2 <- renderPlot({
    ggplot(fire) + geom_bar(aes(x=Region, fill=Classes),
                            color="black",
                            position="fill")
  })
}
shinyApp(ui = ui, server = server) 

