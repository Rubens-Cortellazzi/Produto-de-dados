############################### Modelo Dashboard ###############################
# Carregando os pacotes
library(shiny)
library(bs4Dash)
library(GGally)
library(thematic)
library(waiter)
library(plotly)
#library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(pacoteFRT)

# auto-configurando o tema

thematic_shiny()

######## Definindo objetos importantes ########

#### Def Header ####

titulo <- dashboardBrand(
  title = "Modelagem sobre Marketing",
  color = "primary")

header <- dashboardHeader(
  title = titulo,
  skin = "dark"
)

#### Def SideBar ####
Sidebar <- dashboardSidebar(
  sidebarUserPanel(
    name = "Modelando os Dados"
  ),
  sidebarMenu(
    id = "sidebarmenu",
    menuItem("Introdução", tabName = "intro"),
    menuItem("Análise Descritiva", icon = icon("list-alt"),
             menuSubItem(
               "Variáveis do modelo",
               tabName = "vari_mod"
             ),
             menuSubItem(
               "Variável resposta",
               tabName = "vari_res"
             ),
              menuSubItem(
               "Relação entre as variáveis",
               tabName = "relacao_vari"
             )),
    menuItem("Seleção de Modelos", icon = icon("chart-bar"),
             menuSubItem(
               "Todas as variáveis",
               tabName = "todas_vari"
             ),
             menuSubItem(
               "Melhores variáveis",
               tabName = "melhores_vari"
             )
    ),
    menuItem("Análise de resíduos", icon = icon("signal"),
             tabName = "residuos"
    ),
    menuItem("Predição", icon = icon("chart-line"),
             tabName = "predicao"
    ),
    menuItem("Conclusão",  icon = icon("check"),
             tabName = "conclusao")
  )
)

#### Def Footer ####
footer <- dashboardFooter(
  left = "Felipe ra: 236106. Pedro ra: 186935. Rubens ra: 236292. Tiago ra:217517. Vitor ra:168264.",
  right = "ME918"
)

#### Def body ####

#Introdução
intro1 <- fluidRow(
  column(
    width = 8,
    offset = 2,
    h1("ME918 - Produto de Dados", align = "center")
  ),
  column(
    width = 2,
    tags$img(src = "https://upload.wikimedia.org/wikipedia/pt/thumb/b/b2/UNICAMP_logo.svg/1024px-UNICAMP_logo.svg.png",
             width = "75px", height = "75px", style = "display: block; margin: auto;")
  )
)

# --------------------------------------------------------
intro2 <- fluidRow(
  column(
    width = 12,
    p("Nossa empresa,", strong("Modelagens FPRTV"), "foi contratada por uma empresa de Marketing para auxiliar estatísticamente
a empresa. Para isso, eles disseram que já tinham o banco de dados montado e precisavam de ajuda para conseguir
visualizar, também gostariam de algo interativo e gostariam de ter uma parte estatística muito desenvolvida.
Para isso, pensamos em desenvolver um pacote que contém funções para auxiliar na modelagem da regressão linear,
no pacote também, foi inserido a base dedados que a empresa forneceu para nós. Além disso, pensamos em desenvolver
um shiny, que seja iterativo para usuário e assim facilitar a vida da empresa. Por último, desenvolvemos uma
API em que é uma forma rápida de conseguir fazer a predição de vendas, alterando os valores para cada fator
envolvido, sendo que na API foi desenvolvida também para retornar um gráfico dependendo de qual dos três fatores
que inflenciam as vendas ser chamado.", align = "center")
    ))

################## Análise descritiva #############################

#Variáveis do modelo
texto_eda_var_mod <- fluidRow(
  column(
    width = 12,
    p("Notaremos abaixo a análise descritiva do banco de dados, com excessão da variável resposta. Podendo ser visto que possui informações como média, mediana, quartis, gráficos de distribuição (boxplot e histograma) de cada uma das variáveis.", align = "center")
  )
)

variaveis_modelo <- fluidRow(
  column(width = 12,
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel_escolhida", "Selecione a Variável",
                  choices = colnames(pacoteFRT::dadosFRT),
                  selected = "Youtube"),
      sliderInput("numero_dados", "Número de Dados para Exibir",
                  min = 1, max = 187, value = 10),
      tabsetPanel(
        tabPanel("Dados", tableOutput("tabela_dados")),
        tabPanel("Sumário", verbatimTextOutput("sumario")),
        tabPanel("Histograma", plotOutput("histograma")),
        tabPanel("Boxplot", plotOutput("boxplot"))
      )
    ),
    mainPanel()
  )
))


#Variável Resposta
texto_eda_var_resp <- fluidRow(
  column(
    width = 12,
    p("Notaremos abaixo a análise descritiva da variável resposta (sales). Assim, podemos observar
    diversas informações relevantes como os quantis, mediana, média, os dados em si, histograma, boxplot."
      , align = "center")
  )
)

# Abas para variável resposta
variaveis_resposta <- fluidRow(column(
  width = 12,
  tabsetPanel(
           fluidRow(
             column(
               width = 6,
               tabBox(
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 tabPanel("Dados",
                          tableOutput(outputId = "dados_resposta")),
                 tabPanel("Sumário", verbatimTextOutput("sumario_resposta")),
                 tabPanel("Histograma",
                          plotOutput("histograma_resposta")),
                 tabPanel("Boxplot",
                          plotOutput("boxplot_resposta")),
                 sidebar = boxSidebar(
                   id = "sidebar_resposta",
                   sliderInput(inputId = "Numero_resposta",
                               "Número de dados:",
                               min = 1,
                               max = 20,
                               value = 10
                   )
                 )
               )
             )
           )
  )
)
)

#Relação entre as variáveis
texto_relacao <- fluidRow(
  column(
    width = 12,
    p("Notaremos relação entre sales e as outras variáveis. Além da correlação entre as variáveis.", align = "center")
  )
)

relacao_variaveis <- fluidRow(
  column(
    width = 10,
    tabBox(
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tabPanel("Sales e Facebbok",
               plotOutput(outputId = "sales_facebook")),
      tabPanel("Sales e Youtube",
               plotOutput(outputId = "sales_youtube")),
      tabPanel("Sales e Newspaper",
               plotOutput(outputId = "sales_newspaper")),
      tabPanel("Sales e Todos",
               plotOutput(outputId = "sales_todos")),
      tabPanel("Correlações",
               plotOutput(outputId = "Correlacoes"))
      )
  )
)

################## Seleção de Modelos #############################

#Todas as variáveis
texto_tod_var <- fluidRow(
  column(
    width = 12,
    p("Nesta parte iremos realizar a modelagem de uma regressão linear múltipla com todas as variáveis
      d modelo. Além disso, iremos observar a análise de variância (ANOVA), os coeficientes, a significância
      de cada variável, R^2 e R^2_adj", align = "center")
  )
)


todas_variaveis <- fluidRow(
  column(
  width = 10,
  tabBox(
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    tabPanel("Coeficientes",
             tableOutput(outputId = "coeficientes")),
    tabPanel("Significância das variâncias",
             tableOutput(outputId = "significancia_var")),
    tabPanel("Resíduos", verbatimTextOutput("resid")),
    tabPanel("Anova",
             tableOutput(outputId = "anova")),
    tabPanel("R^2",
             tableOutput(outputId = "R2")),
    tabPanel("R^2_adj",
             tableOutput(outputId = "R2_adj"))
  )
  )
)

modelo <- lm(sales ~ ., data = pacoteFRT::dadosFRT)


#Melhores variáveis
texto_melh_var <- fluidRow(
  column(
    width = 12,
    p("Nesta sessãoiremos elaborar a seleção de variáveis segundo dois métodos diferentes (Backward
      Selection e Forward Selection) e também iremos ilustrar o C_p de Mallow e o critério BIC de escolha,
      sendo que estes foram desenvolvidos no pacoteFRT.", align = "center")
  )
)

melhores_variaveis <- fluidRow(
    column(
      width = 10,
      tabBox(
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        tabPanel("Backward Selection",
                 box(
                   width =  10,
                   status = "warning",
                   solidHeader = TRUE,
                   title = textOutput("text_coefs_back"),
                   id = "coefs_back",
                   collapsible = FALSE,
                   closable = FALSE,
                   tableOutput("coefs_back")
                 ),
                 box(
                   width =  10,
                   status = "warning",
                   solidHeader = TRUE,
                   title = textOutput("text_signi_back"),
                   id = "signi_back",
                   collapsible = FALSE,
                   closable = FALSE,
                   tableOutput("signi_back")
                 ),
                 box(
                   width =  10,
                   status = "warning",
                   solidHeader = TRUE,
                   title = textOutput("text_anova_back"),
                   id = "anova_back",
                   collapsible = FALSE,
                   closable = FALSE,
                   tableOutput("anova_back")
                 )),
        tabPanel("Forward Selection",
                 box(
                   width =  10,
                   status = "warning",
                   solidHeader = TRUE,
                   title = textOutput("text_coefs_for"),
                   id = "coefs_for",
                   collapsible = FALSE,
                   closable = FALSE,
                   tableOutput("coefs_for")
                 ),
                 box(
                   width =  10,
                   status = "warning",
                   solidHeader = TRUE,
                   title = textOutput("text_signi_for"),
                   id = "signi_for",
                   collapsible = FALSE,
                   closable = FALSE,
                   tableOutput("signi_for")
                 ),
                 box(
                   width =  10,
                   status = "warning",
                   solidHeader = TRUE,
                   title = textOutput("text_anova_for"),
                   id = "anova_for",
                   collapsible = FALSE,
                   closable = FALSE,
                   tableOutput("anova_for")
                 )),
        tabPanel("C_p de Mallow",
                 plotOutput(outputId = "C_p_mallow")),
        tabPanel("BIC",
                 plotOutput(outputId = "bic"))
      )
    )
  )

back <- backward_regression(pacoteFRT::dadosFRT, "sales")
forw <- forward_regression(pacoteFRT::dadosFRT, "sales")

################################ Análise dos resíduos ##############################
texto_anal_res <- fluidRow(
  column(
    width = 12,
    p("Após selecionar as variáveis precisamos verificar as pressuposições do modelo de regressão, que
      são que a soma dos resíduos é 0, os resíduos seguem uma distribuição normal, homocedasticidade dos
      resíduos (variância constante dos resíduos). Assim, para isso iremos ilustrar graficamente QQplot
      para verificar normalidade, iremos realizar o teste de Kruskall Wallis, o gráfico de
      homocedasticidade.", align = "center")
  )
)

analise_residual <- fluidRow(
  column(
    width = 10,
    tabBox(
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tabPanel("Backward Selection",
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_QQplot_back"),
                 id = "QQplot_back",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("QQplot_back")
               ),
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_cook_back"),
                 id = "cook_back",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("cook_back")
               ),
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_outro_back"),
                 id = "outro_back",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("outro_back")
               )),
      tabPanel("Forward Selection",
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_QQplot_for"),
                 id = "QQplot_for",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("QQplot_for")
               ),
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_cook_for"),
                 id = "cook_for",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("cook_for")
               ),
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_outro_for"),
                 id = "outro_for",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("outro_for")
               )),
      tabPanel("Modelo Completo",
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_QQplot_completo"),
                 id = "QQplot_completo",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("QQplot_completo")
               ),
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_cook_completo"),
                 id = "cook_completo",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("cook_completo")
               ),
               box(
                 width =  10,
                 status = "warning",
                 solidHeader = TRUE,
                 title = textOutput("text_outro_completo"),
                 id = "outro_v",
                 collapsible = FALSE,
                 closable = FALSE,
                 plotOutput("outro_completo")
               ))
    )
  )
)

################################ Predição #############################
texto_pred <- fluidRow(
  column(
    width = 12,
    p("A seguir iremos realizar a predição de variável resposta (sales) dependendo do modelo
      de seleção escolhido  e também você poderá escolher qual valor você quer para caa variável.
      Além disso, iremos realizar o intervalo de confiança.", align = "center")
  )
)

# xnewf = data.frame(facebook = 100, youtube = 100, newspaper = 100)
# #xnewb = data.frame(facebook = 100, youtube = 100), da o mesmo que o de cima, então não influencia colocar mais em qual nã tem.
# predict(forw, xnewf, interval = "confidence")
# predict(forw, xnewf, interval = "prediction")


predicao <- fluidRow(
  column(
    width = 5,
    sidebarLayout(
      sidebarPanel(
        selectInput("modelo_selecionado", "Selecione o Modelo",
                    choices = c("Modelo Completo", "Backward Selection", "Forward Selection"),
                    selected = "Modelo Completo"
        ),
        textInput("facebook_id", "Facebook"),
        textInput("youtube_id", "Youtube"),
        textInput("newspaper_id", "Newspaper")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Resultados", tableOutput("resultados"))
        )
      )
    )
  )
)


################################ Conclusão #############################
texto_concl <- fluidRow(
  column(
    width = 12,
    p("Assim, finalizamos a parte do Shiny em que o usuário consegue fazer alterações e analisar
    qual a parte principal para o usuário. Sendo que é realizada uma análise completa do banco de
    dados. Além disso, notamos que as vendas por newspaper não são relevantes para o backward e
    forward regression sendo que a predição e o intervalo de confiança são alterados um pouco
    dependendo do modelo que está sendo implementado.", align = "center")
  )
)

# -------------------- BODY   ----------------------
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "intro",
      intro1,
      intro2
    ),
    tabItem(
      tabName = "vari_mod",
      h2("Variáveis do modelo"),
      texto_eda_var_mod,
      variaveis_modelo
    ),
    tabItem(
      tabName = "vari_res",
      h2("Variável Resposta"),
      texto_eda_var_resp,
      variaveis_resposta
    ),
    tabItem(
      tabName = "relacao_vari",
      h2("Relação entre as variáveis"),
      texto_relacao,
      relacao_variaveis
    ),
    tabItem(
      tabName = "todas_vari",
      h2("Todas as variáveis"),
      texto_tod_var,
      todas_variaveis
    ),
    tabItem(
      tabName = "melhores_vari",
      h2("Melhores variáveis"),
      texto_melh_var,
      melhores_variaveis
    ),
    tabItem(
      tabName = "residuos",
      texto_anal_res,
      analise_residual
    ),
    tabItem(
      tabName = "predicao",
      texto_pred,
      predicao
    ),
    tabItem(
      tabName = "conclusao",
      h2("Conclusão"),
      texto_concl
    )
  )
)


#### Construindo a Interface Gráfica ####

ui <- dashboardPage(
  title = "Dashboard Produto de Dados",
  header = header,
  sidebar = Sidebar,
  footer = footer,
  body = body
)

######## Construindo o Servidor ########
server <- function(input, output, session){

  ### Análise Descritiva
  ## Variáveis do Modelo

  #Escolhendo a variável
  dados_selecionados <- reactive({
    dados <- pacoteFRT::dadosFRT[, input$variavel_escolhida, drop = FALSE]
    head(dados, input$numero_dados)
  })

  #Dados
  output$tabela_dados <- renderTable({
    dados_selecionados()
  })

  #Sumário
  output$sumario <- renderPrint({
    summary(dados_selecionados())
  })

  #Histograma
  output$histograma <- renderPlot({
    dados <- as.numeric(pacoteFRT::dadosFRT[[input$variavel_escolhida]])
    if (!all(is.na(dados))) {
      hist(dados, main = paste("Histograma de", input$variavel_escolhida))
    }
  })

  #Boxplot
  output$boxplot <- renderPlot({
    boxplot(dados_selecionados(), main = paste("Boxplot de", input$variavel_escolhida))
  })

  ##Variável resposta
  # Função para criar um subconjunto dos dados com base no número escolhido
  dados_subset <- reactive({
    head(pacoteFRT::dadosFRT$sales, input$Numero_resposta)
  })

  # Aba de "Dados"
  output$dados_resposta <- renderTable({
    data.frame(Observação = seq_len(input$Numero_resposta), Sales = dados_subset())
  })

  # Aba de "Sumário"
  output$sumario_resposta <- renderPrint({
    summary(pacoteFRT::dadosFRT$sales)
  })

  # Aba de "Histograma"
  output$histograma_resposta <- renderPlot({
    ggplot(data.frame(Sales = pacoteFRT::dadosFRT$sales), aes(x = Sales)) +
      geom_histogram(color = "black", fill = "orange") +
      theme_bw() +
      labs(title = "Histograma de Sales")
  })

  # Aba de "Boxplot"
  output$boxplot_resposta <- renderPlot({
    ggplot(data.frame(Sales = pacoteFRT::dadosFRT$sales), aes(x = "Sales", y = Sales)) +
      geom_boxplot(color = "black", fill = "orange") +
      theme_bw() +
      labs(title = "Boxplot de Sales")
  })

  ## Relação entre as Variáveis
  # Sales e Facebook
  output$sales_facebook <- renderPlot({
    pacoteFRT::dadosFRT %>% ggplot() +
      geom_line(aes(x = sales, y = facebook), color = "Blue") +
      theme_bw()
  })

  # Sales e Youtube
  output$sales_youtube <- renderPlot({
    pacoteFRT::dadosFRT %>% ggplot() +
      geom_line(aes(x = sales, y = youtube), color = "Red") +
      theme_bw()
  })

  # Sales e Newspaper
  output$sales_newspaper <- renderPlot({
    pacoteFRT::dadosFRT %>% ggplot() +
      geom_line(aes(x = sales, y = newspaper), color = "Green") +
      theme_bw()
  })

  # sales_todos
  output$sales_todos <- renderPlot({
    pacoteFRT::dadosFRT %>% ggplot() +
      geom_line(aes(x = sales, y = newspaper), color = "Green") +
      geom_line(aes(x = sales, y = youtube), color = "Red") +
      geom_line(aes(x = sales, y = facebook), color = "Blue") +
      theme_bw()
  })
  # sales_todos
  output$Correlacoes <- renderPlot({
    ggpairs(pacoteFRT::dadosFRT)
  })

  ###Seleção de Modelos
  ##Todas as variáveis

  #Coeficientes
  output$coeficientes <- renderTable({
    coef_table <- data.frame(Coeficientes = coef(modelo))  # Crie um data.frame com uma única coluna
    coef_table
  })

  #Significâncias das variáveis
  output$significancia_var <- renderTable({
    summary(modelo)$coefficients
  })

  #Resíduos
  output$resid <- renderPrint({
    summary(modelo$residuals)
  })

  #Anova
  output$anova <- renderTable({
    anova(modelo)
  })

  #R2
  output$R2 <- renderTable({
    R2_table <- data.frame(R2 = summary(modelo)$r.squared)
    R2_table
  })

  #R2_adj
  output$R2_adj <- renderTable({
    R2_adj_table <- data.frame(R2_adj = summary(modelo)$adj.r.squared)
    R2_adj_table
  })


  ##Melhores Variáveis

  # Coeficientes Back
  output$coefs_back <- renderTable({
    coef_tableb <- data.frame(Coeficientes_Back = coef(back))  # Crie um data.frame com uma única coluna
    coef_tableb
  })

  output$text_coefs_back <- renderText({
    "Coeficientes do Backward Selection"
  })

  # Significancia Back
  output$signi_back <- renderTable({
    summary(back)$coefficients
  })

  output$text_signi_back <- renderText({
    "Significância dos coeficientes do Backward Selection"
  })

  # Anova Back
  output$anova_back <- renderTable({
    anova(back)
  })

  output$text_anova_back <- renderText({
    "Anova do Backward Selection"
  })

  # Coeficientes For

  output$coefs_for <- renderTable({
    coef_tablef <- data.frame(Coeficientes_Forward = coef(forw))  # Crie um data.frame com uma única coluna
    coef_tablef
  })

  output$text_coefs_for <- renderText({
    "Coeficientes do Forward Selection"
  })

  # Significancia for
  output$signi_for <- renderTable({
    summary(forw)$coefficients
  })

  output$text_signi_for <- renderText({
    "Significância dos coeficientes do Forward Selection"
  })

  # Anova for
  output$anova_for <- renderTable({
    anova(forw)
  })

  output$text_anova_for <- renderText({
    "Anova do Forward Selection"
  })

  # Cp Mallow
  output$C_p_mallow <- renderPlot({
    Cp_Mallow(pacoteFRT::dadosFRT, "sales")
  })

  # BIC
  output$bic <- renderPlot({
    bic_grafico(pacoteFRT::dadosFRT, "sales")
  })

  ##Análise de resíduos

  # QQplot Back
  output$QQplot_back <- renderPlot({
    plot(back, which = 2, las = 1, pch = 19, col = "blue")
  })

  output$text_QQplot_back <- renderText({
    "QQplot do Backward Selection"
  })

  # Distância de Cook Back
  output$cook_back <- renderPlot({
    plot(back, which = 4, las = 1, pch = 19, col = "blue")
  })

  output$text_cook_back <- renderText({
    "Distância de cook do Backward Selection"
  })

  # Outro Back
  output$outro_back <- renderPlot({
    plot(back, which = 1, las = 1, pch = 19, col = "blue")
  })

  output$text_outro_back <- renderText({
    "Outro do Backward Selection"
  })

  # QQplot for
  output$QQplot_for <- renderPlot({
    plot(forw, which = 2, las = 1, pch = 19, col = "blue")
  })

  output$text_QQplot_for <- renderText({
    "QQplot do Forward Selection"
  })

  # Distância de Cook for
  output$cook_for <- renderPlot({
    plot(forw, which = 4, las = 1, pch = 19, col = "blue")
  })

  output$text_cook_for <- renderText({
    "Distância de cook do Forward Selection"
  })

  # Outro for
  output$outro_for <- renderPlot({
    plot(forw, which = 1, las = 1, pch = 19, col = "blue")
  })

  output$text_outro_for <- renderText({
    "Outro do Forward Selection"
  })

  # QQplot completo
  output$QQplot_completo <- renderPlot({
    plot(modelo, which = 2, las = 1, pch = 19, col = "blue")
  })

  output$text_QQplot_completo <- renderText({
    "QQplot do Modelo Completo"
  })

  # Distância de Cook completo
  output$cook_completo <- renderPlot({
    plot(modelo, which = 4, las = 1, pch = 19, col = "blue")
  })

  output$text_cook_completo <- renderText({
    "Distância de cook do Modelo Completo"
  })

  # Outro for
  output$outro_completo <- renderPlot({
    plot(modelo, which = 1, las = 1, pch = 19, col = "blue")
  })

  output$text_outro_completo <- renderText({
    "Outro do Modelo Completo"
  })

  ##Predição

  observe({
    if (input$modelo_selecionado != "Modelo Completo") {
      updateTextInput(session, "facebook_id", value = "")
      updateTextInput(session, "youtube_id", value = "")
      updateTextInput(session, "newspaper_id", value = "")
    }
  })

  output$resultados <- renderTable({
    xnew <- data.frame(
      facebook = as.numeric(input$facebook_id),
      youtube = as.numeric(input$youtube_id),
      newspaper = as.numeric(input$newspaper_id)
    )

    predicao <- NULL  # Inicializa predicao
    confianca <- NULL  # Inicializa confianca

    if (input$modelo_selecionado == "Backward Selection") {
      predicao <- predict(back, xnew, interval = "prediction")
      confianca <- predict(back, xnew, interval = "confidence")
    } else if (input$modelo_selecionado == "Forward Selection") {
      predicao <- predict(forw, xnew, interval = "prediction")
      confianca <- predict(forw, xnew, interval = "confidence")
    } else if (input$modelo_selecionado == "Modelo Completo") {
      predicao <- predict(modelo, xnew, interval = "prediction")
      confianca <- predict(modelo, xnew, interval = "confidence")
    }

    resultado_predicao <- data.frame(
      "Predição" = predicao[, 1],
      "Intervalo de Predição Inferior" = predicao[, 2],
      "Intervalo de Predição Superior" = predicao[, 3]
    )

    resultado_confianca <- data.frame(
      "Intervalo de Confiança Inferior" = confianca[, 2],
      "Intervalo de Confiança Superior" = confianca[, 3]
    )

    list(resultado_predicao, resultado_confianca)
  })
}

shinyApp(ui, server)
