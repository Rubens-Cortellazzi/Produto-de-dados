############################### Modelo Dashboard ###############################
# Carregando os pacotes
library(shiny)
library(bs4Dash)
library(GGally)
library(thematic)
library(waiter)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(sortable) 
library(readxl)
library(pacoteFRT)

# auto-configurando o tema

thematic_shiny()

######## Definindo objetos importantes ########

#### Def Header ####

titulo <- dashboardBrand(
  title = "Modelagem linear",
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
    menuItem("Escolha do banco de dados", tabName = "escolha", 
             icon = icon("database")),
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
               "Validação Cruzada", 
               tabName = "validacao"
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
  left = "Felipe RA: 236106. Pedro RA: 186935. Rubens RA: 236292. Tiago RA: 217517. Vitor RA: 168264.",
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
#################### Escolha do banco de dados #####################
texto_escolha_banco <- intro2 <- fluidRow(
  column(
    width = 12,
    p("Nesta primeira parte do dashboard, você poderá escolher qual banco de dados você deseja analisar,
      sendo que serão aceitos bancos de dados no formato csv, txt e xlsx. Além disso, você terá que
      escolher qual a variável resposta do banco de dados que você deseja analisar. ", align = "center"),
    p("* A primeira versão deste dashboard ", strong("só aceita variáveis numéricas."), style = "color:red", align = "center")
  ))

escolha_banco <- fluidRow(
  sidebarLayout(
    sidebarPanel(
      fileInput("arquivo", "Selecione um arquivo"),
      selectInput("variavel_resposta", "Selecione a Variável Resposta", choices = NULL)
      # ,actionButton("realizar_analise", "Realizar Análise")
    ),
    mainPanel(
      uiOutput("bucket")
    )
  )
)


################## Análise descritiva #############################

#Variáveis do modelo
texto_eda_var_mod <- fluidRow(
  column(
    width = 12,
    p("Notaremos abaixo a análise descritiva do banco de dados, com excessão da variável resposta.", align = "center")
  )
)

variaveis_modelo <- fluidRow(
  column(width = 12,
         sidebarLayout(
           sidebarPanel(
             selectInput("variavel_escolhida", "Selecione a Variável",
                         choices = NULL),
             sliderInput("numero_dados", "Número de Dados para Exibir",
                         min = 1, max = 187, value = 10)
           ),
           mainPanel(
             tabBox(
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               tabPanel("Dados",
                        tableOutput(outputId = "tabela_dados")),
               tabPanel("Sumário", dataTableOutput("sumario")),
               tabPanel("Histograma",
                        plotOutput("histograma")),
               tabPanel("Boxplot",
                        plotOutput("boxplot"))
               )
             )
           )
         )
  )


#Variável Resposta
texto_eda_var_resp <- fluidRow(
  column(
    width = 12,
  )
)

# Abas para variável resposta
variaveis_resposta <- fluidRow(column(
  width = 12,
  tabsetPanel(
    fluidRow(
      column(
        width = 12,
        tabBox(
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          tabPanel("Dados",
                   tableOutput(outputId = "dados_subset")),
          tabPanel("Sumário", dataTableOutput("sumario_resposta")),
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
    width = 12
  )
)


relacao_variaveis <- fluidRow(
  column(width = 12,
         selectizeInput("variavel_escolhida_relacao", "Selecione a Variável",
                     choices = NULL, multiple = T),
         tabsetPanel(
           tabPanel("Gráfico de Linha", plotOutput("grafico_linha_variavel")),
           tabPanel("Correlação", plotOutput("correlacao_variavel"))
         )
  ),
  mainPanel()
)

################## Seleção de Modelos #############################

#Todas as variáveis
texto_tod_var <- fluidRow(
  column(
    width = 12,
    p("Todas as combinações de modelos lineares podem ser vistas abaixo, junto com seus 
      valores estimados, estatísticas e métricas de ajuste.", align = "center")
  )
)

todas_variaveis <- fluidRow(
  column(width = 10,
         sidebarLayout(
           sidebarPanel(
             selectInput("modelo_selecionado", "Selecione o Modelo", choices = NULL),
             tabsetPanel(
               tabPanel("Coeficientes", tableOutput("tabela_coefs")), 
               tabPanel("Métricas", tableOutput("metricas")), 
               tabPanel("ANOVA", tableOutput("anova_todos"))
             )
           ),

          mainPanel()
        )
 )
)


#Melhores variáveis
texto_validacao <- fluidRow(
  column(
    width = 12,
    p("Separe seu conjunto entre treino e teste e verifique a perfomance dos 
      modelos fora da amostra.", align = "center")
  )
)

validacao_cruzada <- fluidRow(
  column(width = 6, 
         sidebarLayout(
           sidebarPanel(
             sliderInput("prop_cv", "Proporção de Dados de Treino",
                         min = 0.1, max = 0.9, value = 0.7, step = 0.01)
           ), 
           mainPanel()
         )
  ), column(width = 6, 
            sidebarLayout(
              sidebarPanel(
                selectInput("metrica_cv", "Ordenar por:", choices = NULL)
              ), 
              
              mainPanel()
            )
  ), tabsetPanel(
    tabPanel("Validação Cruzada", tableOutput("tab_cv"))
  )
)


################################ Análise dos resíduos ##############################
texto_anal_res <- fluidRow(
  column(
    width = 12,
    p("Após selecionar as variáveis precisamos verificar as pressuposições do modelo de regressão, que
      são que a média dos resíduos é 0, os resíduos seguem uma distribuição normal, homocedasticidade dos
      resíduos (variância constante dos resíduos).", align = "center")
  )
)

analise_residual <- fluidRow(
  column(
    width = 10,
    tabBox(
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tabPanel(
        "Histograma",
        plotOutput("histograma_residuos")
      ),
      tabPanel(
        "Boxplot",
        plotOutput("boxplot_residuos")
      ),
      tabPanel(
        "Dispersão",
        plotOutput("dispersao_residuos")
      ),
      tabPanel(
        "QQplot",
        plotOutput("qqplot_residuos")
      )
  )
)
)

################################ Predição #############################
texto_pred <- fluidRow(
  column(
    width = 12,
    h1("Predição"),
    hr()
  )
)


predicao <- fluidRow(
  column(
    width = 4,
    actionButton(
      "realizar_predicao",
      label = "Realizar Predição"
    ), 
    hr(),
    uiOutput("col")
  ),
  column(
    width = 8,
    tableOutput("resultados")
  )
)

################################ Conclusão #############################
texto_concl <- fluidRow(
  column(
    width = 12,
    p("Assim, finalizamos a parte do Shiny em que o usuário consegue fazer alterações e analisar
    qual a parte principal para o usuário. Sendo que é realizada uma análise completa do banco de
    dados.", align = "center")
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
      tabName = "escolha",
      texto_escolha_banco,
      escolha_banco
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
      tabName = "validacao", 
      h2("Validação Cruzada"),
      texto_validacao,
      validacao_cruzada,
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

  ###Escolha do banco de dados
  dados <- reactive({
    req(input$arquivo)
    extensao <- tools::file_ext(input$arquivo$name)

    if (extensao == "csv") {
      return(read.csv(input$arquivo$datapath))
    } else if (extensao == "xlsx") {
      return(read_xlsx(input$arquivo$datapath))
    } else if (extensao == "txt") {
      return(read.delim(input$arquivo$datapath))
    } else {
      return(NULL)
    }
  })

  #Variavel Resposta
  observe({
    if (!is.null(dados())) {
      updateSelectInput(session, "variavel_resposta", choices = colnames(dados()))
    } else {
      updateSelectInput(session, "variavel_resposta", choices = NULL)
    }
  })
  
  #Escolher as variáveis que farão parte do seu modelo
  observe({
    if (!is.null(dados())) {
      output$bucket <- renderUI({
        bucket_list(
          header = "Arraste as variáveis desejadas no modelo",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Variáveis da base",
            labels = colnames(dados())[!colnames(dados()) %in% input$variavel_resposta],
            input_id = "rank_list_1"
          ),
          add_rank_list(
            text = "Variáveis desejadas no modelo",
            labels = NULL,
            input_id = "rank_list_2"
          )
        )  
      })
      
          } else {
            output$bucket <- renderUI({
              bucket_list(
                header = "Arraste as variáveis desejadas no modelo",
                group_name = "bucket_list_group",
                orientation = "horizontal",
                add_rank_list(
                  text = "Variáveis da base",
                  labels = NULL,
                input_id = "rank_list_1"
              ),
              add_rank_list(
                text = "Variáveis desejadas no modelo",
                labels = NULL,
                input_id = "rank_list_2"
              )
            )  
            })
    }
  })
  

  ### Análise Descritiva
  ## Variáveis do Modelo

  #Variáveis do banco de dados para escolher
  observe({
    if (!is.null(dados())) {
      updateSelectInput(session, "variavel_escolhida", choices = input$rank_list_2)
    } else {
      updateSelectInput(session, "variavel_escolhida", choices = NULL)
    }
  })

  #Escolhendo a variável
  dados_preditora <- reactive({
    dados()[, input$variavel_escolhida, drop = FALSE]
  })

  dados_selecionados <- reactive({
    head(dados_preditora(), input$numero_dados)
  })

  #Dados
  output$tabela_dados <- renderTable({
    dados_selecionados()
  })

  #Sumário
  output$sumario <- renderDataTable({
    tabela_descritiva(dados_preditora(), input$variavel_escolhida)
  })

  #Histograma
  output$histograma <- renderPlot({
    if (!all(is.na(dados_preditora()))) {
      data.frame("valores" = as.numeric(dados_preditora()[[input$variavel_escolhida]])) %>% 
        ggplot(aes(x = valores)) +
        geom_histogram(fill = "lightblue", color = "black") +
        labs(x="Valores", y="Frequência", title = paste("Histograma de", input$variavel_escolhida)) +
        theme_bw()
    }
  })

  #Boxplot
  output$boxplot <- renderPlot({
    if (!all(is.na(dados_preditora()))) {
      data.frame("valores" = as.numeric(dados_preditora()[[input$variavel_escolhida]])) %>% 
        ggplot(aes(x = valores)) +
        geom_boxplot(fill = "lightblue", color = "black") +
        labs(x="Valores", y="Distribuição", title = paste("Boxplot de", input$variavel_escolhida)) +
        theme_bw()
    }
  })

  ##Variável resposta
  # Função para criar um subconjunto dos dados com base no número escolhido

  # Aba de "Dados"
  output$dados_subset <- renderTable({
    data.frame(Observação = seq_len(input$Numero_resposta), head(dados()[, input$variavel_resposta, drop = FALSE], input$Numero_resposta))
  })

  # Aba de "Sumário"
  output$sumario_resposta <- renderDataTable({
    tabela_descritiva(dados(), input$variavel_resposta)
  })

  # Aba de "Histograma"
  output$histograma_resposta <- renderPlot({
    ggplot(dados(), aes(x = .data[[input$variavel_resposta]])) +
      geom_histogram(color = "black", fill = "orange") +
      theme_bw() +
      labs(title = paste("Histograma de", input$variavel_resposta),
           x = "Valores", y = "Contagem")
  })

  # Aba de "Boxplot"
  output$boxplot_resposta <- renderPlot({
    ggplot(dados(), aes(x = input$variavel_resposta, y = .data[[input$variavel_resposta]])) +
      geom_boxplot(color = "black", fill = "orange") +
      theme_bw() +
      labs(title = paste("Boxplot de", input$variavel_resposta),
           x = "", y = "Valores")
  })

  ## Relação entre as Variáveis
  #Variáveis do banco de dados
  
  observe({
    if (!is.null(dados())) {
      updateSelectizeInput(session, "variavel_escolhida_relacao", choices = c(input$rank_list_2, input$variavel_resposta), selected = head(input$rank_list_2, 2), options = list(maxItems = 2))
    } else {
      updateSelectizeInput(session, "variavel_escolhida_relacao", choices = NULL)
    }
  })

  #Escolhendo a variável
  
  # # Aba de line"

  output$grafico_linha_variavel <- renderPlot({
    
    if (!is.null(dados()) && !is.na(input$variavel_escolhida_relacao[1]) && !is.na(input$variavel_escolhida_relacao[2])) {
      
        ggplot(dados(), aes(x = .data[[input$variavel_escolhida_relacao[1]]], y = .data[[input$variavel_escolhida_relacao[2]]])) +
          geom_line(color = "Red") +
          theme_bw() +
          labs(title = "Gráfico de Linha") 
      
    } 
    
  })

  
  # # correlação

  output$correlacao_variavel <- renderPlot({
    
    if (!is.null(dados()) && !is.na(input$variavel_escolhida_relacao[1]) && !is.na(input$variavel_escolhida_relacao[2])) {
  
      dados() %>%
        select(input$variavel_escolhida_relacao[1], input$variavel_escolhida_relacao[2]) %>%
        ggpairs()
        
    }
      
  })

  ###Seleção de Modelos
  ##Todas as variáveis

  observe({
    if (!is.null(dados()) && !is.null(input$variavel_resposta) && any(names(dados()) %in% input$rank_list_2)) {
     
      updateSelectInput(session, "modelo_selecionado",
                        choices = paste("Modelo", 1:length(modelos(dados(), input$variavel_resposta, input$rank_list_2))))
    } else {
      updateSelectInput(session, "modelo_selecionado", choices = NULL)
    }
  })

  modelo <- reactive({
    lista_modelos <- modelos(dados(), input$variavel_resposta, input$rank_list_2)
    names(lista_modelos) <- paste("Modelo", 1:length(lista_modelos))
    
    lista_modelos
  })
  
  modelo_metricas <- reactive({
    summary(modelo()[[input$modelo_selecionado]])
  })
  
  output$tabela_coefs <- renderTable({
      coef <- data.frame(coef(summary(modelo()[[input$modelo_selecionado]])))
      coef <- tibble::rownames_to_column(coef)
      colnames(coef) <- c("Parâmetros", "Estimativa", "E. Padrão", "t", "Pr(>|t|)")
      
      coef
    })
  
  output$metricas <- renderTable({
    data.frame("AIC" = AIC(modelo()[[input$modelo_selecionado]]), 
               "R^2" = modelo_metricas()[["r.squared"]], 
               "R^2 Ajustado" = modelo_metricas()[["adj.r.squared"]])
  })
  
  output$anova_todos <- renderTable({
    anova <- data.frame(anova(modelo()[[input$modelo_selecionado]]))
    anova <- tibble::rownames_to_column(anova)
    colnames(anova) <- c(" ", "GL", "SQ", "MQ", "F", "Pr(>F)")
    anova <- anova[1:2] |> cbind(round(anova[3:6], 3))
    anova$`F` <- replace(anova$`F`, is.na(anova$`F`), " ")
    anova$`Pr(>F)` <- replace(anova$`Pr(>F)`, is.na(anova$`Pr(>F)`), " ")
    
    anova
  })


  ## Validação Cruzada
  cross <- reactive({
    cross_validate(modelo(), dados(), input$variavel_resposta, input$prop_cv)
  })
  
  observe({
    if (!is.null(dados()) && any(names(dados()) %in% input$rank_list_2)) {
      updateSelectInput(session, "metrica_cv", choices = colnames(cross()))
    } else {
      updateSelectInput(session, "metrica_cv", choices = NULL)
    }
  })

  output$tab_cv <- renderTable({
    cv <- tibble::rownames_to_column(data.frame(cross()), "Modelos")
    cv$Modelos <- paste("Modelo", 1:nrow(cv))
    
    dplyr::arrange(cv, cv[[input$metrica_cv]])
  })

  ##Análise de resíduos

  ## Resíduos todos modelos
  
  output$histograma_residuos <- renderPlot({
    residuos <- modelo()[[input$modelo_selecionado]]$residuals
    
    as.data.frame(residuos) %>% 
      ggplot(aes(x = residuos)) + 
      geom_histogram(fill="lightblue", color="black") +
      labs(y = "Frequência") + theme_bw()

  })
  
  output$boxplot_residuos <- renderPlot({
    residuos <- modelo()[[input$modelo_selecionado]]$residuals
    
    as.data.frame(residuos) %>% 
      ggplot(aes(x = residuos)) + geom_boxplot(fill="lightblue", color="black") +
      labs(y = "Frequência") + theme_bw()
    
  })
  
  output$dispersao_residuos <- renderPlot({
    residuos <- modelo()[[input$modelo_selecionado]]$residuals
    ajustado <- modelo()[[input$modelo_selecionado]]$fitted.values
    
    data.frame(residuos, ajustado) %>% 
      ggplot(aes(x = residuos, y = ajustado)) + 
      geom_point() +
      geom_smooth(se = F, method = "lm") +
      labs(y = "Ajustado", x = "Resíduos") + theme_bw()
    
  })
  
  output$qqplot_residuos <- renderPlot({
    residuos <- modelo()[[input$modelo_selecionado]]$residuals
    
    as.data.frame(residuos) %>% 
      ggplot(aes(sample = residuos)) + 
      geom_qq(color = "blue") +
      geom_qq_line() +
      labs(y = "Resíduos", x = "Quantis-Teóricos") +
      theme_bw()
  })

  ##Predição
  
  # Talvez no começo não exista covariaveis
  covariaveis <- reactive(names(modelo()[[input$modelo_selecionado]]$model)[2:length(names(modelo()[[input$modelo_selecionado]]$model))])
  data_pred <- reactiveValues(data = NULL)
  
  output$col <- renderUI({
    map(covariaveis(), ~ textInput(.x, label = .x))
  })
  
  observeEvent(input$realizar_predicao, {
    valores <- map_vec(covariaveis(), ~ as.numeric(input[[.x]]) %||% "")
    
    data_pred$data <- data.frame(t(valores))
    
    colnames(data_pred$data) <- covariaveis()
  })
  
  output$resultados <- renderTable({
    if (is.null(data_pred$data)) {return()}
    
    pred <- predict(modelo()[[input$modelo_selecionado]], newdata = data_pred$data, interval = "confidence") 
    
    data.frame("predito" = pred[1], "intervalo inf" = pred[2]
               , "intervalo sup" = pred[3])
  })
  
}

shinyApp(ui, server)
