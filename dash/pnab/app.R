if(require(shiny) == F) install.packages("shiny"); require(shiny)
if(require(shinydashboard) == F) install.packages("shinydashboard"); require(shinydashboard)
if(require(shinydashboardPlus) == F) install.packages("shinydashboardPlus"); require(shinydashboardPlus)
if(require(DT) == F) install.packages("DT"); require(DT)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(ggrepel) == F) install.packages("ggrepel"); require(ggrepel)
if(require(shinyWidgets) == F) install.packages("shinyWidgets"); require(shinyWidgets)
if(require(plotly) == F) install.packages("plotly"); require(plotly)
if(require(sf) == F) install.packages("sf"); require(sf)
if(require(ggraph) == F) install.packages("ggraph"); require(ggraph)
if(require(tidygraph) == F) install.packages("tidygraph"); require(tidygraph)
if(require(splitstackshape) == F) install.packages("splitstackshape"); require(splitstackshape)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(plotly) == F) install.packages("plotly"); require(plotly)



options(scipen = 999)

format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}

pnab <- readRDS("BANCO PNAB.RDS") %>% mutate(EDITAL_TIPO = ifelse(EDITAL_TIPO == "TECs", "Fomento", EDITAL_TIPO)) %>%
  mutate(funcao = ifelse(str_detect(funcao, "\\(") == T, gsub("\\(.*?\\)", "", funcao),
                         funcao)) %>% 
  mutate(sit_endereco = ifelse(sit_endereco == "Situação de Itinerância", "Itinerância",
                               sit_endereco))

pnab_faixa <- read_xlsx("PNAB - Faixas.xlsx")

pnab <- pnab %>% left_join(pnab_faixa, by = c("EDITAL", "categoria"))

rd_mun <- read_xlsx("RD_Municipio.xlsx") %>% select(-macrorreg_new)

rd_mun1 <- read_xlsx("RD_Municipio.xlsx") %>% 
  mutate(macrorreg_new = ifelse(macrorreg_new == "Mata", "Zona da Mata", macrorreg_new))

pnab <- pnab %>% left_join(rd_mun, by = "municipio")


pernambuco <- st_read("PE_Municipios_2022.shp") %>% 
  left_join(rd_mun %>% mutate(municipio = ifelse(municipio == "Belém de São Francisco", "Belém do São Francisco",
                                                 ifelse(municipio == "São Caetano", "São Caitano",
                                                        ifelse(municipio == "Iguaraci", "Iguaracy",
                                                               municipio)))), 
            by = c("NM_MUN" = "municipio")) %>% 
  mutate(macrorreg_f = ifelse(str_detect(rd, "Mata") == T, "Zona da Mata",
                              ifelse(str_detect(rd, "Metropolita") == T, "RMR",
                                     ifelse(str_detect(rd, "Agreste") == T, "Agreste",
                                            "Sertão"))))

municipios <- c(unique(pnab$municipio)) %>% sort()

rds <- c(unique(pnab$rd)) %>% sort()

edital <- unique(pnab$EDITAL) %>% sort()

status <- unique(pnab$status) %>% sort()

macrorregiao <- unique(pnab$macrorreg) %>% sort()

theme_set(theme_bw())

header <- dashboardHeader(title = span("PNAB",
                                       style = "font-weight: bold;"))

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Bem-Vindo", tabName = "bemvindo", icon=icon("house", lib = "font-awesome")),
              menuItem("Instruções", tabName = "welcome", icon=icon("wrench", lib = "font-awesome")),
              menuItem("Edital", tabName="edital58", icon=icon("chart-line", lib = "font-awesome"),
                       menuSubItem("Edital", tabName="edital"),
                       menuSubItem("Faixa", tabName="faixa")),
              menuItem("Natureza do Proponente", tabName="natureza", icon=icon("user-tie", lib = "font-awesome")),
              menuItem("Linguagem Cultural", tabName="linguagem", icon=icon("masks-theater", lib = "font-awesome")),
              menuItem("Função", tabName="profissao", icon=icon("address-card", lib = "font-awesome")),
              menuItem("Regionalização", tabName="regiaonalizacao", icon=icon("map-location-dot", lib = "font-awesome"),
                       menuSubItem("Municípios", tabName = "municipio"),
                       menuSubItem("Regiões", tabName = "regiao")),
              menuItem("Grupos", tabName="grupo", icon=icon("people-group", lib = "font-awesome")),
              menuItem("Indução e Cotas", tabName="inducao", icon=icon("codepen", lib = "font-awesome")),
              menuItem("Escolaridade e Renda", tabName="esco_renda", icon=icon("graduation-cap", lib = "font-awesome")),
              menuItem("Recurso Cultura 5 anos", tabName="recurso5anos", icon=icon("bullhorn", lib = "font-awesome")),
              menuItem("Informações Pontuais", tabName="pontuais", icon=icon("chart-line", lib = "font-awesome"))
  ))


body <- dashboardBody(
  tags$style(type = "text/css", "#image1 {height: calc(100vh - 80px) !important;}"),
  tags$head(tags$style(HTML('
      
        .skin-blue .main-header .logo {
                              background-color: #f8b000;
                              }

 
        
        .skin-blue .main-header .navbar {
                              background-color: #00a339;
                              }
       
        .skin-blue .main-sidebar {
                              background-color: #102040;
                              }  
                              '
  ))),
  tabItems(
    tabItem(tabName = "bemvindo", imageOutput("image1")),
    tabItem(tabName = "welcome",
            h2("Instruções e Dicas de Uso"),
            p("O objetivo deste painel de dados é mostrar o alcance sociocultural da Política Nacional Aldir Blanc (PNAB) executado pela Secretaria de Cultura de Pernambuco."),
            p(" "),
            p("A utilização desse recurso para visualizar os dados referentes à PNAB é simples e intuitiva. Ao seguir os passos abaixo, você poderá analisar qualquer recorte específico identificando a variável, o(s) edital(is) e o(s) município(s) de interesse."),
            fluidRow(
              box(title = "Passo 1", status = "warning", width = 4,
                  "Escolha a variável sociocultural de interesse no menu lateral esquerdo."),
              box(title = "Passo 2", status = "warning", width = 4,
                  "Você verá um gráfico mostrando a distribuição da variável. Há algumas opções acima para que você escolha o recorte específico de interesse. Todos os municípios e todos os editais estarão pré-selecionados, mostrando a distribuição de interesse da PNAB como um todo. Caso queira analisar um ou mais municípios específicos, pressione a opção 'Deselect All' na opção de seleção de Municípios e selecione O(s) de interesse."),
              box(title = "Passo 3", status = "warning", width = 4,
                  "Uma vez escolhido os municípios de interesse, os editais de interesse e o Status da inscrição, você visualizará as informações desejadas. Para baixar o gráfico, aperte o botão ao lado de cada figura. Para o caso da variável regionalização, você também pode baixar a planilha com os respectivos municípios e região de desenvolvimento.")
            )),
    tabItem(tabName = "edital",
            fluidRow(
              column(2,
                     pickerInput("macrorreg1", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds1", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio1", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status1", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(10,
                     box(plotOutput("plotedital", height = 550), title = "Distribuição das Inscrições por Edital", width = 11,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotedital", "Baixar Gráfico")
              )),
            fluidRow(
              column(6, offset = 2,
                     box(textOutput("total1"), style = "font-size: 20px;",
                         width = 12))
            )
    ),
    tabItem(tabName = "faixa",
            fluidRow(
              column(2,
                     pickerInput("edital111", "Selecione um ou mais Editais", choices = edital, selected = edital,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg111", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds111", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio111", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status111", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(10,
                     box(plotOutput("plotfaixa1", height = 550), title = "Demanda por valor de Faixas", width = 11,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotfaixa1", "Baixar Gráfico")
              )),
            fluidRow(
              column(2,
                     pickerInput("edital112", "Selecione um ou mais Editais", choices = edital, selected = "Formação",
                                 options = list(`actions-box` = TRUE),multiple = F)),
              column(2,
                     pickerInput("macrorreg112", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds112", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio112", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status112", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(column(8,
                     box(plotOutput("plotfaixa2", height = 550), title = "Inscrição por Faixa", width = 11,
                         solidHeader = TRUE, status = "success")),
                     column(2,
                     downloadButton("downloadplotfaixa2", "Baixar Gráfico")
              ))
    ),
    tabItem(tabName = "natureza",
            fluidRow(
              column(2,
                     pickerInput("edital2", "Selecione um Edital", choices = edital, selected = edital,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg2", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds2", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio2", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status2", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(8,
                     box(plotOutput("plotnatureza"), 
                         title = "Inscrição por Natureza do Proponente", width = 11, 
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotnatureza", "Baixar Gráfico")
              ))
    ),
    tabItem(tabName = "linguagem",
            fluidRow(
              column(2,
                     pickerInput("edital3", "Selecione um ou mais Editais", choices = edital, selected = edital,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg3", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds3", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio3", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status3", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(10,
                     box(plotOutput("plotlinguagem", height = 600), title = "PROPONENTE: Distribuição das Linguagens Artístico-Culturais", width = 11, 
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotlinguagem", "Baixar Gráfico")
              )),
            fluidRow(
              column(8,
                     box(plotOutput("plotlinguagemproposta", height = 600), title = "PROPOSTA: Distribuição das Linguagens Artístico-Culturais", width = 11, 
                         solidHeader = TRUE, status = "success")
              ),
              column(4,
                     downloadButton("downloadplotlinguagemproposta", "Baixar Gráfico"),
                     box(title = "Observação", status = "warning", width = 8,
                         uiOutput("selected_var")
                     )))
    ),
    tabItem(tabName = "profissao",
            fluidRow(
              column(2,
                     pickerInput("edital4", "Selecione um Edital", choices = c("Bolsas Brincadeiras",
                                                                               "Hip Hop",
                                                                               "Mulher Negra",
                                                                               "Salvaguarda",
                                                                               "Técnicos"), selected = "Hip Hop",
                                 options = list(`actions-box` = TRUE),multiple = F)),
              column(2,
                     pickerInput("macrorreg4", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds4", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio4", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status4", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(8,
                     box(uiOutput("plot_ui"), title = "Distribuição das Profissões Culturais", width = 11, 
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotprofissao", "Baixar Gráfico")
              ))
    ),
    tabItem(tabName = "municipio",
            fluidRow(
              column(2,
                     pickerInput("edital5", "Selecione um ou mais Editais", choices = edital, selected = edital,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2,
                     pickerInput("regiao5", "Selecione uma Macrorregião", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2, 
                     pickerInput("status5", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(column(12,
              box(plotlyOutput("plotregiao"), title = "Inscrições por Municípios", width = 10,
                  solidHeader = TRUE, status = "success")
            )),
            fluidRow(
              column(11,
                     dataTableOutput("tabelamun"), style="text-align:justify"
              ))
    ),
    tabItem(tabName = "regiao",
            fluidRow(
              column(2,
                     pickerInput("edital10", "Selecione um ou mais Editais", choices = edital, selected = edital,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2, 
                     pickerInput("status10", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(8,
              box(plotOutput("plotrd"), title = "Inscrições por Região de Desenvolvimento", width = 11, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotrd", "Baixar Gráfico")
            )),
            fluidRow(
              column(8,
              box(plotOutput("plotmacroreg"), title = "Inscrições por Macrorregião", width = 11, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotmacroreg", "Baixar Gráfico")
            ))
    ),
    tabItem(tabName = "grupo",
            fluidRow(
              column(2,
                     pickerInput("edital6", "Selecione um ou mais Editais", choices = edital, selected = edital,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg6", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds6", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio6", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status6", "Selecione o Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(8,
              box(plotOutput("plotgrupo1"), title = "Inscrições por Gênero", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotgrupo1", "Baixar Gráfico Gênero"))),
            fluidRow(
              column(8,
              box(plotOutput("plotgrupo2"), title = "Inscrições por Grupo Étnico-Racial", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotgrupo2", "Baixar Gráfico Étnico-Racial"))),
            fluidRow(
              column(8,
              box(plotOutput("plotgrupo4"), title = "Inscrições por Tipo de PcD", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotgrupo4", "Baixar Gráfico Tipo PcD"))),
            fluidRow(
              column(8,
              box(plotOutput("plotgrupo5"), title = "Inscrições por Situação de Endereço", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotgrupo5", "Baixar Gráfico Endereço"))),
            fluidRow(
              column(8,
              box(plotOutput("plotgrupo6"), title = "Inscrições por Tipo de Proponente", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotgrupo6", "Baixar Gráfico Endereço")))
    ),
    tabItem(tabName = "inducao",
            fluidRow(
              column(2,
                     pickerInput("edital7.1", "Edital de Mulher Negra", choices = c("Sim", "Não"), selected = "Não",
                                 options = list(`actions-box` = TRUE),multiple = F)),
              column(2,
                     pickerInput("edital7", "Selecione um ou mais Editais", choices = edital[-9], selected = edital[-9],
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg7", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds7", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio7", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status7", "Selecione o Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(8,
              box(plotOutput("plotinducao1"), title = "Inscrições por Indução", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotinducao1", "Baixar Gráfico Indução"))),
            fluidRow(
              column(8,
              box(plotOutput("plotinducao2"), title = "Inscrições por Cota", width = 8, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotinducao2", "Baixar Gráfico Cota")))
    ),
    tabItem(tabName = "esco_renda",
            fluidRow(
              column(2,
                     pickerInput("edital8", "Selecione um ou mais Editais", choices = edital, selected = edital,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg8", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds8", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio8", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status8", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(8,
              box(plotOutput("plotescolaridade"), title = "Distribuição do Nível de Escolaridade", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotescolaridade", "Baixar Gráfico Escolaridade"))),
            fluidRow(
              column(8,
              box(plotOutput("plotrenda"), title = "Distribuição do Nível de Renda", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotrenda", "Baixar Gráfico Renda"))),
            fluidRow(
              column(8,
              box(plotOutput("plotfaturamento"), title = "Distribuição do Faturamento de PJs/Grupos", width = 10, 
                  solidHeader = TRUE, status = "success")),
              column(2,
              downloadButton("downloadplotfaturamento", "Baixar Gráfico Faturamento")))
    ),
    tabItem(tabName = "recurso5anos",
            fluidRow(
              column(3,
                     pickerInput("edital9", "Selecione um ou mais Tipos de Editais", choices = edital, selected = edital,
                                 options = list(`actions-box` = TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg9", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds9", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio9", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status9", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(8,
                     box(plotOutput("plotrecurso5anos", height = 500), title = "Os proponentes acessaram recurso para cultura nos últimos 5 anos?", 
                         solidHeader = TRUE, status = "success", width = 11)),
              column(2,
                     downloadButton("downloadplotrecurso5anos", "Baixar Gráfico")
              ))
    ),
    tabItem(tabName = "pontuais",
            fluidRow(
              column(2,
                     pickerInput("macrorreg11", "Selecione uma ou mais Macrorregiões", choices = macrorregiao, selected = macrorregiao,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rds11", "Selecione uma ou mais Regiões", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("municipio11", "Selecione um ou mais Municípios", choices = NULL,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2, 
                     pickerInput("status11", "Selecione o Status da Inscrição", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE),multiple = T))),
            fluidRow(
              column(8,
                     box(plotOutput("pontuais11"), title = "Edição dos Festivais", 
                         solidHeader = TRUE, status = "success", width = 11)),
              column(2,
                     downloadButton("downloadpontuais11", "Baixar Gráfico")
              )),
            fluidRow(
              column(8,
                     box(plotOutput("pontuais111"), title = "Museus Cadastrados", 
                         solidHeader = TRUE, status = "success", width = 11)),
              column(2,
                     downloadButton("downloadpontuais111", "Baixar Gráfico")
              ))
    )
    
  )
)




ui <- dashboardPage(header, sidebar, body,
                    controlbar = dashboardControlbar(),
                    footer = dashboardFooter(),
                    #setBackgroundImage(src = "https://raw.githubusercontent.com/observatorio-secult/pnab/main/Dashboard_PNAB2.png", shinydashboard = TRUE)
                    )

server <- function(input, output, session) {
  
  output$image1 <- renderImage({
    width  <- session$clientData$output_image1_width
    height <- session$clientData$output_image1_height
    filename <- normalizePath(file.path("Dashboard_PNAB.jpg"))
    list(src = filename,
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  data111 <- reactive(
    pnab %>% 
      filter(macrorreg %in% input$macrorreg1) %>% 
      filter(rd %in% input$rds1) %>% 
      filter(municipio %in% input$municipio1) %>% 
      filter(status %in% input$status1) %>% 
      group_by(EDITAL, EDITAL_TIPO) %>% summarise(inscricoes = n())
  )
  
  data1 <- reactive({
    g1 <- ggplot(data111(), aes(x = reorder(EDITAL, inscricoes), y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", aes(fill = EDITAL_TIPO)) +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            legend.text = element_text(size = 15)) +
      coord_flip() +
      labs(x = "Edital",
           y = "Inscrições",
           fill = "Edital") +
      expand_limits(y = c(0, max(data111()$inscricoes) + (max(data111()$inscricoes))/3)) +
      scale_fill_manual(values = c("Prêmios" = "#f8b000",
                                   "Fomento" = "#df0209",
                                   "Bolsas" = "#102040"))
    
    plot(g1)
  })
  
  
  output$plotedital <- renderPlot({
    data1()
  })
  
  output$total1 <- renderText({
    paste0("O total de inscritos para o(s) município(s) selecionados é de ", 
           sum(data111()$inscricoes), ". Sendo ", 
           sum((data111() %>% filter(EDITAL_TIPO == "Prêmios"))$inscricoes), " referentes aos Editais de Premiação, ", 
           sum((data111() %>% filter(EDITAL_TIPO == "Fomento"))$inscricoes), " aos Editais de Fomento e ",
           sum((data111() %>% filter(EDITAL_TIPO == "Bolsas"))$inscricoes), " aos Editais de Bolsas.")
  })
  
  output$downloadplotedital <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data1(), filename = file, width = 12)
    }
  )
  
  data33 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital3) %>%
      filter(macrorreg %in% input$macrorreg3) %>% 
      filter(rd %in% input$rds3) %>%
      filter(municipio %in% input$municipio3) %>%
      filter(status %in% input$status3) %>%
      group_by(linguagem) %>% summarise(inscricoes = n())
  )
  
  data3 <- reactive({
    g3 <- ggplot(data33(), aes(x = reorder(linguagem, inscricoes), y = inscricoes)) +
      geom_segment(aes(x = linguagem,
                       xend = linguagem,
                       y = 0,
                       yend = inscricoes),
                   lty = "dashed", alpha = .35) +
      geom_point(color = "#df0209", size = 4) +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.2) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank()) +
      coord_flip() +
      labs(x = "Linguagem",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data33()$inscricoes) + (max(data33()$inscricoes))/3))
    
    plot(g3)
  })
  
  
  output$plotlinguagem <- renderPlot({
    data3()
  })
  
  output$downloadplotlinguagem <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data3(), filename = file, width = 12)
    }
  )
  
  data33_2 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital3) %>%
      filter(macrorreg %in% input$macrorreg3) %>% 
      filter(rd %in% input$rds3) %>%
      filter(municipio %in% input$municipio3) %>%
      filter(status %in% input$status3) %>%
      group_by(linguagem_proposta) %>% summarise(inscricoes = n()) %>% 
      drop_na()
  )
  
  data3_2 <- reactive({
    g3 <- ggplot(data33_2(), aes(x = reorder(linguagem_proposta, inscricoes), y = inscricoes)) +
      geom_segment(aes(x = linguagem_proposta,
                       xend = linguagem_proposta,
                       y = 0,
                       yend = inscricoes),
                   lty = "dashed", alpha = .35) +
      geom_point(color = "#f8b000", size = 4) +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.2) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank()) +
      coord_flip() +
      labs(x = "Linguagem",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data33_2()$inscricoes) + (max(data33_2()$inscricoes))/2))
    
    plot(g3)
  })
  
  
  output$plotlinguagemproposta <- renderPlot({
    data3_2()
  })
  
  output$downloadplotlinguagemproposta <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data3_2(), filename = file, width = 12)
    }
  )
  
  data44 <- reactive(
    pnab %>% filter(EDITAL == input$edital4) %>%
      filter(macrorreg %in% input$macrorreg4) %>% 
      filter(rd %in% input$rds4) %>%
      filter(municipio %in% input$municipio4) %>%
      filter(status %in% input$status4) %>% 
      cSplit('funcao', sep = ",", direction = "long") %>% 
      group_by(funcao) %>% summarise(inscricoes = n()) %>% 
      arrange(-inscricoes) %>% rowid_to_column(var = "check") %>% 
      filter(check < 40)
  )
  
  plot_height <- reactive({ 
    switch(input$edital4, 
           "Bolsas Brincadeiras" = 1000, 
           "Hip Hop" = 600, 
           "Mulher Negra" = 1000, 
           "Salvaguarda" = 1000, 
           "Técnicos" = 1000, 
           600) 
  }) 
  
  output$plot_ui <- renderUI({ 
    plotOutput("plotprofissao", height = plot_height()) 
  })
  
  data4 <- reactive({
    g4 <- ggplot(data44(), 
                 aes(x = reorder(funcao, inscricoes), y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", fill = "#102040") +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), 
                size = ifelse(input$edital4 == "Técnicos", 4, 5), hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            plot.caption = element_text(size = 15)) +
      coord_flip() +
      labs(x = "Função",
           y = "Inscrições",
           caption = "O proponente teve a opção de escolher mais de uma função/profissão.\nO gráfico apresenta, no máximo, as top 40 funções/profissões.") +
      expand_limits(y = c(0, max(data44()$inscricoes) + (max(data44()$inscricoes))/3))
    
    plot(g4)
  })
  
  
  output$plotprofissao <- renderPlot({
    data4()
  })
  
  output$downloadplotprofissao <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data4(), filename = file, width = 12)
    }
  )
  
  data55 <- reactive({
    mun <- pnab %>% mutate(municipio = ifelse(municipio == "Exú", "Exu", municipio)) %>%
      filter(EDITAL %in% input$edital5) %>%
      filter(macrorreg %in% input$regiao5) %>%
      filter(status %in% input$status5) %>%
      group_by(municipio) %>% summarise(inscricoes = n()) %>% 
      mutate(municipio = ifelse(municipio == "São Caetano", "São Caitano",
                                ifelse(municipio == "Belém de São Francisco", "Belém do São Francisco",
                                       municipio)))
    
    pernambuco <- pernambuco %>% left_join(mun, by = c("NM_MUN" = "municipio"))
    
    pernambuco %>% filter(CD_MUN != 2605459) %>% filter(macrorreg_f %in% input$regiao5)
  })
  
  
  data5 <- reactive({
    g5 <- ggplot() +
      geom_sf(data = data55(),
              aes(fill = inscricoes,
                  text = paste("Estado:", NM_MUN, "<br>Inscrições:", inscricoes)), color = "black") +
      labs(title = "",
           fill = "Inscrições",
           caption = "*As áreas cinzentas representam municípios que não obtiveram nenhuma inscrição.\n**As inscrições de Fernando de Noronha estão disponíveis na tabela abaixo.") +
      theme_minimal() +
      theme(plot.caption = element_text(size = 15))
    ggplotly(g5, tooltip = "text")
  })
  
  
  output$plotregiao <- renderPlotly({
    data5()
  })
  
  # output$downloadplotregiao <- downloadHandler(
  #   filename = function() {
  #     paste0("download", ".png")
  #   },
  #   content = function(file) {
  #     ggsave(plot = data5(), filename = file, width = 12)
  #   }
  # )
  
  output$tabelamun <-  DT::renderDT(server = FALSE, {
    DT::datatable(
      pnab %>%
        filter(EDITAL %in% input$edital5) %>%
        filter(macrorreg %in% input$regiao5) %>%
        filter(status %in% input$status5) %>%
        group_by(municipio, rd) %>% summarise(inscricoes = n()) %>%
        group_by() %>% mutate(prop_selecao = scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L)) %>% 
        arrange(-inscricoes),
      extensions = c("Buttons"),
      options = list(
        dom = 'Bfrtip',
        pageLength = length(municipios),
        buttons = list(
          list(extend = "excel", text = "Baixar Planilha", filename = "data",
               exportOptions = list(
                 modifier = list(page = "all")
               )
          )
        )
      )
    )
  })
  
  data101_1 <- reactive(
    pnab %>% 
      filter(EDITAL %in% input$edital10) %>%
      filter(status %in% input$status10) %>% 
      group_by(macrorreg) %>% summarise(inscricoes = n()) %>% 
      mutate(INTERIOR = ifelse(macrorreg == "RMR", "Metropolitana", "Interior"))
    
  )
  
  data101_11 <- reactive(
    pnab %>% 
      filter(EDITAL %in% input$edital10) %>%
      filter(status %in% input$status10) %>% 
      group_by(macrorreg) %>% summarise(inscricoes = n()) %>% 
      mutate(INTERIOR = ifelse(macrorreg == "RMR", "Metropolitana", "Interior")) %>% 
      group_by(INTERIOR) %>% summarise(inscricoes = sum(inscricoes))
    
  )
  
  data101_2 <- reactive(
    pnab %>% 
      filter(EDITAL %in% input$edital10) %>%
      filter(status %in% input$status10) %>%
      group_by(rd) %>% summarise(inscricoes = n()) %>% 
      drop_na()
  ) 
  
  data10_1 <- reactive({
    g101 <- ggplot(data101_1()) +
      geom_bar(stat = "identity", width = .6, aes(x = INTERIOR, y = inscricoes, fill = reorder(macrorreg, inscricoes))) +
      geom_text(aes(x = INTERIOR, y = inscricoes, fill = reorder(macrorreg, inscricoes),
                    label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), 
                position = position_stack(vjust = 0.5), size = 5) +
      geom_text(aes(x = INTERIOR, y = inscricoes,
                    label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), 
                size = 5, fontface = "bold", vjust = -.5,
                data = data101_11()) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            legend.text = element_text(size = 12)) +
      labs(x = "Região do Estado",
           y = "Inscrições",
           fill = "Macrorregião") +
      scale_fill_manual(values = c("Agreste" = "#F35E23",
                                   "Zona da Mata" = "#90C842", 
                                   "Sertão" = "goldenrod3", 
                                   "RMR" = "lightslateblue")) +
      expand_limits(y = c(0, max(data101_11()$inscricoes) + max(data101_11()$inscricoes)/4))
    
    plot(g101)
  })
  
  data10_2 <- reactive({
    g102 <- ggplot(data101_2(), aes(x = reorder(rd, inscricoes), y = inscricoes)) +
      geom_segment(aes(x = rd,
                       xend = rd,
                       y = 0,
                       yend = inscricoes),
                   color = "grey80", alpha = .35) +
      geom_point(color = "#F35E23", size = 4) +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.2) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank()) +
      coord_flip() +
      labs(x = "RDs",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data101_2()$inscricoes) + (max(data101_2()$inscricoes))/3))
    
    plot(g102)
  })
  
  output$plotmacroreg <- renderPlot({
    data10_1()
  })
  
  output$plotrd <- renderPlot({
    data10_2()
  })
  
  
  output$downloadplotmacroreg <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data10_1(), filename = file, width = 12)
    }
  )
  
  output$downloadplotrd <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data10_2(), filename = file, width = 12)
    }
  )
  
  data66_1 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital6) %>%
      filter(macrorreg %in% input$macrorreg6) %>% 
      filter(rd %in% input$rds6) %>%
      filter(municipio %in% input$municipio6) %>%
      filter(status %in% input$status6) %>%
      group_by(genero) %>% summarise(inscricoes = n())
  )
  
  data66_2 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital6) %>%
      filter(macrorreg %in% input$macrorreg6) %>% 
      filter(rd %in% input$rds6) %>%
      filter(municipio %in% input$municipio6) %>%
      filter(status %in% input$status6) %>%
      group_by(raca) %>% summarise(inscricoes = n())
  )
  
  data66_4 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital6) %>%
      filter(macrorreg %in% input$macrorreg6) %>% 
      filter(rd %in% input$rds6) %>%
      filter(municipio %in% input$municipio6) %>%
      filter(status %in% input$status6) %>%
      filter(is.na(pcd_tipo) == F) %>% 
      group_by(pcd_tipo) %>% summarise(inscricoes = n())
  )
  
  data66_5 <- reactive({
    df <- pnab %>% filter(EDITAL %in% input$edital6) %>%
      filter(macrorreg %in% input$macrorreg6) %>% 
      filter(rd %in% input$rds6) %>%
      filter(municipio %in% input$municipio6) %>%
      filter(status %in% input$status6) %>%
      mutate(endereco_fixo = ifelse(sit_endereco %in% c("Itinerância",
                                                        "Situação de Rua"), "Não", "Sim")) %>% 
      group_by(endereco_fixo, sit_endereco) %>% 
      summarise(inscricoes = n()) %>% mutate(sit_endereco = ifelse(sit_endereco == "Endereço Fixo", NA,
                                                                   sit_endereco))
    
    df %>% ungroup() %>% 
      add_row(endereco_fixo = "Endereço Fixo",
              sit_endereco = "Não",
              inscricoes = sum((df %>% filter(endereco_fixo == "Não"))$inscricoes)) %>% 
      add_row(endereco_fixo = "Endereço Fixo",
              sit_endereco = "Sim",
              inscricoes = sum((df %>% filter(endereco_fixo == "Sim"))$inscricoes)) %>% 
      filter(endereco_fixo != "Sim") %>% 
      rename(source = endereco_fixo, destination = sit_endereco)
    
  })
  
  agreg66_5 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital6) %>%
      filter(macrorreg %in% input$macrorreg6) %>% 
      filter(rd %in% input$rds6) %>%
      filter(municipio %in% input$municipio6) %>%
      filter(status %in% input$status6) %>%
      mutate(endereco_fixo = ifelse(sit_endereco %in% c("Itinerância",
                                                        "Situação de Rua"), "Não", "Sim")) %>% 
      group_by(endereco_fixo) %>% 
      summarise(n = n()) %>% rename(label = endereco_fixo) %>% 
      rbind(pnab %>% filter(EDITAL %in% input$edital6) %>%
              filter(macrorreg %in% input$macrorreg6) %>% 
              filter(rd %in% input$rds6) %>%
              filter(municipio %in% input$municipio6) %>%
              filter(status %in% input$status6) %>% 
              mutate(endereco_fixo = ifelse(sit_endereco %in% c("Itinerância",
                                                                "Situação de Rua"), "Não", "Sim")) %>% 
              group_by(endereco_fixo, sit_endereco) %>% 
              summarise(n = n()) %>% filter(endereco_fixo == "Não") %>% 
              group_by(sit_endereco) %>% 
              summarise(n = sum(n)) %>% 
              rename(label = sit_endereco)) %>% 
      filter(is.na(label) == F)
  )
  
  data66_6 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital6) %>%
      filter(macrorreg %in% input$macrorreg6) %>% 
      filter(rd %in% input$rds6) %>%
      filter(municipio %in% input$municipio6) %>%
      filter(status %in% input$status6) %>%
      cSplit('mestre', sep = ",", direction = "long") %>% 
      group_by(mestre) %>% 
      summarise(inscricoes = n()) %>% drop_na()
  )
  
  data6_1 <- reactive({
    g61 <- ggplot(data66_1(), aes(x = reorder(genero, inscricoes), y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", fill = "#f8b000") +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      coord_flip() +
      labs(x = "Gênero",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data66_1()$inscricoes) + max(data66_1()$inscricoes)/3))
    
    plot(g61)
  })
  
  data6_2 <- reactive({
    g62 <- ggplot(data66_2(), aes(x = reorder(raca, inscricoes), y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", fill = "#00a339") +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      coord_flip() +
      labs(x = "Etnia",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data66_2()$inscricoes) + max(data66_1()$inscricoes)/3))
    
    plot(g62)
  })
  
  data6_4 <- reactive({
    g64 <- ggplot(data66_4(), aes(x = reorder(pcd_tipo, inscricoes), y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", fill = "#df0209") +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      coord_flip() +
      labs(x = "Tipo PcD",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data66_4()$inscricoes) + max(data66_4()$inscricoes)/3))
    
    plot(g64)
  })
  
  data6_5 <- reactive({
    #  Get distinct source names
    sources <- data66_5() %>%
      distinct(source) %>%
      rename(label = source)
    # Get distinct destination names
    destinations <- data66_5() %>%
      distinct(destination) %>%
      rename(label = destination)
    # Join the two data to create node
    # Add unique ID for each country
    nodes <- full_join(sources, destinations, by = "label") 
    nodes <- nodes %>%
      mutate(id = 1:nrow(nodes)) %>%
      select(id, everything()) %>% 
      left_join(agreg66_5(), by = "label")
    
    
    # Rename the n.call column to weight
    termo_pares1 <- data66_5() %>%
      mutate(weight = inscricoes,
             inscricoes = NULL)
    # (a) Join nodes id for source column
    edges <- termo_pares1 %>% 
      left_join(nodes, by = c("source" = "label")) %>% 
      rename(from = id)
    # (b) Join nodes id for destination column
    edges <- edges %>% 
      left_join(nodes, by = c("destination" = "label")) %>% 
      rename(to = id)
    # (c) Select/keep only the columns from and to
    edges <- select(edges, from, to, weight)
    
    net.tidy <- tbl_graph(
      nodes = nodes, edges = edges, directed = TRUE
    )
    
    
    ggraph(net.tidy, layout = "auto") + 
      geom_edge_link(aes(width = weight), alpha = .8, color = "black") + 
      scale_edge_width(range = c(0.3, 2)) +
      geom_node_text(aes(label = ifelse(label == "Endereço Fixo", label, 
                                        paste0(label, "\n", n, " (",
                                               scales::percent(n/nrow(pnab %>% filter(EDITAL %in% input$edital6) %>%
                                                                        filter(municipio %in% input$municipio6) %>%
                                                                        filter(status %in% input$status6)),
                                                               accuracy = 0.01L), ")")),
                         vjust = ifelse(label == "Sim", 1.5, -.8)), 
                     repel = F, size = 5) +
      geom_node_point(aes(size = n), color = "#df0209", alpha = 1) +
      labs(edge_width = "EC") +
      theme(legend.position = "none") +
      expand_limits(x = c(-1.1, .7),
                    y = c(0, 2.4))
  })
  
  data6_6 <- reactive({
    g66 <- ggplot(data66_6(), aes(x = reorder(mestre, inscricoes), y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", fill = "#00a339") +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), 
                size = 5, hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            plot.caption = element_text(size = 15)) +
      coord_flip() +
      labs(x = "Tipo de Proponente",
           y = "Inscrições",
           caption = "Essa dimensão não foi capturada nas oportunidades de Mulher Negra e de Técnicos.") +
      expand_limits(y = c(0, max(data66_6()$inscricoes) + max(data66_6()$inscricoes)/3))
    
    plot(g66)
  })
  
  output$plotgrupo1 <- renderPlot({
    data6_1()
  })
  
  output$plotgrupo2 <- renderPlot({
    data6_2()
  })
  
  output$plotgrupo3 <- renderPlot({
    data6_3()
  })
  
  output$plotgrupo4 <- renderPlot({
    data6_4()
  })
  
  output$plotgrupo5 <- renderPlot({
    data6_5()
  })
  
  output$plotgrupo6 <- renderPlot({
    data6_6()
  })
  
  output$downloadplotgrupo1 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data6_1(), filename = file, width = 12)
    }
  )
  
  output$downloadplotgrupo2 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data6_2(), filename = file, width = 12)
    }
  )
  
  output$downloadplotgrupo3 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data6_3(), filename = file, width = 12)
    }
  )
  
  output$downloadplotgrupo4 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data6_4(), filename = file, width = 12)
    }
  )
  
  output$downloadplotgrupo5 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data6_5(), filename = file, width = 12)
    }
  )
  
  output$downloadplotgrupo6 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data6_6(), filename = file, width = 12)
    }
  )
  
  output$selected_var <- renderUI({
    HTML("A linguagem artístico Cultural foi capturada para as seguintes oportunidades:<br><br>-Bolsas Artísticas<br>-Diversidade Cultural<br>-Economia Criativa<br>-Festivais<br>-Formação<br>-Multilinguagem")
  })
  
  data77_1 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital7) %>%
      filter(macrorreg %in% input$macrorreg7) %>% 
      filter(rd %in% input$rds7) %>%
      filter(municipio %in% input$municipio7) %>%
      filter(status %in% input$status7) %>%
      group_by(inducao) %>% summarise(inscricoes = n())
  )
  
  data77_2 <- reactive(
    pnab %>% filter(EDITAL %in% input$edital7) %>%
      filter(macrorreg %in% input$macrorreg7) %>% 
      filter(rd %in% input$rds7) %>%
      filter(municipio %in% input$municipio7) %>%
      filter(status %in% input$status7) %>%
      group_by(cota) %>% summarise(inscricoes = n())
  )
  
  data77_3 <- reactive(
    pnab %>% filter(EDITAL == "Mulher Negra") %>%
      filter(macrorreg %in% input$macrorreg7) %>% 
      filter(rd %in% input$rds7) %>%
      filter(municipio %in% input$municipio7) %>%
      filter(status %in% input$status7) %>%
      cSplit('inducao', sep = ";", direction = "long") %>% 
      group_by(inducao) %>% summarise(inscricoes = n())
  )
  
  data77_4 <- reactive(
    pnab %>% filter(EDITAL == "Mulher Negra") %>%
      filter(macrorreg %in% input$macrorreg7) %>% 
      filter(rd %in% input$rds7) %>%
      filter(municipio %in% input$municipio7) %>%
      filter(status %in% input$status7) %>%
      group_by(cota) %>% summarise(inscricoes = n())
  )
  
  data7_1 <- reactive({
    if(input$edital7.1 == "Sim"){
      g71 <- ggplot(data77_3(), aes(x = reorder(inducao, inscricoes), y = inscricoes)) +
        geom_bar(stat = "identity", width = .6, color = "black", fill = "#3865AE") +
        geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/nrow(pnab %>% filter(EDITAL == "Mulher Negra")), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
        geom_label(aes(label = paste0("A média de indução\nconcomitante foi de ", 
                                      round(mean((pnab %>%
                                                    filter(macrorreg %in% input$macrorreg7) %>% 
                                                    filter(rd %in% input$rds7) %>%
                                                    filter(municipio %in% input$municipio7) %>%
                                                    filter(status %in% input$status7) %>%
                                                    filter(EDITAL == "Mulher Negra") %>%
                                                    mutate(ind_qtd = str_count(inducao, ";")+1))$ind_qtd), 3),
                                      " com um pedido máximo de ",
                                      max((pnab %>%
                                             filter(macrorreg %in% input$macrorreg7) %>% 
                                             filter(rd %in% input$rds7) %>%
                                             filter(municipio %in% input$municipio7) %>%
                                             filter(status %in% input$status7) %>%
                                             filter(EDITAL == "Mulher Negra") %>%
                                             mutate(ind_qtd = str_count(inducao, ";")+1))$ind_qtd)),
                       x = 0, y = 0),
                   size = 5, hjust = 0) +
        theme(axis.text = element_text(size = 12, face = "bold"),
              panel.grid = element_blank(),
              axis.text.x = element_text(face = "bold")) +
        coord_flip() +
        labs(x = "Indução",
             y = "Inscrições") +
        expand_limits(y = c(0, max(data77_3()$inscricoes) + max(data77_3()$inscricoes)/3),
                      x = c(-1, nrow(data77_3())))
    } else {
      g71 <- ggplot(data77_1(), aes(x = reorder(inducao, inscricoes), y = inscricoes)) +
        geom_bar(stat = "identity", width = .6, color = "black", fill = "#3865AE") +
        geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
        theme(axis.text = element_text(size = 12, face = "bold"),
              panel.grid = element_blank(),
              axis.text.x = element_text(face = "bold")) +
        coord_flip() +
        labs(x = "Indução",
             y = "Inscrições") +
        expand_limits(y = c(0, max(data77_1()$inscricoes) + max(data77_1()$inscricoes)/3))
    }
    plot(g71)
  })
  
  
  data7_2 <- reactive({
    if(input$edital7.1 == "Sim"){
      g72 <- ggplot(data77_4(), aes(x = reorder(cota, inscricoes), y = inscricoes)) +
        geom_bar(stat = "identity", width = .6, color = "black", fill = "#FECA0A") +
        geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, vjust = -.5) +
        theme(axis.text = element_text(size = 12, face = "bold"),
              panel.grid = element_blank(),
              axis.text.x = element_text(face = "bold")) +
        labs(x = "Cota",
             y = "Inscrições") +
        expand_limits(y = c(0, max(data77_4()$inscricoes) + max(data77_4()$inscricoes)/3))
    } else {
      g72 <- ggplot(data77_2(), aes(x = reorder(cota, inscricoes), y = inscricoes)) +
        geom_bar(stat = "identity", width = .6, color = "black", fill = "#FECA0A") +
        geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, vjust = -.5) +
        theme(axis.text = element_text(size = 12, face = "bold"),
              panel.grid = element_blank(),
              axis.text.x = element_text(face = "bold")) +
        labs(x = "Cota",
             y = "Inscrições") +
        expand_limits(y = c(0, max(data77_2()$inscricoes) + max(data77_2()$inscricoes)/3))
    }
    plot(g72)
  })
  
  
  output$plotinducao1 <- renderPlot({
    data7_1()
  })
  
  output$plotinducao2 <- renderPlot({
    data7_2()
  })
  
  output$downloadplotinducao1 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data7_1(), filename = file, width = 12)
    }
  )
  
  output$downloadplotinducao2 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data7_2(), filename = file, width = 12)
    }
  )
  
  data88_1 <- reactive(
    pnab %>% mutate(escolaridade = factor(escolaridade, levels = c("Não declarar",
                                                                   "Não tenho Educação Formal",
                                                                   "Ensino Fundamental",
                                                                   "Ensino Médio",
                                                                   "Profissionalizante",
                                                                   "Curso Técnico",
                                                                   "Ensino Superior",
                                                                   "Pós-Graduação",
                                                                   "Mestrado",
                                                                   "Doutorado"))) %>% 
      filter(EDITAL %in% input$edital8) %>%
      filter(macrorreg %in% input$macrorreg8) %>% 
      filter(rd %in% input$rds8) %>%
      filter(municipio %in% input$municipio8) %>%
      filter(status %in% input$status8) %>%
      group_by(escolaridade) %>% summarise(inscricoes = n())
  )
  
  data88_2 <- reactive(
    pnab %>% filter(is.na(renda) == F) %>% 
      mutate(renda = factor(renda, levels = c("Não declarar",
                                              "Até 1 salário-mínimo",
                                              "De 1 a 3 salários-mínimos",
                                              "De 3 a 5 salários-mínimos",
                                              "De 5 a 8 salários-mínimos",
                                              "De 8 a 10 salários-mínimos",
                                              "Acima de 10 salários-mínimos"))) %>% 
      filter(EDITAL %in% input$edital8) %>%
      filter(macrorreg %in% input$macrorreg8) %>% 
      filter(rd %in% input$rds8) %>%
      filter(municipio %in% input$municipio8) %>%
      filter(status %in% input$status8) %>%
      group_by(renda) %>% summarise(inscricoes = n())
  )
  
  data88_3 <- reactive(
    pnab %>% filter(is.na(faturamento_pj) == F) %>% 
      mutate(faturamento_pj = factor(faturamento_pj, levels = c("Não declarar",
                                                                   "Não possui faturamento anual",
                                                                   "Entre R$ 1.000,00 e R$ 10.000,00",
                                                                   "Entre R$ 10.000,00 e R$ 20.000,00",
                                                                   "Entre R$ 20.000,00 e R$ 30.000,00",
                                                                   "Entre R$ 30.000,00 e R$ 40.000,00",
                                                                   "Acima de R$ 40.000,00"))) %>% 
      filter(EDITAL %in% input$edital8) %>%
      filter(macrorreg %in% input$macrorreg8) %>% 
      filter(rd %in% input$rds8) %>%
      filter(municipio %in% input$municipio8) %>%
      filter(status %in% input$status8) %>%
      group_by(faturamento_pj) %>% summarise(inscricoes = n())
  )
  
  data8_1 <- reactive({
    g81 <- ggplot(data88_1(), aes(x = escolaridade, y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", fill = "#FECA0A") +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      coord_flip() +
      labs(x = "Escolaridade",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data88_1()$inscricoes) + max(data88_1()$inscricoes)/3))
    
    plot(g81)
  })
  
  data8_2 <- reactive({
    g82 <- ggplot(data88_2(), aes(x = renda, y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", fill = "#3865AE") +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      coord_flip() +
      labs(x = "Renda",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data88_2()$inscricoes) + max(data88_2()$inscricoes)/3))
    
    plot(g82)
  })
  
  data8_3 <- reactive({
    g83 <- ggplot(data88_3(), aes(x = faturamento_pj, y = inscricoes)) +
      geom_bar(stat = "identity", width = .6, color = "black", fill = "#E51F27") +
      geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes/sum(inscricoes), accuracy = 0.1L), ")")), size = 5, hjust = -.1) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      coord_flip() +
      labs(x = "Faturamento",
           y = "Inscrições") +
      expand_limits(y = c(0, max(data88_3()$inscricoes) + max(data88_3()$inscricoes)/4))
    
    plot(g83)
  })
  
  output$plotescolaridade <- renderPlot({
    data8_1()
  })
  
  output$plotrenda <- renderPlot({
    data8_2()
  })
  
  output$plotfaturamento <- renderPlot({
    data8_3()
  })
  
  output$downloadplotescolaridade <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data8_1(), filename = file, width = 12)
    }
  )
  
  output$downloadplotrenda <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data8_2(), filename = file, width = 12)
    }
  )
  
  output$downloadplotfaturamento <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data8_3(), filename = file, width = 12)
    }
  )
  
  data99_1 <- reactive(
    a <- pnab %>% filter(EDITAL %in% input$edital9) %>%
      filter(macrorreg %in% input$macrorreg9) %>% 
      filter(rd %in% input$rds9) %>%
      filter(municipio %in% input$municipio9) %>% 
      filter(status %in% input$status9) %>%
      group_by(recurso_cultura, EDITAL) %>% summarise(inscricoes = n()) %>%
      mutate(inscricoes_x = ifelse(recurso_cultura == "Não", -inscricoes, inscricoes)) %>% 
      filter(!recurso_cultura %in% c("Não sei", "Não declarar")) %>% 
      group_by(EDITAL) %>% mutate(label = sum(inscricoes)) %>% 
      mutate(label = paste0(inscricoes, " (", scales::percent(inscricoes/label, accuracy = 0.1L), ")"))
  )
  
  data9_1 <- reactive({
    g91 <- ggplot(data99_1(), aes(x = reorder(EDITAL, -inscricoes), y = inscricoes_x)) +
      geom_bar(stat = "identity", width = .6, color = "black", aes(fill = recurso_cultura)) +
      geom_text(aes(label = label, hjust = ifelse(inscricoes_x < 0, 1.1, -.1)), size = 5) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            plot.caption = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 12)) +
      coord_flip() +
      labs(x = "Recurso 5 Anos",
           y = "Inscrições",
           fill = "Acesso a Recurso",
           caption = "Foi retirado do gráfico aqueles que responderam 'Não sei' e 'Não declarar'") +
      scale_fill_manual(values = c("Sim" = "#90C842",
                                   "Não" = "#F35E23")) +
      expand_limits(y = c(min(data99_1()$inscricoes_x) - 800, max(data99_1()$inscricoes_x) + 800))
    
    plot(g91)
  })
  
  output$plotrecurso5anos <- renderPlot({
    data9_1()
  })
  
  output$downloadplotrecurso5anos <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data9_1(), filename = file, width = 12)
    }
  )
  
  data11 <- reactive(
    pnab %>% filter(EDITAL == "Festivais") %>% 
      filter(macrorreg %in% input$macrorreg11) %>% 
      filter(rd %in% input$rds11) %>%
      filter(municipio %in% input$municipio11) %>% 
      filter(status %in% input$status11) %>% 
      mutate(fest_edicao = ifelse(fest_edicao > 10, "11 ou mais", fest_edicao)) %>% 
      group_by(fest_edicao) %>% summarise(inscricoes = n()) %>% filter(str_detect(fest_edicao, "-") == F) %>% 
      mutate(fest_edicao = factor(fest_edicao, 
                                  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11 ou mais")))
  )
  
  data11_1 <- reactive({
    g111 <- ggplot(data11(), aes(x = fest_edicao, y = inscricoes, group = 1)) +
      geom_line(color = "black", lty = "dashed") +
      geom_point(size = 4) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            plot.caption = element_text(size = 15)) +
      geom_text(aes(label = inscricoes), vjust = -.7, size = 5) +
      labs(x = "Número da Edição",
           y = "Inscrições",
           caption = "Esse gráfico é específico para o edital de Festivais") +
      expand_limits(y = c(0, max(data11()$inscricoes) + max(data11()$inscricoes)/3))
    plot(g111)
  })
  
  output$pontuais11 <- renderPlot({
    data11_1()
  })
  
  output$downloadpontuais11 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data11_1(), filename = file, width = 12)
    }
  )
  
  data11111 <- reactive(
    pnab %>% filter(EDITAL == "Museus") %>% 
      filter(macrorreg %in% input$macrorreg11) %>% 
      filter(rd %in% input$rds11) %>%
      filter(municipio %in% input$municipio11) %>% 
      filter(status %in% input$status11) %>%
      group_by(museus_cadastro) %>% summarise(inscricoes = n())
  )
  
  data111_1 <- reactive({
    g1111 <- ggplot(data11111(), aes(x = museus_cadastro, y = inscricoes)) +
      geom_bar(stat = "identity", aes(fill = museus_cadastro), width = .3,
               color = "black") +
      theme(axis.text = element_text(size = 13, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            plot.caption = element_text(size = 15),
            legend.position = "none") +
      geom_text(aes(label = paste0(inscricoes, " (", 
                                   scales::percent(inscricoes/sum(inscricoes), accuracy = 0.01),
                                   ")")), vjust = -.5, size = 5) +
      labs(x = "Cadastro",
           y = "Inscrições",
           caption = "Esse gráfico é específico para o edital de Museus",
           legend.position = "none") +
      scale_fill_manual(values = c("Não" = "#df0209",
                                   "Sim" = "#00a339")) +
      expand_limits(y = c(0, max(data11111()$inscricoes) + max(data11111()$inscricoes)/7))
    plot(g1111)
  })
  
  output$pontuais111 <- renderPlot({
    data111_1()
  })
  
  output$downloadpontuais111 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = data111_1(), filename = file, width = 12)
    }
  )
  
  datafaixa1 <- reactive(
    pnab %>% mutate(valor = as.numeric(valor),
                    faixa_cat = ifelse(valor <= 20000, "Até 20 mil",
                                       ifelse(valor > 20000 & valor <= 40000, "25 a 40 mil",
                                              ifelse(valor > 40000 & valor <= 70000, "50 a 70 mil",
                                                     ifelse(valor > 70000, "100 a 130 mil", "erro"))))) %>% 
      mutate(faixa_cat = factor(faixa_cat, levels = c("Até 20 mil", 
                                                     "25 a 40 mil", 
                                                     "50 a 70 mil", 
                                                     "100 a 130 mil"))) %>% 
      filter(EDITAL %in% input$edital111) %>% 
      filter(macrorreg %in% input$macrorreg111) %>% 
      filter(rd %in% input$rds111) %>%
      filter(municipio %in% input$municipio111) %>% 
      filter(status %in% input$status111) %>%
      group_by(faixa_cat) %>% summarise(inscricoes = n())
  )
  
  plotfaixa_1 <- reactive({
    g <- ggplot(datafaixa1(), aes(x = faixa_cat, y = inscricoes)) +
      geom_bar(stat = "identity", fill = "#00a339", width = .6,
               color = "black") +
      theme(axis.text = element_text(size = 13, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            plot.caption = element_text(size = 15),
            legend.position = "none") +
      geom_text(aes(label = paste0(inscricoes, " (", 
                                   scales::percent(inscricoes/sum(inscricoes), accuracy = 0.01),
                                   ")")), vjust = -.5, size = 5) +
      labs(x = "Valor das Faixas",
           y = "Inscrições") +
      expand_limits(y = c(0, max(datafaixa1()$inscricoes) + max(datafaixa1()$inscricoes)/7))
    plot(g)
  })
  
  output$plotfaixa1 <- renderPlot({
    plotfaixa_1()
  })
  
  output$downloadplotfaixa1 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = plotfaixa_1(), filename = file, width = 12)
    }
  )
  
  datafaixa2 <- reactive(
    pnab %>% mutate(faixa_label = str_to_title(faixa_label)) %>% 
      filter(EDITAL == input$edital112) %>% 
      filter(macrorreg %in% input$macrorreg112) %>% 
      filter(rd %in% input$rds112) %>%
      filter(municipio %in% input$municipio112) %>% 
      filter(status %in% input$status112) %>%
      group_by(faixa_label) %>% summarise(inscricoes = n())
  )
  
  plotfaixa_2 <- reactive({
    g <- ggplot(datafaixa2(), aes(x = reorder(faixa_label, inscricoes), y = inscricoes)) +
      geom_segment(aes(x = faixa_label, xend = faixa_label, y = 0, yend = inscricoes),
                   lty = "dashed") +
      geom_point(color = "#f8b000", size = 4) +
      theme(axis.text = element_text(size = 13, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            plot.caption = element_text(size = 15),
            legend.position = "none") +
      coord_flip() +
      geom_text(aes(label = paste0(inscricoes, " (", 
                                   scales::percent(inscricoes/sum(inscricoes), accuracy = 0.01),
                                   ")")), hjust = -.1, size = 5) +
      labs(x = "Faixas",
           y = "Inscrições") +
      expand_limits(y = c(0, max(datafaixa2()$inscricoes) + max(datafaixa2()$inscricoes)/3))
    plot(g)
  })
  
  output$plotfaixa2 <- renderPlot({
    plotfaixa_2()
  })
  
  output$downloadplotfaixa2 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = plotfaixa_2(), filename = file, width = 12)
    }
  )
  
  datanatureza <- reactive(
    pnab %>%  
      filter(EDITAL == input$edital2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rds2) %>%
      filter(municipio %in% input$municipio2) %>% 
      filter(status %in% input$status2) %>%
      group_by(natureza) %>% summarise(inscricoes = n())
  )
  
  plotnatureza_2 <- reactive({
    g <- ggplot(datanatureza(), aes(x = reorder(natureza, inscricoes), y = inscricoes)) +
      geom_segment(aes(x = natureza, xend = natureza, y = 0, yend = inscricoes)) +
      geom_point(color = "#102040", size = 4) +
      theme(axis.text = element_text(size = 13, face = "bold"),
            panel.grid = element_blank(),
            axis.text.x = element_text(face = "bold"),
            plot.caption = element_text(size = 15),
            legend.position = "none") +
      geom_text(aes(label = paste0(inscricoes, " (", 
                                   scales::percent(inscricoes/sum(inscricoes), accuracy = 0.01),
                                   ")")), vjust = -.5, size = 5) +
      labs(x = "Natureza",
           y = "Inscrições") +
      expand_limits(y = c(0, max(datanatureza()$inscricoes) + max(datanatureza()$inscricoes)/7))
    plot(g)
  })
  
  output$plotnatureza <- renderPlot({
    plotnatureza_2()
  })
  
  output$downloadplotnatureza <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = plotnatureza_2(), filename = file, width = 12)
    }
  )
    
  # Definir os friltro a partir da selecao das mecrorregioes e rds
  
  observeEvent(input$macrorreg1, { 
    selected_macrorreg <- input$macrorreg1 
    updatePickerInput(session, "rds1", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds1, { 
    selected_rds <- input$rds1 
    updatePickerInput(session, "municipio1", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg111, { 
    selected_macrorreg <- input$macrorreg111 
    updatePickerInput(session, "rds111", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds111, { 
    selected_rds <- input$rds111 
    updatePickerInput(session, "municipio111", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg112, { 
    selected_macrorreg <- input$macrorreg112 
    updatePickerInput(session, "rds112", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds112, { 
    selected_rds <- input$rds112 
    updatePickerInput(session, "municipio112", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg2, { 
    selected_macrorreg <- input$macrorreg2 
    updatePickerInput(session, "rds2", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds2, { 
    selected_rds <- input$rds2 
    updatePickerInput(session, "municipio2", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg3, { 
    selected_macrorreg <- input$macrorreg3 
    updatePickerInput(session, "rds3", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds3, { 
    selected_rds <- input$rds3 
    updatePickerInput(session, "municipio3", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg4, { 
    selected_macrorreg <- input$macrorreg4 
    updatePickerInput(session, "rds4", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds4, { 
    selected_rds <- input$rds4 
    updatePickerInput(session, "municipio4", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg6, { 
    selected_macrorreg <- input$macrorreg6 
    updatePickerInput(session, "rds6", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds6, { 
    selected_rds <- input$rds6 
    updatePickerInput(session, "municipio6", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg7, { 
    selected_macrorreg <- input$macrorreg7 
    updatePickerInput(session, "rds7", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds7, { 
    selected_rds <- input$rds7 
    updatePickerInput(session, "municipio7", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) })
  
  observeEvent(input$macrorreg8, { 
    selected_macrorreg <- input$macrorreg8 
    updatePickerInput(session, "rds8", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds8, { 
    selected_rds <- input$rds8 
    updatePickerInput(session, "municipio8", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg9, { 
    selected_macrorreg <- input$macrorreg9 
    updatePickerInput(session, "rds9", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds9, { 
    selected_rds <- input$rds9 
    updatePickerInput(session, "municipio9", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
  observeEvent(input$macrorreg11, { 
    selected_macrorreg <- input$macrorreg11 
    updatePickerInput(session, "rds11", 
                      choices = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique(), 
                      selected = rd_mun1 %>% filter(macrorreg_new %in% selected_macrorreg) %>% select(rd) %>% pull() %>% sort() %>% unique()) }) 
  observeEvent(input$rds11, { 
    selected_rds <- input$rds11 
    updatePickerInput(session, "municipio11", 
                      choices = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort(), 
                      selected = rd_mun1 %>% filter(rd %in% selected_rds) %>% select(municipio) %>% pull() %>% sort()) }) 
  
}

shinyApp(ui, server)



# library(rsconnect)

# deployApp()
