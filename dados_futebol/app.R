
# --- 1. CARREGAR PACOTES (BIBLIOTECAS) ---
library(shiny)
library(shinydashboard) 
library(tidyverse)
library(ggrepel)
library(scales) 
library(DT) 
library(plotly) 
library(fontawesome) 

# --- 2. PREPARAÇÃO DOS DADOS ---

# 2.1 Carga
dados_brutos <- read_csv("campeonato_brasileiro_2018+.csv")

# 2.2 Limpeza
dados_limpos <- dados_brutos %>%
  filter(
    ano_campeonato >= 2018 & ano_campeonato <= 2023
  ) %>%
  filter(
    !is.na(tecnico_mandante),
    !is.na(tecnico_visitante)
  )

# 2.3 Empilhamento
dados_mandante <- dados_limpos %>%
  transmute(
    ano = ano_campeonato, rodada, time = time_mandante, adversario = time_visitante,
    tecnico = tecnico_mandante, colocacao = colocacao_mandante,
    gols_feitos = gols_mandante, gols_sofridos = gols_visitante, mandante = 1
  )

dados_visitante <- dados_limpos %>%
  transmute(
    ano = ano_campeonato, rodada, time = time_visitante, adversario = time_mandante,
    tecnico = tecnico_visitante, colocacao = colocacao_visitante,
    gols_feitos = gols_visitante, gols_sofridos = gols_mandante, mandante = 0
  )

dados_por_time <- bind_rows(dados_mandante, dados_visitante) %>%
  arrange(time, ano, rodada)

# 2.4 Detecção de Trocas
dados_analise <- dados_por_time %>%
  mutate(
    pontos = case_when(
      gols_feitos > gols_sofridos ~ 3,
      gols_feitos == gols_sofridos ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(time, ano) %>%
  mutate(
    tecnico_anterior = lag(tecnico),    
    troca_meio_temporada = if_else(tecnico != tecnico_anterior & !is.na(tecnico_anterior), TRUE, FALSE),
    id_passagem_tecnico = cumsum(troca_meio_temporada)
  ) %>%
  ungroup()

# 2.5 Análise de Trocas 'Reais'
JOGOS_PARA_NAO_SER_INTERINO <- 2

sumario_passagens <- dados_analise %>%
  group_by(time, ano, id_passagem_tecnico, tecnico) %>%
  summarise(
    media_pontos_total = mean(pontos, na.rm = TRUE),
    jogos_total = n()
  ) %>%
  ungroup()

comparativo_bruto <- sumario_passagens %>%
  group_by(time, ano) %>%
  mutate(
    media_pontos_anterior = lag(media_pontos_total),
    jogos_anterior = lag(jogos_total),
    tecnico_anterior = lag(tecnico)
  ) %>%
  ungroup() %>%
  filter(id_passagem_tecnico > 0)

# --- CORREÇÃO AQUI (FILTRO DE INTERINOS NOS DOIS LADOS) ---
comparativo_trocas_reais <- comparativo_bruto %>%
  filter(
    jogos_anterior > JOGOS_PARA_NAO_SER_INTERINO &
      jogos_total > JOGOS_PARA_NAO_SER_INTERINO # Novo técnico também precisa ser efetivo
  ) %>%
  mutate(
    media_pontos_novo = media_pontos_total,
    variacao_media_pontos = media_pontos_novo - media_pontos_anterior
  ) %>%
  select(
    time, ano, 
    tecnico_novo = tecnico, 
    tecnico_anterior, 
    media_pontos_novo, 
    media_pontos_anterior, 
    variacao_media_pontos, 
    jogos_anterior,
    jogos_novo = jogos_total
  )

# 2.6 Criar lista de times para o filtro
lista_de_times <- c("Todos os Times", sort(unique(dados_analise$time)))


# 2.7 CÁLCULOS GERAIS DE FIM DE TEMPORADA
posicao_final <- dados_analise %>%
  group_by(time, ano) %>%
  filter(rodada == max(rodada)) %>%
  summarise(posicao_final = first(colocacao))

trocas_durante_ano <- dados_analise %>%
  filter(troca_meio_temporada == TRUE) %>%
  count(time, ano, name = "total_trocas_ano")

tecnico_inicio_ano <- dados_analise %>% filter(rodada == 1) %>% select(time, ano, tecnico_inicio = tecnico)
tecnico_fim_ano <- dados_analise %>% group_by(time, ano) %>% filter(rodada == max(rodada)) %>% summarise(tecnico_fim = first(tecnico))
trocas_entre_temporadas <- tecnico_inicio_ano %>%
  left_join(tecnico_fim_ano, by=c("time", "ano")) %>%
  group_by(time) %>%
  arrange(time, ano) %>%
  mutate(tecnico_fim_anterior = lag(tecnico_fim)) %>%
  ungroup() %>%
  mutate(trocou_entre_temporadas = (tecnico_inicio != tecnico_fim_anterior))

analise_final_consolidada <- posicao_final %>%
  left_join(trocas_durante_ano, by = c("time", "ano")) %>%
  left_join(trocas_entre_temporadas, by = c("time", "ano")) %>%
  mutate(
    total_trocas_ano = replace_na(total_trocas_ano, 0),
    categoria_troca_ano = case_when(
      total_trocas_ano == 0 ~ "0 Trocas",
      total_trocas_ano == 1 ~ "1 Troca",
      total_trocas_ano > 1  ~ "2+ Trocas"
    )
  )

kpi_q1_campeao_trocou = analise_final_consolidada %>% filter(posicao_final == 1, trocou_entre_temporadas == TRUE) %>% nrow()
kpi_q1_rebaixado_trocou = analise_final_consolidada %>% filter(posicao_final > 16, trocou_entre_temporadas == TRUE) %>% nrow()
total_campeoes <- analise_final_consolidada %>% filter(posicao_final == 1) %>% nrow()
total_rebaixados <- analise_final_consolidada %>% filter(posicao_final > 16) %>% nrow()
kpi_q1_campeao_perc = (kpi_q1_campeao_trocou / total_campeoes) * 100 
kpi_q1_rebaixado_perc = (kpi_q1_rebaixado_trocou / total_rebaixados) * 100 

kpi_q2_media_posicao_por_troca <- analise_final_consolidada %>%
  group_by(categoria_troca_ano) %>%
  summarise(media_posicao = mean(posicao_final, na.rm = TRUE))

# 2.8 CÁLCULOS DE TEMPO
passagens_reais <- sumario_passagens %>%
  filter(jogos_total > JOGOS_PARA_NAO_SER_INTERINO)
kpi_q6_media_jogos_por_tecnico <- mean(passagens_reais$jogos_total, na.rm = TRUE)

# 2.9 DADOS PARA GRÁFICO
trocas_totais_time <- dados_analise %>%
  filter(troca_meio_temporada == TRUE) %>%
  count(time, name = "total_trocas_2018_2023")

media_posicao_time <- analise_final_consolidada %>%
  group_by(time) %>%
  summarise(posicao_media = mean(posicao_final, na.rm = TRUE))

dados_grafico_q8 <- trocas_totais_time %>%
  left_join(media_posicao_time, by = "time")


# --- 3. DEFINIR A INTERFACE (UI) ---

custom_css <- HTML('
/* --- 1. Definição da Paleta e Fontes --- */
@import url("https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap");

:root {
  --bg-color: #121212;         
  --card-color: #1E1E1E;     
  --text-color: #E0E0E0;     
  --text-color-muted: #A0A0A0; 
  --accent-color: #00F5D4;   
  --shadow: 0 4px 12px rgba(0,0,0,0.5); 
  --radius: 8px; 
}

/* --- 2. Layout Geral --- */
body {
  font-family: "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen, Ubuntu, Cantarell, "Open Sans", "Helvetica Neue", sans-serif;
  background-color: var(--bg-color) !important;
  color: var(--text-color) !important;
}
.content-wrapper, .right-side {
  background-color: var(--bg-color) !important;
}

/* --- 3. Cabeçalho e Barra Lateral --- */
.main-header .logo, .main-header .navbar {
  background-color: var(--card-color) !important;
  border-bottom: 1px solid #333;
}
.main-header .logo { font-weight: 700; font-size: 22px; }
.main-header .sidebar-toggle:hover {
  background-color: var(--accent-color) !important;
  color: #000 !important;
}
.main-sidebar {
  background-color: var(--card-color) !important;
  border-right: 1px solid #333;
}
.sidebar-menu li a { color: var(--text-color) !important; }
.sidebar-menu li.active > a, .sidebar-menu li a:hover {
  background-color: #333 !important;
  color: var(--accent-color) !important;
  border-left-color: var(--accent-color) !important;
}

/* --- 4. Caixas e Cards (.box) --- */
.box {
  background: var(--card-color) !important;
  border: 1px solid #333;
  border-radius: var(--radius) !important;
  box-shadow: var(--shadow) !important;
  color: var(--text-color) !important;
  border-top: 1px solid #333 !important; 
}
.box-header {
  color: var(--text-color) !important;
  padding: 15px;
}
.box.box-primary .box-header, .box.box-info .box-header, .box.box-warning .box-header {
  background: transparent !important;
  border-bottom: 1px solid var(--accent-color);
}
.box-title { font-weight: 600; font-size: 18px; }

/* --- 5. KPIs (InfoBox) --- */
.info-box {
  background: var(--card-color) !important;
  border: 1px solid #333;
  border-radius: var(--radius) !important;
  box-shadow: var(--shadow) !important;
  color: var(--text-color) !important;
}
.info-box-content { color: var(--text-color) !important; }
.info-box-number { font-weight: 700; font-size: 24px; }
.info-box-icon {
  background: rgba(255, 255, 255, 0.05) !important;
  border-radius: var(--radius) 0 0 var(--radius);
  color: var(--text-color);
  font-size: 40px !important;
}
.bg-green .info-box-icon { color: #28a745 !important; }
.bg-red .info-box-icon { color: #dc3545 !important; }
.bg-yellow .info-box-icon { color: #ffc107 !important; }
.bg-purple .info-box-icon { color: #6f42c1 !important; }
.bg-blue .info-box-icon { color: #007bff !important; }
.bg-light-blue .info-box-icon { color: #3c8dbc !important; }


/* --- 6a. Tabelas Interativas (DT) --- */
.dataTables_wrapper { color: var(--text-color-muted) !important; }
table.dataTable th {
  background-color: #333 !important;
  color: var(--text-color) !important;
  border-bottom: 1px solid var(--accent-color) !important;
}
table.dataTable td {
  background-color: var(--card-color) !important;
  color: var(--text-color-muted) !important;
  border-top: 1px solid #333 !important;
}
table.dataTable.hover tbody tr:hover {
  background-color: #4a5568 !important; 
}
.dataTables_paginate .paginate_button {
  background-color: #333 !imporant;
  color: var(--text-color) !important;
  border: 1px solid #555 !important;
}
.dataTables_paginate .paginate_button.current {
  background-color: var(--accent-color) !important;
  color: #000 !important;
  border-color: var(--accent-color) !important;
}
.dataTables_info, .dataTables_length, .dataTables_filter { color: var(--text-color-muted) !important; }
.dataTables_filter input {
  background-color: #333 !important;
  color: var(--text-color) !important;
  border: 1px solid #555 !important;
}

/* --- 6b. Tabelas Simples (renderTable) --- */
table.table {
  background-color: var(--card-color) !important;
  color: var(--text-color-muted) !important;
  border: 1px solid #333 !important;
}
table.table th {
  color: var(--text-color) !important;
  background-color: #333 !important;
  border-bottom: 2px solid var(--accent-color) !important;
  border-left: 1px solid #333 !important;
  border-right: 1px solid #333 !important;
}
table.table td {
  border: 1px solid #333 !important;
}
/* Striped tables */
.table-striped > tbody > tr:nth-of-type(odd) > * {
  background-color: var(--card-color) !important;
  color: var(--text-color-muted) !important;
}
.table-striped > tbody > tr:nth-of-type(even) > * {
  background-color: #2a2a2a !important;
  color: var(--text-color-muted) !important;
}
/* Hover tables */
.table-hover > tbody > tr:hover > * {
  background-color: #4a5568 !important;
  color: var(--text-color) !important;
}


/* --- 7. Controles (SelectInput) --- */
.selectize-input {
  background-color: #333 !important;
  color: var(--text-color) !important;
  border: 1px solid #555 !important;
  border-radius: var(--radius) !important;
}
.selectize-dropdown-content .option {
  background-color: var(--card-color) !important;
  color: var(--text-color-muted) !important;
}
.selectize-dropdown-content .option:hover {
  background-color: #4a5568 !important;
  color: var(--text-color) !important;
}

/* --- 8. Títulos customizados --- */
.custom-section-header {
  font-size: 20px;
  font-weight: 600;
  color: var(--text-color);
  margin-top: 20px;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 2px solid var(--accent-color);
}
')


ui <- dashboardPage(
  dashboardHeader(title = "Análise de Técnicos", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    selectInput(
      inputId = "time_selecionado", 
      label = "Filtro Rápido por Time:",   
      choices = lista_de_times,      
      selected = "Todos os Times"    
    ),
    hr(), 
    h4("Sobre este Dashboard", style = "padding-left: 15px;"),
    p("Este app analisa trocas 'reais', ignorando passagens interinas (<= 2 jogos).",
      style = "padding-left: 15px; padding-right: 15px; font-size: 12px;")
  ),
  
  dashboardBody(
    tags$head(tags$style(custom_css)),
    
    # Linha 1: KPIs Dinâmicos
    fluidRow(
      h3("Análise Dinâmica", class = "custom-section-header"),
      infoBoxOutput("kpi_total_trocas"), 
      infoBoxOutput("kpi_variacao_media_pontos"), 
      infoBoxOutput("kpi_tecnicos_unicos")
    ),
    
    # Linha 2: Painel "Deep Dive"
    fluidRow(
      uiOutput("painel_deep_dive_time")
    ),
    
    # Linha 3: Gráfico Interativo e Tabela
    fluidRow(
      box(
        title = "Eficácia da Troca: Antes vs. Depois (Interativo)",
        width = 7, status = "primary", 
        plotlyOutput("grafico_interativo_q4") 
      ),
      box(
        title = "Detalhes das Trocas (Time Selecionado)",
        width = 5, status = "primary",
        DTOutput("tabela_detalhes_trocas") 
      )
    ),
    
    # Linha 4: Análises Gerais
    fluidRow(
      h3(textOutput("titulo_analises_gerais"), class = "custom-section-header"),
      uiOutput("conteudo_analises_gerais") 
    )
  )
)


# --- 4. DEFINIR O SERVIDOR (SERVER) ---
server <- function(input, output) {
  
  # --- 4.0 Definições de Tema para Gráficos ---
  theme_dark_custom <- function(base_size = 14) {
    theme_minimal(base_size = base_size) +
      theme(
        plot.background = element_rect(fill = "#1E1E1E", color = NA),
        panel.background = element_rect(fill = "#1E1E1E"),
        panel.grid.major = element_line(color = "#4A4A4A", linetype = "dotted"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#E0E0E0"),
        axis.text = element_text(color = "#E0E0E0"),
        title = element_text(color = "#E0E0E0", face = "bold"),
        plot.title = element_text(size = 18, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "#A0A0A0", margin = margin(b = 10)),
        axis.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "#1E1E1E"),
        legend.key = element_rect(fill = "#1E1E1E"),
        legend.text = element_text(color = "#E0E0E0")
      )
  }
  
  layout_dark_custom <- function(p) {
    p %>% layout(
      plot_bgcolor = "#1E1E1E", 
      paper_bgcolor = "#1E1E1E",
      font = list(color = "#E0E0E0"),
      xaxis = list(gridcolor = "#4A4A4A"),
      yaxis = list(gridcolor = "#4A4A4A"),
      legend = list(bgcolor = "#1E1E1E", bordercolor = "#333")
    )
  }
  
  
  # 4.1 Criar dados REATIVOS (que mudam com o filtro)
  dados_filtrados_reativos <- reactive({
    if (input$time_selecionado == "Todos os Times") {
      comparativo_trocas_reais
    } else {
      comparativo_trocas_reais %>%
        filter(time == input$time_selecionado)
    }
  })
  
  # 4.2 Lógica para os KPIs INTERATIVOS (Linha 1)
  
  output$kpi_total_trocas <- renderInfoBox({
    num_trocas <- nrow(dados_filtrados_reativos())
    infoBox(
      title = "Trocas 'Reais'",
      value = num_trocas,
      subtitle = paste("em", input$time_selecionado),
      icon = icon("repeat"), 
      color = "purple"
    )
  })
  
  output$kpi_variacao_media_pontos <- renderInfoBox({ 
    media_var <- mean(dados_filtrados_reativos()$variacao_media_pontos, na.rm = TRUE)
    media_var_fmt <- if (is.nan(media_var)) { 0 } else { round(media_var, 2) }
    cor <- if_else(media_var_fmt >= 0, "green", "red")
    
    infoBox(
      title = "Variação Média de Pontos",
      value = media_var_fmt,
      subtitle = "Após a troca",
      icon = icon("chart-line"), 
      color = cor
    )
  })
  
  output$kpi_tecnicos_unicos <- renderInfoBox({
    if (input$time_selecionado == "Todos os Times") {
      num_tecnicos <- length(unique(dados_analise$tecnico))
      subtitulo <- "Técnicos Únicos (Total)"
    } else {
      num_tecnicos <- dados_analise %>%
        filter(time == input$time_selecionado) %>%
        distinct(tecnico) %>%
        nrow()
      subtitulo <- paste("Técnicos Únicos em", input$time_selecionado)
    }
    
    infoBox(
      title = "Técnicos Diferentes",
      value = num_tecnicos,
      subtitle = subtitulo,
      icon = icon("users"), 
      color = "blue"
    )
  })
  
  
  # 4.3 Lógica do Painel "DEEP DIVE" (Linha 2)
  
  output$painel_deep_dive_time <- renderUI({
    
    if(input$time_selecionado == "Todos os Times") {
      return(NULL) 
    }
    
    box(
      title = paste("Análise Específica:", input$time_selecionado),
      width = 12, status = "primary", solidHeader = TRUE, 
      
      fluidRow(
        infoBoxOutput("kpi_time_posicao_media"),
        infoBoxOutput("kpi_time_melhor_troca"),
        infoBoxOutput("kpi_time_pior_troca")
      ),
      fluidRow(
        box(
          title = "Desempenho de todos os técnicos 'reais' do time",
          width = 12,
          DTOutput("tabela_deep_dive_tecnicos")
        )
      )
    )
  })
  
  output$kpi_time_posicao_media <- renderInfoBox({
    pos_media <- analise_final_consolidada %>%
      filter(time == input$time_selecionado) %>%
      summarise(media = mean(posicao_final, na.rm = TRUE)) %>%
      pull(media)
    
    infoBox(
      "Posição Média (2018-23)",
      round(pos_media, 1),
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  output$kpi_time_melhor_troca <- renderInfoBox({
    melhor_troca <- dados_filtrados_reativos() %>% 
      arrange(desc(variacao_media_pontos)) %>%
      head(1)
    
    if(nrow(melhor_troca) == 0) {
      return(infoBox("Melhor Troca", "N/A", icon = icon("arrow-up"), color = "green"))
    }
    
    infoBox(
      "Melhor Troca (Variação Média Pontos)",
      paste0(melhor_troca$tecnico_novo, " (+", round(melhor_troca$variacao_media_pontos, 2), ")"),
      subtitle = paste("Substituindo", melhor_troca$tecnico_anterior),
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$kpi_time_pior_troca <- renderInfoBox({
    pior_troca <- dados_filtrados_reativos() %>%
      arrange(variacao_media_pontos) %>%
      head(1) 
    
    if(nrow(pior_troca) == 0) {
      return(infoBox("Pior Troca", "N/A", icon = icon("arrow-down"), color = "red"))
    }
    
    infoBox(
      "Pior Troca (Variação Média Pontos)",
      paste0(pior_troca$tecnico_novo, " (", round(pior_troca$variacao_media_pontos, 2), ")"),
      subtitle = paste("Substituindo", pior_troca$tecnico_anterior),
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  output$tabela_deep_dive_tecnicos <- renderDT({
    df <- sumario_passagens %>%
      filter(time == input$time_selecionado, 
             jogos_total > JOGOS_PARA_NAO_SER_INTERINO) %>%
      group_by(tecnico) %>%
      summarise(
        media_pontos_geral = mean(media_pontos_total, na.rm = TRUE),
        total_jogos_comandados = sum(jogos_total, na.rm = TRUE)
      ) %>%
      select(
        "Técnico" = tecnico,
        "Média de Pontos" = media_pontos_geral,
        "Total de Jogos" = total_jogos_comandados
      ) %>%
      mutate("Média de Pontos" = round(`Média de Pontos`, 2)) %>%
      arrange(desc(`Média de Pontos`))
    
    datatable(
      df,
      class = 'stripe hover', 
      options = list(pagingType = "simple_numbers", 
                     pageLength = 5, 
                     lengthMenu = c(5, 10),
                     searching = FALSE, 
                     info = FALSE) 
    )
  })
  
  
  # 4.4 Lógica para o Gráfico INTERATIVO (Linha 3)
  output$grafico_interativo_q4 <- renderPlotly({
    
    df_reativo = dados_filtrados_reativos()
    
    if (nrow(df_reativo) == 0) {
      plot_ly() %>% layout(title = "Nenhuma troca 'real' registrada para este time.") %>%
        layout_dark_custom()
    } else {
      g <- ggplot(df_reativo, aes(x = media_pontos_anterior, 
                                  y = media_pontos_novo,
                                  text = paste("Time:", time,
                                               "<br>Ano:", ano,
                                               "<br>Téc. Novo:", tecnico_novo,
                                               "<br>Téc. Anterior:", tecnico_anterior,
                                               "<br>Variação:", round(variacao_media_pontos,2))
      )) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        geom_point(aes(color = variacao_media_pontos > 0), size = 2, alpha = 0.7) +
        labs(
          title = NULL, 
          x = "Média de Pontos (Téc. Anterior)",
          y = "Média de Pontos (Téc. Novo)",
          color = "Melhorou?"
        ) +
        theme_dark_custom(base_size = 12) + 
        scale_color_manual(values = c("TRUE" = "#00F5D4", "FALSE" = "#FF5A5F"), 
                           labels = c("Piorou ou Manteve", "Melhorou"))
      
      ggplotly(g, tooltip = "text") %>%
        layout_dark_custom() 
    }
  })
  
  # 4.5 Lógica para a Tabela de Detalhes (Linha 3)
  output$tabela_detalhes_trocas <- renderDT({
    df <- dados_filtrados_reativos() %>%
      select(
        Ano = ano,
        Técnico_Novo = tecnico_novo,
        Técnico_Anterior = tecnico_anterior,
        "Média Pontos (Novo)" = media_pontos_novo,
        "Média Pontos (Ant.)" = media_pontos_anterior,
        "Variação" = variacao_media_pontos
      ) %>%
      mutate(across(where(is.numeric), round, 2))
    
    datatable(
      df,
      class = 'stripe hover', 
      options = list(pagingType = "simple_numbers", 
                     pageLength = 10,
                     lengthMenu = c(10, 20),
                     info = FALSE) 
    )
  })
  
  
  # 4.6 LÓGICA DA SEÇÃO DINÂMICA (Linha 4)
  
  output$titulo_analises_gerais <- renderText({
    if(input$time_selecionado == "Todos os Times") {
      "Contexto Geral da Liga (2018-2023)"
    } else {
      paste("Contexto Histórico:", input$time_selecionado)
    }
  })
  
  output$conteudo_analises_gerais <- renderUI({
    
    # SE "TODOS OS TIMES" FOR SELECIONADO
    if(input$time_selecionado == "Todos os Times") {
      
      tagList( 
        fluidRow(
          box(
            title = "Posição Média x Nº de Trocas DURANTE a Temporada",
            width = 4, status = "primary",
            tableOutput("tabela_q2_posicao_media") 
          ),
          box(
            title = "Trocas ENTRE Temporadas",
            width = 4, status = "primary",
            infoBox("Campeões que trocaram", paste0(round(kpi_q1_campeao_perc,0), "%"), 
                    icon = icon("trophy"), color = "yellow", width = 12),
            infoBox("Rebaixados que trocaram", paste0(round(kpi_q1_rebaixado_perc,0), "%"), 
                    icon = icon("arrow-down"), color = "red", width = 12)
          ),
          box(
            title = "Tempo de Permanência (Geral)",
            width = 4, status = "primary",
            infoBox("Média de Jogos por Técnico", round(kpi_q6_media_jogos_por_tecnico, 1), 
                    icon = icon("calendar-alt"), color = "light-blue", width = 12), 
            p("Considera apenas passagens 'reais' (> 2 jogos).", style="color: var(--text-color-muted);")
          )
        ),
        
        fluidRow(
          box(
            title = "Posição Média vs. Total de Trocas (Por Time)",
            width = 12, status = "primary",
            plotOutput("grafico_q8_trocas_vs_posicao")
          )
        )
      )
      
    } else {
      # SE UM TIME FOR SELECIONADO
      tagList(
        fluidRow(
          infoBoxOutput("kpi_time_tempo_permanencia", width = 6),
          infoBoxOutput("kpi_time_trocas_entre_temp", width = 6)
        ),
        fluidRow(
          box(
            title = "Histórico Anual (Posição e Trocas)",
            width = 12,
            tableOutput("tabela_time_historico_ano")
          )
        )
      ) 
    }
  })
  
  # --- 4.7 LÓGICA PARA OS OUTPUTS (Geral e Time) ---
  
  output$tabela_q2_posicao_media <- renderTable({
    kpi_q2_media_posicao_por_troca %>%
      mutate(media_posicao = round(media_posicao, 0)) %>% 
      rename("Nº de Trocas" = categoria_troca_ano, "Posição Média Final" = media_posicao)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$grafico_q8_trocas_vs_posicao <- renderPlot({
    ggplot(dados_grafico_q8, aes(x = total_trocas_2018_2023, y = posicao_media)) +
      geom_point(color = "#00F5D4", size = 3) + 
      geom_text_repel(aes(label = time), size = 4, color = "#E0E0E0") + 
      scale_y_reverse(breaks = seq(1, 20, by = 2)) +
      labs(
        x = "Total de Trocas de Técnico (2018-2023)",
        y = "Posição Média Final no Campeonato"
      ) +
      theme_dark_custom() # Aplicando nosso tema escuro
  })
  
  output$kpi_time_tempo_permanencia <- renderInfoBox({
    media_jogos_time <- passagens_reais %>%
      filter(time == input$time_selecionado) %>%
      summarise(media = mean(jogos_total, na.rm = TRUE)) %>%
      pull(media)
    
    media_jogos_time_fmt <- if(is.nan(media_jogos_time)) { "N/A" } else { round(media_jogos_time, 1) }
    
    infoBox(
      "Média de Jogos por Técnico (Time)",
      media_jogos_time_fmt,
      icon = icon("calendar-check"),
      color = "light-blue"
    )
  })
  
  output$kpi_time_trocas_entre_temp <- renderInfoBox({
    total_trocas_entre_time <- analise_final_consolidada %>%
      filter(time == input$time_selecionado, trocou_entre_temporadas == TRUE) %>%
      nrow()
    
    infoBox(
      "Trocas ENTRE Temporadas (Total)",
      total_trocas_entre_time,
      icon = icon("shuffle"), 
      color = "yellow"
    )
  })
  
  output$tabela_time_historico_ano <- renderTable({
    analise_final_consolidada %>%
      filter(time == input$time_selecionado) %>%
      select(
        "Ano" = ano,
        "Posição Final" = posicao_final,
        "Trocas Durante o Ano" = total_trocas_ano,
        "Trocou Entre Temporadas?" = trocou_entre_temporadas
      ) %>%
      mutate("Trocou Entre Temporadas?" = if_else(`Trocou Entre Temporadas?` == TRUE, "Sim", "Não")) %>%
      arrange(Ano)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
}


# --- 5. LIGAR O APP ---
shinyApp(ui = ui, server = server)