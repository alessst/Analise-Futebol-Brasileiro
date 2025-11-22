# --- 1. CONFIGURAÇÃO ---
library(tidyverse)
library(ggrepel)
library(scales)

# Tema Escuro Limpo (para Canva - Transparente)
theme_infographic <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      # Fundo transparente
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      
      # Texto claro para contrastar no Canva
      text = element_text(color = "#FFFFFF", family = "sans"),
      axis.text = element_text(color = "#CCCCCC", size = 12),
      
      # Remover títulos dos eixos
      axis.title = element_blank(), 
      
      # Grades sutis
      panel.grid.major.y = element_line(color = "#333333", linetype = "dotted"), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "none"
    )
}

print("⏳ Calculando as probabilidades (2018-2023)...")

# --- 2. PREPARAÇÃO DOS DADOS ---
dados_brutos <- read_csv("campeonato_brasileiro_2018+.csv")

dados_limpos <- dados_brutos %>%
  filter(ano_campeonato >= 2018 & ano_campeonato <= 2023) %>%
  filter(!is.na(tecnico_mandante), !is.na(tecnico_visitante))

# Empilhar para ter dados por time
d_mand <- dados_limpos %>% transmute(ano=ano_campeonato, rodada, time=time_mandante, tecnico=tecnico_mandante, colocacao=colocacao_mandante)
d_vis <- dados_limpos %>% transmute(ano=ano_campeonato, rodada, time=time_visitante, tecnico=tecnico_visitante, colocacao=colocacao_visitante)
dados_analise <- bind_rows(d_mand, d_vis) %>% arrange(time, ano, rodada)

# Identificar Trocas
dados_analise <- dados_analise %>%
  group_by(time, ano) %>%
  mutate(
    tecnico_ant = lag(tecnico),
    troca = if_else(tecnico != tecnico_ant & !is.na(tecnico_ant), TRUE, FALSE),
    id_passagem = cumsum(troca)
  ) %>% ungroup()

# Contar passagens REAIS (ignorando interinos < 2 jogos)
passagens <- dados_analise %>%
  group_by(time, ano, id_passagem, tecnico) %>%
  summarise(jogos = n(), .groups="drop")

contagem_trocas_reais <- passagens %>%
  filter(jogos > 2, id_passagem > 0) %>% # Filtra interinos e o técnico que começou o ano
  group_by(time, ano) %>%
  summarise(num_trocas = n(), .groups="drop")

# Pegar posição final
posicao_final <- dados_analise %>%
  group_by(time, ano) %>%
  summarise(pos_final = last(colocacao), .groups="drop")

# Juntar tudo
tabela_risco <- posicao_final %>%
  left_join(contagem_trocas_reais, by = c("time", "ano")) %>%
  mutate(num_trocas = replace_na(num_trocas, 0)) %>% # Se for NA, é 0 trocas
  # Categorizar para o gráfico (0, 1, 2, 3+)
  mutate(cat_trocas = case_when(
    num_trocas == 0 ~ "0 Trocas",
    num_trocas == 1 ~ "1 Troca",
    num_trocas == 2 ~ "2 Trocas",
    num_trocas >= 3 ~ "3+ Trocas"
  ))

# --- 3. CÁLCULO DAS PROBABILIDADES ---

probabilidades <- tabela_risco %>%
  group_by(cat_trocas) %>%
  summarise(
    total_times = n(),
    # Campeão (1º)
    qtd_campeao = sum(pos_final == 1),
    prob_campeao = (qtd_campeao / total_times),
    
    # Libertadores (2º ao 6º)
    qtd_liberta = sum(pos_final >= 2 & pos_final <= 6),
    prob_liberta = (qtd_liberta / total_times),
    
    # Rebaixado (17º ao 20º)
    qtd_rebaixado = sum(pos_final >= 17),
    prob_rebaixado = (qtd_rebaixado / total_times)
  )

print("--- TABELA DE PROBABILIDADES ---")
print(probabilidades)


# --- 4. GERAÇÃO DOS GRÁFICOS (LIMPOS E TRANSPARENTES) ---

# Função auxiliar atualizada para remover títulos
plot_probabilidade_clean <- function(dados, coluna_y, cor_barra) {
  ggplot(dados, aes(x = cat_trocas, y = .data[[coluna_y]])) +
    geom_col(fill = cor_barra, width = 0.6, alpha = 0.9) +
    geom_text(aes(label = scales::percent(.data[[coluna_y]], accuracy = 1)), 
              vjust = -0.5, color = "white", fontface = "bold", size = 6) +
    scale_y_continuous(labels = scales::percent, limits = c(0, max(dados[[coluna_y]]) * 1.2)) + 
    # REMOVIDO: Títulos e subtítulos
    labs(title = NULL, subtitle = NULL, x = NULL, y = NULL) +
    theme_infographic()
}

print("🎨 Gerando Gráfico Limpo: Risco de Rebaixamento...")
g_rebaixamento <- plot_probabilidade_clean(
  probabilidades, 
  "prob_rebaixado", 
  "#FF5A5F" # Vermelho
)
ggsave("prob_rebaixamento_clean.png", g_rebaixamento, width=8, height=6, dpi=300, bg="transparent")


print("🎨 Gerando Gráfico Limpo: Chance de Libertadores...")
g_liberta <- plot_probabilidade_clean(
  probabilidades, 
  "prob_liberta", 
  "#00F5D4" # Ciano/Verde Neon
)
ggsave("prob_libertadores_clean.png", g_liberta, width=8, height=6, dpi=300, bg="transparent")


print("🎨 Gerando Gráfico Limpo: Chance de Título...")
g_campeao <- plot_probabilidade_clean(
  probabilidades, 
  "prob_campeao", 
  "#FFD700" # Dourado
)
ggsave("prob_campeao_clean.png", g_campeao, width=8, height=6, dpi=300, bg="transparent")

print("--- Gráficos de Probabilidade Limpos Salvos! ---")