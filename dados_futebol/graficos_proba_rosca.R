# --- 1. CONFIGURAÇÃO ---
library(tidyverse)
library(ggrepel)
library(scales)

# Tema Escuro Limpo (para Canva - Transparente)
theme_infographic <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      text = element_text(color = "#FFFFFF", family = "sans"),
      axis.text = element_text(color = "#CCCCCC", size = 12),
      axis.title = element_blank(), 
      panel.grid.major.y = element_line(color = "#333333", linetype = "dotted"), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}

# Tema Específico para Rosca (Remove eixos e grades)
theme_rosca_clean <- function(base_size = 16) {
  theme_void(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      text = element_text(color = "#FFFFFF", family = "sans"),
      legend.position = "right",
      legend.text = element_text(size = 12)
    )
}

print("⏳ Calculando as probabilidades (2018-2023)...")

# --- 2. PREPARAÇÃO DOS DADOS ---
dados_brutos <- read_csv("campeonato_brasileiro_2018+.csv")

dados_limpos <- dados_brutos %>%
  filter(ano_campeonato >= 2018 & ano_campeonato <= 2023) %>%
  filter(!is.na(tecnico_mandante), !is.na(tecnico_visitante))

d_mand <- dados_limpos %>% transmute(ano=ano_campeonato, rodada, time=time_mandante, tecnico=tecnico_mandante, colocacao=colocacao_mandante)
d_vis <- dados_limpos %>% transmute(ano=ano_campeonato, rodada, time=time_visitante, tecnico=tecnico_visitante, colocacao=colocacao_visitante)
dados_analise <- bind_rows(d_mand, d_vis) %>% arrange(time, ano, rodada)

dados_analise <- dados_analise %>%
  group_by(time, ano) %>%
  mutate(
    tecnico_ant = lag(tecnico),
    troca = if_else(tecnico != tecnico_ant & !is.na(tecnico_ant), TRUE, FALSE),
    id_passagem = cumsum(troca)
  ) %>% ungroup()

passagens <- dados_analise %>%
  group_by(time, ano, id_passagem, tecnico) %>%
  summarise(jogos = n(), .groups="drop")

contagem_trocas_reais <- passagens %>%
  filter(jogos > 2, id_passagem > 0) %>% 
  group_by(time, ano) %>%
  summarise(num_trocas = n(), .groups="drop")

posicao_final <- dados_analise %>%
  group_by(time, ano) %>%
  summarise(pos_final = last(colocacao), .groups="drop")

tabela_risco <- posicao_final %>%
  left_join(contagem_trocas_reais, by = c("time", "ano")) %>%
  mutate(num_trocas = replace_na(num_trocas, 0)) %>% 
  mutate(cat_trocas = case_when(
    num_trocas == 0 ~ "0 Trocas",
    num_trocas == 1 ~ "1 Troca",
    TRUE ~ "2+ Trocas" # Simplifiquei para 3 grupos para a rosca ficar mais bonita
  ))

# --- 3. CÁLCULO DAS PROBABILIDADES (Para Barras) ---
probabilidades <- tabela_risco %>%
  group_by(cat_trocas) %>%
  summarise(
    total_times = n(),
    qtd_campeao = sum(pos_final == 1),
    prob_campeao = (qtd_campeao / total_times),
    qtd_liberta = sum(pos_final >= 2 & pos_final <= 6),
    prob_liberta = (qtd_liberta / total_times),
    qtd_rebaixado = sum(pos_final >= 17),
    prob_rebaixado = (qtd_rebaixado / total_times)
  )

# --- 4. FUNÇÕES DE GRÁFICO ---

# Barras (Probabilidade)
plot_probabilidade_clean <- function(dados, coluna_y, cor_barra) {
  ggplot(dados, aes(x = cat_trocas, y = .data[[coluna_y]])) +
    geom_col(fill = cor_barra, width = 0.6, alpha = 0.9) +
    geom_text(aes(label = scales::percent(.data[[coluna_y]], accuracy = 1)), 
              vjust = -0.5, color = "white", fontface = "bold", size = 6) +
    scale_y_continuous(labels = scales::percent, limits = c(0, max(dados[[coluna_y]]) * 1.2)) + 
    labs(title = NULL, subtitle = NULL, x = NULL, y = NULL) +
    theme_infographic()
}

# Rosca (Composição)
plot_rosca_clean <- function(dados_filtrados, titulo_arquivo) {
  
  # Calcular composição
  composicao <- dados_filtrados %>%
    count(cat_trocas) %>%
    mutate(perc = n / sum(n)) %>%
    mutate(ymax = cumsum(perc), ymin = c(0, head(ymax, n=-1))) %>%
    mutate(label_pos = (ymax + ymin) / 2)
  
  g <- ggplot(composicao, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat_trocas)) +
    geom_rect(color = "transparent") + # O anel
    coord_polar(theta="y") + # Transforma em rosca
    xlim(c(1, 4)) + # Define o tamanho do buraco no meio
    scale_fill_manual(values = c("0 Trocas"="#77DD77", "1 Troca"="#FFCC5C", "2+ Trocas"="#FF5A5F")) +
    # Adicionar Texto da Porcentagem
    geom_text(aes(x=3.5, y=label_pos, label=scales::percent(perc, accuracy=1)), 
              color="white", size=6, fontface="bold") +
    labs(fill = NULL) + # Remove título da legenda
    theme_rosca_clean() +
    theme(legend.position = "bottom", legend.text = element_text(color="white"))
  
  ggsave(titulo_arquivo, g, width=8, height=8, dpi=300, bg="transparent")
}


# --- 5. GERAÇÃO DOS ARQUIVOS ---

print("🎨 Gerando Gráficos de Barras (Risco)...")
g_rebaixamento <- plot_probabilidade_clean(probabilidades, "prob_rebaixado", "#FF5A5F")
ggsave("prob_rebaixamento_barras.png", g_rebaixamento, width=8, height=6, dpi=300, bg="transparent")

g_liberta <- plot_probabilidade_clean(probabilidades, "prob_liberta", "#00F5D4")
ggsave("prob_libertadores_barras.png", g_liberta, width=8, height=6, dpi=300, bg="transparent")

g_campeao <- plot_probabilidade_clean(probabilidades, "prob_campeao", "#FFD700")
ggsave("prob_campeao_barras.png", g_campeao, width=8, height=6, dpi=300, bg="transparent")


print("🍩 Gerando Gráficos de Rosca (Perfil/Composição)...")

# 1. Rosca: Quem são os REBAIXADOS?
rebaixados_df <- tabela_risco %>% filter(pos_final >= 17)
plot_rosca_clean(rebaixados_df, "perfil_rebaixados_rosca.png")

# 2. Rosca: Quem são os CAMPEÕES?
campeoes_df <- tabela_risco %>% filter(pos_final == 1)
plot_rosca_clean(campeoes_df, "perfil_campeoes_rosca.png")

# 3. Rosca: Quem vai pra LIBERTADORES?
liberta_df <- tabela_risco %>% filter(pos_final >= 2 & pos_final <= 6)
plot_rosca_clean(liberta_df, "perfil_libertadores_rosca.png")

print("--- Todos os gráficos (Barras e Roscas) gerados! ---")