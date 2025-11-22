# --- 1. CONFIGURAÇÃO INICIAL ---
library(tidyverse)
library(ggrepel)
library(scales)

# TEMA TRANSPARENTE (Específico para sobrepor no Canva)
theme_transparent_clean <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      # FUNDOS TRANSPARENTES
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      
      # LINHAS DE GRADE (Sutis, para guiar o olhar)
      panel.grid.major = element_line(color = "#555555", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      
      # TEXTOS (Claros, para contrastar com fundos escuros no Canva)
      text = element_text(color = "#FFFFFF", family = "sans"),
      axis.text = element_text(color = "#CCCCCC"), # Eixos um pouco mais cinzas
      axis.title = element_text(color = "#FFFFFF", face = "bold"),
      legend.text = element_text(color = "#FFFFFF"),
      
      # POSIÇÃO DA LEGENDA
      legend.position = "bottom"
    )
}

print("⏳ Processando dados (2018-2023)...")

# --- 2. PROCESSAMENTO DE DADOS ---
dados_brutos <- read_csv("campeonato_brasileiro_2018+.csv")

dados_limpos <- dados_brutos %>%
  filter(ano_campeonato >= 2018 & ano_campeonato <= 2023) %>%
  filter(!is.na(tecnico_mandante), !is.na(tecnico_visitante))

d_mand <- dados_limpos %>% transmute(ano=ano_campeonato, rodada, time=time_mandante, tecnico=tecnico_mandante, colocacao=colocacao_mandante, gols_f=gols_mandante, gols_s=gols_visitante, pts=if_else(gols_mandante>gols_visitante,3,if_else(gols_mandante==gols_visitante,1,0)))
d_vis <- dados_limpos %>% transmute(ano=ano_campeonato, rodada, time=time_visitante, tecnico=tecnico_visitante, colocacao=colocacao_visitante, gols_f=gols_visitante, gols_s=gols_mandante, pts=if_else(gols_visitante>gols_mandante,3,if_else(gols_visitante==gols_mandante,1,0)))
dados_analise <- bind_rows(d_mand, d_vis) %>% arrange(time, ano, rodada)

dados_analise <- dados_analise %>%
  group_by(time, ano) %>%
  mutate(
    tecnico_ant = lag(tecnico),
    troca = if_else(tecnico != tecnico_ant & !is.na(tecnico_ant), TRUE, FALSE),
    id_passagem = cumsum(troca)
  ) %>% ungroup()

JOGOS_INTERINO <- 2
passagens <- dados_analise %>%
  group_by(time, ano, id_passagem, tecnico) %>%
  summarise(media_pts = mean(pts), jogos = n(), .groups="drop")

# CORREÇÃO APLICADA AQUI (O CASO MÁRIO JORGE)
# Agora filtramos para que TANTO o técnico anterior quanto o novo tenham > 2 jogos
trocas_reais <- passagens %>%
  group_by(time, ano) %>%
  mutate(pts_ant = lag(media_pts), jogos_ant = lag(jogos)) %>%
  ungroup() %>%
  filter(
    id_passagem > 0, 
    jogos_ant > JOGOS_INTERINO, # Técnico anterior não era interino
    jogos > JOGOS_INTERINO      # NOVO filtro: Novo técnico também não é interino
  ) %>%
  mutate(variacao = media_pts - pts_ant)

resumo_temporada <- dados_analise %>%
  group_by(time, ano) %>%
  summarise(pos_final = last(colocacao), total_trocas = sum(troca), .groups="drop") %>%
  mutate(cat_trocas = case_when(total_trocas == 0 ~ "0 Trocas", total_trocas == 1 ~ "1 Troca", TRUE ~ "2+ Trocas"))

# --- 3. GERAÇÃO DOS GRÁFICOS (LIMPOS E TRANSPARENTES) ---

print("🎨 Gerando Asset 1 (CORRIGIDO): Posição vs Técnicos Reais...")
# Correção: Usamos a tabela 'passagens' e filtramos jogos > 2 para contar apenas técnicos efetivos
tecnicos_efetivos <- passagens %>%
  filter(jogos > 2) %>% 
  group_by(time) %>%
  summarise(total_tecnicos = n(), .groups = "drop")

# Pegamos a posição média do resumo já calculado
posicao_media_times <- resumo_temporada %>%
  group_by(time) %>%
  summarise(media_pos = mean(pos_final))

# Juntamos
g1_dados <- tecnicos_efetivos %>%
  left_join(posicao_media_times, by = "time")

g1 <- ggplot(g1_dados, aes(x = total_tecnicos, y = media_pos)) +
  geom_point(color = "#00F5D4", size = 5, alpha = 0.9) + 
  geom_text_repel(aes(label = time), color = "white", size = 5, max.overlaps = 20, fontface = "bold") +
  scale_y_reverse(breaks = seq(1, 20, 2)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) + # Garante numeros inteiros no eixo X
  # SEM TÍTULOS
  labs(title = NULL, subtitle = NULL, x = "Nº de Técnicos Efetivos (2018-23)", y = "Posição Média") +
  theme_transparent_clean()

ggsave("asset_1_posicao_vs_tecnicos_reais.png", g1, width=10, height=8, dpi=300, bg = "transparent")


print("🎨 Gerando Asset 2: Antes vs Depois...")
g2 <- ggplot(trocas_reais, aes(x = pts_ant, y = media_pts)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="#AAAAAA", linewidth=1) +
  geom_point(aes(color = variacao > 0), size = 4, alpha = 0.8) +
  scale_color_manual(values = c("TRUE"="#00F5D4", "FALSE"="#FF5A5F"), labels=c("Piorou/Igual", "Melhorou")) +
  labs(title = NULL, subtitle = NULL, x = "Pontos (Antigo)", y = "Pontos (Novo)", color = NULL) +
  theme_transparent_clean() + theme(legend.position = "top")

ggsave("asset_2_antes_vs_depois.png", g2, width=10, height=8, dpi=300, bg = "transparent")


print("🎨 Gerando Asset 3: Estabilidade...")
# CORES PASTEL APLICADAS AQUI
g3_dados <- resumo_temporada %>% group_by(cat_trocas) %>% summarise(pos_media = mean(pos_final))
g3 <- ggplot(g3_dados, aes(x = cat_trocas, y = pos_media, fill = cat_trocas)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pos_media, 0), "º")), vjust = -0.5, color = "white", fontface="bold", size=8) +
  scale_fill_manual(values = c("0 Trocas"="#77DD77", "1 Troca"="#FFCC5C", "2+ Trocas"="#FF6961")) + # Verde, Amarelo, Vermelho Pastel
  scale_y_reverse(limits = c(20, 0)) +
  labs(title = NULL, subtitle = NULL, x = NULL, y = "Posição Final Média") +
  theme_transparent_clean() + theme(legend.position = "none")

ggsave("asset_3_estabilidade.png", g3, width=10, height=8, dpi=300, bg = "transparent")


print("🎨 Gerando Asset 4: Duração...")
passagens_longas <- passagens %>% filter(jogos > JOGOS_INTERINO)
media_jogos <- mean(passagens_longas$jogos)
g4 <- ggplot(passagens_longas, aes(x = jogos)) +
  geom_histogram(binwidth = 5, fill = "#6f42c1", color = NA, alpha=0.9) +
  geom_vline(xintercept = media_jogos, color = "#00F5D4", linewidth=2, linetype="dashed") +
  annotate("text", x = media_jogos + 2, y = 50, label = paste("Média:", round(media_jogos,1)), color = "#00F5D4", hjust = 0, size=6, fontface="bold") +
  labs(title = NULL, subtitle = NULL, x = "Jogos no Cargo", y = "Quantidade") +
  theme_transparent_clean()

ggsave("asset_4_duracao.png", g4, width=10, height=8, dpi=300, bg = "transparent")


print("🎨 Gerando Asset 5: Reciclagem...")
contagem <- dados_analise %>% group_by(tecnico, id_passagem) %>% summarise(j=n(), .groups="drop") %>% filter(j>5) %>% count(tecnico, sort=T) %>% head(10)
g5 <- ggplot(contagem, aes(x = reorder(tecnico, n), y = n)) +
  geom_col(fill = "#00F5D4", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.5, color = "white", fontface = "bold", size=6) +
  coord_flip() +
  labs(title = NULL, subtitle = NULL, x = NULL, y = "Nº de Clubes Assumidos") +
  theme_transparent_clean()

ggsave("asset_5_reciclagem.png", g5, width=10, height=8, dpi=300, bg = "transparent")


print("🎨 Gerando Asset 6: Zona de Perigo...")
demissoes <- dados_analise %>% filter(troca == TRUE) %>% count(rodada)
g6 <- ggplot(demissoes, aes(x = rodada, y = n)) +
  geom_area(fill = "#FF5A5F", alpha = 0.4) +
  geom_line(color = "#FF5A5F", linewidth = 2) +
  scale_x_continuous(breaks = seq(1, 38, 4)) +
  labs(title = NULL, subtitle = NULL, x = "Rodada", y = "Demissões") +
  theme_transparent_clean()

ggsave("asset_6_zona_perigo.png", g6, width=10, height=6, dpi=300, bg = "transparent")


print("🎨 Gerando Asset 7: Lua de Mel...")
evolucao <- dados_analise %>% group_by(time, ano, id_passagem) %>% mutate(j_num = row_number()) %>% filter(j_num <= 10) %>% ungroup() %>% group_by(time, ano, id_passagem) %>% filter(max(j_num) >= 10) %>% ungroup() %>% group_by(j_num) %>% summarise(media = mean(pts))
g7 <- ggplot(evolucao, aes(x = j_num, y = media)) +
  geom_line(color = "#00F5D4", linewidth = 2.5) +
  geom_point(color = "white", size = 4) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = NULL, subtitle = NULL, x = "Jogos (Início do Trabalho)", y = "Pontos/Jogo") +
  theme_transparent_clean()

ggsave("asset_7_lua_de_mel.png", g7, width=10, height=6, dpi=300, bg = "transparent")


print("🎨 Gerando Asset 8: Ataque vs Defesa...")
pass_stats <- dados_analise %>% group_by(time, ano, id_passagem) %>% summarise(gp=mean(gols_f), gs=mean(gols_s), j=n(), .groups="drop")
comp_estilo <- pass_stats %>% group_by(time, ano) %>% mutate(gp_ant=lag(gp), gs_ant=lag(gs), j_ant=lag(j)) %>% ungroup() %>% filter(id_passagem>0, j_ant>2) %>% mutate(m_ataque=gp-gp_ant, m_defesa=gs_ant-gs)
d8 <- comp_estilo %>% pivot_longer(cols=c(m_ataque, m_defesa), names_to="tipo", values_to="val") %>% mutate(tipo=if_else(tipo=="m_ataque","Ataque","Defesa"))
g8 <- ggplot(d8, aes(x = val, fill = tipo)) +
  geom_density(alpha = 0.6, color = NA) +
  geom_vline(xintercept = 0, linetype="dashed", color="white") +
  scale_fill_manual(values = c("Ataque"="#00F5D4", "Defesa"="#FF5A5F")) +
  labs(title = NULL, subtitle = NULL, x = "Variação (Gols)", y = NULL, fill=NULL) +
  theme_transparent_clean() + theme(axis.text.y = element_blank())

ggsave("asset_8_ataque_defesa.png", g8, width=10, height=6, dpi=300, bg = "transparent")

print("--- PRONTO! 8 Gráficos Transparentes gerados. ---")