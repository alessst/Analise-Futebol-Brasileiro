#-------------------------------------------------------------------------------------------------------------
# --- INÍCIO DO SEU SCRIPT ---

# 1. Instalar o pacote (rode só uma vez)
# install.packages("tidyverse")

# 2. Carregar a "caixa de ferramentas"
library(tidyverse)

# 3. Carregar os dados (usando o read_csv do tidyverse, que é melhor)
# Ele é mais rápido e inteligente com colunas
dados_brutos <- read_csv("campeonato_brasileiro_2018+.csv")

# 4. Ver se funcionou (o print do tidyverse é mais completo)
print(dados_brutos)

#-------------------------------------------------------------------------------------------------------------

# --- ETAPA 1: LIMPEZA ---

dados_limpos <- dados_brutos %>%
  filter(
    ano_campeonato >= 2018 & ano_campeonato <= 2023
  ) %>%
  filter(
    !is.na(tecnico_mandante),  # Remove linhas onde o técnico mandante é nulo
    !is.na(tecnico_visitante) # Remove linhas onde o técnico visitante é nulo
  )

print("Dados limpos e filtrados para 2018-2023:")
glimpse(dados_limpos)

#-------------------------------------------------------------------------------------------------------------

# --- ETAPA 2: EMPILHAMENTO ---

# 2.1 Criar tabela de "Mandantes"
dados_mandante <- dados_limpos %>%
  transmute( # 'transmute' cria novas colunas e descarta as antigas
    ano = ano_campeonato,
    rodada,
    time = time_mandante,
    adversario = time_visitante,
    tecnico = tecnico_mandante,
    colocacao = colocacao_mandante,
    gols_feitos = gols_mandante,
    gols_sofridos = gols_visitante,
    mandante = 1 # Flag para sabermos que foi em casa
  )

# 2.2 Criar tabela de "Visitantes"
dados_visitante <- dados_limpos %>%
  transmute(
    ano = ano_campeonato,
    rodada,
    time = time_visitante,
    adversario = time_mandante,
    tecnico = tecnico_visitante,
    colocacao = colocacao_visitante,
    gols_feitos = gols_visitante,
    gols_sofridos = gols_mandante,
    mandante = 0 # Flag para sabermos que foi fora
  )

# 2.3 Juntar tudo e ordenar (CRUCIAL!)
dados_por_time <- bind_rows(dados_mandante, dados_visitante) %>%
  arrange(time, ano, rodada) # Ordenar é a chave para a próxima etapa

# 2.4 Criar a coluna 'pontos' que faltava
dados_por_time <- dados_por_time %>%
  mutate(
    pontos = case_when(
      gols_feitos > gols_sofridos ~ 3,
      gols_feitos == gols_sofridos ~ 1,
      TRUE ~ 0
    )
  )

print("Dados empilhados e prontos para análise:")
glimpse(dados_por_time)


#-------------------------------------------------------------------------------------------------------------

# --- ETAPA 3: DETECTAR TROCAS (CORRIGIDO) ---

dados_analise <- dados_por_time %>%
  
  # --- INÍCIO DA CORREÇÃO ---
  # Adicionando a criação da coluna 'pontos' que faltou na Etapa 2
  mutate(
    pontos = case_when(
      gols_feitos > gols_sofridos ~ 3,  # Vitória
      gols_feitos == gols_sofridos ~ 1, # Empate
      TRUE ~ 0                          # Derrota
    )
  ) %>%
  # --- FIM DA CORREÇÃO ---
  
  group_by(time, ano) %>% # Analisar cada time/ano separadamente
  mutate(
    # lag() pega o valor da linha anterior
    tecnico_anterior = lag(tecnico),    
    
    # Se o técnico atual é DIFERENTE do anterior, é uma troca
    troca_meio_temporada = if_else(
      tecnico != tecnico_anterior & !is.na(tecnico_anterior),  
      TRUE,  
      FALSE
    ),
    
    # id_passagem_tecnico: 0 = 1º técnico, 1 = 2º técnico, etc.
    id_passagem_tecnico = cumsum(troca_meio_temporada)
  ) %>%
  ungroup() # Sempre desagrupar!

# Respondendo seu Ponto 2: Qual time mais troca?
# Isso conta o total de "TRUE" na coluna que criamos
trocas_por_time <- dados_analise %>%
  filter(troca_meio_temporada == TRUE) %>%
  count(time, sort = TRUE, name = "total_trocas_2018_2023")

print("Ranking de times que mais trocaram de técnico (2018-2023):")
print(trocas_por_time)

print("--- Etapa 3 Corrigida e Executada! A tabela 'dados_analise' agora tem a coluna 'pontos'. ---")

#-------------------------------------------------------------------------------------------------------------

# --- ETAPA 4: EFEITO DA TROCA ---

# 4.1 Calcular a performance COMPLETA de cada passagem
sumario_passagens <- dados_analise %>%
  group_by(time, ano, id_passagem_tecnico, tecnico) %>%
  summarise(
    ppg_total = mean(pontos, na.rm = TRUE), # Média de Pontos por Jogo
    jogos_total = n() # Quantos jogos ficou
  ) %>%
  ungroup()

# 4.2 Isolar o PPG do técnico ANTERIOR
ppg_anterior <- sumario_passagens %>%
  group_by(time, ano) %>%
  mutate(
    ppg_tecnico_anterior = lag(ppg_total) # Pega o PPG da passagem anterior
  ) %>%
  filter(id_passagem_tecnico > 0) %>% # Filtra só as passagens que SÃO uma troca
  select(time, ano, id_passagem_tecnico, ppg_tecnico_anterior)

# 4.3 Calcular a performance nos 3, 6, 9, 12 jogos do NOVO técnico
# Criamos o "número do jogo" para cada técnico (1º jogo, 2º jogo, etc.)
desempenho_novo_tecnico <- dados_analise %>%
  group_by(time, ano, id_passagem_tecnico) %>%
  mutate(jogo_num = row_number()) %>% # O 1º jogo desse técnico, o 2º, etc.
  summarise(
    # mean(pontos[jogo_num <= X]) calcula a média SÓ dos X primeiros jogos
    ppg_3_jogos = mean(pontos[jogo_num <= 3]),
    ppg_6_jogos = mean(pontos[jogo_num <= 6]),
    ppg_9_jogos = mean(pontos[jogo_num <= 9]),
    ppg_12_jogos = mean(pontos[jogo_num <= 12])
  ) %>%
  filter(id_passagem_tecnico > 0) # Queremos só os técnicos "novos"

# 4.4 JUNTAR TUDO!
analise_curto_prazo <- desempenho_novo_tecnico %>%
  left_join(ppg_anterior, by = c("time", "ano", "id_passagem_tecnico")) %>%
  # Agora temos o ppg NOVO (em 3,6,9,12) e o ppg ANTIGO lado a lado
  mutate(
    melhora_6_jogos = ppg_6_jogos - ppg_tecnico_anterior
  )

print("Análise de Curto Prazo (Novos Técnicos vs. Anteriores):")
glimpse(analise_curto_prazo)


#-------------------------------------------------------------------------------------------------------------


# --- ETAPA 5: ANÁLISE FINAL (CAMPEÕES E REBAIXADOS) ---

# 5.1 Descobrir a posição final de cada time
posicao_final <- dados_analise %>%
  group_by(time, ano) %>%
  filter(rodada == max(rodada)) %>% # Pega só a última rodada do ano
  summarise(posicao_final = first(colocacao)) # first() só pra garantir 1 valor

# 5.2 Descobrir se o time trocou de técnico no ano
times_que_trocaram_no_ano <- dados_analise %>%
  group_by(time, ano) %>%
  summarise(
    trocou_no_ano = any(troca_meio_temporada) # 'any()' = existe algum "TRUE"?
  )

# 5.3 Juntar tudo
analise_final <- posicao_final %>%
  left_join(times_que_trocaram_no_ano, by = c("time", "ano"))

print("Análise de Fim de Temporada (Posição vs. Troca):")
glimpse(analise_final)

#-------------------------------------------------------------------------------------------------------------

# --- ETAPA 6: TROCAS ENTRE TEMPORADAS ---

# 6.1 Pegar o técnico que INICIOU cada temporada
tecnico_inicio_ano <- dados_analise %>%
  filter(rodada == 1) %>%
  select(time, ano, tecnico_inicio = tecnico)

# 6.2 Pegar o técnico que TERMINOU cada temporada
tecnico_fim_ano <- dados_analise %>%
  group_by(time, ano) %>%
  filter(rodada == max(rodada)) %>%
  summarise(tecnico_fim = first(tecnico)) %>%
  ungroup()

# 6.3 Comparar ano atual com o anterior (usando lag)
comparativo_intertemporada <- tecnico_inicio_ano %>%
  left_join(tecnico_fim_ano, by=c("time", "ano")) %>%
  group_by(time) %>%
  arrange(time, ano) %>%
  mutate(
    tecnico_fim_anterior = lag(tecnico_fim)
  ) %>%
  ungroup() %>%
  mutate(
    trocou_entre_temporadas = (tecnico_inicio != tecnico_fim_anterior)
  )

# 6.4 Juntar com a tabela de posição final
analise_final_completa <- analise_final %>%
  left_join(comparativo_intertemporada, by = c("time", "ano"))

# Resposta Ponto 7: % de campeões que trocaram ENTRE temporadas
campeoes_intertemporada <- analise_final_completa %>%
  filter(posicao_final == 1) %>%
  count(trocou_entre_temporadas, name = "total")

print("CAMPEÕES: Trocaram ENTRE temporadas? (TRUE=Sim, FALSE=Não)")
print(campeoes_intertemporada)


#-------------------------------------------------------------------------------------------------------------

# Ponto 6: % de campeões que trocaram
campeoes <- analise_final %>%
  filter(posicao_final == 1) %>%
  count(trocou_no_ano, name = "total")
print("CAMPEÕES: Trocaram no ano? (TRUE=Sim, FALSE=Não)")
print(campeoes)

# Ponto 6: % de rebaixados que trocaram
rebaixados <- analise_final %>%
  filter(posicao_final > 16) %>% # Assumindo 4 rebaixados
  count(trocou_no_ano, name = "total")
print("REBAIXADOS: Trocaram no ano? (TRUE=Sim, FALSE=Não)")
print(rebaixados)

# Ponto 4: Trocar na Z4 (colocação > 16) ajudou a escapar?
# 1. Encontrar times que trocaram ENQUANTO estavam no Z4
trocas_no_z4 <- dados_analise %>%
  filter(troca_meio_temporada == TRUE & colocacao > 16) %>%
  distinct(time, ano) # Pega só o time e ano que isso aconteceu

# 2. Ver a posição final desses times
analise_z4 <- analise_final %>%
  # inner_join() filtra SÓ os times que apareceram na tabela 'trocas_no_z4'
  inner_join(trocas_no_z4, by = c("time", "ano")) %>%
  mutate(
    escapou = if_else(posicao_final <= 16, "Sim", "Não")
  )

print("Times que trocaram no Z4, escaparam?")
print(count(analise_z4, escapou))



