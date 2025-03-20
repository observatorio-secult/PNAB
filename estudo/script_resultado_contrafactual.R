if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(here) == F) install.packages("here"); require(here)
if(require(lubridate) == F) install.packages("lubridate"); require(lubridate)
theme_set(theme_bw()+ theme(panel.grid = element_blank(),
                            legend.position = "top",
                            axis.text = element_text(face = "bold"))) 


setwd(here("estudo"))

pnab <- readRDS("BANCO PNAB.RDS")

resultado <- read_xlsx("PNAB - TOTAL.xlsx") %>% select(EDITAL, CATEGORIA, `Número da inscrição`,
                                                       `NOTA SEM INDUÇÃO\r\n (pós recurso)`, M1...7,
                                                       M2...8, M3...9, M4...10, M5...11, M6...12,
                                                       M7...13) %>% 
  mutate(M1...7 = as.numeric(M1...7),
         M2...8 = as.numeric(M2...8),
         M3...9 = as.numeric(M3...9),
         M4...10 = as.numeric(M4...10),
         M5...11 = as.numeric(M5...11),
         M6...12 = as.numeric(M6...12),
         M7...13 = as.numeric(M7...13))

colnames(resultado) <- c("EDITAL", "CATEGORIA", "id", "NOTA", "M1", "M2", "M3", "M4", "M5", "M6", "M7")

resultado_split <- split(resultado, resultado$CATEGORIA)
pnab_split <- split(pnab, pnab$EDITAL)

# BOLSA ARTISTICA-------

bolsa_artistica <- resultado_split[["BOLSAS ARTÍSTICAS"]]
bolsa_artistica <- bolsa_artistica %>% left_join(pnab_split[["Bolsas Artísticas"]], by = "id") %>% 
  select(-M4, -M5, M6, -M7) # RETIRAR OS CRITERIOS QUE NAO CORRESPONDEM

# RANQUEAMENTO SEM COTA

bolsa_artistica <- bolsa_artistica %>% arrange(-NOTA, -M1, -M2, -M3, -idade, -atuacao) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

bolsa_artistica <- bolsa_artistica %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

bolsa_artistica <- bolsa_artistica %>% 
  mutate(status_estudo = ifelse(NOTA < 9, "Desclassificada",
                                ifelse(categoria == "FAIXA  1- Individual -  R$ 20.000,00" &
                                         rank <= 107, "Selecionada",
                                       ifelse(categoria == "FAIXA   2  - Grupo/Coletivo - R$ 30.000,00" &
                                                rank <= 60, "Selecionada",
                                              "Suplente"))))


bolsa_artistica %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)

    
# GRAFICOS

df_estudo <- bolsa_artistica %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- bolsa_artistica %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, 110)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao bolsa.png")


# raca

df_estudo <- bolsa_artistica %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- bolsa_artistica %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, 110)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial bolsa.png")


# BOLSA BRINCADEIRA-------

bolsa_brincadeira <- resultado_split[["BOLSAS BRINCADEIRAS"]]
bolsa_brincadeira <- bolsa_brincadeira %>% left_join(pnab_split[["Bolsas Brincadeiras"]], by = "id") %>% 
  select(-M5, -M6, -M7) # RETIRAR OS CRITERIOS QUE NAO CORRESPONDEM

# RANQUEAMENTO SEM COTA

bolsa_brincadeira <- bolsa_brincadeira %>% arrange(-NOTA, -M1, -M2, -M3, -atuacao) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

bolsa_brincadeira <- bolsa_brincadeira %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

bolsa_brincadeira <- bolsa_brincadeira %>% 
  mutate(status_estudo = ifelse(NOTA < 12, "Desclassificada",
                                ifelse(categoria == "Faixa 1: R$ 60.000,00" &
                                         rank <= 100, "Selecionada",
                                       "Suplente")))



# GRAFICOS

df_estudo <- bolsa_brincadeira %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- bolsa_brincadeira %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, 55)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao bolsa brincadeira.png")


# raca

df_estudo <- bolsa_brincadeira %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- bolsa_brincadeira %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, 60)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial bolsa brincadeira.png")



# MULTILINGUAGEM-------

multilinguagem <- resultado_split[["MULTILINGUAGEM"]]
multilinguagem <- multilinguagem %>% left_join(pnab_split[["Multilinguagem"]], by = "id") %>% 
  select(-M7) # RETIRAR OS CRITERIOS QUE NAO CORRESPONDEM

# RANQUEAMENTO SEM COTA

multilinguagem <- multilinguagem %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

multilinguagem <- multilinguagem %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

multilinguagem <- multilinguagem %>% 
  mutate(status_estudo = ifelse(NOTA < 21, "Desclassificada",
                                ifelse(categoria == "Faixa 1 - R$ 30.000,00" &
                                         rank <= 100, "Selecionada",
                                       ifelse(categoria == "Faixa 2 - R$ 60.000,00" &
                                                rank <= 65, "Selecionada",
                                              ifelse(categoria == "Faixa 3 - R$ 70.000,00" &
                                                       rank <= 35, "Selecionada",
                                       "Suplente")))))


multilinguagem %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)


# GRAFICOS

df_estudo <- multilinguagem %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- multilinguagem %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, , max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao multilinguagem.png")


# raca

df_estudo <- multilinguagem %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- multilinguagem %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial multilinguagem.png")


# ECONOMIA CRIATIVA-------

ec_criativa <- resultado_split[["ECONOMIA CRIATIVA"]]
ec_criativa <- ec_criativa %>% left_join(pnab_split[["Economia Criativa"]], by = "id") %>% 
  select(-M7) # RETIRAR OS CRITERIOS QUE NAO CORRESPONDEM

# RANQUEAMENTO SEM COTA

ec_criativa <- ec_criativa %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

ec_criativa <- ec_criativa %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

ec_criativa <- ec_criativa %>% 
  mutate(status_estudo = ifelse(NOTA < 21, "Desclassificada",
                                ifelse(categoria == "Aquisição de bens e serviços - Faixa 1 - R$ 30.000,00" &
                                         rank <= 50, "Selecionada",
                                       ifelse(categoria == "Aquisição de bens e serviços - Faixa 2 - R$ 60.000,00" &
                                                rank <= 45, "Selecionada",
                                              ifelse(categoria == "Aquisição de bens e serviços - Faixa 3 - Circos Itinerantes - R$ 40.000,00" &
                                                       rank <= 10, "Selecionada",
                                                     ifelse(categoria == "Feiras e Rodadas de Negócios de Economia Criativa e Solidária - Faixa 1 - R$ 50.000,00" &
                                                              rank <= 10, "Selecionada",
                                                            ifelse(categoria == "Feiras e Rodadas de Negócios de Economia Criativa e Solidária - Faixa 2 - R$ 100.000,00" &
                                                                     rank <= 5, "Selecionada",
                                                     "Suplente")))))))



ec_criativa %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)

# GRAFICOS

df_estudo <- ec_criativa %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- ec_criativa %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao economia criativa.png")


# raca

df_estudo <- ec_criativa %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- ec_criativa %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial economia criativa.png")


# FESTIVAIS-------

fetivais <- resultado_split[["FESTIVAIS"]]
fetivais <- fetivais %>% left_join(pnab_split[["Festivais"]], by = "id") %>% 
  select(-M7) # RETIRAR OS CRITERIOS QUE NAO CORRESPONDEM

# RANQUEAMENTO SEM COTA

fetivais <- fetivais %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

fetivais <- fetivais %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

fetivais <- fetivais %>% 
  mutate(status_estudo = ifelse(NOTA < 21, "Desclassificada",
                                ifelse(categoria == "Faixa 1 - R$ 60.000,00" &
                                         rank <= 30, "Selecionada",
                                       ifelse(categoria == "Faixa 2 - R$  100.000,00" &
                                                rank <= 25, "Selecionada",
                                              ifelse(categoria == "Faixa 3 - R$ 130.000,00" &
                                                       rank <= 5, "Selecionada",
                                                                   "Suplente")))))

fetivais %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)


# GRAFICOS

df_estudo <- fetivais %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- fetivais %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao festivais.png")


# raca

df_estudo <- fetivais %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- fetivais %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial festivais.png")


# MUSEUS-------

museus <- resultado_split[["MUSEUS"]]
museus <- museus %>% left_join(pnab_split[["Museus"]], by = "id")

# RANQUEAMENTO SEM COTA

museus <- museus %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

museus <- museus %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

museus <- museus %>% 
  mutate(status_estudo = ifelse(NOTA < 24, "Desclassificada",
                                ifelse(categoria == "Requalificação e Modernização de Museus" &
                                         rank <= 20, "Selecionada",
                                       ifelse(categoria == "Patrimônio e Memória" &
                                                rank <= 15, "Selecionada",
                                              ifelse(categoria == "Ações de Educação Patrimonial" &
                                                       rank <= 15, "Selecionada",
                                                     ifelse(categoria == "Inventário, catalogação e digitalização" &
                                                              rank <= 10, "Selecionada",
                                                     "Suplente"))))))


museus %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)


# GRAFICOS

df_estudo <- museus %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- museus %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao museus.png")


# raca

df_estudo <- museus %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- museus %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial museus.png")



# FORMACAO-------

formacao <- resultado_split[["FORMAÇÃO"]]
formacao <- formacao %>% left_join(pnab_split[["Formação"]], by = "id")

# RANQUEAMENTO SEM COTA

formacao <- formacao %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -M6, -M7, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

formacao <- formacao %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

formacao <- formacao %>% 
  mutate(status_estudo = ifelse(NOTA < 20, "Desclassificada",
                                ifelse(categoria == "Faixa 01 - Formação em Linguagens Artístico Culturais" &
                                         rank <= 50, "Selecionada",
                                       ifelse(categoria == "Faixa 02 - Formação em Produção Cultural" &
                                                rank <= 50, "Selecionada",
                                              ifelse(categoria == "Faixa 03 - Formação em Direitos Humanos e Diversidade Cultural" &
                                                       rank <= 50, "Selecionada",
                                                     ifelse(categoria == "Faixa 04 - Pesquisa Artístico-cultural" &
                                                              rank <= 70, "Selecionada",
                                                            "Suplente"))))))



formacao %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)

# GRAFICOS

df_estudo <- formacao %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- formacao %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao formacao.png")


# raca

df_estudo <- formacao %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- formacao %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial formacao.png")




# DIVERSIDADE CULTURAL-------

periferia <- resultado_split[["DIVERSIDADE"]]
periferia <- periferia %>% left_join(pnab_split[["Diversidade Cultural"]], by = "id")

# RANQUEAMENTO SEM COTA

periferia <- periferia %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -M6, -M7, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

periferia <- periferia %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

periferia <- periferia %>% 
  mutate(status_estudo = ifelse(NOTA < 20, "Desclassificada",
                                ifelse(categoria == "FAIXA 01: CULTURA NEGRA E PERIFÉRICA" &
                                         rank <= 44, "Selecionada",
                                       ifelse(categoria == "FAIXA 02: MULHERES E GÊNERO" &
                                                rank <= 40, "Selecionada",
                                              ifelse(categoria == "FAIXA 03: INFÂNCIA" &
                                                       rank <= 20, "Selecionada",
                                                     ifelse(categoria == "FAIXA 04: JUVENTUDE" &
                                                              rank <= 20, "Selecionada",
                                                            ifelse(categoria == "FAIXA 05: INICIANTES" &
                                                                     rank <= 30, "Selecionada",
                                                                   ifelse(categoria == "FAIXA 06: CULTURA HIP-HOP" &
                                                                            rank <= 60, "Selecionada",
                                                                          ifelse(categoria == "FAIXA 07: BREGA/BREGA-FUNK/PASSINHO" &
                                                                                   rank <= 30, "Selecionada",
                                                            "Suplente")))))))))



periferia %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)

# GRAFICOS

df_estudo <- periferia %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- periferia %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao periferia.png")


# raca

df_estudo <- periferia %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- periferia %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .9), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial periferia.png")





# TECNICNOS-------

tecnicos <- resultado_split[["TÉCNICOS"]]
tecnicos <- tecnicos %>% left_join(pnab_split[["Técnicos"]], by = "id")

# RANQUEAMENTO SEM COTA

tecnicos <- tecnicos %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

tecnicos <- tecnicos %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

tecnicos <- tecnicos %>% 
  mutate(status_estudo = ifelse(NOTA < 15, "Desclassificada",
                                ifelse(categoria == "FAIXA ÚNICA" &
                                         rank <= 260, "Selecionada",
                                       "Suplente")))



# GRAFICOS

df_estudo <- tecnicos %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- tecnicos %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .6), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao tecnicos.png")


# raca

df_estudo <- tecnicos %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- tecnicos %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .9), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial tecnicos.png")


# SALVAGUARDA-------

salvaguarda <- resultado_split[["SALVAGUARDA"]]
salvaguarda <- salvaguarda %>% left_join(pnab_split[["Salvaguarda"]], by = "id")

# RANQUEAMENTO SEM COTA

salvaguarda <- salvaguarda %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

salvaguarda <- salvaguarda %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

salvaguarda <- salvaguarda %>% 
  mutate(status_estudo = ifelse(NOTA < 18, "Desclassificada",
                                ifelse(categoria == "Agentes Culturais Individuais - Faixa 1- Mestras e Mestras - R$ 10.000,00" &
                                         rank <= 160, "Selecionada",
                                       ifelse(categoria == "Agentes Culturais Individuais - Faixa 2 - Aprendizes - R$ 10.000,00" &
                                                rank <= 100, "Selecionada",
                                              ifelse(categoria == "Agentes Culturais Coletivos - Faixa 1- Grupos e Coletivos - R$ 20.000,00" &
                                                       rank <= 140, "Selecionada",
                                       "Suplente")))))


salvaguarda %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)


# GRAFICOS

df_estudo <- salvaguarda %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- salvaguarda %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao salvaguarda.png")


# raca

df_estudo <- salvaguarda %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- salvaguarda %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .9), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial salvaguarda.png")


# HIPHOP-------

hiphop <- resultado_split[["HIP HOP"]]
hiphop <- hiphop %>% left_join(pnab_split[["Hip Hop"]], by = "id")

# RANQUEAMENTO SEM COTA

hiphop <- hiphop %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

hiphop <- hiphop %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

hiphop <- hiphop %>% 
  mutate(status_estudo = ifelse(NOTA < 15, "Desclassificada",
                                ifelse(categoria == "AGENTES CULTURAIS INDIVIDUAIS" &
                                         rank <= 49, "Selecionada",
                                       ifelse(categoria == "AGENTES CULTURAIS COLETIVOS" &
                                                rank <= 20, "Selecionada",
                                                     "Suplente"))))



hiphop %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)
  

# GRAFICOS

df_estudo <- hiphop %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- hiphop %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao hiphop.png")


# raca

df_estudo <- hiphop %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- hiphop %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .9), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial hiphop.png")












# QUADRILHA-------

quadrilha <- resultado_split[["QUADRILHAS JUNINAS"]]
quadrilha <- quadrilha %>% left_join(pnab_split[["Quadrilhas"]], by = "id")

# RANQUEAMENTO SEM COTA

quadrilha <- quadrilha %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

quadrilha <- quadrilha %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

quadrilha <- quadrilha %>% 
  mutate(status_estudo = ifelse(NOTA < 12, "Desclassificada",
                                ifelse(categoria == "FAIXA ÚNICA" &
                                         rank <= 60, "Selecionada",
                                       "Suplente")))



quadrilha %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)


# GRAFICOS

df_estudo <- quadrilha %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- quadrilha %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao quadrilha.png")


# raca

df_estudo <- quadrilha %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- quadrilha %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .9), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial quadrilha.png")

# COMUNIDADE-------

comunidade <- resultado_split[["PRÁTICAS EXITOSAS"]]
comunidade <- comunidade %>% left_join(pnab_split[["Comunidades"]], by = "id")

# RANQUEAMENTO SEM COTA

comunidade <- comunidade %>% arrange(-NOTA, -M1, -M2, -M3, -M4, -M5, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

comunidade <- comunidade %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

comunidade <- comunidade %>% 
  mutate(status_estudo = ifelse(NOTA < 15, "Desclassificada",
                                ifelse(categoria == "Comunidades Indígenas/Povos Originários" &
                                         rank <= 25, "Selecionada",
                                       ifelse(categoria == "Comunidade Quilombolas" &
                                                rank <= 25, "Selecionada",
                                              ifelse(categoria == "Povos de Terreiro" &
                                                       rank <= 30, "Selecionada",
                                                     ifelse(categoria == "Comunidades Rurais e de Reforma Agrária" &
                                                              rank <= 10, "Selecionada",
                                                            ifelse(categoria == "Pescadores Artesanais" &
                                                                     rank <= 5, "Selecionada",
                                                                   ifelse(categoria == "Outras Comunidades Tradicionais" &
                                                                            rank <= 5, "Selecionada",
                                                                          "Suplente"))))))))



comunidade %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)


comunidade %>% group_by(categoria, status) %>% summarise(n = n()) %>% 
  spread(status, n)

# GRAFICOS

df_estudo <- comunidade %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- comunidade %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao comunidade.png")


# raca

df_estudo <- comunidade %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- comunidade %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .9), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial comunidade.png")


# MULHERES NEGRAS--------

mulher_negra <- resultado_split[["MULHERES NEGRAS"]]
mulher_negra <- mulher_negra %>% left_join(pnab_split[["Mulher Negra"]], by = "id")

# RANQUEAMENTO SEM COTA

mulher_negra <- mulher_negra %>% arrange(-NOTA, -M2, -M1, -M3, -M4, -M5, -idade) # ORDEM DE DESEMPATE, O PRIMEIRO É SEMPRE A NOTA

# criando variavel rank

mulher_negra <- mulher_negra %>% mutate(count = 1) %>% group_by(categoria) %>% 
  mutate(rank = cumsum(count))


# quantidade vagas por faixa

mulher_negra <- mulher_negra %>% 
  mutate(status_estudo = ifelse(NOTA < 15, "Desclassificada",
                                ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos" &
                                         rank <= 20, "Selecionada",
                                       ifelse(categoria == "Mulheres negras com idade entre 30 e 59 anos" &
                                                rank <= 52, "Selecionada",
                                              ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade" &
                                                       rank <= 40, "Selecionada",
                                                                          "Suplente")))))



mulher_negra %>% group_by(categoria, status_estudo) %>% summarise(n = n()) %>% 
  spread(status_estudo, n)




# GRAFICOS

df_estudo <- mulher_negra %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- mulher_negra %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao mulher_negra.png")


# raca

df_estudo <- mulher_negra %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- mulher_negra %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .9), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial mulher_negra.png")


# grafico geral------

pnab_geral_estudo <- rbind(
  bolsa_artistica %>% select(id, status, status_estudo, macrorreg, raca),
  bolsa_brincadeira %>% select(id, status, status_estudo, macrorreg, raca),
  multilinguagem %>% select(id, status, status_estudo, macrorreg, raca),
  ec_criativa %>% select(id, status, status_estudo, macrorreg, raca),
  fetivais %>% select(id, status, status_estudo, macrorreg, raca),
  museus %>% select(id, status, status_estudo, macrorreg, raca),
  formacao %>% select(id, status, status_estudo, macrorreg, raca),
  periferia %>% select(id, status, status_estudo, macrorreg, raca),
  tecnicos %>% select(id, status, status_estudo, macrorreg, raca),
  salvaguarda %>% select(id, status, status_estudo, macrorreg, raca),
  hiphop %>% select(id, status, status_estudo, macrorreg, raca),
  quadrilha %>% select(id, status, status_estudo, macrorreg, raca),
  comunidade %>% select(id, status, status_estudo, macrorreg, raca),
  mulher_negra %>% select(id, status, status_estudo, macrorreg, raca)
)

saveRDS(pnab_geral_estudo, "pnab_contrafactual.RDS")


# GRAFICOS

df_estudo <- pnab_geral_estudo %>% filter(status_estudo == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- pnab_geral_estudo %>% filter(status == "Selecionada") %>% 
  group_by(macrorreg) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .6, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .7), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Macrorregião", y = "Selecionadas")

ggsave("estudo macrorregiao pnab_geral.png")


# raca

df_estudo <- pnab_geral_estudo %>% filter(status_estudo == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))

df <- pnab_geral_estudo %>% filter(status == "Selecionada") %>% 
  group_by(raca) %>% summarise(n = n()) %>% mutate(pond = scales::percent(n/sum(n)))


df_unificado <- rbind(
  df_estudo %>% mutate(TIPO = "Contrafactual"),
  df %>% mutate(TIPO = "Real")
)


ggplot(df_unificado, aes(x = raca, y = n)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = TIPO),
           width = .7, color = "black") +
  geom_text(aes(label = paste0(n, "\n", "(", pond, ")"), group = TIPO),
            position = position_dodge(width = .9), size = 3, vjust = -.3) +
  expand_limits(y = c(0, max(df_unificado$n)+max(df_unificado$n)/4)) +
  labs(x = "Identidade Étnico-Racial", y = "Selecionadas")

ggsave("estudo etnico racial pnab_geral.png")

