library(openxlsx)
library(readxl)
library(tidyverse)
library(scales)
library(here)

lpg_att <- read_xlsx("LPG_DATA.xlsx")
lpg <- readRDS("BANCO_LPG2023.RDS")


lpg_att <- lpg_att %>% left_join(lpg %>% select(id, linguagem), by = c("Número de inscrição" = "id"))


tabela <- lpg_att %>% group_by(EDITAL, FAIXA, STATUS) %>% summarise(n = n(),
                                                                    VALOR = sum(VALOR))

tabela_insc <- tabela %>% select(-VALOR) %>% spread(STATUS, n)

tabela_valor <- tabela %>% select(-n) %>% spread(STATUS, VALOR)

write.xlsx(tabela, "dados para apresentacao.xlsx")


tabela <- lpg_att %>% group_by(linguagem, STATUS) %>% summarise(n = n())

write.xlsx(tabela, "dados para apresentacao linguagem.xlsx")


# DEMANDA DAS LINGUAGENS

linguagem_inscritos <- tabela %>% 
  mutate(linguagem = ifelse(linguagem %in% c("Arquivo", "Arqueologia", "Museu", "Formação"), "Outro Segmento Cultural", 
                            linguagem)) %>% 
  group_by(linguagem) %>% 
  summarise(insc = sum(n)) %>% mutate(prop = percent(insc/sum(insc), accuracy = 0.01))


theme_set(theme_bw())
g <- ggplot(linguagem_inscritos, aes(x = reorder(linguagem, insc), y = insc)) +
  geom_point(size = 3, color = "tomato3") +
  coord_flip() +
  geom_text(aes(label = paste0(insc, " (", prop, ")")), size = 3, hjust = -.1) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 10.5)) +
  labs(x = "Linguagem Cultural", y = "Inscrições", 
       title = "Inscrições por Linguagem (LPG)") +
  expand_limits(y = c(0, 2100))
g

dir.create("linguagem")
setwd(here("linguagem"))

ggsave("inscrcoes x linguagem.png")
write.xlsx(linguagem_inscritos, "inscrcoes x linguagem.xlsx")

# suplentes

linguagem_inscritos <- tabela %>% filter(STATUS == "SUPLENTE") %>% 
  mutate(linguagem = ifelse(linguagem %in% c("Arquivo", "Arqueologia", "Museu", "Formação"), "Outro Segmento Cultural", 
                            linguagem)) %>% 
  group_by(linguagem) %>% 
  summarise(insc = sum(n)) %>% mutate(prop = percent(insc/sum(insc), accuracy = 0.01))


theme_set(theme_bw())
g <- ggplot(linguagem_inscritos, aes(x = reorder(linguagem, insc), y = insc)) +
  geom_point(size = 3, color = "tomato3") +
  coord_flip() +
  geom_text(aes(label = paste0(insc, " (", prop, ")")), size = 3, hjust = -.1) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 10.5)) +
  labs(x = "Linguagem Cultural", y = "Quantidade de Suplentes", 
       title = "Suplentes por Linguagem (LPG)") +
  expand_limits(y = c(0, 1100))
g

# dir.create("linguagem")
setwd(here("linguagem"))

ggsave("suplentes x linguagem.png")
write.xlsx(linguagem_inscritos, "suplentes x linguagem.xlsx")


# Taxa de concorrencia

linguagem_inscritos <- tabela %>%  
  mutate(linguagem = ifelse(linguagem %in% c("Arquivo", "Arqueologia", "Museu", "Formação"), "Outro Segmento Cultural", 
                            linguagem)) %>% 
  group_by(linguagem, STATUS) %>% summarise(n = sum(n)) %>% group_by(linguagem) %>% 
  mutate(total = sum(n)) %>% filter(STATUS == "SELECIONADA") %>% 
  transmute(linguagem, n, total, concorrencia = total/n)


g <- ggplot(linguagem_inscritos, aes(x = reorder(linguagem, concorrencia), y = concorrencia)) +
  geom_point(size = 3, color = "tomato3") +
  coord_flip() +
  geom_text(aes(label = round(concorrencia, 2)), size = 3, hjust = -.5) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 10.5)) +
  labs(x = "Linguagem Cultural", y = "Quantidade de Suplentes", 
       title = "Taxa de Concorrência por Linguagem (LPG)", 
       caption = "Taxa de Concorrência calculada pela razão entre Inscritos e Selecionados. \nO valor é interpretado como sendo x pessoas por vaga") +
  expand_limits(y = c(0, 7.5))
g

ggsave("concorrencia x linguagem.png")
write.xlsx(linguagem_inscritos, "concorrencia x linguagem.xlsx")

# LINGUAGEM DA PROPOSTA

multilinguagem <- lpg_att %>% filter(EDITAL == "FORMACAO" |
                                       EDITAL == "AÇÕES CRIATIVAS")

setwd("C:\\Users\\kibca\\OneDrive\\COMPUTADOR SECULT\\LPG\\ranking\\formacao")

apoio <- read_xlsx("APOIO - RANQUEAMENTO - 08_FOMENTO DE FORMAÇÃO CULTURAL E DIREITOS HUMANOS.xlsx", sheet = "BASE") %>% 
  select(NÚMERO, `5.1. PRINCIPAL ÁREA ARTÍSTICO-CULTURAL DA PROPOSTA -`) %>% 
  rename(linguagem_proposta = 2)

setwd("C:\\Users\\kibca\\OneDrive\\COMPUTADOR SECULT\\LPG\\ranking\\acoes criativas")
apoio2 <- read_xlsx("confirmação Levantamento das Linguagens - Ações Criativas.xlsx") %>% select(NÚMERO, `LINGUAGEM DA PROPOSTA (CONFIRMAÇÃO COORDENAÇÃO)`)
apoio2.1 <- read_xlsx("Linguagem pos recursos - Ações Criativas.xlsx")

apoio2 <- apoio2 %>% left_join(apoio2.1, by = c("NÚMERO" = "INSCRIÇÃO")) %>% 
  transmute(NÚMERO, linguagem_proposta = ifelse(is.na(`LINGUAGEM PÓS RECURSO`) == T, `LINGUAGEM DA PROPOSTA (CONFIRMAÇÃO COORDENAÇÃO)`,
                                                `LINGUAGEM PÓS RECURSO`))


apoio <- rbind(apoio, apoio2)

multilinguagem <- multilinguagem %>% left_join(apoio, by = c("Número de inscrição" = "NÚMERO"))
multilinguagem <- multilinguagem %>% 
  mutate(linguagem_proposta = str_to_lower(linguagem_proposta),
         linguagem_proposta = ifelse(linguagem_proposta %in% c("produção cultural",
                                                               "outro segmento cultural",
                                                               "não declarar",
                                                               "técnico"), "outro segmento cultural", linguagem_proposta))

tabela <- multilinguagem %>% group_by(linguagem_proposta, EDITAL) %>% 
  summarise(insc = n()) %>% group_by(EDITAL) %>% 
  mutate(prop = percent(insc/sum(insc), accuracy = 0.01))


scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


theme_set(theme_bw())

g <- ggplot(tabela %>% 
              filter(EDITAL == "FORMACAO"), 
            aes(x = reorder(linguagem_proposta, insc), y = insc)) +
  geom_point(size = 3, color = "steelblue1") +
  coord_flip() +
  geom_text(aes(label = paste0(insc, " (", prop, ")")), size = 3, hjust = -.1) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 10.5)) +
  labs(x = "Linguagem Cultural", y = "Inscrições", 
       title = "Formação - Inscrições por Linguagem da Proposta") +
  expand_limits(y = c(0, 170))
g

setwd(here("linguagem"))

ggsave("001_2 formacao inscrcoes x linguagem.png")



write.xlsx(tabela, "multilinguagem inscrcoes x linguagem da proposta.xlsx")


# SUPLENTES

tabela <- multilinguagem %>% 
  group_by(linguagem_proposta, EDITAL) %>% filter(STATUS == "SUPLENTE") %>% 
  summarise(insc = n()) %>% group_by(EDITAL) %>% 
  mutate(prop = percent(insc/sum(insc), accuracy = 0.01))


theme_set(theme_bw())
g <- ggplot(tabela %>% 
              filter(EDITAL == "AÇÕES CRIATIVAS"), 
            aes(x = reorder(linguagem_proposta, insc), y = insc)) +
  geom_point(size = 3, color = "tomato3") +
  coord_flip() +
  geom_text(aes(label = paste0(insc, " (", prop, ")")), size = 3, hjust = -.1) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 10.5)) +
  labs(x = "Linguagem Cultural", y = "Inscrições", 
       title = "Ações Criativas - Suplentes por Linguagem da Proposta") +
  expand_limits(y = c(0, 220))
g

setwd(here("linguagem"))

ggsave("002_1 acoes criativas suplentes x linguagem.png")



write.xlsx(tabela, "multilinguagem suplentes x linguagem da proposta.xlsx")


tabela <- multilinguagem %>%  
  group_by(linguagem_proposta, EDITAL, STATUS) %>% 
  summarise(n = n()) %>% group_by(linguagem_proposta, EDITAL) %>% 
  mutate(total = sum(n)) %>% filter(STATUS == "SELECIONADA") %>% 
  transmute(linguagem_proposta, selecionadas = n, total, concorrencia = total/n)


theme_set(theme_bw())
g <- ggplot(tabela %>% 
              filter(EDITAL == "AÇÕES CRIATIVAS"), 
            aes(x = reorder(linguagem_proposta, concorrencia), y = concorrencia)) +
  geom_point(size = 3, color = "tomato3") +
  coord_flip() +
  geom_text(aes(label = format(round(concorrencia, 2), nsmall = 2)), size = 3, hjust = -.5) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 10.5)) +
  labs(x = "Linguagem Cultural", y = "Taxa de Concorrência", 
       title = "Ações Criativas - Concorrência por Linguagem da Proposta") +
  expand_limits(y = c(0, 9))
g

setwd(here("linguagem"))

ggsave("003_1 acoes criativas taxa concorrencia x linguagem.png")


write.xlsx(tabela, "multilinguagem taxa de concorrencia x linguagem da proposta.xlsx")


multilinguagem <- multilinguagem %>% group_by(EDITAL, STATUS, linguagem_proposta) %>% 
  summarise(proposta = n())

write.xlsx(multilinguagem, "base de linguagem das propostas.xlsx")

lpg_linguagem <- lpg_att %>% 
  mutate(linguagem = ifelse(linguagem %in% c("Arquivo", "Arqueologia", "Museu", "Formação"), "Outro Segmento Cultural", 
                            linguagem)) %>% 
group_by(EDITAL, STATUS, linguagem) %>% 
  summarise(proposta = n())

write.xlsx(lpg_linguagem, "base de linguagem dos proponentes.xlsx")


# PARTE DEMANDA BABI

glimpse(lpg_att)


# valor
edital_valor <- lpg_att %>% group_by(EDITAL, FAIXA, STATUS) %>% 
  summarise(VALOR = sum(VALOR)) %>% spread(STATUS, VALOR) %>% 
  left_join(lpg_att %>% group_by(EDITAL, FAIXA, STATUS) %>% 
              summarise(INSC = n()) %>% spread(STATUS, INSC), by = c("EDITAL", "FAIXA"))

write.xlsx(edital_valor, "por faixa.xlsx")

# inscricoes

691

1404  (1404-691)/691
