# OPA

if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(here) == F) install.packages("here"); require(here)
if(require(lubridate) == F) install.packages("lubridate"); require(lubridate)
theme_set(theme_bw()+ theme(panel.grid = element_blank(),
                            legend.position = "top",
                            axis.text = element_text(face = "bold"))) 


setwd(here("EDITAIS"))


# estrutura do banco

# tecnicos <- tecnicos %>% transmute(
#   id = NÚMERO,
#   status = STATUS,
#   natureza = `TIPO DE PROPONENTE`,
#   categoria = `FAIXA/LINHA`,
#   cota = ,
#   inducao = ,
#   nascimento = ,
#   sit_endereco = ,
#   municipio = ,
#   linguagem = ,
#   funcao = ,
#   funcao_outra = ,
#   atuacao = ,
#   mestre = ,
#   genero = ,
#   lgbt = ,
#   raca = ,
#   pcd = ,
#   pcd_tipo = ,
#   escolaridade = ,
#   prog_soc = ,
#   renda = ,
#   recurso_cultura = 
# )


mulher_negra <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - MULHERES NEGRAS")
povos_comunidades <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - POV. COMUN. PRAT. EXIT")
quadrilhas <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - QUADRILHAS JUNINAS")
hiphop <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - HIP HOP")
tecnicos <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - TÉCNICOS")
salvaguarda <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - SALVAGUARDA")


apoio_premios <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "APOIO - BASE GERAL")
colnames(apoio_premios) <- apoio_premios[1,]
apoio_premios <- apoio_premios[-1,]


# tratando o banco
mulher_negra <- mulher_negra %>% transmute(
  id = NÚMERO,
  status = STATUS,
  natureza = `TIPO DE PROPONENTE`,
  categoria = `FAIXA/LINHA`,
  cota = `VOCÊ OPTA POR CONCORRER A RESERVA DE VAGAS PARA:`,
  nascimento = `1.4.	DATA DE NASCIMENTO  DA PROPONENTE -`,
  sit_endereco = `2.1.	EM QUAL DESSAS SITUAÇÕES VOCÊ SE ENQUADRA? -`,
  macrorreg = `2.2.	EM QUAL MACRORREGIÃO DO ESTADO DE PERNAMBUCO VOCÊ RESIDE OU ESTÁ LOCALIZADO? -`,
  municipio = `2.3.	MUNICÍPIO -_2`,
  linguagem = `3.1.	QUAL A PRINCIPAL ÁREA DE ATUAÇÃO NO CAMPO ARTÍSTICO E CULTURAL DA AGENTE CULTURAL? -`,
  funcao = `3.2.	QUAL A PRINCIPAL FUNÇÃO DA AGENTE CULTURAL? -`,
  funcao_outra = `3.2.1. SE OUTRA FUNÇÃO, QUAL? -`,
  atuacao = `3.3.	QUAL A DATA QUE A PROPONENTE INICIOU A ATUAÇÃO NO SETOR CULTURAL? -`,
  genero = `3.4.	COMO VOCÊ SE IDENTIFICA COM RELAÇÃO A SUA IDENTIDADE DE GÊNERO? -`,
  lgbt = `3.5.	A AGENTE CULTURAL É MEMBRO DA COMUNIDADE  LGBTQIAPN+? -`,
  raca = `3.6.	QUAL A IDENTIDADE RACIAL DA PROPONENTE? -`,
  comunidade = `3.7.	A PROPONENTE PERTENCE A ALGUMA COMUNIDADE LISTADA ABAIXO? -`,
  pcd = `3.8.	A PROPONENTE É UMA PESSOA COM DEFICIÊNCIA - PCD? -`,
  pcd_tipo = `3.8.1. SE SIM, QUAL O TIPO DE DEFICIÊNCIA? -`,
  escolaridade = `3.9.	QUAL O GRAU DE ESCOLARIDADE DA PROPONENTE?-`,
  prog_soc = `3.10. A PROPONENTE É BENEFICIÁRIA DE ALGUM PROGRAMA SOCIAL? -`,
  renda = `3.11. QUAL A RENDA INDIVIDUAL DA PROPONENTE? -`,
  recurso_cultura = `3.12. A PROPONENTE ACESSOU RECURSOS PÚBLICOS DO FOMENTO À CULTURA NOS ÚLTIMOS 5 ANOS? -`
)

apoio_mn <- read_xlsx("PLANILHA DE CONTROLE - ANÁLISE DOCUMENTAL - Premiação de Mulheres Negras.xlsx", sheet = "ANÁLISE DOC.")

colnames(apoio_mn) <- apoio_mn[4,]
apoio_mn <- apoio_mn[c(-1:-4),]

apoio_mn <- apoio_mn %>% select(`Nº DE INSCRIÇÃO`, MÃE:`EM SITUAÇÃO DE RUA / VULNERABILIDADE`)
colnames(apoio_mn) <- c("id", "Mãe", "Travesti", "Comunidade Tradicional", "PcD", "Vulnerabilidade Social")

apoio_mn <- apoio_mn %>% gather(key = "inducao", value = "check", Mãe:`Vulnerabilidade Social`) %>% 
  filter(check == "VÁLIDO") %>% group_by(id) %>% summarise(inducao = paste0(inducao, collapse = ";"))


mulher_negra <- mulher_negra %>% left_join(apoio_mn, by = "id")
mulher_negra <- mulher_negra %>% mutate(inducao = ifelse(is.na(inducao) == T, "Ampla Concorrência", 
                                                         inducao))


povos_comunidades <- povos_comunidades %>% transmute(
  id = NÚMERO,
  status = STATUS,
  natureza = `TIPO DE PROPONENTE`,
  categoria = `FAIXA/LINHA`,
  cota = `VOCÊ OPTA POR CONCORRER A RESERVA DE VAGAS PARA:`,
  inducao = `VOCÊ OPTA POR CONCORRER COM QUAL SEGMENTO SOCIAL PARA OBTER CRITÉRIOS DIFERENCIADOS DE PONTUAÇÃO? -`,
  comunidade = `QUAL COMUNIDADE VOCÊ PERTENCE? -`,
  comunidade_outra = `QUAL OUTRA COMUNIDADE TRADICIONAL? -`,
  nascimento = `1.4. DATA DE NASCIMENTO DO AGENTE REPRESENTANTE LEGAL -`,
  sit_endereco = `2.1. EM QUAL DESSAS SITUAÇÕES VOCÊ SE ENQUADRA? -`,
  municipio = `2.3. EM QUAL MUNICÍPIO DO ESTADO DE PERNAMBUCO VOCÊ RESIDE OU ESTÁ LOCALIZADO? -`,
  linguagem = `3.1. QUAL A PRINCIPAL ÁREA DE ATUAÇÃO NO CAMPO ARTÍSTICO E CULTURAL DO AGENTE REPRESENTANTE LEGAL? -`,
  atuacao = `3.2. QUAL A DATA QUE O GRUPO/COLETIVO OU PESSOA JURÍDICA INICIOU A ATUAÇÃO NO SETOR CULTURAL -`,
  mestre = `3.3. O AGENTE REPRESENTANTE LEGAL É: -`,
  genero = `3.4. COMO VOCÊ SE IDENTIFICA COM RELAÇÃO A SUA IDENTIDADE DE GÊNERO? -`,
  lgbt = `3.5. O AGENTE REPRESENTANTE LEGAL  É MEMBRO DA COMUNIDADE  LGBTQIAPN+? -`,
  raca = `3.6. QUAL A IDENTIDADE ÉTNICO-RACIAL DO AGENTE REPRESENTANTE LEGAL? -`,
  pcd = `3.7. O AGENTE REPRESENTANTE LEGAL É UMA PESSOA COM DEFICIÊNCIA - PCD? -`,
  pcd_tipo = `3.7.1. SE SIM, QUAL O TIPO DE DEFICIÊNCIA? -`,
  escolaridade = `3.8. QUAL O GRAU DE ESCOLARIDADE DO AGENTE REPRESENTANTE LEGAL? -`,
  prog_soc = `3.9. O AGENTE REPRESENTANTE LEGAL É BENEFICIÁRIO DE ALGUM PROGRAMA SOCIAL? -`,
  renda = `3.10. QUAL A RENDA INDIVIDUAL DO AGENTE REPRESENTANTE LEGAL? -`,
  recurso_cultura = `3.11. O AGENTE REPRESENTANTE LEGAL ACESSOU RECURSOS PÚBLICOS DO FOMENTO À CULTURA NOS ÚLTIMOS 5 ANOS? -`,
  comunidades_praticas_exitosas = `4.1. QUAIS PRÁTICAS EXITOSAS COLETIVAS DOS TERRITÓRIOS CULTURAIS TRADICIONAIS QUE O GRUPO EXERCE? -`,
  comunidades_praticas_exitosas_outra = `4.2. CASO VOCÊ TENHA RESPONDIDO “OUTRAS FORMAS DE EXPRESSÃO SIMBÓLICA” NA 4.1, ESCREVA QUAL/IS: - (OPCIONAL)`
)

quadrilhas <- quadrilhas %>% transmute(
  id = NÚMERO,
  status = STATUS,
  natureza = `TIPO DE PROPONENTE`,
  categoria = `FAIXA/LINHA`,
  cota = `VOCÊ OPTA POR CONCORRER A RESERVA DE VAGAS PARA:  -`,
  inducao = `VOCÊ OPTA POR CONCORRER COM QUAL SEGMENTO SOCIAL PARA PONTUAÇÃO EXTRA/INDUTOR? -`,
  nascimento = `1.4. DATA DE NASCIMENTO DO AGENTE REPRESENTANTE LEGAL -`,
  sit_endereco = `2.1. EM QUAL DESSAS SITUAÇÕES VOCÊ SE ENQUADRA? -`,
  municipio = `2.3. EM QUAL MUNICÍPIO DO ESTADO DE PERNAMBUCO VOCÊ RESIDE OU ESTÁ LOCALIZADO? -`,
  linguagem = `3.1. QUAL A PRINCIPAL ÁREA DE ATUAÇÃO NO CAMPO ARTÍSTICO E CULTURAL DO AGENTE REPRESENTANTE LEGAL? -`,
  atuacao = `3.2. QUAL A DATA DE ÍNICIO DE ATUAÇÃO? -`,
  atuacao_grupo_char = `4.4. QUAL O TEMPO DE ATUAÇÃO DA QUADRILHA JUNINA? -`,
  mestre = `3.3. O AGENTE REPRESENTANTE LEGAL É:  -`,
  genero = `3.4. COMO VOCÊ SE IDENTIFICA COM RELAÇÃO A SUA IDENTIDADE DE GÊNERO? -`,
  lgbt = `3.5. O AGENTE REPRESENTANTE LEGAL  É MEMBRO DA COMUNIDADE  LGBTQIAPN+? -`,
  raca = `3.6. QUAL A IDENTIDADE ÉTNICO-RACIAL DO AGENTE REPRESENTANTE LEGAL -`,
  pcd = `3.7. O AGENTE REPRESENTANTE LEGAL É UMA PESSOA COM DEFICIÊNCIA - PCD? -`,
  pcd_tipo = `3.7.1. SE SIM, QUAL O TIPO DE DEFICIÊNCIA? -`,
  escolaridade = `3.8. QUAL O GRAU DE ESCOLARIDADE DO AGENTE REPRESENTANTE LEGAL? -`,
  prog_soc = `3.9. O AGENTE REPRESENTANTE LEGAL É BENEFICIÁRIO DE ALGUM PROGRAMA SOCIAL? -`,
  renda = `3.10. QUAL A RENDA INDIVIDUAL DO AGENTE REPRESENTANTE LEGAL? -`,
  recurso_cultura = `3.11.. O AGENTE REPRESENTANTE LEGAL ACESSOU RECURSOS PÚBLICOS DO FOMENTO À CULTURA NOS ÚLTIMOS 5 ANOS? -`
)


hiphop <- hiphop %>% transmute(
  id = NÚMERO,
  status = STATUS,
  natureza = `TIPO DE PROPONENTE`,
  categoria = `FAIXA/LINHA`,
  cota = `VOCÊ OPTA POR CONCORRER A RESERVA DE VAGAS PARA: -`,
  inducao = `VOCÊ OPTA POR CONCORRER COM QUAL SEGMENTO SOCIAL PARA OBTER CRITÉRIOS DIFERENCIADOS DE PONTUAÇÃO? -`,
  nascimento = `1.4.	DATA DE NASCIMENTO DO AGENTE REPRESENTANTE LEGAL -`,
  sit_endereco = `2.1. EM QUAL DESSAS SITUAÇÕES VOCÊ SE ENQUADRA?`,
  municipio = `2.3.	EM QUAL MUNICÍPIO DO ESTADO DE PERNAMBUCO VOCÊ RESIDE OU ESTÁ LOCALIZADO? -`,
  linguagem = `3.1. QUAL A PRINCIPAL ÁREA DE ATUAÇÃO NO CAMPO ARTÍSTICO E CULTURAL DO AGENTE REPRESENTANTE LEGAL? -`,
  funcao = `4.5. QUAL A SUA PRINCIPAL ATIVIDADE/ATUAÇÃO NO HIP HOP? -`,
  atuacao_char = `3.2.	QUAL O TEMPO DE ATUAÇÃO DO AGENTE REPRESENTANTE LEGAL? -`,
  mestre = `3.3.	O AGENTE REPRESENTANTE LEGAL É: -`,
  genero = `3.4.	COMO VOCÊ SE IDENTIFICA COM RELAÇÃO A SUA IDENTIDADE DE GÊNERO? -`,
  lgbt = `3.5. O AGENTE REPRESENTANTE LEGAL  É MEMBRO DA COMUNIDADE  LGBTQIAPN+? -`,
  raca = `3.6.	QUAL A IDENTIDADE ÉTNICO-RACIAL DO AGENTE REPRESENTANTE LEGAL? -`,
  pcd = `3.7.	O AGENTE REPRESENTANTE LEGAL É UMA PESSOA COM DEFICIÊNCIA - PCD? -`,
  pcd_tipo = `3.7.1. SE SIM, QUAL O TIPO DE DEFICIÊNCIA? -`,
  escolaridade = `3.8. QUAL O GRAU DE ESCOLARIDADE DO AGENTE REPRESENTANTE LEGAL? -`,
  prog_soc = `3.9.	O AGENTE REPRESENTANTE LEGAL É BENEFICIÁRIO DE ALGUM PROGRAMA SOCIAL? -`,
  renda = `3.10. QUAL A RENDA INDIVIDUAL DO AGENTE REPRESENTANTE LEGAL? -`,
  recurso_cultura = `3.11. O AGENTE REPRESENTANTE LEGAL ACESSOU RECURSOS PÚBLICOS DO FOMENTO À CULTURA NOS ÚLTIMOS 5 ANOS? -`
)

tecnicos <- tecnicos %>% transmute(
  id = NÚMERO,
  status = STATUS,
  natureza = `TIPO DE PROPONENTE`,
  categoria = "FAIXA ÚNICA",
  cota = `VOCÊ OPTA POR CONCORRER A RESERVA DE VAGAS PARA:`,
  inducao = `VOCÊ OPTA POR CONCORRER COM QUAL SEGMENTO SOCIAL PARA PONTUAÇÃO EXTRA/INDUTOR?`,
  nascimento_char = `1.3. DATA DE NASCIMENTO DO AGENTE`,
  sit_endereco = `2.1. EM QUAL DESSAS SITUAÇÕES VOCÊ SE ENQUADRA?`,
  municipio = `2.3. EM QUAL MUNICÍPIO DO ESTADO DE PERNAMBUCO VOCÊ RESIDE OU ESTÁ LOCALIZADO?`,
  linguagem = `3.1. QUAL A PRINCIPAL ÁREA DE ATUAÇÃO NO CAMPO ARTÍSTICO E CULTURAL DO AGENTE?`,
  funcao = `3.2. QUAL A PRINCIPAL FUNÇÃO/PROFISSÃO NO CAMPO ARTÍSTICO E CULTURAL DO AGENTE?`,
  funcao_outra = `3.3. SE OUTRA FUNÇÃO TÉCNICA NÃO ESPECIFICADA (CULTURA/ARTES), QUAL?`,
  atuacao = `3.4. QUAL A DATA EM QUE A ENTIDADE CULTURAL INICIOU A ATUAÇÃO NO SETOR CULTURAL?`,
  mestre = NA,
  genero = `3.5. COMO VOCÊ SE IDENTIFICA COM RELAÇÃO A SUA IDENTIDADE DE GÊNERO?`,
  lgbt = `3.6. O AGENTE É MEMBRO DA COMUNIDADE  LGBTQIAPN+?`,
  raca = `3.7. QUAL A IDENTIDADE ÉTNICO-RACIAL DO AGENTE?`,
  pcd = `3.8. O AGENTE É UMA PESSOA COM DEFICIÊNCIA - PCD?`,
  pcd_tipo = `3.8.1. SE SIM, QUAL O TIPO DE DEFICIÊNCIA?`,
  escolaridade = `3.9. QUAL O GRAU DE ESCOLARIDADE DO AGENTE?`,
  prog_soc = `3.10. O AGENTE É BENEFICIÁRIO DE ALGUM PROGRAMA SOCIAL?`,
  renda = `3.11. QUAL A RENDA INDIVIDUAL DO AGENTE?`,
  recurso_cultura = `3.12. O AGENTE ACESSOU RECURSOS PÚBLICOS DO FOMENTO À CULTURA NOS ÚLTIMOS 5 ANOS?`
)


salvaguarda <- salvaguarda %>% transmute(
  id = NÚMERO,
  status = STATUS,
  natureza = `TIPO DE PROPONENTE`,
  categoria = `FAIXA/LINHA`,
  cota = `VOCÊ OPTA POR CONCORRER A RESERVA DE VAGAS PARA:`,
  inducao = `VOCÊ OPTA POR CONCORRER COM QUAL SEGMENTO SOCIAL PARA PONTUAÇÃO EXTRA/INDUTOR?`,
  nascimento_char = `1.3. DATA DE NASCIMENTO DO AGENTE REPRESENTANTE LEGAL`,
  sit_endereco = `2.1. EM QUAL DESSAS SITUAÇÕES VOCÊ SE ENQUADRA?`,
  municipio = `2.3. EM QUAL MUNICÍPIO DO ESTADO DE PERNAMBUCO VOCÊ RESIDE OU ESTÁ LOCALIZADO?`,
  linguagem = `3.1. QUAL A PRINCIPAL ÁREA DE ATUAÇÃO NO CAMPO ARTÍSTICO E CULTURAL DO AGENTE REPRESENTANTE LEGAL?`,
  funcao = `3.2. QUAL/IS  EXPRESSÕES/MANIFESTAÇÕES DAS CULTURAS POPULARES DO ESTADO DE PERNAMBUCO VOCÊ SE INSERE?`,
  funcao_outra = NA,
  atuacao = `3.2.1.  QUAL A DATA EM QUE O PROPONENTE INICIOU A ATUAÇÃO NO SETOR CULTURAL?`,
  atuacao_grupo_char = `3.2.1. QUAL A DATA QUE A PESSOA JURÍDICA OU GRUPO INICIOU A ATUAÇÃO NO SETOR CULTURAL? -`,
  mestre = `3.3. O AGENTE REPRESENTANTE LEGAL É:`,
  genero = `3.4. COMO VOCÊ SE IDENTIFICA COM RELAÇÃO A SUA IDENTIDADE DE GÊNERO?`,
  lgbt = `3.5. O AGENTE REPRESENTANTE LEGAL  É MEMBRO DA COMUNIDADE  LGBTQIAPN+?`,
  raca = `3.6. QUAL A IDENTIDADE ÉTNICO-RACIAL DO AGENTE REPRESENTANTE LEGAL`,
  pcd = `3.7. O AGENTE REPRESENTANTE LEGAL É UMA PESSOA COM DEFICIÊNCIA - PCD? -`,
  pcd_tipo = `3.7.1. SE SIM, QUAL O TIPO DE DEFICIÊNCIA?`,
  escolaridade = `3.8. QUAL O GRAU DE ESCOLARIDADE DO AGENTE REPRESENTANTE LEGAL?`,
  prog_soc = `3.9. O AGENTE REPRESENTANTE LEGAL É BENEFICIÁRIO DE ALGUM PROGRAMA SOCIAL?`,
  renda = `3.10. QUAL A RENDA INDIVIDUAL DO AGENTE REPRESENTANTE LEGAL ?`,
  recurso_cultura = `3.11. O AGENTE REPRESENTANTE LEGAL ACESSOU RECURSOS PÚBLICOS DO FOMENTO À CULTURA NOS ÚLTIMOS 5 ANOS?`
)


pnab_premios <- bind_rows(hiphop %>% mutate(EDITAL = "Hip Hop"),
                          mulher_negra %>% mutate(EDITAL = "Mulher Negra"),
                          povos_comunidades %>% mutate(EDITAL = "Comunidades"),
                          quadrilhas %>% mutate(EDITAL = "Quadrilhas"),
                          salvaguarda %>% mutate(EDITAL = "Salvaguarda"),
                          tecnicos %>% mutate(EDITAL = "Técnicos"))



pnab_premios <- pnab_premios %>% 
  left_join(apoio_premios %>% select(INSCRIÇÃO, `MACRORREGIÃO PÓS REENQUADRAMENTO`, 
                                     `COTA PÓS REENQUADRAMENTO`, `INDUTOR PÓS REENQUADRAMENTO`,
                                     `AVALIAÇÃO DOCUMENTAL`), by = c("id" = "INSCRIÇÃO"))



pnab_premios <- pnab_premios %>% mutate(check = tolower(status) == tolower(`AVALIAÇÃO DOCUMENTAL`))


check <- pnab_premios %>% select(id, status, `AVALIAÇÃO DOCUMENTAL`, check)


# atualizando status

pnab_premios <- pnab_premios %>% mutate(status = ifelse(`AVALIAÇÃO DOCUMENTAL` == "SELECIONADA", "Habilitada",
                                                        ifelse(`AVALIAÇÃO DOCUMENTAL` == "NÃO SELECIONADA", "Desclassificada",
                                                               "Inválida")),
                                        `AVALIAÇÃO DOCUMENTAL` = NULL,
                                        inducao = ifelse(EDITAL == "Mulher Negra", inducao,
                                                         `INDUTOR PÓS REENQUADRAMENTO`),
                                        `INDUTOR PÓS REENQUADRAMENTO` = NULL,
                                        cota = `COTA PÓS REENQUADRAMENTO`,
                                        `COTA PÓS REENQUADRAMENTO` = NULL)

pnab_premios <- pnab_premios %>% mutate(inducao = ifelse(inducao == "SOU PESSOA NEGRA", "Pessoa Negra",
                                                         ifelse(inducao == "SOU MULHER (CIS/TRANS) NEGRA OU INDÍGENA OU TRAVESTI NEGRA OU INDÍGENA", "Mulher/Travesti Negra ou Indígena",
                                                                ifelse(inducao == "OPTO POR NÃO CONCORRER COM PONTUAÇÃO EXTRA/INDUTOR", "Não Optante",
                                                                       ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS - INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS", "Povos e Comunidades",
                                                                              ifelse(inducao == "SOU MULHER (CIS/TRANS) OU TRAVESTI", "Mulher/Travesti",
                                                                                     ifelse(inducao == "SOU PESSOA IDOSA (COM A IDADE IGUAL OU SUPERIOR A 60 (SESSENTA) ANOS)", "Pessoa Idosa",
                                                                                            ifelse(inducao == "SOU PESSOA COM DEFICIÊNCIA", "PcD",
                                                                                                   ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO - HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO", "Pessoa Não Cisgênero",
                                                                                                          ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO- HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO)", "Pessoa Não Cisgênero",
                                                                                                                 ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS- INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS.", "Povos e Comunidades",
                                                                                                                        ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS - INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS.", "Povos e Comunidades",
                                                                                                                               ifelse(inducao == "SOU PESSOA EM SITUAÇÃO DE RUA", "Pessoa em Situação de Rua", 
                                                                                                                                      ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO - HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO)", "Pessoa Não Cisgênero",
                                                                                                                                             inducao))))))))))))))

pnab_premios <- pnab_premios %>% mutate(cota = str_to_title(cota),
                                        cota = ifelse(cota == "Pessoa Com Deficiência", "PcD",
                                                      ifelse(cota == "Mulher Negra Com Deficiência", "Mulher Negra com Deficiência",
                                                             cota)))

pnab_premios <- pnab_premios %>% mutate(macrorreg = `MACRORREGIÃO PÓS REENQUADRAMENTO`,
                                        `MACRORREGIÃO PÓS REENQUADRAMENTO` = NULL,
                                        macrorreg = ifelse(macrorreg == "METROPOLITANA DE RECIFE", "RMR",
                                                           ifelse(macrorreg == "SERTÃO PERNAMBUCANO", "Sertão",
                                                                  ifelse(macrorreg == "AGRESTE PERNAMBUCANO", "Agreste",
                                                                         ifelse(macrorreg == "MATA PERNAMBUCANA", "Zona da Mata",
                                                                                "ERRO")))))



# TECS

# multilinguagem <- multilinguagem %>% transmute(
#   id = ,
#   status = ,
#   natureza = ,
#   categoria = ,
#   cota = ,
#   inducao = ,
#   nascimento = ,
#   sit_endereco = ,
#   municipio = ,
#   linguagem = ,
#   funcao = NA,
#   funcao_outra = NA,
#   atuacao = ,
#   mestre = ,
#   genero = ,
#   lgbt = ,
#   raca = ,
#   pcd = ,
#   pcd_tipo = ,
#   escolaridade = ,
#   prog_soc = ,
#   renda = ,
#   faturamento_pj = ,
#   recurso_cultura = ,
#   linguagem_proposta = ,
#   pessoas_contratadas = ,
#   pessoas_envolvidas = ,
#   publico_alvo = 
# )



setwd(here("EDITAIS", "TECs"))

diversidade_cultura <- read_xlsx("Diversidade e Cultura.xlsx")
economia_criativa <- read_xlsx("Economia Criativa .xlsx")
festivais <- read_xlsx("Festivais e Mostras.xlsx")
formacao <- read_xlsx("Formação e Diversidade.xlsx")
multilinguagem <- read_xlsx("Multilinguagens.xlsx")
museus <- read_xlsx("Museus.xlsx")

setwd(here("EDITAIS"))
apoio_tecs <- read_xlsx("Macrorregião pós reenquadramento - Editais de Fomento.xlsx")


diversidade_cultura <- diversidade_cultura %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  categoria = `Faixa/Linha`,
  cota = `Você opta por concorrer a reserva de vagas para: -`,
  inducao = `Você opta por concorrer com qual segmento social para pontuação extra/indutor? -`,
  nascimento = `1.3. Data de nascimento do agente representante legal -`,
  sit_endereco = `2.1. Em qual dessas situações você se enquadra? -`,
  municipio = `2.3. Em qual município do estado de Pernambuco você reside ou está localizado? -`,
  linguagem = `3.1. Qual a principal área de atuação no campo artístico e cultural do agente representante legal? -`,
  funcao = NA,
  funcao_outra = NA,
  atuacao = `3.2. Qual a data de ínicio de atuação? -`,
  mestre = `3.3. O agente representante legal é: -`,
  genero = `3.4. Como você se identifica com relação a sua identidade de gênero? -`,
  lgbt = `3.5. O agente representante legal  é membro da comunidade  LGBTQIAPN+? -`,
  raca = `3.6. Qual a identidade étnico-racial do agente representante legal -`,
  pcd = `3.7. O agente representante legal é uma Pessoa com Deficiência - PcD? -`,
  pcd_tipo = `3.7.1. Se Sim, qual o tipo de deficiência? -`,
  escolaridade = `3.8. Qual o grau de escolaridade do agente representante legal? -`,
  prog_soc = `3.9. O agente representante legal é beneficiário de algum programa social? -`,
  renda = `3.10. Qual a renda individual do agente representante legal? -`,
  recurso_cultura = `3.11. O agente representante legal acessou recursos públicos do fomento à cultura nos últimos 5 anos? -`,
  linguagem_proposta = `4.1. Qual a principal linguagem cultural da sua proposta? -`,
  pessoas_contratadas = `4.16. Quantas pessoas foram contratadas para executar a proposta? -`,
  pessoas_envolvidas = `4.17. Quantas pessoas estarão envolvidas para executar a proposta? -`,
  publico_alvo = `4.10. Público-alvo da Proposta -`,
  municipio_realizacao = `4.11. Local de realização da Proposta -`
)



economia_criativa <- economia_criativa %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  categoria = `Faixa/Linha`,
  cota = `Você opta por concorrer a reserva de vagas para:`,
  inducao = `Você opta por concorrer com qual segmento social para pontuação extra/indutor?`,
  nascimento = `1.3. Data de nascimento do agente representante legal`,
  sit_endereco = `2.1. Em qual dessas situações você se enquadra?`,
  municipio = `2.3. Em qual município do estado de Pernambuco você reside ou está localizado?`,
  linguagem = `3.1. Qual a principal área de atuação no campo artístico e cultural do agente representante legal?`,
  funcao = NA,
  funcao_outra = NA,
  atuacao = `3.2. Qual a data em que a proponente iniciou a atuação no setor cultural?`,
  atuacao_grupo = `3.2.1. Qual a data que a pessoa jurídica ou grupo iniciou a atuação no setor cultural? -`,
  mestre = `3.3. O agente representante legal é:`,
  genero = `3.4. Como você se identifica com relação a sua identidade de gênero?`,
  lgbt = `3.5. O agente representante legal  é membro da comunidade  LGBTQIAPN+?`,
  raca = `3.6. Qual a identidade étnico-racial do agente representante legal`,
  pcd = `3.7. O agente representante legal é uma Pessoa com Deficiência - PcD?`,
  pcd_tipo = `3.7.1. Se Sim, qual o tipo de deficiência?`,
  escolaridade = `3.8. Qual o grau de escolaridade do agente representante legal?`,
  prog_soc = `3.9. O agente representante legal é beneficiário de algum programa social?`,
  renda = `3.10. Qual a renda individual do agente representante legal ?`,
  recurso_cultura = `3.11. O agente representante legal acessou recursos públicos do fomento à cultura nos últimos 5 anos?`,
  faturamento_pj = `3.12. Qual o faturamento da pessoa jurídica ou grupo e coletivo`,
  linguagem_proposta = `4.1. Qual a principal linguagem cultural da sua proposta?`,
  pessoas_contratadas = `4.12. Quantas pessoas foram contratadas para executar a proposta?`,
  pessoas_contratadas2 = `4.15. Quantas pessoas foram contratadas para executar a proposta?`,
  pessoas_envolvidas = `4.13. Quantas pessoas estarão envolvidas para executar a proposta?`,
  pessoas_envolvidas2 = `4.16. Quantas pessoas estarão envolvidas para executar a proposta?`,
  publico_alvo = `4.9. Público-alvo`
) %>% mutate(pessoas_contratadas = ifelse(is.na(pessoas_contratadas) == T, pessoas_contratadas2,
                                          pessoas_contratadas),
             pessoas_envolvidas = ifelse(is.na(pessoas_envolvidas2) == T, pessoas_envolvidas2,
                                         pessoas_envolvidas),
             pessoas_contratadas2 = NULL,
             pessoas_envolvidas2 = NULL)


festivais <- festivais %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  categoria = `Faixa/Linha`,
  cota = `Você opta por concorrer a reserva de vagas para:`,
  inducao = `Você opta por concorrer com qual segmento social para pontuação extra/indutor? -`,
  nascimento = `1.3.	Data de nascimento do agente representante legal -`,
  sit_endereco = `2.1.	Em qual dessas situações você se enquadra? -`,
  municipio = `2.3.	Em qual município do estado de Pernambuco você reside ou está localizado? -`,
  linguagem = `3.1.	Qual a principal área de atuação no campo artístico e cultural do agente representante legal? -`,
  funcao = NA,
  funcao_outra = NA,
  atuacao = `3.2. Qual a data que o agente representante legal iniciou sua atuação no setor cultural? -`,
  atuacao_grupo = `3.2.1. Qual a data que a pessoa jurídica ou grupo iniciou a atuação no setor cultural? -`,
  mestre = `3.3.	O agente representante legal é:`,
  genero = `3.4.	Como você se identifica com relação a sua identidade de gênero? -`,
  lgbt = `3.5.	O agente representante legal  é membro da comunidade  LGBTQIAPN+? -`,
  raca = `3.6.	Qual a identidade étnico-racial do agente representante legal -`,
  pcd = `3.7.	O agente representante legal é uma Pessoa com Deficiência - PcD? -`,
  pcd_tipo = `3.7.1. Se Sim, qual o tipo de deficiência? -`,
  escolaridade = `3.8.	Qual o grau de escolaridade do agente representante legal? -`,
  prog_soc = `3.9.	O agente representante legal é beneficiário de algum programa social? -`,
  renda = `3.10. Qual a renda individual do agente representante legal ? -`,
  faturamento_pj = `3.10.1. Qual o faturamento da pessoa jurídica ou grupo e coletivo? -`,
  recurso_cultura = `3.11. O agente representante legal acessou recursos públicos do fomento à cultura nos últimos 5 anos? -`,
  linguagem_proposta = `4.1. Qual a principal linguagem cultural da sua proposta? -`,
  pessoas_contratadas = as.character(`4.10.3. Quantas pessoas serão contratadas para executar a proposta? -`),
  pessoas_envolvidas = as.character(`4.10.2. Quantas pessoas serão envolvidas para executar a proposta? -`),
  outras_fontes = `4.15. A proposta possui outras fontes de recurso? -`,
  outras_fontes_valor = `4.15.1. Valor total da proposta, contendo outras fontes de recurso -`,
  publico_alvo = `4.5.	Qual o público-alvo da proposta? -`,
  municipio_realizacao = `4.7.	Cidade(s) de realização da proposta -`,
  fest_edicao = `4.8.	Número da edição pleiteada do Festival, Mostra ou Celebração -`
)


formacao <- formacao %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  categoria = `Faixa/Linha`,
  cota = `Você opta por concorrer a reserva de vagas para:`,
  inducao = `Você opta por concorrer com qual segmento social para pontuação extra/indutor? -`,
  nascimento = `1.3.	Data de nascimento do agente representante legal -`,
  sit_endereco = `2.1.	Em qual dessas situações você se enquadra? -`,
  municipio = `2.3.	Em qual município do estado de Pernambuco você reside ou está localizado?  -`,
  linguagem = `3.1.	Qual a principal área de atuação no campo artístico e cultural do agente representante legal? -`,
  funcao = NA,
  funcao_outra = NA,
  atuacao = `3.2.  Qual a data que a pessoa física ou pessoa jurídica ou grupo iniciou a atuação no setor cultural? -`,
  mestre = `3.3.	O agente representante legal é:`,
  genero = `3.4.	Como você se identifica com relação a sua identidade de gênero? -`,
  lgbt = `3.5.	O agente representante legal  é membro da comunidade  LGBTQIAPN+? -`,
  raca = `3.6.	Qual a identidade étnico-racial do agente representante legal -`,
  pcd = `3.7.	O agente representante legal é uma Pessoa com Deficiência - PcD? -`,
  pcd_tipo = `3.7.1. Se Sim, qual o tipo de deficiência? -`,
  escolaridade = `3.8.	Qual o grau de escolaridade do agente representante legal? -`,
  prog_soc = `3.9.	O agente representante legal é beneficiário de algum programa social? -`,
  renda = `3.10. Qual a renda individual do agente representante legal ? -`,
  faturamento_pj = `3.10.1. Qual o faturamento da pessoa jurídica ou grupo e coletivo? -`,
  recurso_cultura = `3.11. O agente representante legal acessou recursos públicos do fomento à cultura nos últimos 5 anos? -`,
  linguagem_proposta = `4.1.	Qual a principal linguagem cultural da sua proposta?  -`,
  pessoas_contratadas = `4.15. Quantas pessoas foram contratadas para executar a proposta? -`,
  pessoas_envolvidas = `4.16. Quantas pessoas estarão envolvidas para executar a proposta? -`,
  publico_alvo = `4.9. Público-alvo da Proposta -`,
  municipio_realizacao = `4.10. Local de realização da Proposta -`
)


multilinguagem <- multilinguagem %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  categoria = `Faixa/Linha`,
  cota = `Você opta por concorrer a reserva de vagas para:`,
  inducao = `Você opta por concorrer com qual segmento social para bonificação ou critérios diferenciados de pontuação?`,
  nascimento = `1.3. Data de nascimento do agente representante legal`,
  sit_endereco = `2.1. Em qual dessas situações você se enquadra?`,
  municipio = `2..3. Em qual município do estado de Pernambuco você reside ou está localizado?`,
  linguagem = `3.1. Qual a principal área de atuação no campo artístico e cultural do agente representante legal?`,
  funcao = NA,
  funcao_outra = NA,
  atuacao_char = `3.2. Qual o tempo de atuação do agente representante legal?`,
  mestre = `3.3. O agente representante legal é:`,
  genero = `3.4. Como você se identifica com relação a sua identidade de gênero?`,
  lgbt = `3.5. O agente representante legal  é membro da comunidade  LGBTQIAPN+?`,
  raca = `3.6. Qual a identidade étnico-racial do agente representante legal`,
  pcd = `3.7. O agente representante legal é uma Pessoa com Deficiência - PcD? -`,
  pcd_tipo = `3.7.1. Se Sim, qual o tipo de deficiência?`,
  escolaridade = `3.8. Qual o grau de escolaridade do agente representante legal?`,
  prog_soc = `3.9. O agente representante legal é beneficiário de algum programa social?`,
  renda = `3.10. Qual a renda individual do agente representante legal ?`,
  faturamento_pj = NA,
  recurso_cultura = `3.11. O agente representante legal acessou recursos públicos do fomento à cultura nos últimos 5 anos?`,
  linguagem_proposta = `4.1. Linguagem principal da proposta`,
  pessoas_contratadas = `4.17. Quantas pessoas serão contratadas para executar a proposta?`,
  pessoas_envolvidas = `4.18. Quantas pessoas serão envolvidas para executar a proposta?`,
  publico_alvo = `4.7. Público-alvo:`,
  outras_fontes = `4.16. A proposta possui outras fontes de recurso?`,
  outras_fontes_valor = `4.16.1. Valor da proposta, contendo outras fontes de recurso:`,
  municipio_realizacao = `4.3. Cidade(s) de realização da proposta:`
)


museus <- museus %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  categoria = `Faixa/Linha`,
  cota = `Você opta por concorrer a reserva de vagas para: -`,
  inducao = `Você opta por concorrer com qual segmento social para pontuação extra/indutor? -`,
  nascimento = `1.3. Data de nascimento do agente representante legal -`,
  sit_endereco = `2.1. Em qual dessas situações você se enquadra? -`,
  municipio = `2.3. Em qual município do estado de Pernambuco você reside ou está localizado? -`,
  linguagem = `3.1. Qual a principal área de atuação no campo artístico e cultural do agente representante legal? -`,
  funcao = NA,
  funcao_outra = NA,
  atuacao_char = `3.2. Qual o tempo de atuação do agente representante legal? -`,
  mestre = `3.3. O agente representante legal é: -`,
  genero = `3.4. Como você se identifica com relação a sua identidade de gênero? -`,
  lgbt = `3.5. O agente representante legal  é membro da comunidade  LGBTQIAPN+? -`,
  raca = `3.6. Qual a identidade étnico-racial do agente representante legal? -`,
  pcd = `3.7. O agente representante legal é uma Pessoa com Deficiência - PcD? -`,
  pcd_tipo = `3.7.1. Se Sim, qual o tipo de deficiência? -`,
  escolaridade = `3.8.	Qual o grau de escolaridade do agente representante legal? -`,
  prog_soc = `3.9. O agente representante legal é beneficiário de algum programa social? -`,
  prog_soc_qual = `3.9.1. Qual? -`,
  renda = `3.10. Qual a renda individual do agente representante legal? -`,
  faturamento_pj = `3.12. Qual o faturamento da pessoa jurídica ou grupo e coletivo? -`,
  recurso_cultura = `3.11. O agente representante legal acessou recursos públicos do fomento à cultura nos últimos 5 anos? -`,
  linguagem_proposta = NA,
  pessoas_contratadas = `4.12. Quantas pessoas foram contratadas para executar a proposta? -`,
  pessoas_envolvidas = `4.13. Quantas pessoas estarão envolvidas para executar a proposta? -`,
  museus_cadastro = `4.11. O agente/museus possui cadastro na plataforma de museus de Pernambuco? -`,
  museus_nome = `1.1.1. Nome fantasia da Pessoa Jurídica ou nome do Grupo/Coletivo -`
)


# unindo os bancos e tratando macrorregiao

head(apoio_tecs)


class(diversidade_cultura$atuacao)
class(economia_criativa$atuacao)
class(festivais$atuacao)
class(formacao$atuacao)
class(multilinguagem$atuacao)
class(museus$atuacao)



pnab_tecs <- bind_rows(diversidade_cultura %>% mutate(EDITAL = "Diversidade Cultural"),
                       economia_criativa %>% mutate(EDITAL = "Economia Criativa"),
                       festivais %>% mutate(EDITAL = "Festivais"),
                       formacao %>% mutate(EDITAL = "Formação"),
                       multilinguagem %>% mutate(EDITAL = "Multilinguagem"),
                       museus %>% mutate(EDITAL = "Museus")) %>% 
  filter(status != "Rascunho")



pnab_tecs <- pnab_tecs %>% 
  left_join(apoio_tecs %>% select(INSCRIÇÃO, `MACRORREGIÃO PÓS REENQUADRAMENTO`), 
            by = c("id" = "INSCRIÇÃO")) %>% 
  rename(macrorreg = `MACRORREGIÃO PÓS REENQUADRAMENTO`)



pnab_tecs <- pnab_tecs %>% mutate(inducao = toupper(inducao),
                                  inducao = ifelse(inducao == "SOU PESSOA NEGRA", "Pessoa Negra",
                                                   ifelse(inducao == "SOU MULHER (CIS/TRANS) NEGRA OU INDÍGENA OU TRAVESTI NEGRA OU INDÍGENA", "Mulher/Travesti Negra ou Indígena",
                                                          ifelse(inducao == "OPTO POR NÃO CONCORRER COM PONTUAÇÃO EXTRA/INDUTOR", "Não Optante",
                                                                 ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS - INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS", "Povos e Comunidades",
                                                                        ifelse(inducao == "SOU MULHER (CIS/TRANS) OU TRAVESTI", "Mulher/Travesti",
                                                                               ifelse(inducao == "SOU PESSOA IDOSA (COM A IDADE IGUAL OU SUPERIOR A 60 (SESSENTA) ANOS)", "Pessoa Idosa",
                                                                                      ifelse(inducao == "SOU PESSOA COM DEFICIÊNCIA", "PcD",
                                                                                             ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO - HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO", "Pessoa Não Cisgênero",
                                                                                                    ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO- HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO)", "Pessoa Não Cisgênero",
                                                                                                           ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS- INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS.", "Povos e Comunidades",
                                                                                                                  ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS - INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS.", "Povos e Comunidades",
                                                                                                                         ifelse(inducao == "SOU PESSOA EM SITUAÇÃO DE RUA", "Pessoa em Situação de Rua", 
                                                                                                                                ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO - HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO)", "Pessoa Não Cisgênero",
                                                                                                                                       ifelse(inducao == "SOU DE POVOS E COMUNIDADES TRADICIONAIS- INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS.", "Povos e Comunidades",
                                                                                                                                              ifelse(inducao == "SOU DE POVOS E COMUNIDADES TRADICIONAIS - INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS", "Povos e Comunidades",
                                                                                                                                                     ifelse(inducao == "OPTO POR NÃO CONCORRER A APLICAÇÃO DE BONIFICAÇÃO OU CRITÉRIOS DIFERENCIADOS DE PONTUAÇÃO", "Não Optante",
                                                                                                                                                            inducao)))))))))))))))))

pnab_tecs <- pnab_tecs %>% mutate(cota = str_to_title(cota),
                                        cota = ifelse(cota == "Pessoa Com Deficiência", "PcD",
                                                      cota))

pnab_tecs <- pnab_tecs %>% mutate(macrorreg = ifelse(macrorreg == "METROPOLITANA DE RECIFE", "RMR",
                                                           ifelse(macrorreg == "SERTÃO PERNAMBUCANO", "Sertão",
                                                                  ifelse(macrorreg == "AGRESTE PERNAMBUCANO", "Agreste",
                                                                         ifelse(macrorreg == "MATA PERNAMBUCANA", "Zona da Mata",
                                                                                "ERRO")))))


# bolsas

setwd(here("EDITAIS"))
bolsas_artisticas <- read_xlsx("Bolsas.xlsx", sheet = "Bolsas Artísticas")
bolsas_brincadeiras <- read_xlsx("Bolsas.xlsx", sheet = "Bolsas Brincadeiras")

apoio_bolsas <- read_xlsx("Macrorregião pós reenquadramento - Editais de Prêmios e Bolsas PNAB.xlsx")

bolsas_artisticas <- bolsas_artisticas %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  categoria = `Faixa/Linha`,
  cota = `Você opta por concorrer a reserva de vagas para: -`,
  inducao = `Você opta por concorrer com qual segmento social para pontuação extra/indutor? -`,
  bolsa_titulo = `Nome do projeto`,
  nascimento = `1.4. Data de nascimento do agente ou representante legal -`,
  sit_endereco = `2.1. Em qual dessas situações você se enquadra? -`,
  municipio = `2.3. Em qual município do estado de Pernambuco você reside ou está localizado? -`,
  linguagem = `3.1. Qual a principal área de atuação no campo artístico e cultural do agente ou representante legal? -`,
  funcao = NA,
  funcao_outra = NA,
  atuacao = `3.2. Qual a data de ínicio de atuação do agente representante legal? -`,
  mestre = `3.3. O agente ou representante legal é: -`,
  genero = `3.4. Como você se identifica com relação a sua identidade de gênero? -`,
  lgbt = `3.5. O agente ou representante legal  é membro da comunidade  LGBTQIAPN+? -`,
  raca = `3.6. Qual a identidade étnico-racial do agente representante legal -`,
  pcd = `3.7. O agente ou representante legal é uma Pessoa com Deficiência - PcD? -`,
  pcd_tipo = `3.7.1. Se Sim, qual o tipo de deficiência? -`,
  escolaridade = `3.8. Qual o grau de escolaridade do agente representante legal? -`,
  prog_soc = `3.9. O agente ou representante legal é beneficiário de algum programa social? -`,
  renda = `3.10. Qual a renda individual do agente ou representante legal? -`,
  faturamento_pj = NA,
  recurso_cultura = `3.11. O agente representante legal acessou recursos públicos do fomento à cultura nos últimos 5 anos? -`,
  linguagem_proposta = `4.5. Qual a principal linguagem cultural da sua proposta? -`
)



bolsas_brincadeiras <- bolsas_brincadeiras %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  categoria = `Faixa/Linha`,
  cota = `Você opta por concorrer a reserva de vagas para: -`,
  inducao = `Você opta por concorrer com qual segmento social para pontuação extra/indutor? -`,
  bolsa_titulo = `Nome do projeto`,
  nascimento = `1.4. Data de nascimento do agente representante legal -`,
  sit_endereco = `2.1. Em qual dessas situações você se enquadra? -`,
  municipio = `2.3. Em qual município do estado de Pernambuco você reside ou está localizado? -`,
  linguagem = `3.1. Qual a principal área de atuação no campo artístico e cultural do agente representante legal? -`,
  funcao = `4.7. Qual/is  Expressões/Manifestações das Culturas Populares do estado de Pernambuco você se insere? -`,
  funcao_outra = NA,
  atuacao_char = `3.2. Qual a data que a entidade iniciou a atuação cultural?  -`,
  mestre = `3.3. O agente representante legal é: -`,
  genero = `3.4. Como você se identifica com relação a sua identidade de gênero? -`,
  lgbt = `3.5. O agente representante legal  é membro da comunidade  LGBTQIAPN+? -`,
  raca = `3.6. Qual a identidade étnico-racial do agente representante legal -`,
  pcd = `3.7. O agente representante legal é uma Pessoa com Deficiência - PcD? -`,
  pcd_tipo = `3.7.1. Se Sim, qual o tipo de deficiência? -`,
  escolaridade = `3.8. Qual o grau de escolaridade do agente representante legal? -`,
  prog_soc = `3.9. O agente representante legal é beneficiário de algum programa social? -`,
  renda = `3.10. Qual a renda individual do agente representante lega? -`,
  faturamento_pj = `3.12. Qual o faturamento da pessoa jurídica ou grupo e coletivo? -`,
  recurso_cultura = `3.11. O agente representante legal acessou recursos públicos do fomento à cultura nos últimos 5 anos? -`
)


class(bolsas_artisticas$atuacao)
class(bolsas_brincadeiras$atuacao)

pnab_bolsas <- bind_rows(bolsas_artisticas %>% mutate(EDITAL = "Bolsas Artísticas"),
                         bolsas_brincadeiras %>% mutate(EDITAL = "Bolsas Brincadeiras")) %>% 
  filter(status != "Rascunho")



pnab_bolsas <- pnab_bolsas %>% 
  left_join(apoio_bolsas %>% select(INSCRIÇÃO, `MACRORREGIÃO PÓS REENQUADRAMENTO`), 
            by = c("id" = "INSCRIÇÃO")) %>% 
  rename(macrorreg = `MACRORREGIÃO PÓS REENQUADRAMENTO`)



pnab_bolsas <- pnab_bolsas %>% mutate(inducao = toupper(inducao),
                                  inducao = ifelse(inducao == "SOU PESSOA NEGRA", "Pessoa Negra",
                                                   ifelse(inducao == "SOU MULHER (CIS/TRANS) NEGRA OU INDÍGENA OU TRAVESTI NEGRA OU INDÍGENA", "Mulher/Travesti Negra ou Indígena",
                                                          ifelse(inducao == "OPTO POR NÃO CONCORRER COM PONTUAÇÃO EXTRA/INDUTOR", "Não Optante",
                                                                 ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS - INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS", "Povos e Comunidades",
                                                                        ifelse(inducao == "SOU MULHER (CIS/TRANS) OU TRAVESTI", "Mulher/Travesti",
                                                                               ifelse(inducao == "SOU PESSOA IDOSA (COM A IDADE IGUAL OU SUPERIOR A 60 (SESSENTA) ANOS)", "Pessoa Idosa",
                                                                                      ifelse(inducao == "SOU PESSOA COM DEFICIÊNCIA", "PcD",
                                                                                             ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO - HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO", "Pessoa Não Cisgênero",
                                                                                                    ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO- HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO)", "Pessoa Não Cisgênero",
                                                                                                           ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS- INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS.", "Povos e Comunidades",
                                                                                                                  ifelse(inducao == "SOU POVOS E COMUNIDADES TRADICIONAIS - INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS.", "Povos e Comunidades",
                                                                                                                         ifelse(inducao == "SOU PESSOA EM SITUAÇÃO DE RUA", "Pessoa em Situação de Rua", 
                                                                                                                                ifelse(inducao == "SOU PESSOA NÃO CISGÊNERO, TAIS COMO - HOMEM TRANS, TRANSMASCULINO, NÃO BINÁRIA, QUEER, PESSOA SEM IDENTIDADE DE GÊNERO (AGENERIDADE) OU COM CONDIÇÃO ESPECÍFICA (INTERSEXO)", "Pessoa Não Cisgênero",
                                                                                                                                       ifelse(inducao == "SOU DE POVOS E COMUNIDADES TRADICIONAIS- INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS.", "Povos e Comunidades",
                                                                                                                                              ifelse(inducao == "SOU DE POVOS E COMUNIDADES TRADICIONAIS - INDÍGENAS/POVOS ORIGINÁRIOS, QUILOMBOLAS, DE TERREIRO, RURAIS E DE REFORMA AGRÁRIA, RIBEIRINHAS, PESCADORES ARTESANAIS, CIGANOS, EXTRATIVISTAS, E OUTRAS COMUNIDADES TRADICIONAIS", "Povos e Comunidades",
                                                                                                                                                     ifelse(inducao == "OPTO POR NÃO CONCORRER A APLICAÇÃO DE BONIFICAÇÃO OU CRITÉRIOS DIFERENCIADOS DE PONTUAÇÃO", "Não Optante",
                                                                                                                                                            inducao)))))))))))))))))

pnab_bolsas <- pnab_bolsas %>% mutate(cota = str_to_title(cota),
                                  cota = ifelse(cota == "Pessoa Com Deficiência", "PcD",
                                                cota))

pnab_bolsas <- pnab_bolsas %>% mutate(macrorreg = ifelse(macrorreg == "METROPOLITANA DE RECIFE", "RMR",
                                                     ifelse(macrorreg == "SERTÃO PERNAMBUCANO", "Sertão",
                                                            ifelse(macrorreg == "AGRESTE PERNAMBUCANO", "Agreste",
                                                                   ifelse(macrorreg == "MATA PERNAMBUCANA", "Zona da Mata",
                                                                          "ERRO")))))


# juntando todos os bancos e realizando os tratamentos adequados

class(pnab_premios$atuacao_grupo)
class(pnab_tecs$atuacao_grupo)
class(pnab_bolsas$atuacao_grupo)



pnab <- bind_rows(pnab_premios %>% mutate(EDITAL_TIPO = "Prêmios"),
                  pnab_tecs %>% mutate(EDITAL_TIPO = "TECs"),
                  pnab_bolsas %>% mutate(EDITAL_TIPO = "Bolsas"))


# tratando a variavel atuacao que ora é texto, ora é data

# VARIAVEIS QUE ESTAO COMO DATA

a <- as.Date("2024-12-31")

pnab <- pnab %>% mutate(atuacao = round(time_length(difftime(a, pnab$atuacao), unit = "years"),0))

# VARIAVEIS COMO TEXTO APARECEM COM STRING DE DATA SEPARADO POR - OU NO FORMATO X ANOS

pnab <- pnab %>% mutate(
  atuacao_char2 = ifelse(str_detect(atuacao_char, "-") == T, atuacao_char, NA),
  atuacao_char = ifelse(str_detect(atuacao_char, "-") == F, atuacao_char, NA)
)

pnab <- pnab %>% mutate(atuacao_char2 = as.Date(atuacao_char2),
                        atuacao_char2 = round(time_length(difftime(a, pnab$atuacao_char2), unit = "years"),0))


pnab <- pnab %>% mutate(atuacao_char = as.numeric(gsub(" anos| ano|Menos de |Mais de ", "", atuacao_char)))


# UNIFICANDO TUDO

pnab <- pnab %>% mutate(atuacao_char = ifelse(is.na(atuacao_char) == T, atuacao_char2,
                                              atuacao_char),
                        atuacao = ifelse(is.na(atuacao) == T, atuacao_char,
                                         atuacao),
                        atuacao_char = NULL,
                        atuacao_char2 = NULL)

# MESMA COISA PARA ATUACAO_GRUPO

pnab <- pnab %>% mutate(atuacao_grupo = round(time_length(difftime(a, pnab$atuacao_grupo), unit = "years"),0))

pnab <- pnab %>% mutate(
  atuacao_grupo_char2 = ifelse(str_detect(atuacao_grupo_char, "an") == T, atuacao_grupo_char, NA),
  atuacao_grupo_char3 = ifelse(str_detect(atuacao_grupo_char, "/") == T, atuacao_grupo_char, NA),
  atuacao_grupo_char4 = ifelse(str_detect(atuacao_grupo_char, "/|an") == F, atuacao_grupo_char, NA)
)

pnab <- pnab %>% mutate(atuacao_grupo_char2 = as.numeric(gsub(" anos| ano|Menos de |Mais de ", "", atuacao_grupo_char2)))
pnab <- pnab %>% mutate(atuacao_grupo_char3 = as.numeric(gsub(".*/", "", atuacao_grupo_char3)))
pnab <- pnab %>% mutate(atuacao_grupo_char3 = 2024 - atuacao_grupo_char3)
pnab <- pnab %>% mutate(atuacao_grupo_char4 = as.Date("1900-1-1") + as.numeric(atuacao_grupo_char4))

pnab <- pnab %>% mutate(atuacao_grupo_char4 = round(time_length(difftime(a, pnab$atuacao_grupo_char4), unit = "years"),0))


# UNIFICANDO TUDO

pnab <- pnab %>% mutate(atuacao_grupo = ifelse(is.na(atuacao_grupo) == T, atuacao_grupo_char2,
                                               atuacao_grupo),
                        atuacao_grupo = ifelse(is.na(atuacao_grupo) == T, atuacao_grupo_char3,
                                               atuacao_grupo),
                        atuacao_grupo = ifelse(is.na(atuacao_grupo) == T, atuacao_grupo_char4,
                                               atuacao_grupo))

pnab <- pnab %>% mutate(atuacao_grupo_char = NULL,
                        atuacao_grupo_char2 = NULL,
                        atuacao_grupo_char3 = NULL,
                        atuacao_grupo_char4 = NULL)

# MESMA COISA PARA NASCIMENTO

pnab <- pnab %>% mutate(idade1 = round(time_length(difftime(a, pnab$nascimento), unit = "years"),0))

pnab <- pnab %>% mutate(
  nascimento1 = ifelse(str_detect(nascimento_char, "-") == T, nascimento_char, NA),
  nascimento2 = ifelse(str_detect(nascimento_char, "an") == T, nascimento_char, NA),
  nascimento3 = ifelse(str_detect(nascimento_char, "/") == T, nascimento_char, NA),
  nascimento4 = ifelse(str_detect(nascimento_char, "/|an") == F, nascimento_char, NA)
)

pnab <- pnab %>% mutate(nascimento1 = round(time_length(difftime(a, as.Date(pnab$nascimento1)), unit = "years"),0))
pnab <- pnab %>% mutate(nascimento2 = as.numeric(gsub(" anos| ano|Menos de |Mais de ", "", nascimento2)))
pnab <- pnab %>% mutate(nascimento3 = as.numeric(gsub(".*/", "", nascimento3)))
pnab <- pnab %>% mutate(nascimento3 = 2024 - nascimento3)
pnab <- pnab %>% mutate(nascimento4 = as.Date("1900-1-1") + as.numeric(nascimento4))

pnab <- pnab %>% mutate(nascimento4 = round(time_length(difftime(a, pnab$nascimento4), unit = "years"),0))


# UNIFICANDO TUDO

pnab <- pnab %>% mutate(idade = idade1,
                        idade = ifelse(is.na(idade) == T, nascimento1,
                                       idade),
                        idade = ifelse(is.na(idade) == T, nascimento2,
                                       idade),
                        idade = ifelse(is.na(idade) == T, nascimento3,
                                       idade),
                        idade = ifelse(is.na(idade) == T, nascimento4,
                                       idade))

pnab <- pnab %>% mutate(idade1 = NULL,
                        nascimento = NULL,
                        nascimento_char = NULL,
                        nascimento1 = NULL,
                        nascimento2 = NULL,
                        nascimento3 = NULL,
                        nascimento4 = NULL)



sort(names(pnab))

pnab$atuacao_grupo %>% na.omit() %>% head()
pnab$atuacao_grupo_char %>% na.omit() %>% head()
unique(pnab$nascimento)
unique(pnab$nascimento_char)
unique(pnab$atuacao_grupo_char4)


# arrumando ordem das colunas

pnab <- pnab %>% select(
  id,
  status,
  EDITAL_TIPO,
  EDITAL,
  categoria,
  natureza,
  cota,
  inducao,
  idade,
  mestre,
  linguagem,
  linguagem_proposta,
  funcao,
  funcao_outra,
  atuacao,
  atuacao_grupo,
  comunidade,
  comunidade_outra,
  genero,
  raca,
  lgbt,
  escolaridade,
  pcd,
  pcd_tipo,
  prog_soc,
  prog_soc_qual,
  recurso_cultura,
  renda,
  faturamento_pj,
  sit_endereco,
  municipio,
  municipio_realizacao,
  macrorreg,
  publico_alvo,
  outras_fontes,
  outras_fontes_valor,
  pessoas_contratadas,
  pessoas_envolvidas,
  comunidades_praticas_exitosas,
  comunidades_praticas_exitosas_outra,
  bolsa_titulo,
  fest_edicao,
  museus_cadastro,
  museus_nome
)

# Homogeneizando o status e outras variaveis

pnab <- pnab %>% mutate(status = ifelse(status %in% c("Inválida", "Desclassificada", "Não Selecionada"), "Desclassificada",
                                        "Habilitada"))

setwd(here())

unique(pnab$sit_endereco)

pnab <- pnab %>% mutate(genero = ifelse(genero %in% c("Mulher cisgênero", "Mulher cisgênera"), "Mulher Cis",
                                        ifelse(genero %in% c("Homem cisgênero"), "Homem Cis",
                                               ifelse(genero %in% c("Mulher transgênero", "Mulher transgênera"), "Mulher Trans",
                                                      ifelse(genero == "Homem transgênero", "Homem Trans",
                                                             genero)))))

pnab <- pnab %>% mutate(raca = ifelse(raca %in% c("Negra (parda)"), "Parda",
                                        ifelse(raca %in% c("Negra (preta)"), "Preta",
                                               raca)))

pnab <- pnab %>% mutate(sit_endereco = ifelse(sit_endereco %in% c("Tenho endereço fixo, mas não tenho comprovante em meu nome", 
                                                            "Tenho endereço fixo em meu nome",
                                                            "Tenho endereço fixo com comprovante de endereço em meu nome",
                                                            "Tenho endereço fixo"), "Endereço Fixo",
                                        ifelse(sit_endereco %in% c("Estou em situação de itinerância",
                                                                   "Não possuo endereço fixo (população ribeirinha, itinerantes ou nômades)"), "Situação de Itinerância",
                                               ifelse(sit_endereco %in% c("Estou em situação de rua"), "Situação de Rua", "Erro"))))


# saveRDS(pnab, "BANCO PNAB.RDS")

pnab <- readRDS("BANCO PNAB.RDS")

a <- pnab %>% group_by(EDITAL, macrorreg) %>% summarise(n = n()) %>% 
  spread(macrorreg, n)

write.xlsx(a, "edital pnab x reg.xlsx")

a <- pnab %>% group_by(EDITAL, raca) %>% summarise(n = n()) %>% 
  spread(raca, n)

a[is.na(a)] <- 0

write.xlsx(a, "edital pnab x raca.xlsx")


a <- pnab %>% group_by(linguagem) %>% summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

write.xlsx(a, "edital pnab x linguagem.xlsx")



# ATUALIZANDO STATUS POS RECURSO-------


setwd(here())
pnab <- readRDS("BANCO PNAB.RDS")

setwd(here("RESULTADO"))

hiphop <- read_xlsx("EXPRESSÃO CULTURAL DO HIP HOP - oportunidade-2454--inscricoes-2024-12-09 10_30_00.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)
mulher_negra <- read_xlsx("MULHERES NEGRAS - oportunidade-2448--inscricoes-2024-12-09 10_30_20.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)
povos_comunidades <- read_xlsx("PRÁTICAS EXITOSAS - oportunidade-2453--inscricoes-2024-12-09 10_31_30.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)
quadrilhas <- read_xlsx("QUADRILHAS JUNINAS - oportunidade-2452--inscricoes-2024-12-09 10_30_19.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)
salvaguarda <- read_xlsx("SALVAGUARDA DAS CULTURAS POPULARES - oportunidade-2450--inscricoes-2024-12-09 10_29_36.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)
tecnicos <- read_xlsx("TÉCNICOS E TÉCNICAS DA CULTURA E DAS ARTES - oportunidade-2455--inscricoes-2024-12-09 10_28_12.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)

bolsas_artisticas <- read_xlsx("BOLSAS ARTÍSTICAS - oportunidade-2449--inscricoes-2024-12-09 10_32_56.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)

bolsas_brincadeiras <- read_xlsx("BOLSAS BRINCADEIRAS - oportunidade-2451--inscricoes-2024-12-09 10_49_06.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)



setwd(here("RESULTADO", "tecs"))


tecs <- read_xlsx("BASE FOMENTO - PÓS RECURSOS (MÉRITO).xlsx")


# premios e bolsas

pnabpb <- pnab %>% filter(EDITAL_TIPO != "TECs") %>% left_join(rbind(
  hiphop,
  mulher_negra,
  povos_comunidades,
  quadrilhas,
  salvaguarda,
  tecnicos,
  bolsas_artisticas,
  bolsas_brincadeiras
), by = "id")


pnabpb <- pnabpb %>% mutate(status2 = ifelse(is.na(status2) == T, "Desclassificada", 
                                           ifelse(status2 == "Não selecionada", "Desclassificada",
                                                  status2)),
                          status = status2,
                          status2 = NULL)


pnabtecs <- pnab %>% filter(EDITAL_TIPO == "TECs") %>% 
  left_join(tecs %>% 
              transmute(id = INSCRIÇÃO,
                        macrorreg_new = `MACRORREGIÃO PÓS REENQUADRAMENTO`,
                        cota_new = `COTA PÓS RECURSOS`,
                        indutor_new = `INDUTOR PÓS RECURSOS`,
                        status2 = `STAUTS DA AVALIAÇÃO DE MÉRITO - PÓS RECURSOS`), 
            by = "id") %>% 
  mutate(macrorreg_new = ifelse(macrorreg_new == "SERTÃO PERNAMBUCANO", "Sertão",
                                ifelse(macrorreg_new == "AGRESTE PERNAMBUCANO", "Agreste",
                                       ifelse(macrorreg_new == "METROPOLITANA DE RECIFE", "RMR",
                                              "Zona da Mata"))),
         macrorreg = ifelse(is.na(macrorreg_new) == T, macrorreg, macrorreg_new),
         macrorreg_new = NULL,
         cota_new = str_to_title(cota_new),
         cota_new = ifelse(cota_new == "Pessoa Com Deficiência", "PcD",cota_new),
         cota = ifelse(is.na(cota_new) == T, cota, cota_new),
         cota_new = NULL,
         indutor_new = ifelse(str_detect(indutor_new, regex(pattern = "idosa", ignore_case = T)) == T, "Pessoa Idosa",
                              ifelse(str_detect(indutor_new, regex(pattern = "não cisgênero", ignore_case = T)) == T, "Pessoa Não Cisgênero",
                                     ifelse(str_detect(indutor_new, regex(pattern = "deficiência", ignore_case = T)) == T, "PcD",
                                            ifelse(str_detect(indutor_new, regex(pattern = "Povos", ignore_case = T)) == T, "Povos e Comunidades",
                                                   ifelse(str_detect(indutor_new, regex(pattern = "rua", ignore_case = T)) == T, "Pessoa em Situação de Rua",
                                                          ifelse(str_detect(indutor_new, regex(pattern = "NÃO concorrer", ignore_case = T)) == T, "Não Optante",
                                                                 ifelse(str_detect(indutor_new, regex(pattern = "Mulher \\(cis/trans\\) Negra", ignore_case = T)) == T, "Mulher/Travesti Negra ou Indígena",
                                                                        ifelse(str_detect(indutor_new, regex(pattern = "Pessoa Negra", ignore_case = T)) == T, "Pessoa Negra",
                                                                               ifelse(str_detect(indutor_new, regex(pattern = "Mulher \\(cis/trans\\) ou Travesti", ignore_case = T)) == T, "Mulher/Travesti",
                                                                                      "ERRO"))))))))),
         inducao = indutor_new,
         indutor_new = NULL,
         status2 = ifelse(status2 == "-" | status2 == "DESCLASSIFICADA", "Desclassificada", status2),
         status = str_to_title(status2),
         status2 = NULL)

table(pnabtecs$status)

# unindo os bancos e salvando arquivo

pnab <- rbind(pnabpb, pnabtecs)

setwd(here())
# saveRDS(pnab, "BANCO PNAB.RDS")


# agregando pontos e pontoes

setwd(here())

pnab <- readRDS("BANCO PNAB.RDS")

setwd(here("RESULTADO", "pontos"))

pontos <- read_xlsx("Cultura Viva V2.xlsx", sheet = "PONTOS DE CULTURA")
pontoes <- read_xlsx("Cultura Viva V2.xlsx", sheet = "PONTÕES DE CULTURA")


# tratando os bancos

sort(names(pontos))


pontos <- pontos %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  cota = `1.1. Em relação às cotas reservadas às pessoas negras, indígenas ou com deficiência, o representante legal da entidade cultural se autoafirma e deseja concorrer -`,
  cultura_popular = `1.2. A entidade tem trajetória comprovadamente ligada às culturas populares e tradicionais, e previu, no plano de trabalho, ações voltadas ao segmento, considerando pertinente concorrer pela reserva de vagas, conforme item 7.7 do edital?`,
  nome = `2.1. Nome da entidade cultural -`,
  cnpp_cadastro = `2.13. A entidade já é certificada pelo Ministério da Cultura, estando inscrita no Cadastro Nacional de Pontos e Pontões de Cultura? (Consultar em: www.gov.br/culturaviva) -`,
  nascimento = `3.3. Data de Nascimento -`,
  municipio = `CORNURA MUNICÍPIO`,
  comunidade = `3.10. O/A representante legal da Entidade pertence a alguma comunidade listada abaixo? -`,
  linguagem = `3.12. Qual a principal linguagem artístico-cultural do/a representante legal da Entidade? -`,
  linguagem_outra = `3.12.1. Se outro, qual?`,
  funcao = `3.13. Qual sua ocupação dentro da cultura? -`,
  funcao_outra = `3.13.1. Se outra função, qual? -`,
  atuacao = `3.14. Qual o tempo de atuação na área artístico-cultural do/a representante legal da Entidade? -`,
  atuacao_entidade = `4.1. Há quanto tempo a entidade cultural atua no setor cultural? -`,
  atuacao_entidade_inicio = `4.1.1. Qual a data em que a entidade cultural iniciou a atuação no setor cultural? -`,
  genero = `3.8. Como você se identifica com relação a sua identidade de gênero? -`,
  genero_outro = `3.8.1. Se outro, qual?`,
  orientacao_sexual = `3.9. Orientação Sexual -`,
  orientacao_sexual_outro = `3.9.1. Se outro, qual? -`,
  raca = `3.7. Qual a identidade étnico-racial do/a representante legal da Entidade? -`,
  pcd = `3.11. Trata-se de pessoa com deficiência? -`,
  pcd_tipo = `3.11.1. Se Sim, qual o tipo de deficiência? -`,
  escolaridade = `3.17. Qual a escolaridade do/a representante legal da Entidade? -`,
  prog_soc = `3.18. O/A representante legal da Entidade é beneficiário/a de algum programa social? -`,
  renda = `3.16. Qual a renda individual do/a representante legal da Entidade? -`,
  principal_renda_cultura = `3.15. Sua principal fonte de renda é por meio de atividade cultural? -`,
  faturamento_pj = `2.14. Qual o faturamento anual da Entidade Cultural no último ano? -`,
  cargo = `3.6. Cargo -`,
  desafios = `4.3. Quais são os principais desafios/dificuldades que a entidade cultural enfrenta na atuação dentro do seu setor cultural e para manter as atividades? -`,
  edital_cultura_viva = `4.30. Informe se a entidade cultural já foi selecionada em algum Edital de apoio da Cultura Viva -`,
  atividade_zona = `4.4. As atividades culturais realizadas pela entidade cultural acontecem em quais dessas áreas? -`,
  acoes_cultura_viva = `4.5. A entidade cultural atua com quais ações estruturantes da Cultura Viva? -`,
  acoes_cultura_viva_outra = `4.5.1. Se outra, qual? - (opcional)`,
  area_conhecimento = `4.6. A entidade cultural atua com quais áreas e temas de conhecimento que podem ser compartilhados? -`,
  area_conhecimento_outro = `4.6.1. Se outro, Qual? - (opcional)`,
  publico = `4.7. A entidade cultural atua diretamente com qual público? -`,
  publico_outro = `4.7.1. Se outro, qual?`,
  faixa_etaria = `4.8. Indique a faixa etária do público atendido diretamente: -`
)




sort(names(pontoes))


pontoes <- pontoes %>% transmute(
  id = `Número da inscrição`,
  status = Status,
  natureza = `Tipo de proponente`,
  cota = `1.1. Em relação às cotas reservadas às pessoas negras, indígenas ou com deficiência, o representante legal da entidade cultural se autoafirma e deseja concorrer`,
  cultura_popular = `1.2. A entidade tem trajetória comprovadamente ligada às culturas populares e tradicionais, e previu, no plano de trabalho, ações voltadas ao segmento, considerando pertinente concorrer pela reserva de vagas, conforme item 7.7 do edital?`,
  nome = `2.1. Nome da entidade cultural`,
  cnpp_cadastro = `2.13. A entidade já é certificada pelo Ministério da Cultura, estando inscrita no Cadastro Nacional de Pontos e Pontões de Cultura? (Consultar em www.gov.br/culturaviva)`,
  nascimento = `3.3. Data de Nascimento`,
  municipio = `CORNURA MUNICIPAL`,
  comunidade = `3.10. O/A representante legal da Entidade pertence a alguma comunidade listada abaixo?`,
  linguagem = `3.12. Qual a principal linguagem artístico-cultural do/a representante legal da Entidade?`,
  linguagem_outra = `3.12.1. Se outro, qual?`,
  funcao = `3.13. Qual sua ocupação dentro da cultura?`,
  funcao_outra = `3.13.1. Se outra função, qual?`,
  atuacao = `3.14. Qual o tempo de atuação na área artístico-cultural do/a representante legal da Entidade?`,
  atuacao_entidade = `4.1. Há quanto tempo a entidade cultural atua no setor cultural? -`,
  atuacao_entidade_inicio = `4.1.1. Qual a data em que a entidade cultural iniciou a atuação no setor cultural? -`,
  genero = `3.8. Como você se identifica com relação a sua identidade de gênero?`,
  genero_outro = `3.8.1. Se outro, qual?`,
  orientacao_sexual = `3.9. Orientação Sexual`,
  orientacao_sexual_outro = `3.9.1. Se outro, qual?`,
  raca = `3.7. Qual a identidade étnico-racial do/a representante legal da Entidade?`,
  pcd = `3.11. Trata-se de pessoa com deficiência?`,
  pcd_tipo = `3.11.1. Se Sim, qual o tipo de deficiência?`,
  escolaridade = `3.17. Qual a escolaridade do/a representante legal da Entidade?`,
  prog_soc = `3.18. O/A representante legal da Entidade é beneficiário/a de algum programa social?`,
  renda = `3.16. Qual a renda individual do/a representante legal da Entidade?`,
  principal_renda_cultura = `3.15. Sua principal fonte de renda é por meio de atividade cultural?`,
  faturamento_pj = `2.14. Qual o faturamento anual da Entidade Cultural no último ano?`,
  cargo = `3.6. Cargo`,
  desafios = `4.3. Quais são os principais desafios/dificuldades que a entidade cultural enfrenta na atuação dentro do seu setor cultural e para manter as atividades?`,
  edital_cultura_viva = `4.18. Informe se a entidade cultural já foi selecionada em algum Edital de apoio da Cultura Viva`,
  atividade_zona = `4.4. As atividades culturais realizadas pela Entidade Cultural acontecem em quais dessas áreas?(`,
  acoes_cultura_viva = `4.5. A Entidade Cultural atua com quais ações estruturantes da Cultura Viva?`,
  acoes_cultura_viva_outra = `4.5.1. Se outra, qual?`,
  area_conhecimento = `4.6. A Entidade Cultural atua com quais áreas e temas de conhecimento que podem ser compartilhados?`,
  area_conhecimento_outro = `4.6.1. Se outra, qual?`,
  publico = `4.7. A Entidade Cultural atua diretamente com qual público?`,
  publico_outro = `4.7.1. Se Outro, Qual?`,
  faixa_etaria = `4.8. Indique a faixa etária do público atendido diretamente:`
)


saveRDS(pontos, "pontos.RDS")
saveRDS(pontoes, "pontoes.RDS")




# resultado

pontos_r <- read_xlsx("Base - Pontos.xlsx")
colnames(pontos_r) <- pontos_r[1,]
pontos_r <- pontos_r[-1,]

pontoes_r <- read_xlsx("Base Pontões.xlsx")
colnames(pontoes_r) <- pontoes_r[1,]
pontoes_r <- pontoes_r[-1,]


# caruaru

nrow(pnab2 %>% filter(municipio == "Caruaru" & status2 == "Selecionada"))


pnab2 %>% filter(EDITAL_TIPO == "Prêmios") %>% group_by(status2, macrorreg) %>% summarise(n = n()) %>% 
  spread(status2, n) %>% mutate(total = Desclassificada + Selecionada + Suplente)

pnab2 %>% select(EDITAL_TIPO, EDITAL) %>% filter(EDITAL_TIPO == "Prêmios") %>% unique()


pnab %>% group_by(EDITAL) %>% summarise(n = n())



setwd(here())
pnab <- readRDS("BANCO PNAB.RDS")

setwd(here("RESULTADO"))


bolsas_art <- read_xlsx("BOLSAS ARTÍSTICAS - oportunidade-2449--inscricoes-2024-12-09 10_32_56.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)
bolsas_brinc <- read_xlsx("BOLSAS BRINCADEIRAS - oportunidade-2451--inscricoes-2024-12-09 10_49_06.xlsx") %>% 
  transmute(id = `Número da inscrição`,
            status2 = Status)


pnab2 <- pnab %>% left_join(rbind(
  bolsas_art,
  bolsas_brinc
), by = "id")


pnab2 <- pnab2 %>% mutate(status2 = ifelse(is.na(status2) == T, "Desclassificada", 
                                           ifelse(status2 == "Não selecionada", "Desclassificada",
                                                  status2)))



# caruaru

nrow(pnab2 %>% filter(municipio == "Caruaru" & status2 == "Selecionada"))

pnab2 %>% filter(EDITAL_TIPO == "Bolsas") %>% group_by(status2, macrorreg) %>% summarise(n = n()) %>% 
  spread(status2, n) %>% mutate(total = Desclassificada + Selecionada + Suplente)

pnab2 %>% select(EDITAL_TIPO, EDITAL) %>% filter(EDITAL_TIPO == "Prêmios") %>% unique()


pnab %>% group_by(EDITAL) %>% summarise(n = n())


# Mulher negra

mn <- pnab2 %>% filter(EDITAL == "Mulher Negra") %>% 
  filter(str_detect(inducao, "Mãe") == T)

# 1. Com base nos indutores (em números absolutos e em porcentagem):
#   
#   1.1. Quantas Mulheres Negras Mãe (cis e trans) se inscreveram no total? por faixa de inscrição? 
# por região (RMR, Zona da Mata, Agreste e Sertão)?

nrow(mn) # inscritas

faixa1 <- mn %>% group_by(categoria) %>% summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(faixa1, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", width = .6, fill = "tomato3", color = "black") +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5) +
  expand_limits(y = c(0, 90)) +
  labs(x = "Faixa", y = "Qtd. de Inscritas")

setwd(here())
#dir.create("graficos mulheres negras")
setwd(here("graficos mulheres negras"))

ggsave("001 insc x faixa.png")


# por macrorregiao

macrorreg <- mn %>% group_by(macrorreg) %>% summarise(n = n())

ggplot(macrorreg, aes(x = reorder(macrorreg, n), y = n)) +
  geom_bar(stat = "identity", width = .6, fill = "tomato3", color = "black") +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5) +
  expand_limits(y = c(0, 85)) +
  labs(x = "Faixa", y = "Qtd. de Inscritas")


ggsave("002 insc x macrorreg.png")

#   1.2. Quantas Mulheres Negras Mãe (cis e trans) foram selecionadas no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?

nrow(mn %>% filter(status2 == "Selecionada")) # inscritas

faixa1 <- mn %>% filter(status2 == "Selecionada") %>% group_by(categoria) %>% summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(faixa1, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", width = .6, fill = "tomato3", color = "black") +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5) +
  expand_limits(y = c(0, 35)) +
  labs(x = "Faixa", y = "Qtd. de Selecionadas")


ggsave("003 selec x faixa.png")


# por macrorregiao

macrorreg <- mn %>% filter(status2 == "Selecionada") %>% group_by(macrorreg) %>% summarise(n = n())

ggplot(macrorreg, aes(x = reorder(macrorreg, n), y = n)) +
  geom_bar(stat = "identity", width = .6, fill = "tomato3", color = "black") +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5) +
  expand_limits(y = c(0, 30)) +
  labs(x = "Faixa", y = "Qtd. de Selecionadas")


ggsave("004 selec x macrorreg.png")

#   1.3. Quantas Mulheres Negras TRANS e TRAVESTI se inscreveram no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?


mn_trans <- mn %>% filter(genero %in% c("Travesti", "Mulher Trans")) %>% group_by(genero) %>% 
  summarise(n = n())

mn_trans <- mn %>% filter(genero %in% c("Travesti", "Mulher Trans")) %>% group_by(genero, categoria) %>% 
  summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(mn_trans, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  facet_wrap(~genero) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 3.5) +
  expand_limits(y = c(0, 6)) +
  labs(x = "Faixa", y = "Qtd. de Inscritas")

ggsave("005 selec x faixa.png")

mn_trans <- mn %>% filter(genero %in% c("Travesti", "Mulher Trans")) %>% group_by(macrorreg, genero) %>% 
  summarise(n = n())

ggplot(mn_trans, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  facet_wrap(~genero) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 3.5) +
  expand_limits(y = c(0, 6)) +
  labs(x = "Faixa", y = "Qtd. de Inscritas")

ggsave("006 selec x macrorreg.png")

#   1.4 Quantas Mulheres Negras TRANS e TRAVESTI foram selecionadas no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?

mn <- pnab2 %>% filter(EDITAL == "Mulher Negra")

mn_trans <- mn %>% filter(status2 == "Selecionada") %>% filter(genero %in% c("Travesti", "Mulher Trans")) %>% group_by(genero) %>% 
  summarise(n = n())

mn_trans <- mn %>% filter(status2 == "Selecionada")  %>% filter(genero %in% c("Travesti", "Mulher Trans")) %>% group_by(genero, categoria) %>% 
  summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(mn_trans, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  facet_wrap(~genero) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 3.5) +
  expand_limits(y = c(0, 6)) +
  labs(x = "Faixa", y = "Qtd. de Selecionadas")

ggsave("007 selec x faixa.png")

mn_trans <- mn %>% filter(status2 == "Selecionada")  %>% filter(genero %in% c("Travesti", "Mulher Trans")) %>% group_by(macrorreg, genero) %>% 
  summarise(n = n())

ggplot(mn_trans, aes(x = macrorreg, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  facet_wrap(~genero) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 3.5) +
  expand_limits(y = c(0, 6)) +
  labs(x = "Faixa", y = "Qtd. de Selecionadas")

ggsave("008 selec x macrorreg.png")
   
#   1.5. Quantas Mulheres Negras pertencentes a povos e comunidades tradicionais se inscreveram no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?

mn_comunidade <- mn %>% group_by(comunidade) %>% 
  summarise(n = n()) %>% mutate(prop = scales::percent(n/sum(n), accuracy = 0.01)) %>% 
  arrange(-n)

write.xlsx(mn_comunidade, "tabela comunidade insc.xlsx")

mn_comunidade <- mn %>% filter(!comunidade %in% c("Não pertenço")) %>% group_by(comunidade, categoria) %>% 
  summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(mn_comunidade, aes(x = reorder(comunidade, n), y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  facet_wrap(~categoria, ncol = 2) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            hjust = -.1, size = 3.5) +
  expand_limits(y = c(0, 80)) +
  coord_flip() +
  labs(x = "Comunidade", y = "Qtd. de Inscritas")

ggsave("009 selec x faixa.png")

mn_comunidade <- mn %>% filter(!comunidade %in% c("Não pertenço")) %>% group_by(comunidade, macrorreg) %>% 
  summarise(n = n())

ggplot(mn_comunidade, aes(x = reorder(comunidade, n), y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  facet_wrap(~macrorreg) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            hjust = -.1, size = 3.5) +
  expand_limits(y = c(0, 95)) +
  coord_flip() +
  labs(x = "Comunidade", y = "Qtd. de Inscritas")

ggsave("010 selec x faixa.png")


#   1.6. Quantas Mulheres Negras pertencentes a povos e comunidades tradicionais foram selecionadas no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?

mn_comunidade <- mn %>% filter(status2 == "Selecionada") %>% group_by(comunidade) %>% 
  summarise(n = n()) %>% mutate(prop = scales::percent(n/sum(n), accuracy = 0.01)) %>% 
  arrange(-n)

write.xlsx(mn_comunidade, "tabela comunidade selec.xlsx")

mn_comunidade <- mn %>% filter(status2 == "Selecionada") %>% filter(!comunidade %in% c("Não pertenço")) %>% group_by(comunidade, categoria) %>% 
  summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(mn_comunidade, aes(x = reorder(comunidade, n), y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  facet_wrap(~categoria, ncol = 2) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            hjust = -.1, size = 3.5) +
  expand_limits(y = c(0, 20)) +
  coord_flip() +
  labs(x = "Comunidade", y = "Qtd. de Selecionadas")

ggsave("011 selec x faixa.png")

mn_comunidade <- mn %>% filter(status2 == "Selecionada") %>% filter(!comunidade %in% c("Não pertenço")) %>% group_by(comunidade, macrorreg) %>% 
  summarise(n = n())

ggplot(mn_comunidade, aes(x = reorder(comunidade, n), y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  facet_wrap(~macrorreg) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            hjust = -.1, size = 3.5) +
  expand_limits(y = c(0, 30)) +
  coord_flip() +
  labs(x = "Comunidade", y = "Qtd. de Inscritas")

ggsave("012 selec x faixa.png")

#   1.7. Quantas Mulheres Negras com deficiência se inscreveram no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?

mn_pcd <- mn %>% group_by(pcd) %>% 
  summarise(n = n()) %>% mutate(prop = scales::percent(n/sum(n), accuracy = 0.01)) %>% 
  arrange(-n)

write.xlsx(mn_pcd, "tabela comunidade insc.xlsx")

mn_pcd <- mn %>% filter(pcd == "Sim") %>% group_by(categoria) %>% 
  summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(mn_pcd, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 4) +
  labs(x = "Faixa", y = "Qtd. de Inscritas")

ggsave("013 selec x faixa.png")

mn_pcd <- mn %>% filter(pcd == "Sim") %>% group_by(macrorreg) %>% 
  summarise(n = n())

ggplot(mn_pcd, aes(x = reorder(macrorreg, n), y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 4) +
  expand_limits(y = c(0, 10)) +
  labs(x = "Macrorregião", y = "Qtd. de Inscritas")

ggsave("014 selec x faixa.png")

#   1.8. Quantas Mulheres Negras com deficiência foram selecionadas no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?

mn_pcd <- mn %>% filter(status2 == "Selecionada") %>% group_by(pcd) %>% 
  summarise(n = n()) %>% mutate(prop = scales::percent(n/sum(n), accuracy = 0.01)) %>% 
  arrange(-n)

write.xlsx(mn_pcd, "tabela comunidade insc.xlsx")

mn_pcd <- mn %>% filter(status2 == "Selecionada") %>% filter(pcd == "Sim") %>% group_by(categoria) %>% 
  summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(mn_pcd, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 4) +
  labs(x = "Faixa", y = "Qtd. de Selecionadas") +
  expand_limits(y = c(0, 5))

ggsave("015 selec x faixa.png")

mn_pcd <- mn %>% filter(status2 == "Selecionada") %>% filter(pcd == "Sim") %>% group_by(macrorreg) %>% 
  summarise(n = n())

ggplot(mn_pcd, aes(x = reorder(macrorreg, n), y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 4) +
  expand_limits(y = c(0, 7)) +
  labs(x = "Macrorregião", y = "Qtd. de Selecionadas")

ggsave("016 selec x faixa.png")
 
#   1.9. Quantas Mulheres Negras em situação de rua ou em vulnerabilidade social se inscreveram no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?

rua <- mn %>% filter(str_detect(inducao, "Vulnerabilidade") == T)

nrow(rua)


rua <- mn %>% filter(str_detect(inducao, "Vulnerabilidade") == T) %>% group_by(categoria) %>% 
  summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(rua, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 4) +
  labs(x = "Faixa", y = "Qtd. de Inscritas") +
  expand_limits(y = c(0, 35))

ggsave("017 selec x faixa.png")

rua <- mn %>% filter(str_detect(inducao, "Vulnerabilidade") == T) %>% group_by(macrorreg) %>% 
  summarise(n = n())

ggplot(rua, aes(x = reorder(macrorreg, n), y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 4) +
  expand_limits(y = c(0, 40)) +
  labs(x = "Macrorregião", y = "Qtd. de Inscritas")

ggsave("018 selec x faixa.png")

#   1.10. Quantas Mulheres Negras em situação de rua ou em vulnerabilidade social foram selecionadas no total? por faixa de inscrição? por região (RMR, Zona da Mata, Agreste e Sertão)?


rua <- mn %>% filter(str_detect(inducao, "Vulnerabilidade") == T)

nrow(rua %>% filter(status2 == "Selecionada"))


rua <- mn %>% filter(status2 == "Selecionada") %>% filter(str_detect(inducao, "Vulnerabilidade") == T) %>% group_by(categoria) %>% 
  summarise(n = n()) %>% 
  mutate(categoria = ifelse(categoria == "Mulheres negras a partir dos 60 anos de idade", "Maior que 60",
                            ifelse(categoria == "Mulheres negras com idade entre 18 e 29 anos", "Entre 18 e 29",
                                   "Entre 30 e 59")),
         categoria = factor(categoria, levels = c("Entre 18 e 29", "Entre 30 e 59", "Maior que 60")))

ggplot(rua, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 4) +
  labs(x = "Faixa", y = "Qtd. de Selecionadas") +
  expand_limits(y = c(0, 15))

ggsave("019 selec x faixa.png")

rua <- mn %>% filter(status2 == "Selecionada") %>% filter(str_detect(inducao, "Vulnerabilidade") == T) %>% group_by(macrorreg) %>% 
  summarise(n = n())

ggplot(rua, aes(x = reorder(macrorreg, n), y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "tomato3", width = .6) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            vjust = -.5, size = 4) +
  expand_limits(y = c(0, 15)) +
  labs(x = "Macrorregião", y = "Qtd. de Selecionadas")

ggsave("020 selec x faixa.png")

#   2. Para além dos indutores qual a média de notas no total, por faixa de inscrição e por região (RMR, Zona da Mata, Agreste e Sertão)?
#   
#   3. Qual a faixa de inscrição teve maior número de inscritas?
#   
#   4. Qual a faixa de inscrição teve maior número de selecionadas?
#   
#   5. Qual a faixa de inscrição teve maior número de suplentes?

mn %>% group_by(categoria, status2) %>% summarise(n = n()) %>% 
  spread(status2, n) %>% mutate(nselec = Desclassificada + Suplente)

#   6. Qual a faixa de inscrição teve maior número de não selecionadas?
#   
#   5. Qual a região teve maior número de inscritas?
#   
#   6. Qual a região teve maior número de selecionadas?
#   
#   7. Qual a região teve maior número de suplentes?

mn %>% group_by(macrorreg, status2) %>% summarise(n = n()) %>% 
  spread(status2, n) %>% mutate(nselec = Desclassificada + Suplente)

#   8. Qual a região teve maior número de não selecionadas?
list.files(pattern = ".RDS") 
setwd()
pnab <- readRDS("BANCO PNAB.RDS")


pnab %>% filter(EDITAL == "Técnicos") %>% select(status) %>% unique()


write.xlsx(pnab, "temp.xlsx")
