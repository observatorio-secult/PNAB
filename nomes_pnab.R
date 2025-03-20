

mulher_negra <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - MULHERES NEGRAS")
povos_comunidades <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - POV. COMUN. PRAT. EXIT")
quadrilhas <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - QUADRILHAS JUNINAS")
hiphop <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - HIP HOP")
tecnicos <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - TÉCNICOS")
salvaguarda <- read_xlsx("APOIO - EDITAIS DE PRÊMIOS.xlsx", sheet = "BASE - SALVAGUARDA")


mulher_negra <- mulher_negra %>% 
  transmute(id = NÚMERO,
            nome = `1.1.\tNOME DA PROPONENTE -`,
            projeto = "PRÊMIO")


povos_comunidades <- povos_comunidades %>% 
  transmute(id = NÚMERO,
            nome = `1.1. NOME DO AGENTE REPRESENTANTE LEGAL -`,
            projeto = "PRÊMIO")


quadrilhas <- quadrilhas %>% 
  transmute(id = NÚMERO,
            nome = `1.1. NOME DO AGENTE REPRESENTANTE LEGAL -`,
            projeto = `1.3. NOME DA QUADRILHA JUNINA -`)

hiphop <- hiphop %>% 
  transmute(id = NÚMERO,
            nome = `1.1.\tNOME DO AGENTE REPRESENTANTE LEGAL -`,
            projeto = "PRÊMIO")


tecnicos <- tecnicos %>% 
  transmute(id = NÚMERO,
            nome = `1.1. NOME DO AGENTE`,
            projeto = "PRÊMIO")

salvaguarda <- salvaguarda %>% 
  transmute(id = NÚMERO,
            nome = `1.1. NOME DO AGENTE REPRESENTANTE LEGAL`,
            projeto = "PRÊMIO")



setwd(here("EDITAIS", "TECs"))

diversidade_cultura <- read_xlsx("Diversidade e Cultura.xlsx")
economia_criativa <- read_xlsx("Economia Criativa .xlsx")
festivais <- read_xlsx("Festivais e Mostras.xlsx")
formacao <- read_xlsx("Formação e Diversidade.xlsx")
multilinguagem <- read_xlsx("Multilinguagens.xlsx")
museus <- read_xlsx("Museus.xlsx")

diversidade_cultura <- diversidade_cultura %>% 
  transmute(id = `Número da inscrição`,
            nome = `1.1. Nome do agente representante legal -`,
            projeto = `4.2. Título da Proposta -`)


economia_criativa <- economia_criativa %>% 
  transmute(id = `Número da inscrição`,
            nome = `1. Nome do agente representante legal`,
            projeto = "ECONOMIA CRIATIVA")


festivais <- festivais %>% 
  transmute(id = `Número da inscrição`,
            nome = `1.1.\tNome do agente representante legal -`,
            projeto = "FESTIVAIS")


formacao <- formacao %>% 
  transmute(id = `Número da inscrição`,
            nome = `1.1.\tNome do agente representante legal -`,
            projeto = `4.2.\tTítulo da Proposta -`)

multilinguagem <- multilinguagem %>% 
  transmute(id = `Número da inscrição`,
            nome = `1.1. Nome do agente representante legal`,
            projeto = `4.2. Nome da proposta:`)

museus <- museus %>% 
  transmute(id = `Número da inscrição`,
            nome = `1.1. Nome do agente representante legal -`,
            projeto = `4.7. Museu/espaço/território a ser beneficiado -`)


setwd(here("EDITAIS"))
bolsas_artisticas <- read_xlsx("Bolsas.xlsx", sheet = "Bolsas Artísticas")
bolsas_brincadeiras <- read_xlsx("Bolsas.xlsx", sheet = "Bolsas Brincadeiras")


bolsas_artisticas <- bolsas_artisticas %>% 
  transmute(id = `Número da inscrição`,
            nome = `1.1. Nome do agente ou representante legal -`,
            projeto = "BOLSAS ARTÍSTICAS")

bolsas_brincadeiras <- bolsas_brincadeiras %>% 
  transmute(id = `Número da inscrição`,
            nome = `1.1. Nome do agente representante legal -`,
            projeto = "BOLSAS BRINCADEIRAS")


pnab_nomes <- rbind(bolsas_artisticas,
                    bolsas_brincadeiras,
                    diversidade_cultura,
                    economia_criativa,
                    festivais,
                    formacao,
                    hiphop,
                    mulher_negra,
                    multilinguagem,
                    museus,
                    povos_comunidades,
                    quadrilhas,
                    salvaguarda,
                    tecnicos)


setwd(here())

pnab <- readRDS("BANCO PNAB.RDS")


pnab_nomes <- pnab_nomes %>% left_join(
  pnab %>% select(id, status, municipio, categoria, EDITAL)
)

pnab_nomes <- pnab_nomes %>% filter(municipio == "Vicência")

write.xlsx(pnab_nomes, "NOMES E PROJETOS VICÊNCIA.xlsx")

