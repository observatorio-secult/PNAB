

df <- data.frame(politica = c("LAB", "LPG", "PNAB2024", "PNAB2025"),
                 ano = c(2021, 2023, 2024, 2025),
                 valor = c(73534903.27, 98652254.48,74000000, 440000000),
                 tempo_insc = c(40,28,31,31),
                 inscritos = c(12540,11367,9476, 5553-977+1991+226+1929+198+1265+145),
                 rmr = c(8442, 6317, 5553, 5553-977),
                 agreste = c(1392, 2162, 1991, 1991+226),
                 sertao = c(1342, 1767, 1929, 1929+198),
                 mata = c(830, 1121, 1265, 1265+145))


df_ate2024 <- drop_na(df)


df_ate2024_long <- df_ate2024 %>% select(politica, ano, rmr, agreste, sertao, mata) %>% 
  gather(key = regiao, value = inscritos, rmr:mata)

ggplot(df_ate2024_long, aes(x = ano, y = inscritos)) +
  geom_point(aes(color = regiao)) +
  geom_line(aes(color = regiao))


p <- lm(rmr ~ ano, data = df_ate2024)
summary(p)

p <- lm(agreste ~ ano, data = df_ate2024)
summary(p)

p <- lm(sertao ~ ano, data = df_ate2024)
summary(p)

options(scipen = 999)
p <- lm(mata ~ ano, data = df_ate2024)
summary(p)


df_ate2024_long <- df %>% select(politica, ano, rmr, agreste, sertao, mata) %>% 
  gather(key = regiao, value = inscritos, rmr:mata)

df_ate2024_long %>% group_by(politica) %>% mutate(prop = scales::percent(inscritos/sum(inscritos),
                                                                            accuracy = 0.01)) %>% 
  arrange(ano)

