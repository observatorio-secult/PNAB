if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)


# ouvir para mudar

opm <- read_xlsx("opmpe.xlsx")

head(opm)

opm <- opm %>% group_by(proposta, votos, temática, TIPO) %>% 
  summarise(texto = paste0(`corpo da proposta`, collapse = " "))


opm$consulta <- paste0(opm$proposta, " - ", opm$texto)

opm$equipamentos <- str_detect(opm$consulta, regex("\\bmuseu|cinema|galeria|biblioteca|mercado municipal|mercados municipais", ignore_case = TRUE))
opm$cultura <- str_detect(opm$consulta, regex("\\bcultura|\\barte|\\bsecult\\b|fundarpe", ignore_case = TRUE))
opm$linguagem <- str_detect(opm$consulta, regex("\\bartesanato|\\bmúsica|\\bmusica|\\bdança|\\bcultura popular|\\bteatro|\\bópera\\b|\\bopera\\b|\\bliteratura|\\bgastronomia|\\bdesign|\\bmoda|\\baudiovisual|\\bfilme|\\bartes visuais|\\bcirco|\\bcircense", ignore_case = TRUE))

opm <- opm %>% mutate(geral = ifelse(equipamentos == 1 | 
                                       cultura == 1 |
                                       linguagem == 1, 1, 0))



sum(opm$geral)


opm_cultura <- opm %>% filter(geral == 1)


tipo <- opm_cultura %>% group_by(TIPO) %>% summarise(n = n())

theme_set(theme_bw())

g <- ggplot(tipo, aes(x = TIPO, y = n)) +
  geom_bar(aes(fill = TIPO), width = .6, stat = "identity") +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")), 
            size = 4, vjust = -.5) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "", y = "Proposta")
g


tipo_comp <- opm_cultura %>% gather(key = tipo_cult, value = check, equipamentos:linguagem) %>% 
  filter(check == 1) %>% group_by(TIPO, tipo_cult) %>% summarise(n = n()) %>% 
  group_by(tipo_cult) %>% mutate(total = sum(n), prop = n/total)

g1 <- ggplot(tipo_comp, aes(fill=TIPO, y=prop, x=tipo_cult)) + 
  geom_bar(position="fill", stat="identity", width = .8) +
  geom_text(aes(label = paste0(scales::percent(prop, accuracy = 0.01), "\n", n, " propostas")), hjust = .5, position=position_fill(vjust = .5)) +
  geom_label(aes(label = total, y = .95), data = tipo_comp %>% filter(TIPO == "PROPOSTA"), fill = "white") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(size = 12)) +
  labs(x = "", y = "Proporção (%)")
g1


require(cowplot)

plot_grid(g, g1)

ggsave("plot opm.png")

# nuvem de palavras

if(require(wordcloud) == F) install.packages("wordcloud"); require(wordcloud)
if(require(RColorBrewer) == F) install.packages("RColorBrewer"); require(RColorBrewer)
if(require(wordcloud2) == F) install.packages("wordcloud2"); require(wordcloud2)
if(require(tm) == F) install.packages("tm"); require(tm)

nuvem1 <- opm_cultura$consulta

# Create a corpus  
docs <- Corpus(VectorSource(nuvem1))
docs1 <- tm_map(docs, textstem::lemmatize_strings)
# Limpando dado

docs1 <- docs1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs1 <- tm_map(docs1, content_transformer(tolower))
docs1 <- tm_map(docs1, removeWords, stopwords("portuguese"))

dtm <- TermDocumentMatrix(docs1) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
write.xlsx(df, "opm_words.xlsx")

df <- read_xlsx("opm_words.xlsx") %>% group_by(word) %>% summarise(freq = sum(freq))

wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=50, random.order=FALSE, rot.per=0.35)


# CONFERENCIA ESTADUAL DE CULTURA

cec <- read_xlsx("cec.xlsx")

sum(cec$PROP)

cec_obj <- cec %>% group_by(OBJ) %>% summarise(n = sum(PROP))


g <- ggplot(cec_obj, aes(x = reorder(OBJ, n), y = n)) +
  geom_point(size = 4, color = "tomato3", shape = 18) +
  coord_flip() +
  theme(panel.grid.minor = element_blank()) +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")), 
            size = 3.5, hjust = -.2, vjust = -.1) +
  expand_limits(y = c(0, 30)) +
  labs(x = "")
g

ggsave2("cec obj.png")


cec_obj <- cec %>% group_by(EIXO) %>% summarise(n = sum(PROP))


g <- ggplot(cec_obj, aes(x = reorder(EIXO, -n), y = n)) +
  geom_bar(stat = "identity", width = .6, fill = "tomato3", color = "black") +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            size = 3, vjust = -.5) +
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "Propostas")
g

ggsave("cec eixos.png")




if(require(treemapify) == F) install.packages("treemapify"); require(treemapify)

ggplot(cec,aes(area=PROP,fill=OBJ, 
                       label=EIXO,subgroup=OBJ))+ 
  geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre",size = 12) +
  geom_treemap_subgroup_text(size = 15, colour = "black",
                             fontface = "italic") +
  theme(legend.position = "none")

ggsave("treemap.png")


# lpg

df <- data.frame(inter = c("Pergunta", "Proposição"),
                 n = c(452, 239))

g <- ggplot(df, aes(x = reorder(inter, -n), y = n)) +
  geom_bar(stat = "identity", width = .6, aes(fill = inter), color = "black") +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")),
            size = 4, vjust = -.5) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 12)) +
  labs(x = "", y = "Propostas")
g

ggsave("lpg prop.png")
