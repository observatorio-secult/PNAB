if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(splitstackshape) == F) install.packages("splitstackshape"); require(splitstackshape)


# bancos

geral <- read_xlsx("banco_geral.xlsx", sheet = "CONSOLIDADE X TEMA")



# GERAL---------

a <- geral %>% group_by(OITIVA, TEMA) %>% summarise(n = n())

write.xlsx(a, "tabela1.xlsx")

# MAPA----------

# ESCUTA PRESENCIAL/ONLINE------
