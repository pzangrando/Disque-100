# PASTA-MÃE (SETWD)
setwd("C:/R-Projetos/DISQUE-100")


# INSTALAR PACOTES E CARREGÁ-LOS

# CARREGAR TUDO (readr, knitr, ggplot2, writexls)
library(readr)
library(knitr)
library(ggplot2)
library(writexl)

disque100_primeiro_semestre_2023 <- read_delim("disque100-primeiro-semestre-2023.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) 
disque100_segundo_semestre_2023 <- read_delim("disque100-segundo-semestre-2023.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# DEIXAR COLUNAS IGUAIS
names(disque100_segundo_semestre_2023) <- names(disque100_primeiro_semestre_2023)

# MESCLAR BASES
disque100_2023 <- rbind(disque100_primeiro_semestre_2023, disque100_segundo_semestre_2023)

# FILTRAR DADOS DF
df_dados <- subset(disque100_2023, UF == "DF")

# Exibir dados de uma coluna só
unique(df_dados$Faixa_etária_da_vítima)

# FILTRAR DADOS CRIANÇA-ADOLESCENTE

faixas_criancas_adolescentes <- c("01 ANO", "02 ANOS", "03 ANOS", "04 ANOS", "05 ANOS", 
                                  "06 ANOS", "07 ANOS", "08 ANOS", "09 ANOS", "10 ANOS", 
                                  "11 ANOS", "12 ANOS", "13 ANOS", "14 ANOS", "15 ANOS", 
                                  "16 ANOS", "17 ANOS", "CRIANÇA/ADOLESCENTE IDADE NÃO INFORMADA")

# Filtrar os dados
df_cri <- subset(df_dados, Faixa_etária_da_vítima %in% faixas_criancas_adolescentes)

# Exibir os dados filtrados
head(df_cri)

# EXIBIR TABELA
View(df_cri)

# Sumarização dos DADOS
summary(df_cri)

# Frequência da coluna violação
frequencia <- table(df_cri$violacao)
print(frequencia)
barplot(frequencia)

# Obter as 10 maiores frequências
top_frequencias <- sort(frequencia, decreasing = TRUE)[1:10]

View(top_frequencias)

# Gráfico com GGPLOT2

# Criar um dataframe a partir das frequências
df_top <- as.data.frame(top_frequencias)
colnames(df_top) <- c("Violação", "Frequência")

# Renomear as violações para deixar mais limpo (opcional)
df_top$Violação <- gsub("INTEGRIDADE>", "", df_top$Violação)
df_top$Violação <- gsub("PSÍQUICA>", "", df_top$Violação)  
df_top$Violação <- gsub("FÍSICA>", "", df_top$Violação)

# Gráfico com ggplot2
ggplot(df_top, aes(x = reorder(Violação, -Frequência), y = Frequência)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = Frequência), vjust = 2.0, check_overlap = TRUE) +  # Rótulos acima das barras
  labs(title = "10 Maiores Tipos de Violações para Crianças e Adolescentes",
       x = "Tipos de Violações",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(vjust = 0.5)) 

# Se necessário, salve o gráfico
ggsave("grafico.png", width = 12, height = 7)

# Criar a tabela resumo
print(df_top)

# Salvar df_cri
write_xlsx(df_cri, path = "df_cri.xlsx", col_names = TRUE, format_headers = TRUE, use_zip64 = TRUE)
