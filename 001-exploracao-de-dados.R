################################################################################
#
#
#
#
#
################################################################################
library(ggplot2)
library(dplyr)
#library(tidyr)

# -- loading data -- 
dados <- readxl::read_excel("data/quantities-by-location-and-type-of-collection.xlsx",
                                      sheet = 2)
# -- data structure -- 
str(dados)
summary(dados)
head(dados)

dados$condicao <- as.factor(dados$condicao)

str(dados)
summary(dados)
head(dados)

# -- distribution of species by location -- 

# Função para gerar os gráficos e salvar como PDF
gerar_graficos <- function(dados, arquivo_saida) {
  # Lista de locais
  locais <- colnames(dados)[c(-1,-10)] # Remove a coluna de espécie e condicao
  
  # Configuração do PDF
  pdf(file = arquivo_saida, width = 10, height = 8)
  
  for (local in locais) {
    # Criando gráfico para o local específico
    p <- ggplot(dados, aes(x = reorder(especie, !!sym(local)), y = !!sym(local))) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +  # Barras horizontais
      theme_minimal(base_size = 14) + 
      theme(axis.text.y = element_text(size = 12, hjust = 1),
            axis.text.x = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_line(color = "gray90")) +
      labs(title = paste("Abundância de Mosquitos -", local),
           x = "Espécies",
           y = "Quantidade") +
      theme(panel.grid.minor = element_blank())
    
    print(p) # Adiciona ao PDF
  }
  
  dev.off() # Fecha o arquivo PDF
}

# Chamando a função
gerar_graficos(dados, "abundancia_mosquitos.pdf")

# Dividir os dados por condição
dados_chuvoso <- subset(dados, condicao == "ch")
dados_menos_chuvoso <- subset(dados, 
                              condicao == "n_ch")

gerar_graficos_por_condicao <- function(dados, condicao, arquivo_saida) {
  locais <- colnames(dados)[c(-1, -10)] # Excluir as colunas de espécie e condição
  
  # Configuração do PDF
  pdf(file = arquivo_saida, width = 10, height = 8)
  
  for (local in locais) {
    p <- ggplot(dados, aes(x = reorder(especie, !!sym(local)), y = !!sym(local))) +
      geom_bar(stat = "identity", fill = ifelse(condicao == "ch", "blue", "orange")) + # Diferenciar cores
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_text(size = 12, hjust = 1),
            axis.text.x = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_line(color = "gray90")) +
      labs(title = paste("Abundância de Mosquitos -", local, "(", condicao, ")"),
           x = "Espécies",
           y = "Quantidade")
    
    print(p)
  }
  
  dev.off()
}

# Gerar gráficos separados para cada condição
gerar_graficos_por_condicao(dados_chuvoso, "ch", "graficos_chuvoso.pdf")
gerar_graficos_por_condicao(dados_menos_chuvoso, "n_ch", "graficos_menos_chuvoso.pdf")

################################################################################

dados_coleta <- readxl::read_excel("data/capture -type.xlsx") 

# Total de mosquitos coletados por método
abundancia_tipo <- colSums(dados_coleta[,-1])
print(abundancia_tipo)

library(vegan)

# Cálculo da diversidade para cada método
diversidade <- apply(dados_coleta[,-1], 2, function(x) diversity(x, index = "shannon"))
print(diversidade)

library(ggplot2)

# Transformação para formato longo (long format) para ggplot2
dados_long <- tidyr::pivot_longer(dados_coleta, 
                                  cols = -especie, 
                                  names_to = "metodo_coleta", 
                                  values_to = "quantidade")

# Gráfico de barras
ggplot(dados_long, aes(x = reorder(especie, quantidade), y = quantidade, fill = metodo_coleta)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Abundância de Espécies por Método de Coleta",
       x = "Espécies",
       y = "Quantidade",
       fill = "Método de Coleta")

# Espécie mais abundante por método
dados_long %>%
  group_by(metodo_coleta) %>%
  top_n(1, quantidade)
################################################################################