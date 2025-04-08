library(iNEXT)

# Ler os dados diretamente do arquivo
dados <- readxl::read_excel("data/quantities-by-location-and-type-of-collection.xlsx", sheet = "Planilha2")
dados_tipoColeta <- readxl::read_excel("data/capture -type.xlsx")

head(dados)
head(dados_tipoColeta)

library(ggplot2)

# Abundância total por método
abundancia_metodo <- colSums(dados_tipoColeta[, -1]) # Exclui a coluna de espécies

# Criar dataframe para plotagem
df_metodo <- data.frame(
  Metodo = names(abundancia_metodo),
  Abundancia = abundancia_metodo
)

# Gráfico de barras para métodos de coleta
cairo_pdf("metodos_coleta.pdf", width = 10, height = 8, family = "Helvetica", onefile = TRUE)
ggplot(df_metodo, aes(x = Metodo, y = Abundancia, fill = Metodo)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Abundância por Método de Coleta",
       x = "Método de Coleta",
       y = "Abundância Total")
dev.off()

# Criar tabela com abundância total por local e condição
abundancia_local <- aggregate(cbind(CRV_01, CRV_02, CRV_03, CRV_04,
                                    MZG_01, MZG_02, MZG_03, MZG_04) ~ condicao, 
                              data = dados, sum)

# Transformar para formato longo
library(tidyr)
abundancia_long <- pivot_longer(abundancia_local, cols = -condicao, 
                                names_to = "Local", values_to = "Abundancia")

# Gráfico de barras por local e condição
cairo_pdf("local_e_clima.pdf", width = 10, height = 8, family = "Helvetica", onefile = TRUE)
ggplot(abundancia_long, aes(x = Local, y = Abundancia, fill = condicao)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Distribuição de Abundância por Local e Condição",
       x = "Local",
       y = "Abundância")
dev.off()


















# Filtrar abundância por condição climática
abundancia_condicao <- aggregate(cbind(CRV_01, CRV_02, CRV_03, CRV_04, MZG_01, MZG_02, MZG_03, MZG_04) ~ condicao, data = dados, sum)

# Gráfico de barras comparando períodos climáticos
ggplot(abundancia_condicao, aes(x = condicao, y = rowSums(abundancia_condicao[,-1]), fill = condicao)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 14) +
  labs(title = "Comparação de Abundância entre Condições Climáticas",
       x = "Condição Climática",
       y = "Abundância Total")


library(vegan)
# Preparar dados para análise multivariada
dados_multivariados <- dados[, -c(1, 10)]
rownames(dados_multivariados) <- dados$especie

# nMDS usando Bray-Curtis
nmds <- metaMDS(dados_multivariados, distance = "bray")
plot(nmds, type = "text", main = "nMDS da Composição de Espécies por Local")

# Teste ANOSIM
anosim_test <- anosim(vegdist(dados_multivariados, method = "bray"), dados$condicao)
print(anosim_test)
