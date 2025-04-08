################################################################################
#
#
#
#
#
################################################################################
library(vegan)

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

# Dividir os dados por condição climática
dados_chuvoso <- dados[dados$condicao == "ch", -c(1, 10)] # Retira 'especie' e 'condicao'
dados_menos_chuvoso <- dados[dados$condicao == "n_ch", -c(1, 10)]

# Índice de Shannon-Wiener para as duas condições
shannon_chuvoso <- diversity(dados_chuvoso, index = "shannon")
shannon_menos_chuvoso <- diversity(dados_menos_chuvoso, index = "shannon")

# Índice de Uniformidade de Pielou
pielou_chuvoso <- shannon_chuvoso / log(specnumber(dados_chuvoso))
pielou_menos_chuvoso <- shannon_menos_chuvoso / log(specnumber(dados_menos_chuvoso))

# Resultados
cat("Diversidade (H') - Chuvoso:", shannon_chuvoso, "\n")
cat("Diversidade (H') - Menos Chuvoso:", shannon_menos_chuvoso, "\n")
cat("Uniformidade (J') - Chuvoso:", pielou_chuvoso, "\n")
cat("Uniformidade (J') - Menos Chuvoso:", pielou_menos_chuvoso, "\n")

#
wilcox.test(shannon_chuvoso, shannon_menos_chuvoso, exact = FALSE)

library(ggplot2)

# Preparar dados para o gráfico
dados_long <- dados %>%
  tidyr::pivot_longer(cols = -c(especie, condicao), names_to = "local", values_to = "abundancia")

# Boxplot
ggplot(dados_long, aes(x = condicao, y = log2(abundancia+1), fill = condicao)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Comparação de Abundância entre Condições Climáticas",
       x = "Condição Climática",
       y = "Abundância")

# Curva de acumulação de espécies por condição
curva_chuvoso <- specaccum(dados_chuvoso)
curva_menos_chuvoso <- specaccum(dados_menos_chuvoso)

# Plotar as curvas
plot(curva_chuvoso, col = "blue", lwd = 2, xlab = "Amostras", ylab = "Número de Espécies",
     main = "Curvas de Acumulação por Condição")
lines(curva_menos_chuvoso, col = "orange", lwd = 2)
legend("bottomright", legend = c("Chuvoso", "Menos Chuvoso"), col = c("blue", "orange"), lty = 1)


library(ggplot2)

# Preparar os dados para o gráfico
diversidade <- data.frame(
  Periodo = c(rep("Chuvoso", length(shannon_chuvoso)), 
              rep("Menos Chuvoso", length(shannon_menos_chuvoso))),
  Shannon = c(shannon_chuvoso, shannon_menos_chuvoso),
  Pielou = c(pielou_chuvoso, pielou_menos_chuvoso)
)

# Realizar o teste estatístico
teste_shannon <- wilcox.test(
  Shannon ~ Periodo, data = diversidade, exact = FALSE
)
teste_pielou <- wilcox.test(
  Pielou ~ Periodo, data = diversidade, exact = FALSE
)

# Extrair os p-valores
p_shannon <- round(teste_shannon$p.value, 4)
p_pielou <- round(teste_pielou$p.value, 4)

# gráfico de Shannon-Wiener 
cairo_pdf("shannon_wiener_comparacao.pdf", width = 10, height = 8, family = "Helvetica", onefile = TRUE)
ggplot(diversidade, aes(x = Periodo, y = Shannon, fill = Periodo)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = paste("Comparação do Índice de Shannon-Wiener (H') entre Períodos",
                     "\nP-valor:", p_shannon),
       x = "Período Climático",
       y = "Índice de Shannon-Wiener") +
  scale_fill_manual(values = c("blue", "orange")) +
  theme(legend.position = "none")
dev.off()

# gráfico de Uniformidade 
cairo_pdf("pielou_comparacao.pdf", width = 10, height = 8, family = "Helvetica", onefile = TRUE)
ggplot(diversidade, aes(x = Periodo, y = Pielou, fill = Periodo)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = paste("Comparação do Índice de Uniformidade de Pielou (J') entre Períodos",
                     "\nP-valor:", p_pielou),
       x = "Período Climático",
       y = "Índice de Pielou") +
  scale_fill_manual(values = c("blue", "orange")) +
  theme(legend.position = "none")
dev.off()
################################################################################
# Diversidade por Local
# Carregar bibliotecas necessárias
library(vegan)

# Ler os dados diretamente do arquivo
dados <- readxl::read_excel("data/quantities-by-location-and-type-of-collection.xlsx", sheet = "Planilha2")

# Criar subconjuntos de dados para Carvão Velho e Mazagão
dados_long <- tidyr::pivot_longer(dados[, -10], cols = -especie, names_to = "local", values_to = "abundancia")

# Dividir em Carvão Velho e Mazagão
carvao <- dados_long[dados_long$local %in% c("CRV_01", "CRV_02", "CRV_03", "CRV_04"), ]
mazagao <- dados_long[dados_long$local %in% c("MZG_01", "MZG_02", "MZG_03", "MZG_04"), ]

carvao$local <- as.factor(carvao$local)
mazagao$local <- as.factor(mazagao$local)

# Verificar os níveis
carvao$local <- droplevels(carvao$local)
mazagao$local <- droplevels(mazagao$local)

# Teste de Kruskal-Wallis para Carvão Velho
kruskal_carvao <- kruskal.test(abundancia ~ local, data = carvao)
print(kruskal_carvao)

# Teste de Kruskal-Wallis para Mazagão
kruskal_mazagao <- kruskal.test(abundancia ~ local, data = mazagao)
print(kruskal_mazagao)

# Filtrar apenas os locais Carvão Velho e Mazagão
global_subset <- dados_long[dados_long$local %in% c("CRV_01", "CRV_02", "CRV_03", "CRV_04", "MZG_01", "MZG_02", "MZG_03", "MZG_04"), ]
global_subset$local <- as.factor(global_subset$local)
global_subset$local <- droplevels(global_subset$local)

# Teste de Mann-Whitney para comparar locais
mann_whitney_global <- wilcox.test(abundancia ~ grepl("CRV", local), data = global_subset) # "CRV" para Carvão e "MZG" para Mazagão
print(mann_whitney_global)

# Boxplot para Carvão Velho
ggplot(carvao, aes(x = local, y = log2(abundancia+1), fill = local)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Abundância de Mosquitos nos Pontos de Coleta - Carvão Velho",
       x = "Ponto de Coleta",
       y = "Abundância") +
  scale_fill_manual(values = c("blue", "orange", "green", "purple")) +
  theme(legend.position = "none")

# Boxplot para Mazagão
ggplot(mazagao, aes(x = local, y = log2(abundancia+1), fill = local)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Abundância de Mosquitos nos Pontos de Coleta - Mazagão",
       x = "Ponto de Coleta",
       y = "Abundância") +
  scale_fill_manual(values = c("blue", "orange", "green", "purple")) +
  theme(legend.position = "none")


library(ggplot2)

# Teste de Kruskal-Wallis para Carvão Velho
kruskal_carvao <- kruskal.test(abundancia ~ local, data = carvao)
p_carvao <- round(kruskal_carvao$p.value, 4) # Extrair o p-valor e arredondar

# Teste de Kruskal-Wallis para Mazagão
kruskal_mazagao <- kruskal.test(abundancia ~ local, data = mazagao)
p_mazagao <- round(kruskal_mazagao$p.value, 4) # Extrair o p-valor e arredondar

# Boxplot para Carvão Velho com p-valor
cairo_pdf("abundancia_comparacao_carvaoVelho.pdf", width = 10, height = 8, family = "Helvetica", onefile = TRUE)
ggplot(carvao, aes(x = local, y = log2(abundancia + 1), fill = local)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Abundância de Mosquitos nos Pontos de Coleta - Carvão Velho",
       x = "Ponto de Coleta",
       y = "log2(Abundância + 1)") +
  scale_fill_manual(values = c("blue", "orange", "green", "purple")) +
  theme(legend.position = "none") +
  annotate("text", x = 2, y = max(log2(carvao$abundancia + 1), na.rm = TRUE) + 1, 
           label = paste("P-valor:", p_carvao), color = "black", size = 5)
dev.off()

# Boxplot para Mazagão com p-valor
cairo_pdf("abundancia_comparacao_mazagao.pdf", width = 10, height = 8, family = "Helvetica", onefile = TRUE)
ggplot(mazagao, aes(x = local, y = log2(abundancia + 1), fill = local)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Abundância de Mosquitos nos Pontos de Coleta - Mazagão",
       x = "Ponto de Coleta",
       y = "log2(Abundância + 1)") +
  scale_fill_manual(values = c("blue", "orange", "green", "purple")) +
  theme(legend.position = "none") +
  annotate("text", x = 2, y = max(log2(mazagao$abundancia + 1), na.rm = TRUE) + 1, 
           label = paste("P-valor:", p_mazagao), color = "black", size = 5)
dev.off()

library(dunn.test)

# Teste de Dunn para comparações múltiplas entre os pontos de Mazagão
dunn_result <- dunn.test(mazagao$abundancia, mazagao$local, method = "bonferroni")
print(dunn_result)

# P-valor ajustado entre grupos específicos (exemplo: MZG_01 vs MZG_02)
p_dunn <- round(dunn_result$P.adjusted[1], 4) # Extraindo o p-valor ajustado da comparação específica

# Boxplot com a comparação significativa adicionada
cairo_pdf("abundancia_comparacao_par_mazagao.pdf", width = 10, height = 8, family = "Helvetica", onefile = TRUE)
ggplot(mazagao, aes(x = local, y = log2(abundancia + 1), fill = local)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Abundância de Mosquitos nos Pontos de Coleta - Mazagão",
       x = "Ponto de Coleta",
       y = "log2(Abundância + 1)") +
  scale_fill_manual(values = c("blue", "orange", "green", "purple")) +
  theme(legend.position = "none") +
  annotate("text", x = 1.5, y = max(log2(mazagao$abundancia + 1), na.rm = TRUE) + 1, 
           label = paste("MZG_01 vs MZG_02\nP-valor:", p_dunn), color = "black", size = 4)
dev.off()

# Diversidade

library(vegan)

# Removendo a coluna de espécie para análise
dados_diversidade <- dados[, c(-1,-10)]

# Cálculo do índice de Shannon-Wiener (H') para cada local
shannon <- apply(dados_diversidade, 2, function(x) diversity(x, index = "shannon"))

# Cálculo do índice de Simpson para cada local
simpson <- apply(dados_diversidade, 2, function(x) diversity(x, index = "simpson"))

# Exibindo os resultados
cat("Índice de Shannon-Wiener (H') por local:\n", shannon, "\n")
cat("Índice de Simpson por local:\n", simpson, "\n")

kruskal_shannon <- kruskal.test(shannon ~ colnames(dados_diversidade))
print(kruskal_shannon)

kruskal_simpson <- kruskal.test(simpson ~ colnames(dados_diversidade))
print(kruskal_simpson)

library(ggplot2)

# Criando um dataframe para os índices de diversidade
diversidade_df <- data.frame(
  Local = colnames(dados_diversidade),
  Shannon = shannon,
  Simpson = simpson
)

# Transformação para formato longo
diversidade_long <- tidyr::pivot_longer(diversidade_df, cols = c("Shannon", "Simpson"), 
                                        names_to = "Índice", values_to = "Valor")

# Gráfico de barras horizontal
cairo_pdf("shannon_simpson_comparacao.pdf", width = 10, height = 8, family = "Helvetica", onefile = TRUE)
ggplot(diversidade_long, aes(x = reorder(Local, Valor), y = Valor, fill = Índice)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Índices de Diversidade por Local",
       x = "Local",
       y = "Valor do Índice") +
  scale_fill_manual(values = c("blue", "orange"))
dev.off()

# Teste de normalidade
shapiro_test <- shapiro.test(dados_long$abundancia)
print(shapiro_test)

# Teste de homogeneidade de variância
library(car)
levene_test <- leveneTest(abundancia ~ local, data = dados_long)
print(levene_test)
