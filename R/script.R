
# Carregando pacotes 
library(readxl)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

#ler base de dados
baseq01 <- read_xlsx("dados/bd_surveyquaest.xlsx")


#Recodificando as variáveis
baseq01 <- baseq01 %>%
  mutate(voto1_cat = case_when(voto1 == "Candidato 2" ~ "Candidato 2",
                               voto1 == "Candidato 1" ~ "Candidato 1",
                               voto1 == "Candidato 8" ~ "Candidato 8",
                               voto1 == "Candidato 5" ~ "Candidato 5",
                               voto1 == "Candidato 10" ~ "Candidato 10",
                               voto1 == "Ninguém/Branco/Nulo" ~ "Ninguém/Branco/Nulo",
                               voto1 == "NS/NR" ~ "NS/NR",
                               T ~ "Outros"),
         idadef = case_when(idade < 30 ~ "Jovem",
                            idade >29 & idade < 60 ~ "Adulto",
                            T ~ "Maduro"),
         rendafa = case_when(rendaf == "Até R$ 1.045,00 (até 1 SM)" | rendaf == "De R$ 1.046,00 a R$ 2.090,00 (+ de 1SM até 2 SM)" | rendaf == "De R$ 2.091,00 a R$ 3.135,00 (+ de 2SM até 3 SM)" | rendaf == "De R$ 3.136,00 a R$ 5.225,00 (+ de 3SM até 5 SM)" ~ "Até 5 SM",
                             rendaf == "De R$ 5.226,00 a R$ 10.450,00 (+ de 5SM até 10 SM)" ~ "Mais de 5SM até 10 SM",
                             T ~ "Mais de 10 SM"),
         escr = case_when(esc == "Ensino fundamental completo" | esc == "Ensino fundamental incompleto" ~ "Ensino Fund.",
                          esc == "Ensino médio completo" | esc == "Ensino médio incompleto" ~ "Ensino Medio",
                          esc == "Ensino superior completo" | esc == "Ensino superior incompleto" ~ "Ensino Sup.",
                          T ~ "Sem instrução e menos de 1 ano de estudo"),
         aval_gov_rec = case_when(aval_gov == "Boa" | aval_gov == "Ótima" ~ "Positiva",
                                  aval_gov == "Regular negativa" | aval_gov == "Regular positiva" ~ "Regular",
                                  aval_gov == "Péssima" | aval_gov == "Ruim" ~ "Negativa",
                                  T ~ "NS/NR"),
         aval_gov_rec = fct_relevel(aval_gov_rec, c("Positiva", "Regular", "Negativa", "NS/NR")),
         idadef = fct_relevel(idadef, c("Jovem", "Adulto", "Maduro")),
         rendafa = fct_relevel(rendafa, c("Até 5 SM", "Mais de 5SM até 10 SM", "Mais de 10 SM")),
         escr = fct_relevel(escr, c("Ensino Fund.", "Ensino Medio", "Ensino Sup.")),
         voto1_cat = fct_relevel(fct_infreq(voto1_cat), c("Outros", "Ninguém/Branco/Nulo", "NS/NR"), after = Inf)
         
  )


###########################################################################################
#  QUESTÃO 1 - Crie uma função em alguma linguagem de programação, preferencialmente em R #
#  ou Python, que automatize a construção de tabelas de contingência.                     #
#  O objetivo é identificar se há uma diferença sociodemográfica na intenção de voto.     #
#  Em outras palavras, por ex.: As mulheres e os homens estão votando no mesmo candidato? #
###########################################################################################

source("R/funcoes.R")

var_demograficas <- c("sexo", "idadef", "escr", "rendafa")

# relatorio de tabelas automatizado
rmarkdown::render("R/tab_contigencia_relatorio.Rmd", params = list(
  dados = baseq01,
  var_demografica = var_demograficas,
  var_voto = "voto1_cat"
))

# ou rodar idividualmente para cada variavel
tabela_contig(dados = baseq01, sexo, voto1_cat)
tabela_contig(dados = baseq01, idadef, voto1_cat)
tabela_contig(dados = baseq01, escr, voto1_cat)
tabela_contig(dados = baseq01, rendafa, voto1_cat)


######################################################################
#  QUESTAO 2 - Ainda com essa base de dados, construa dois gráficos. #
######################################################################

# 2.1) O primeiro gráfico será da variável intenção de voto.

int_voto <- baseq01 %>%
  count(voto1_cat) %>%
  mutate(prop = n/sum(n),
         indicador_ns_nr = voto1_cat == "NS/NR")

ggplot(int_voto, aes(x = fct_rev(voto1_cat), y = prop, fill = indicador_ns_nr)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(prop, 1)),
            hjust = -0.2,
            size = 4) +
  scale_y_continuous(limits = c(0, min(max(int_voto$prop) + 0.1, 1))) +
  labs(x = "", y = "") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = '#1fa8a9', "TRUE" = '#969696')) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(vjust = 0.5, size = 14),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 10, angle = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none"
  )

# 2.2) Já o segundo, plot um gráfico que represente o cruzamento entre as variáveis intenção de voto e avaliação do governo. 
# Quem avalia o governo de forma positiva, vota em qual candidato ? E quem avalia de forma negativa ?

aval_cand <- baseq01 %>%
  count(aval_gov_rec, voto1_cat) %>%
  group_by(aval_gov_rec) %>%
  mutate(prop = n/sum(n))

ggplot(aval_cand, aes(x = fct_rev(aval_gov_rec), y = prop, fill = aval_gov_rec)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(prop, 0.1)),
            hjust = -0.2,
            size = 4) +
  scale_y_continuous(limits = c(0, min(max(aval_cand$prop) + 0.1, 1))) +
  scale_fill_manual(values = c('#1fa8a9', 'orange','#cc0000','#969696','#1fa8a9', 'orange','#cc0000','#969696')) +
  labs(x = "", y = "") +
  coord_flip() +
  facet_wrap(~voto1_cat, nrow = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(vjust = 0.5, size = 14),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 11, angle = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none"
  )










