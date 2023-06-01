#### Instalação e Carga de bibliotecas ####
if(!require(pacman)){ install.packages('pacman') }
if(!require(ggplot2)){ install.packages('ggplot2') }
if(!require(waffle)){ install.packages('waffle') }
if(!require(geobr)){ install.packages('geobr', dependencies = TRUE) }

# pacman::p_load(rio, ggspatial, lubridate, tidyverse, abjutils, magick, geobr, ggplot2, zoo, waffle)
pacman::p_load(rio, tidyverse, geobr, ggspatial, waffle)

library(rio)
library(ggspatial)
#library(lubridate)
library(tidyverse)
#library(abjutils)
#library(magick)
library(geobr)
#library(ggplot2)
#library(zoo)
library(waffle)

# packageVersion('rio')       # 0.5.29
# packageVersion('ggspatial') # 1.1.8
# packageVersion('lubridate') # 1.9.2
# packageVersion('tidyverse') # 2.0.0
# packageVersion('abjutils')  # 0.3.2
# packageVersion('magick')    # 2.7.4
# packageVersion('geobr')     # 1.7.0
# packageVersion('ggplot2')   # 3.4.1
# packageVersion('zoo')       # 1.8.11
# packageVersion('waffle')    # 0.7.0

#### Carga de dados brutos ####
dadosbrutos = import("dados/1680959547993.metadata.tsv")
head(dadosbrutos)

# Ver dados
# view(dadosbrutos)

# Ver colunas
names(dadosbrutos)

# Para tirar colunas
dadosbrutos <- dadosbrutos %>% 
  select(-strain, -virus, -gisaid_epi_isl, -genbank_accession, -region, -country,
         -division, -region_exposure:-host, -originating_lab:-purpose_of_sequencing,
         -Nextstrain_clade, -GISAID_clade)

# Renomear variável location para municipality
dadosbrutos <- dadosbrutos %>%
  rename(municipality = location)
head(dadosbrutos)

# Exportar a tabela dadosbrutos para ser curada no excel (municipios e idades)
export(dadosbrutos, "dados/dados_brutos.txt")

#### Curadoria automática dos dados brutos ####

dados_brutos <- import("dados/dados_brutos.txt")
head(dados_brutos)
str(dados_brutos)

dados_brutos <- dados_brutos %>%
  mutate(sex = if_else(sex == 'Male', 'Masculino', sex),
         sex = if_else(sex == 'Female', 'Feminino', sex),
         sex = if_else(sex == 'unknown', 'Não Informado', sex)) %>%
  mutate(age_month = case_when(grepl('months', age, fixed = TRUE) ~ gsub('months', '', age),
                               grepl('month', age, fixed = TRUE) ~ gsub('month', '', age),
                               TRUE ~ '0')) %>%
  mutate(age = case_when(grepl('months', age, fixed = TRUE) ~ '0',
                         grepl('month', age, fixed = TRUE) ~ '0',
                         TRUE ~ age))

dados_brutos <- dados_brutos %>%
  mutate(age = as.numeric(age)) %>%
  mutate(age_month = as.numeric(age_month)) %>%
  mutate(age_month = age_month/12) %>% 
  mutate(age = if_else(age_month > 0, age_month, age))
head(dados_brutos)
str(dados_brutos)

dados_brutos <- dados_brutos %>%
  mutate(municipality_lower = str_to_lower(municipality)) %>% 
  mutate(municipality_lower = str_trim(municipality_lower)) %>% 
  mutate(municipality_lower = str_squish(municipality_lower)) %>% 
  mutate(municipality_lower = abjutils::rm_accent(municipality_lower))
head(dados_brutos)

municipios_ms = read_municipality(code_muni = "MS", year = 2020)
municipios_ms$geom = NULL
municipios_ms = municipios_ms %>%
  select(code_muni, name_muni) %>%
  mutate(name_lower = str_to_lower(name_muni)) %>% 
  mutate(name_lower = str_trim(name_lower)) %>% 
  mutate(name_lower = str_squish(name_lower)) %>% 
  mutate(name_lower = abjutils::rm_accent(name_lower)) %>%
  rename(code = code_muni, name = name_muni)
head(municipios_ms)

dados_brutos = left_join(dados_brutos, municipios_ms, by = c("municipality_lower" = "name_lower"))
head(dados_brutos)

dados_brutos = dados_brutos %>%
  select(code, date, name, age, sex, pangolin_lineage) %>%
  rename(municipality = name, lineage = pangolin_lineage)
head(dados_brutos)

export(dados_brutos, "dados/dados_curados.txt")

dados_curados <- import("dados/dados_curados.txt")        
head(dados_curados)
str(dados_curados)

#### [Plots] ####

#### Plot: Gráfico barras de sexos ####
# Criar tabela sexo
tabela_sexo <- dados_curados %>%
  count(sex) %>% 
  rename(Sexo = sex, Casos = n) %>% 
  mutate('% de Casos' = round(100 * Casos / sum(Casos), 2))

export(tabela_sexo, "resultados/tabela_sexo.xlsx")

# Plotar gráfico da variável sexo
tabela_sexo <- tabela_sexo %>%
  mutate(Sexo = factor(Sexo,
                       levels = c("Masculino", "Feminino", "Não Informado"),
                       # labels = c("Masculino", "Feminino", "Não Informado")
  ))

grafico_sexo <- ggplot(data = tabela_sexo) +
  geom_bar(aes(x = Sexo, y = Casos, fill = Sexo),
           width = 0.6,
           stat  = "identity") +
  geom_text(aes(x = Sexo,
                y = Casos,
                label = Casos),
            vjust = -.75,
            size = 2.25,
            color = '#6b705c') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 560),
                     breaks = seq(0, 560, 50)) +
  labs(# title = "Número de amostras sequenciadas segundo o sexo",
       # subtitle = "Amostras sequenciadas para SARS-COV-2 em Mato Grosso do Sul no ano de 2022",
       # caption = "Dados extraídos do GISAID",
       x = "Sexo do paciente",
       y = "Nº de amostras sequenciadas",
       fill = "Sexo") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, color = "black", face = "bold"),
        panel.grid.major = element_line(linewidth = 0.2, colour = "#EEF0F3", linetype = "dashed"),
        panel.grid.minor = element_blank())

grafico_sexo

ggsave(plot = grafico_sexo,
       filename = paste0("resultados/gráfico_sexo_", Sys.Date(), ".png"),
       width = 8, height = 5, dpi = 500)

#### Plot: Gráfico barras por categorias e sexo ####

table_age_sex = dados_curados %>%
  filter(sex != 'Não Informado') %>%
  count(sex, cat_idade) %>%
  rename(cat_age = cat_idade, count = n)

table_age_sex = as.data.frame(table_age_sex)
head(table_age_sex)

limit_max = max(table_age_sex$count) + 20

colors = c('#fe0084',
           '#008fd5')

ggplot(table_age_sex, aes(x = cat_age)) +
  geom_col(data = subset(table_age_sex, sex == 'Feminino'), aes(y = -count, fill = 'Feminino')) +
  geom_col(data = subset(table_age_sex, sex == 'Masculino'), aes(y = count, fill = 'Masculino')) +
  geom_text(data = subset(table_age_sex, sex == 'Feminino'), aes(y = -count, label = count), size = 3, hjust = 1.5, color = '#6b705c') +
  geom_text(data = subset(table_age_sex, sex == 'Masculino'), aes(y = count, label = count), size = 3, hjust = -0.5, color = '#6b705c') +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(-limit_max, limit_max, by = 10),
                     limits = c(-limit_max, limit_max),
                     ) +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values = colors) +
  coord_flip() +
  labs(x = NULL, y = 'Número de amostras') +
  theme(legend.title = element_blank(),
        # legend.position = "bottom", # right | bottom
        legend.key = element_blank(),
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm'), # top, right, bottom, left
        axis.text.x = element_text(size = 7, colour = "#000000"),
        axis.text.y = element_text(size = 9, colour = "#000000"),
        axis.title.x = element_text(size = 14, colour = "#000000"),
        axis.line.x = element_line(linewidth = 0.15, colour = "#000000"),
        axis.ticks.x = element_line(linewidth = 0.15,  colour = "#000000"),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(1, 'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        # panel.grid.major.y = element_line(linewidth = 0.25, colour = "#c8c8b9"),
        # panel.border = element_rect(fill = NA, colour = "#000000", linewidth = .75)
  ) -> data_plot_sex_age

data_plot_sex_age

ggsave(plot = data_plot_sex_age,
       filename = paste0("resultados/grafico_cat_age_sex_", Sys.Date(), ".png"),
       width = 8, height = 4.5, dpi = 600)

#### Plot: Gráfico waffle de sexos ####

labels = as.character(tabela_sexo$Sexo)
values = as.numeric(tabela_sexo$Casos)

val_names <- sprintf("%s (%s)", labels, scales::percent(values/sum(values)))
names(values) <- val_names

waffle(values, rows = 15, size = .3, legend_pos = "bottom",
       colors = c('#fe0084',
                  '#008fd5',
                  '#6d6d6d')) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 6.5),
        legend.key.size = unit(.4, 'cm'),
        plot.background = element_rect(fill = 'white', colour = NA)) -> grafico_waffle

grafico_waffle

ggsave(plot = grafico_waffle,
       filename = paste0("resultados/grafico_waffle_", Sys.Date(), ".png"),
       width = 8, height = 2.5, dpi = 500)

#### Plot: Gráfico barras por categorías de idade ####
min(dados_curados$age)
max(dados_curados$age)

dados_curados <- dados_curados %>%
  mutate(cat_idade = case_when(age < 10 ~ "< 10 anos",
                               age >= 10 & age <20 ~ "10 a 19 anos",
                               age >= 20 & age <30 ~ "20 a 29 anos",
                               age >= 30 & age <40 ~ "30 a 39 anos",
                               age >= 40 & age <50 ~ "40 a 49 anos",
                               age >= 50 & age <60 ~ "50 a 59 anos",
                               age >= 60 & age <70 ~ "60 a 69 anos",
                               age >= 70 & age <80 ~ "70 a 79 anos",
                               age >= 80 & age <90 ~ "80 a 89 anos",
                               age >= 90 ~ "> 90 anos"))

dados_curados <- dados_curados %>%
  mutate(cat_idade = factor(cat_idade,
                            levels = c("< 10 anos", "10 a 19 anos", "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos", "60 a 69 anos", "70 a 79 anos", "80 a 89 anos", "> 90 anos"),
                            # labels = c("< 10 anos", "10 a 19 anos", "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos", "60 a 69 anos", "70 a 79 anos", "80 a 89 anos", "> 90 anos")
  ))
head(dados_curados)

dados_curados %>%
  count(cat_idade)

# Plotar gráfico da variável categorias de idade
grafico_idade <- ggplot(data = dados_curados, aes(x = cat_idade, fill = cat_idade)) +
  geom_bar(stat  = 'count',
           width = 0.6) +
  geom_text(stat = 'count',
            aes(label = after_stat(count)),
            vjust = -.75,
            size = 2.25,
            color = '#6b705c') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 175),
                     breaks = seq(0, 175, 10)) +
  labs(# title = "Número de amostras sequenciadas por categoria de idade",
       # subtitle = "Amostras sequenciadas para SARS-COV-2 em Mato Grosso do Sul no ano de 2022",
       # caption = "Dados extraídos do GISAID",
       x = "Categoria de idade",
       y = "Nº de amostras sequenciadas") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(size = 20, color = "black", face = "bold"),
        panel.grid.major = element_line(linewidth = 0.2, colour = "#EEF0F3", linetype = "dashed"),
        panel.grid.minor = element_blank())

grafico_idade

# Expotar o grafico_idade
ggsave(plot = grafico_idade,
       filename = paste0("resultados/gráfico_cat_idade_", Sys.Date(), ".png"),
       width = 7, height = 5, dpi = 500)

#### Plot: Gráfico barras por semana epidemiológica (com média móvel centralizada) ####
# Dados SE
dados_se = dados_curados %>%
  count(epiweek(date)) %>% 
  rename(casos = n, se = 'epiweek(date)') %>%
  arrange(se) %>%
  mutate(mm_centro = zoo::rollmean(casos, 
                                   k = 3,
                                   fill = 0,        
                                   align = "right"))
head(dados_se)

grafico_se_mmc <- ggplot(data = dados_se, aes(x = se)) +
  geom_histogram(aes(y = casos),
                 stat = "identity",
                 fill = "#00CED1") +
  geom_text(stat = 'identity',
            aes(y = casos, label = casos),
            vjust = -.75,
            size = 2,
            color = '#6b705c') +
  geom_line(aes(y = mm_centro),
            linewidth = 0.75,
            color = '#707070') +
  scale_x_continuous(breaks = seq(0, 53, 1)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 130),
                     breaks = seq(0, 130, 5)) +
  labs(# title = "Amostras sequenciadas por semana epidemiológica",
       # subtitle = "Amostras sequenciadas para SARS-COV-2 em Mato Grosso do Sul no ano de 2022",
       # caption = "Dados extraídos do GISAID",
       x = "Semana epidemiológica",
       y = "Nº de amostras sequenciadas") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(size = 20, color = "black", face = "bold"),
        panel.grid.major = element_line(linewidth = 0.2, colour = "#EEF0F3", linetype = "dashed"),
        panel.grid.minor = element_blank())

grafico_se_mmc

# Expotar o grafico_se_mmc
ggsave(plot = grafico_se_mmc,
       filename = paste0("resultados/gráfico_se_mmc_", Sys.Date(), ".png"),
       width = 9, height = 5, dpi = 500)

#### Plot: Gráfico barras por semana epidemiológica ####
grafico_se <- ggplot(data = dados_curados, aes(x = epiweek(date))) +
  geom_histogram(stat = "count",
                 fill = "#00CED1") +
  geom_text(stat = 'count',
            aes(label = after_stat(count)),
            vjust = -.75,
            size = 2,
            color = '#6b705c') +
  scale_x_continuous(breaks = seq(0, 53, 1)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 130),
                     breaks = seq(0, 130, 5)) +
  labs(# title = "Amostras sequenciadas por semana epidemiológica",
       # subtitle = "Amostras sequenciadas para SARS-COV-2 em Mato Grosso do Sul no ano de 2022",
       # caption = "Dados extraídos do GISAID",
       x = "Semana epidemiológica",
       y = "Nº de amostras sequenciadas") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(size = 20, color = "black", face = "bold"),
        panel.grid.major = element_line(linewidth = 0.2, colour = "#EEF0F3", linetype = "dashed"),
        panel.grid.minor = element_blank())

grafico_se

# Expotar o grafico_se
ggsave(plot = grafico_se,
       filename = paste0("resultados/gráfico_se_", Sys.Date(), ".png"),
       width = 9, height = 5, dpi = 500)

#### Plot: Gráfico barras dos sequenciamentos por mês ####

# Grafico de seq por mês segundo data da coleta
grafico_mes <- ggplot(data = dados_curados, aes(x = month(date, label = TRUE))) +
  geom_histogram(stat = "count",
                 fill = "#FF6A6A") +
  geom_text(stat = 'count',
            aes(label = after_stat(count)),
            vjust = -.75,
            size = 2.25,
            color = '#6b705c') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 250),
                     breaks = seq(0, 250, 20)) +
  labs(# title = "Amostras sequenciadas segundo mês de coleta",
       # subtitle = "Amostras sequenciadas para SARS-COV-2 em Mato Grosso do Sul no ano de 2022",
       # caption = "Dados extraídos do GISAID",
       x = "Mês",
       y = "Nº de amostras sequenciadas") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 20, color = "black", face = "bold"),
        panel.grid.major = element_line(linewidth = 0.2, colour = "#EEF0F3", linetype = "dashed"),
        panel.grid.minor = element_blank())

grafico_mes

# Expotar o grafico_mes
ggsave(plot = grafico_mes,
       filename = paste0("resultados/gráfico_mês_", Sys.Date(), ".png"),
       width = 8, height = 5, dpi = 500)

#### Plot: Mapa geral ####

# municípios onde temos amostras sequenciadas
munic <- table(dados_curados$municipality)
class(munic)

munic = as.data.frame(munic)
head(munic)
class(munic)

# rename(munic, municipio = Var1, casos = Freq)

munic = munic %>%
  rename(municipio = Var1, casos = Freq) %>%
  filter(municipio != "")
head(munic)

# mapa estado
# Carregar microrregiões
municipios = read_municipality(code_muni = "MS", year = 2020)

municipios_mapa = left_join(municipios, munic, by = c("name_muni" = "municipio"))

# Para zerar os NAs
municipios_mapa = municipios_mapa %>%
  mutate(casos = if_else(is.na(casos), 0, casos))

# Municipios sem dados
municipios_mapa_blank = municipios_mapa %>%
  filter(casos == 0)

# Mapa 1
mapa_ms_amos_seq <- ggplot() +
  geom_sf(data = municipios_mapa,
          aes(fill = casos),
          color = "black",
          size = .45,
          show.legend = T) +
  geom_sf(data = municipios_mapa_blank, fill = "white") +
  geom_sf_text(data = municipios_mapa, aes(label = name_muni), size = 3) +
  scale_fill_distiller(palette = "PuRd", direction = 1) +    # YlGnBu paleta de outras cores
  labs(# title = "Quantidade de amostras sequenciadas em Mato Grosso do Sul, 2022",
       # caption = "Fonte: GISAID",
       fill = "Nº de amostras sequenciadas") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 13),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        title = element_text(size = 30)) +
  annotation_north_arrow(style = north_arrow_nautical(), pad_x = unit(8.5, "cm")) +
  annotation_scale()

mapa_ms_amos_seq

ggsave(plot = mapa_ms_amos_seq,
       filename = paste0("resultados/mapa_ms_amos_seq_", Sys.Date(), ".png"),
       width = 18, height = 12, dpi = 300)

#### Plot: Mapa por categorías ####

# Grouping
municipios_mapa_range = municipios_mapa %>%
  mutate(casos_range = case_when(casos == 0 ~ '0',
                                 casos >= 1 & casos < 5 ~ '1 - 4',
                                 casos >= 5 & casos < 10 ~ '5 - 9',
                                 casos >= 10 & casos < 15 ~ '10 - 14',
                                 casos >= 15 & casos < 20 ~ '15 - 19',
                                 casos >= 20 & casos < 25 ~ '20 - 24',
                                 casos >= 25 & casos < 30 ~ '25 - 29',
                                 casos >= 30 & casos < 35 ~ '30 - 34',
                                 casos >= 35 & casos < 40 ~ '35 - 39',
                                 casos >= 40 & casos < 45 ~ '40 - 44',
                                 casos >= 50 & casos < 100 ~ '50 - 99',
                                 casos >= 100 & casos < 200 ~ '100 - 199',
                                 casos >= 200 & casos < 300 ~ '200 - 299',
                                 casos >= 300 ~ '300 ou mais'))

municipios_mapa_range <- municipios_mapa_range %>%
  mutate(casos_range = factor(casos_range,
                            levels = c("0",
                                       "1 - 4",
                                       "5 - 9",
                                       "10 - 14",
                                       "15 - 19",
                                       "20 - 24",
                                       "25 - 29",
                                       "30 - 34",
                                       "35 - 39",
                                       "40 - 44",
                                       "50 - 99",
                                       "100 - 199",
                                       "200 - 299",
                                       "300 ou mais")))
head(municipios_mapa_range)

colors = c('#ffffff',
           '#ffc6ff',
           '#fdffb6',
           '#9bf6ff',
           '#caffbf',
           '#a0c4ff',
           '#ffd670',
           '#e9ff70',
           '#fff1e6',
           '#84dcc6',
           '#ff70a6',
           '#a5ffd6',
           '#b3dee2',
           '#eaf2d7',
           '#e27396',
           '#ea9ab2',
           '#efcfe3',
           '#ffa69e',
           '#ff686b',
           '#70d6ff',
           '#ff9770')

mapa_ms_amos_seq_range <- ggplot() +
  geom_sf(data = municipios_mapa_range,
          aes(fill = casos_range),
          color = "black",
          size = .45,
          show.legend = T) +
  geom_sf_text(data = municipios_mapa_range, aes(label = name_muni), size = 3) +
  scale_fill_manual(values = colors) +
  labs(# title = "Quantidade de amostras sequenciadas em Mato Grosso do Sul, 2022",
       # caption = "Fonte: GISAID",
       fill = "Nº de amostras sequenciadas") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 15),
        legend.key = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 13),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm'), # top, right, bottom, left
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        title = element_text(size = 30)) +
  annotation_north_arrow(style = north_arrow_nautical(), pad_x = unit(8.5, "cm")) +
  annotation_scale()

mapa_ms_amos_seq_range

ggsave(plot = mapa_ms_amos_seq_range,
       filename = paste0("resultados/mapa_ms_amos_seq_range_", Sys.Date(), ".png"),
       width = 18, height = 12, dpi = 300)

#### Plot: Gráfico barras da distribuição das linhagens por SE ####

# Linhagens por SE
dados_curados = dados_curados %>%
  mutate(se = epiweek(date)) %>%
  mutate(se = str_pad(se, 2, pad = '0'))
head(dados_curados)

data_lineage = dados_curados %>%
  select(lineage, se) %>%
  group_by(lineage, se) %>%
  summarise(count = n()) # , .groups = 'drop'
head(data_lineage)

data_long = data_lineage %>%
  group_by(se) %>%
  mutate(count = 100 * count / sum(count))
head(data_long)

# if(!require(reshape2)){ install.packages('reshape2') }
# data_casted = dcast(data_long, lineage ~ se, value.var = 'count')
# data_casted[is.na(data_casted)] = 0
# head(data_casted)

#data_long$week = as.character(data_long$week)

#data_long$week = factor(data_long$week,
#                        levels = sort(unique(data_long$week), decreasing = FALSE))

ggplot(data_long, aes(x = se, y = count, fill = lineage)) +
  geom_bar(position = "fill", stat = "identity", colour = "black", linewidth = 0.3) +
  scale_y_continuous(name = "Abundância relativa (%)",
                     expand = c(0, 0),
                     breaks = seq(0, 1, .05),
                     labels = scales::percent_format()) +
  scale_x_discrete(name = "Semana Epidemiologica 2022") +
  theme(legend.title = element_blank(),
        legend.position = "right", # right | bottom
        legend.key = element_blank(),
        legend.key.size = unit(0.125, "cm"),
        legend.text = element_text(size = 2.25),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm'), # top, right, bottom, left
        axis.text.x = element_text(size = 2.75, colour = "#000000", hjust = 1, vjust = 1, angle = 45),
        axis.text.y = element_text(size = 4, colour = "#000000"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.25, colour = "#000000"),
        panel.border = element_rect(fill = NA, colour = "#000000", linewidth = .75)) -> data_plot

data_plot

ggsave(plot = data_plot,
       filename = paste0("resultados/plot_bar_lineages_", Sys.Date(), ".png"),
       width = 4.5, height = 2, dpi = 800, units = 'in', device = 'png')
#ggsave(plot = data_plot,
#       filename = paste0("resultados/plot_bar_lineages_", Sys.Date(), ".svg"),
#       width = 4.5, height = 2, dpi = 800, units = 'in', device = 'svg')

#### Plot: Mapa de calor das linhagens por SE ####


ggplot(data_long, aes(se, forcats::fct_rev(lineage), fill = count, size = count)) +
  geom_point(shape = 21, stroke = 0, alpha = 1) +
  scale_x_discrete(position = "top") +
  scale_radius(range = c(.75, 8)) +
  scale_fill_gradient(low = "#fca311", # orange
                      high = "#007200", # green
                      # limits = c(0, 1),
                      # breaks = c(0, .5, 1),
                      # labels = c("Great", "OK", "Bad")
  ) +
  labs(size = '# Genomas', fill = NULL, x = 'Semana Epidemiologica 2022', y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 7, color = "black", hjust = .5, vjust = .5, angle = 45),
        axis.text.y = element_text(size = 5, color = "black"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        plot.background = element_rect(fill = "#FFFFFF", color = NA)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .5),
                             label.position = "bottom",
                             # title.position = "right",
                             nrow = 1,
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) -> heatmap_plot

heatmap_plot

ggsave(plot = heatmap_plot,
       filename = paste0("resultados/plot_heatmap_lineages_", Sys.Date(), ".png"),
       width = 7, height = 5, dpi = 800, units = 'in', device = 'png')

#### Plot: Gráfico de barras da abundância de linhagens ####

subdata = dados_curados %>%
  select(lineage) %>%
  group_by(lineage) %>%
  summarise(count = n()) # , .groups = 'drop'
head(subdata)

limit_y = ceiling(max(subdata$count, na.rm = TRUE)) + 5

ggplot(data = subdata, aes(x = lineage, y = count)) + # , fill = lineage
  geom_bar(stat = 'identity', width = 0.7, fill = "#2a9d8f") + # steelblue
  geom_text(aes(x = lineage,
                y = count,
                label = count),
            vjust = -.5,
            size = 1.5,
            color = '#6b705c') +
  scale_y_continuous(name = "Frequência",
                     expand = c(0, 0),
                     limits = c(0, limit_y),
                     breaks = seq(0, limit_y, by = 5)) +
  scale_x_discrete(name = "Linhagens") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.text.x = element_text(colour = 'black', size = 4, hjust = 1, vjust = 1, angle = 45),
        axis.text.y = element_text(colour = 'black', size = 5),
        axis.title.x = element_text(colour = 'black', size = 8),
        axis.title.y = element_text(colour = 'black', size = 8),
        panel.grid.major = element_line(linewidth = 0.2, colour = "#EEF0F3", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey50", linewidth = .5),
        axis.ticks = element_line(colour = 'grey50', linewidth = 0.25),
        axis.ticks.length.y = unit(.05, "cm"),
        axis.ticks.length.x = unit(.05, "cm"),
        plot.background =  element_rect(fill = "#FFFFFF", color = NA)) -> data_plot_h

data_plot_h

ggsave(plot = data_plot_h,
       filename = paste0("resultados/gráfico_abund_linhagens_horizontal_", Sys.Date(), ".png"),
       width = 5.5, height = 3.5, dpi = 800)

limit_y = ceiling(max(subdata$count, na.rm = TRUE)) + 15

ggplot(data = subdata, aes(x = lineage, y = count)) + # , fill = lineage
  geom_bar(stat = 'identity', width = 0.6, fill = "#2a9d8f") + # steelblue
  geom_text(aes(x = lineage,
                y = count,
                label = count),
            hjust = -.5,
            size = 1.25,
            color = '#6b705c') +
  scale_y_continuous(name = "Frequency",
                     expand = c(0, 0),
                     limits = c(0, limit_y),
                     breaks = seq(0, limit_y, by = 10)) +
  scale_x_discrete(name = "Lineages", limits = rev) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.text.x = element_text(colour = 'black', size = 4),
        axis.text.y = element_text(colour = 'black', size = 4),
        axis.title.x = element_text(colour = 'black', size = 8),
        axis.title.y = element_text(colour = 'black', size = 8),
        panel.grid.major = element_line(linewidth = 0.2, colour = "#EEF0F3", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey50", linewidth = .5),
        axis.ticks = element_line(colour = 'grey50', linewidth = 0.25),
        axis.ticks.length.y = unit(.05, "cm"),
        axis.ticks.length.x = unit(.05, "cm"),
        plot.background =  element_rect(fill = "#FFFFFF", color = NA)) -> data_plot_v

ggsave(plot = data_plot_v,
       filename = paste0("resultados/gráfico_abund_linhagens_vertical_", Sys.Date(), ".png"),
       width = 2.25, height = 3.5, dpi = 800)
