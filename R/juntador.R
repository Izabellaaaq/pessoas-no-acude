
library(dplyr)

ajeita.formato <- function(df, primeiras.linhas = 0, ultimas.linhas = 0, primeiras.colunas = 0, ultimas.colunas = 0) {
  df %>%
    rename_(horario.inicial = names(.)[1], horario.final = names(.)[2]) %>%
    head(nrow(.)-ultimas.linhas) %>%
    tail(nrow(.)-primeiras.linhas) %>%
    select(., one_of(names(.[(primeiras.colunas+1):(ncol(.)-ultimas.colunas)]))) %>%
    mutate(total = rowSums(.[grep(names(.), pattern = 'trajeto\\.[0-9]+')]))
}

filtra.colunas <- function(df, colunas, local) {
  local <- gsub('([a-z]+)\\.[a-z]+', '\\1', local)
  cbind(select(df, which(names(df) %in% colunas)),
        local = local)
}

junta.dfs <- function(categoria) {
  df <- data.frame()
  nomes.dfs.filtrados <- subset(nomes.dfs, grepl(paste0('\\.', categoria), nomes.dfs))
  colunas.categoria <- subset(nomes.colunas, grepl(paste0('\\.', categoria), nomes.colunas))
  for(nome in nomes.dfs.filtrados) {
    df <- rbind(df, filtra.colunas(get(nome), get(colunas.categoria), nome))
  }
  df
}


bobs.carros <- read.csv('dados/brutos/Contagens Açude Velho - Bob´s - Carros.csv') %>%
  ajeita.formato(ultimas.linhas = 1, ultimas.colunas = 2) %>%
  rename(onibus = ônibus, caminhao = caminhão)

bobs.ciclistas <- read.csv('dados/brutos/Contagens Açude Velho - Bob´s - Ciclistas.csv') %>%
  ajeita.formato(ultimas.linhas = 2, ultimas.colunas = 1) %>%
  mutate(calcada = 0) %>%
  subset(select=c(horario.inicial:mulher, calcada, total))

bobs.motos <- read.csv('dados/brutos/Contagens Açude Velho - Bob´s - Motos.csv') %>%
  ajeita.formato(ultimas.linhas = 1, ultimas.colunas = 1)

bobs.pedestres <- read.csv('dados/brutos/Contagens Açude Velho - Bob´s - Pedestres.csv') %>%
  ajeita.formato(ultimas.linhas = 2, ultimas.colunas = 1) %>%
  mutate(fora.da.faixa = 0) %>%
  subset(select=c(horario.inicial:mulher, fora.da.faixa, cachorro:total))

burrinhos.carros <- read.csv('dados/brutos/Contagens Açude Velho - Burrinhos - Carros.csv') %>%
  ajeita.formato(primeiras.linhas = 1, ultimas.linhas = 1, ultimas.colunas = 2) %>%
  rename(onibus = ônibus, caminhao = caminhão)

burrinhos.ciclistas <- read.csv('dados/brutos/Contagens Açude Velho - Burrinhos - Ciclistas.csv') %>%
  ajeita.formato(primeiras.linhas = 1, ultimas.linhas = 1) %>%
  rename(calcada = calçada)
burrinhos.ciclistas[22,3:ncol(burrinhos.ciclistas)] <- 0

burrinhos.motos <- read.csv('dados/brutos/Contagens Açude Velho - Burrinhos - Motos.csv') %>%
  ajeita.formato(ultimas.linhas = 1, ultimas.colunas = 2)

burrinhos.pedestres <- read.csv('dados/brutos/Contagens Açude Velho - Burrinhos - Pedestres.csv') %>%
  mutate(trajeto.8 = as.integer(replace(trajeto.8, trajeto.8 == 'protesto', NA))) %>%
  mutate(trajeto.8 = replace(trajeto.8, is.na(trajeto.8), max(trajeto.8, na.rm = T))) %>%
  mutate(trajeto.9 = as.integer(replace(trajeto.9, trajeto.9 == 'protesto', NA))) %>%
  mutate(trajeto.9 = replace(trajeto.9, is.na(trajeto.9), max(trajeto.9, na.rm = T))) %>%
  mutate(trajeto.12 = as.integer(replace(trajeto.12, trajeto.12 == 'protesto', NA))) %>%
  mutate(trajeto.12 = replace(trajeto.12, is.na(trajeto.12), max(trajeto.12, na.rm = T))) %>%
  mutate(trajeto.13 = as.integer(replace(trajeto.13, trajeto.13 == 'protesto', NA))) %>%
  mutate(trajeto.13 = replace(trajeto.13, is.na(trajeto.13), max(trajeto.13, na.rm = T))) %>%
  ajeita.formato(ultimas.linhas = 2, ultimas.colunas = 1)

jackson.carros <- read.csv('dados/brutos/Contagens Açude Velho - Jackson - Carros.csv') %>%
  ajeita.formato(ultimas.linhas = 1, ultimas.colunas = 1) %>%
  rename(onibus = ônibus, caminhao = caminhão)

jackson.ciclistas <- read.csv('dados/brutos/Contagens Açude Velho - Jackson - Ciclistas.csv') %>%
  ajeita.formato(ultimas.linhas = 2, ultimas.colunas = 1) %>%
  rename(calcada = na.calçada)

jackson.motos <- read.csv('dados/brutos/Contagens Açude Velho - Jackson - Motos.csv') %>%
  ajeita.formato(ultimas.linhas = 1, ultimas.colunas = 1)

jackson.pedestres <- read.csv('dados/brutos/Contagens Açude Velho - Jackson - Pedestres.csv') %>%
  ajeita.formato(ultimas.linhas = 2, ultimas.colunas = 1) %>%
  mutate(fora.da.faixa = replace(fora.da.faixa, is.na(fora.da.faixa), 0)) %>%
  mutate(cachorro = as.integer(as.character(cachorro))) %>%
  mutate(cachorro = replace(cachorro, is.na(cachorro), 0))


# Nomes das colunas presentes em cada tipo de data.frame
colunas.base <- c('horario.inicial', 'horario.final', 'total')
colunas.carros <- c(colunas.base, 'caminhao', 'onibus')
colunas.ciclistas <- c(colunas.base, 'mulher', 'calcada')
colunas.motos <- colunas.base
colunas.pedestres <- c(colunas.base, 'mulher', 'fora.da.faixa', 'cachorro')


# Vetores com os nomes das variáveis
nomes.colunas <- names(which(unlist(eapply(.GlobalEnv, is.character)))) %>%
  subset(., grepl('colunas\\.', .))
nomes.dfs <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))

# Juntando dfs de vários locais para uma só categoria
carros <- junta.dfs('carros') %>%
  mutate(carros = total - onibus - caminhao)
ciclistas <- junta.dfs('ciclistas')
motos <- junta.dfs('motos')
pedestres <- junta.dfs('pedestres')

# Agrupando os modais
modal.motorizado <- merge(carros, motos, by = c('horario.inicial', 'horario.final', 'local')) %>%
  rename(veiculos = total.x, motos = total.y) %>%
  mutate(total = veiculos + motos)

modal.sustentavel <- merge(ciclistas, pedestres, by = c('horario.inicial', 'horario.final', 'local')) %>%
  rename(mulheres.ciclistas = mulher.x, mulheres.pedestres = mulher.y, ciclistas = total.x, pedestres = total.y) %>%
  mutate(total = ciclistas + pedestres)

modais <- merge(modal.motorizado, modal.sustentavel, by = c('horario.inicial', 'horario.final', 'local')) %>%
  rename(motorizados = total.x, sustentaveis = total.y)

remove(colunas.base, colunas.carros, colunas.ciclistas, colunas.motos, colunas.pedestres, nomes.colunas, nomes.dfs)

write.csv(carros, 'dados/processados/carros.csv', row.names = F)
write.csv(ciclistas, 'dados/processados/ciclistas.csv', row.names = F)
write.csv(motos, 'dados/processados/motos.csv', row.names = F)
write.csv(pedestres, 'dados/processados/pedestres.csv', row.names = F)
write.csv(modal.motorizado, 'dados/processados/modal.motorizado.csv', row.names = F)
write.csv(modal.sustentavel, 'dados/processados/modal.sustentavel.csv', row.names = F)
write.csv(modais, 'dados/processados/modais.csv', row.names = F)
