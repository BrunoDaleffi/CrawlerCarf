0#' All job on a only one command
#'
#' @param date_min minimum date to explore on format "09/2018"
#' @param date_max maximum date to explore on format "09/2018"
#' @param path_html directory where html will be save
#' @param path_pdf directory where pdfs will be save
#' 
#' @return html's downloads, pdf's downloads, tidy data
#'
#' @export
all_job <- function(date_min, date_max, path_html, path_pdf, max_page = Inf, parallel = T){
  start <- Sys.time()
  
  print(str_c('Inicio: ', start))
  
  #padroniza os meses
  mesano_min <- stringr::str_replace_all(date_min, '/','.')
  mesano_max <- stringr::str_replace_all(date_max, '/','.')
  
  #nome da pasta a ser criada dentro de path_html
  path1 <- glue::glue(path_html, "/", mesano_min, '-ate-',mesano_max)
  
  #baixa as paginas e seus respectivos htmls
  download_decision_period(date_min = date_min, 
                           date_max = date_max,
                           max_page = max_page,
                           path = path_html)
  
  print(str_c('htmls baixados: ', Sys.time()))
  
  #pega o caminho de cada pagina
  caminho_paginas <- str_c(path1,'/',list.files(path1))
  
  pega_arquivos <- function(path){
    arquivo <- str_c(path,'/',list.files(path))
    return(arquivo)
  }
  
  #lista do caminho de todos os arquivos.
  caminho_arquivos <- purrr::map(caminho_paginas, pega_arquivos) %>% unlist()
  
  
  #parse
  tabela<- parse_decision(caminho_arquivos,verbose = F)
  
  print(str_c('Informações dos htmls colhidas: ', Sys.time()))
  
  #prepara pro comprot
  
  #Pega os ids decisoes
  id_dec <- tabela$decisions$id_lawsuit %>% unique()
  
  # #prepara o plano de execucao future
  if(parallel) plan(multicore)
  
  #roda o comprot
  comprot <- future_get_comprot(id_dec) 
  
  #salva a tabela do comprot
  saveRDS(comprot,'data/d_comprot_ultimo.rds')
  
  print(str_c('Comprot baixado e salvo: ', Sys.time()))
  
  #pega o id de cada caminho de cada arquivo
  aux_file <- data_frame(file = caminho_arquivos) %>%
    mutate(id_decision2 = str_replace_all(file, '.html','') %>% str_extract('[0-9]+$')) %>%
    filter(!is.na(id_decision2))
  
  #consolida informações
  tabelao <- consolidate(pages = tabela$pages, 
                         decisions = tabela$decisions,
                         comprot = comprot) %>%
    mutate(rowname = row.names(.),
           n= 1,
           id_decision2 = clean_lawsuit(id_decision))  %>%
    left_join(aux_file, 'id_decision2')%>%
    left_join(tabela$decisions %>% select(id_lawsuit, id_decision, txt_decision = decision), by = c('id_lawsuit' ='id_lawsuit', 'id_decision'= 'id_decision')) %>%
    select(rowname,
           arq = file,
           n_processo = id_lawsuit,
           n_acordao = id_decision,
           tipo_recurso = type_appeal,
           contribuinte = taxpayer,
           relator = rapporteur,
           txt_ementa = summary,
           txt_decisao = txt_decision,
           data_pub = date_publication,
           tipo_pessoa = type_party,
           cn = cpf_cnpj,
           uf = state,
           votacao = vote,
           resultado = result,
           tributos = taxes,
           tipo = type,
           camara_turma = chamber,
           decisao = decision,
           n = n,
           -id_decision2)  
  
  
  print(str_c('Tabela final pronta: ', Sys.time()))
  
  #pega os pdf's dos processos
  ids <- tabelao$n_processo %>% map_chr(clean_lawsuit)
  
  #baixa os pdfs
  future_download_lawsuit_new(id= ids, path = path_pdf)
  
  #nova classifica turmas e secoes e camaras. Ele classifica usando o pdf baixado com future_download_lawsuit_new
  
  turmas<-future_classificador_turma(id = ids,path = path_pdf)
  
  
  print(str_c('Seções/Câmara/Turmas classificadas: ', Sys.time()))
  
  
  #junta tudo
  base <- tabelao %>%
    mutate(processo = clean_lawsuit(n_processo))
  
  d_vis_novo<-base %>% 
    left_join(turmas %>% distinct(), by = c('processo'='n_processo')) %>%
    mutate(secao = ifelse(is.na(SECAO), 'NAO IDENTIFICADO',SECAO),
           camara = ifelse(is.na(CAMARA), 'NAO IDENTIFICADO', CAMARA),
           turma = ifelse(is.na(TURMA), 'NAO IDENTIFICADO',TURMA)) %>%
    select(-processo, - SECAO, - CAMARA, - TURMA) %>%
    parse_results_()
  
  saveRDS(d_vis_novo, 'data/d_vis_ultimo.rds')
  
  print(str_c('Tudo pronto =) ', Sys.time()))
  
  print(str_c('Tempo total: ', Sys.time()-start))
}

#' Consolidate pages, decisions, and comprot
#'
#' @param id a list containing one or more lawsuits id's (in numeric format if's possible)
#' @param path directory where exists the directory of pdf decisions
#' @return A tibble with aggregated data about sections
#'
#' @export
classificador_turma <- function(id, path){

  resp <- purrr::map(id, classificador_turma_, path) %>%
    purrr::reduce(rbind)

  return(resp)

}


#' Consolidate pages, decisions, and comprot from future
#'
#' @param id a list containing one or more lawsuits id's (in numeric format if's possible)
#' @param path directory where exists the directory of pdf decisions
#' @return A tibble with aggregated data about sections
#'
#' @export
future_classificador_turma <- function(id, path){
  
  resp <- furrr::future_map(id, classificador_turma_, path) %>%
    purrr::keep(function(x){tibble::is_tibble(x)}) %>%
    purrr::reduce(rbind)
  
  return(resp)
  
}

#' Consolidate pages, decisions, and comprot with furrr
#'
#' @param id a list containing one or more lawsuits id's (in numeric format if's possible)
#' @param path directory where exists the directory of pdf decisions
#' @return A tibble with aggregated data about sections
#'
#' @export
future_classificador_turma <- function(id, path){
  
  resp <- furrr::future_map(id, classificador_turma_, path) %>%
    purrr::keep(function(x){tibble::is_tibble(x)}) %>%
    purrr::reduce(rbind)
  
  return(resp)
  
}


#' Consolidate pages, decisions, and comprot
#'
#' @param pages `pages` element returned by [parse_decision()]
#' @param decisions `decisions` element returned by [parse_decision()]
#' @param comprot Table returned by [get_comprot()]
#' @return A tibble with aggregated data about CARF
#'
#' @export
consolidate <- function(pages, decisions, comprot){
  
  detect_result <- function(result, decision, pattern_decision, type_appeal, replacement){
    pattern_appeal <- c("RECURSO DE OFICIO", "RECURSO ESPECIAL DO PROCURADOR")
    ifelse(decision == pattern_decision & type_appeal %in% pattern_appeal, replacement, result)}
  
  consolidate_decisions(pages, decisions) %>%
    consolidate_origins(comprot) %>%
    consolidate_states() %>%
    consolidate_appeals() %>%
    consolidate_votes() %>%
    consolidate_results() %>%
    consolidate_taxes() %>%
    consolidate_rapporteurs() %>%
    consolidate_chambers() %>%
    dplyr::select(
      id_lawsuit, id_decision, type_appeal, taxpayer, rapporteur,
      summary, decision, date_publication, type_party, cpf_cnpj,
      origin, state, vote, result, taxes, type, chamber, section) %>%
    dplyr::mutate(
      type_appeal = ifelse(stringr::str_detect(type_appeal, "RECURSO DE OF"), "RECURSO DE OFICIO", type_appeal),
      origin = ifelse(origin == "auto_infracao_impugnacao_multa", "AUTO INFRACAO/IMPUGNACAO/MULTA", "OUTROS"),
      vote = ifelse(vote == "NA", "VAZIO", stringr::str_to_upper(vote)),
      decision = result,
      result = result %>%
        detect_result(decision, "negar_provimento", type_appeal, "FAVORAVEL") %>%
        detect_result(decision, "dar_provimento", type_appeal, "DESFAVORAVEL") %>%
        detect_result(decision, "dar_provimento", type_appeal, "FAVORAVEL") %>%
        detect_result(decision, "negar_provimento", type_appeal, "DESFAVORAVEL") %>%
        detect_result(decision, "em_parte", type_appeal, "PARCIALMENTE FAVORAVEL"),
      result = ifelse(decision == "", "", result),
      result = stringr::str_to_upper(result),
      result = ifelse(stringr::str_detect(result, "_"), "VAZIO", result),
      type = ifelse(stringr::str_detect(type, "IDENTIF"), "NAO IDENTIFICADO", type))
  
}