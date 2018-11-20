#' Parse jurisprudence decision
#'
#' @param file A character vector with the path to a decision
#' @param verbose Whether or not to print the progress bar
#' @param pb Progress bar created by parse_decision()
#' @return A tibble with one row containing the data parsed
parse_decision_ <- function(file, verbose, pb) {

  # Read html
  html <- xml2::read_html(file)

  # Create table with information
  table <- dplyr::tibble(
    id_lawsuit = get_text(html, "//*[@id='formAcordaos:numProcesso']"),
    taxpayer = get_text(html, "//*[@id='formAcordaos:contribuinte']"),
    type_appeal = get_text(html, "//*[@id='formAcordaos:tdivTipoRecurso']/span[2]"),
    rapporteur = get_text(html, "//*[@id='formAcordaos:relator']"),
    date_session = get_text(html, "//*[@id='formAcordaos:dataSessao']"),
    id_decision = get_text(html, "//*[@id='formAcordaos:numDecisao']"),
    tax = get_text(html, "//*[@id='formAcordaos:tdivMateria']/span[2]"),
    summary = get_text(html, "//*[@id='formAcordaos:ementa']"),
    decision = get_text(html, "//*[@id='formAcordaos:textDecisao']"),
    num_attachments = length(xml2::xml_find_all(html, "//img[contains(@id, 'imageAnexos')]")))

  if (verbose) { pb$tick() }
  return(table)
}

#' Auxiliary function to call [parse_div()] for each div
#'
#' @param file A character vector of one or more paths to page files
#' @param verbose Whether or not to create a progress bar
#' @param pb Progress bar created by [parse_decision()]
#' @return A tibble with the parsed data
parse_decision_page <- function(file, verbose, pb) {

  # Extract divs from file
  xpath <- "//div[contains(@id, 'tblJurisprudencia') and contains(@id, 'body')]"
  divs <- xml2::xml_find_all(xml2::read_html(file), xpath)

  # Iterate over divs
  purrr::map_dfr(divs, parse_div, verbose, pb, .id = "id")
}

#' Parse each div from a page
#'
#' @param div Div to parse for relevant decision information
#' @param verbose Whether to tick the progress bar
#' @param pb Progress bar created by [parse_decision()]
#' @return A tibble with one row of parsed data
parse_div <- function(div, verbose, pb) {

  # Get keys
  keys <- div %>%
    rvest::html_nodes("span") %>%
    rvest::html_text() %>%
    tolower() %>%
    abjutils::rm_accent() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" +", "_") %>%
    stringr::str_replace_all("[^a-z_]", "")

  # Get values
  values <- div %>%
    rvest::html_nodes(xpath = "./span") %>%
    purrr::map(xml2::xml_find_first, "following-sibling::text()") %>%
    purrr::map_chr(rvest::html_text) %>%
    stringr::str_trim() %>%
    stringr::str_replace_na("")

  # Get first value
  values[1] <- div %>%
    rvest::html_node("a") %>%
    rvest::html_text()

  # Get texts
  texts <- div %>%
    rvest::html_nodes(xpath = "./div/span") %>%
    purrr::map(xml2::xml_find_first, "following-sibling::text()") %>%
    purrr::map_chr(rvest::html_text) %>%
    stringr::str_trim() %>%
    stringr::str_replace_na("")

  # Tick progress bar if necessaryg
  if (verbose) { pb$tick() }

  # Build tibble
  dplyr::tibble(key = keys, val = c(values, texts))
}


#' create the classification fro decision's text
#' 
#' @param dt d_vis data frame containing the decision text
#' @return classified d_vis
parse_results_ <- function(dt){
  
  limpa_decisao <- function(str){
    str %>%
      str_replace_all(',','') %>%
      str_replace_all('\\.','') %>%
      str_replace_all('provinento','provimento') %>%
      str_replace_all('provirnento','provimento') %>%
      str_replace_all('pro ento','provimento') %>%
      str_replace_all('provimenento','provimento') %>%
      str_replace_all(' ovimento','provimento') %>%
      str_replace_all('provvimento','provimento') %>%
      str_squish() %>%
      str_trim() %>%
      str_to_lower() %>%
      rm_accent()
  }
  
  conhecer_parcialmente <- 'conhec(eu)?(er)? parcialmente (d)?o recurso|nao conhec(eu)?(er)? parcialmente (d)?o recurso'
  nao_reconhece_recurso <- 'nao (tomar )?(se tomou )?(se conhec(eu)?(e)?(er)?)?(reconhecimento)?(conhecimento)?(reconhecer)?(conhecer)? (a)?(d)?o(s)? (presente(s)? )?recurso(s)?|recurso (voluntario )?nao conhecido|recurso desconhecido|nao conhecer por propositura (de acao )?judicial'  
  negar_provimento <- 'neg(a)?(ado)?(ou-se)?(ar)?(aram)?(aram-se)?(ar-lhe)? (o )?provimento'
  dar_parcial_provimento <- 'd(a)?(ado)?(ar)?(eram)?(eram-se)?(eu-se)?(ar-lhe)? parcial provimento(s)?|d(a)?(ado)?(ar)?(eram)?(eram-se)?(eu-se)?(ar-lhe)? provimento(s)? parcial|acolh(ido)?(er)?(eram)?(eram-se)?(eu-se)? parcialmente os embargos|conced(e)?(ido)?(er)?(eram)?(eram-se)?(eu-se)? parcial provimento(s)?|conced(e)?(ido)?(er)?(eram)?(eram-se)?(eu-se)? provimento(s)? parcial|neg(a)?(ado)?(ar)?(aram)?(aram-se)?(ou-se)? parcial provimento'
  dar_provimento <- 'd(ado)?(ar)?(eram)?(eram-se)?(eu-se)?(ar-lhe)?( i)?( )?provimento( ao recurso)?|conced(e)?(ido)?(er)?(eram)?(eram-se)?(eu-se)? provimento ao recurso|prover o recurso'
  decadencia <- 'declar(a)?(ada)?(ar)?(aram)?(aram-se)?(ou-se)? a decadencia|acolher (a preliminar de )?(a )?decadencia|reconhec(ida)?(er)?(eram)?(eram-se)?(eu-se)? (de oficio )?a decadencia'
  rejeitar_embargos <- 'rejeit(ado)?(ar)?(aram)?(aram-se)?(ou-se)? os embargos|embargos de declaracao rejeitando-os'
  nao_reconhece_embargos <- 'nao (tomar )?(se tomou )?(se conheceu)?(reconhecimento)?(conhecimento)?(reconhecer)?(conhecer)? (ao)?(d)?o(s)? embargo(s)?|embargo(s)? nao conhecido(s)?|embargo(s)? desconhecido(s)?'
  acolher_embargos <- 'acolh(ido)?(er)?(eram)?(eram-se)?(eu-se)? os embargos|admit(e)?(ido)?(ir)?(iram)?(iram-se)?(iu-se)? os embargos'
  converter_diligencia <- 'convert(ido)?(er)?(eram)?(eram-se)?(eu-se)? (o(s)? auto(s)? )?(o (presente )?recurso )?(o (presente )?julgamento (do )?recurso )?(o julgamento )?em diligencia|converter o julgamento na realizacao de diligencia'
  declinar_competencia <- 'declin(a)?(ada)?(ar)?(aram)?(aram-se)?(ou-se)? ((d)?a )?competencia'
  anular_primeira_instancia <- 'anul(ada)?(ar)?(aram)?(aram-se)?(ou-se)? a(s)? decis(ao)?(oes)? de primeira instancia'
  anular_decisao <- 'anul(a)?(ada)?(ar)?(aram)?(aram-se)?(ou-se)? a decisao'
  anular_lancamento <- 'anul(ado)?(ar)?(aram)?(aram-se)?(ou-se)? (por vicio material )?o lancamento|declar(ada)?(ar)?(aram)?(ou-se)? a nulidade (do lancamento)?(por vicio)?|anul(ada)?(ar)?(aram)?(ou-se)? o presente lancamento'
  julgar_em_conjunto <- 'julg(a)?(ado)?(ar)?(aram)?(aram-se)?(ou-se)? em conjunto'
  acatar_preliminar_decadencia <- 'declar(a)?(ada)?(ar)?(aram)?(aram-se)?(ou-se)? decadente(s)?|acat(a)?(ada)?(ar)?(aram)?(aram-se)?(ou-se)? a preliminar de decadencia|acolh(ida)?(er)?(eram)?(eram-se)?(eu-se)? a (preliminar )?(arguicao )?de decadencia|reconhec(ida)?(er)?(eram)?(eram-se)?(eu-se)? a decadencia'
  afastar_decadencia <- 'afast(ada)?(ar)?(aram)?(aram-se)?(ou-se)? (a preliminar de )?(a )?decadencia|acolh(ida)?(er)?(eram)?(eram-se)?(eu-se)? a preliminar de decadencia|reconhec(ida)?(er)?(eram)?(eram-se)?(eu-se)? a decadencia'
  anular_processo <- 'anul(ado)?(ar)?(aram)?(aram-se)?(ou-se)? o processo'
  declarar_extinta <- 'declar(ada(s)?)?(ar)?(aram)?(aram-se)?(ou-se)? extinta(s)? (todas )?a(s)? contribuic(ao)?(oes)? apurada(s)?'
  reconhecer_nulidade_lancamento <- '(re)?conhec(ida)?(er)?(eram)?(eram-se)?(eu-se)? a nulidade do lancamento|acolh(ida)?(er)?(eram)?(eram-se)?(eu-se)? a preliminar de nulidade'
  rejeitar_preliminar_nulidade_lancamento <- 'rejeit(ada)?(ar)?(aram)?(aram-se)?(ou-se)? a(s)? preliminar(es)? de nulidade do lancamento|afast(a)?(ada)?(ar)?(aram)?(aram-se)?(ou-se)? a(s) preliminar(es)? de nulidade'
  decisao_nula <- 'declar(a)?(ada)?(ar)?(aram)?(aram-se)?(ou-se)? nula a decisao'
  comeca_com_assunto <- 'assunto: '
  excluir_contribuicoes <- 'exclu(i)?(ida)?(ir)?(iram)?(iram-se)?(iu-se)?(iu)? as contribuicoes'
  anular_auto_infracao <- 'anul(ado)?(ar)?(aram)?(aram-se)?(ou-se)? o auto de infracao'
  sobrestar_julgamento <- 'sobrest(ado)?(ar)?(aram)?(aram-se)?(ou-se)? o julgamento'
  
  resp <- dt %>%
    select(-resultado, -n, ) %>%
    mutate(txt_decisao = limpa_decisao(txt_decisao),
           decisao = case_when(str_detect(txt_decisao,dar_parcial_provimento) | ((str_detect(txt_decisao,negar_provimento) & str_detect(txt_decisao,dar_provimento)))~ 'DAR PARCIAL PROVIMENTO',
                                 str_detect(txt_decisao, negar_provimento) ~ 'NEGAR PROVIMENTO',
                                 str_detect(txt_decisao, dar_provimento) ~ 'DAR PROVIMENTO',
                                 str_detect(txt_decisao, conhecer_parcialmente) ~ 'CONHECER PARCIALMENTE',
                                 str_detect(txt_decisao, nao_reconhece_recurso)~ 'NAO RECONHECE O RECURSO',
                                 str_detect(txt_decisao, decadencia) ~ 'DECLARA DECADENCIA',
                                 str_detect(txt_decisao, rejeitar_embargos)~ 'REJEITAR EMBARGOS',
                                 str_detect(txt_decisao, acolher_embargos)~ 'ACOLHER EMBARGOS',
                                 str_detect(txt_decisao, nao_reconhece_embargos)~ 'NAO RECONHECE EMBARGOS',
                                 str_detect(txt_decisao, converter_diligencia)~ 'CONVERTIDO DILIGENCIA',
                                 str_detect(txt_decisao, declinar_competencia)~ 'DECLINAR COMPETENCIA',
                                 str_detect(txt_decisao, anular_primeira_instancia)~ 'ANULAR DECISAO DE PRIMEIRA ISNTANCIA',
                                 str_detect(txt_decisao, anular_decisao)~ 'ANULAR DECISAO',
                                 str_detect(txt_decisao, anular_lancamento)~ 'ANULAR LANCAMENTO',
                                 str_detect(txt_decisao, julgar_em_conjunto)~ 'JULGAR EM CONJUNTO COM OUTRO',
                                 str_detect(txt_decisao, acatar_preliminar_decadencia)~ 'ACATAR PRELIMINAR DE DECADENCIA',
                                 str_detect(txt_decisao, afastar_decadencia)~ 'AFASTAR DECADENCIA',
                                 str_detect(txt_decisao, anular_processo)~ 'ANULAR O PROCESSO',
                                 str_detect(txt_decisao, declarar_extinta)~ 'CONTRIUICAO EXTINTA',
                                 str_detect(txt_decisao, reconhecer_nulidade_lancamento)~ 'RECONHECER NULIDADE LANCAMENTO',
                                 str_detect(txt_decisao, rejeitar_preliminar_nulidade_lancamento)~ 'REJEITAR PRELIMINAR NULIDADE LANCAMENTO',
                                 str_detect(txt_decisao, decisao_nula)~ 'DECLARADA DECISAO NULA',
                                 str_detect(txt_decisao, comeca_com_assunto)~ 'COMECA COM ASSUNTO',
                                 str_detect(txt_decisao, excluir_contribuicoes)~ 'EXCLUIR CONTRIBUICOES',
                                 str_detect(txt_decisao, anular_auto_infracao)~ 'ANULAR AUTO DE INFRACAO',
                                 str_detect(txt_decisao, sobrestar_julgamento)~ 'SOBRESTAR O JULGAMENTO',
                                 txt_decisao== '' | is.na(txt_decisao) ~ 'VAZIO OU NULO',
                                 T~'classificar'))
  
  classificacoes <- tibble::tribble(
    # ------------------------------------------------------------------------------------------------------
    ~decisao,                      ~tipo_recurso,     ~n, ~resultado_num, ~tirar,
    "NEGAR PROVIMENTO",                    "NÃO INFORMADO",    38L,        2,      1,
    "DAR PROVIMENTO",                    "NÃO INFORMADO",    14L,        2,      1,
    "DAR PARCIAL PROVIMENTO",                    "NÃO INFORMADO",     6L,        3,      1,
    "classificar",                    "NÃO INFORMADO",     3L,        2,      1,
    "ACOLHER EMBARGOS",                    "NÃO INFORMADO",     2L,        2,      1,
    "ANULAR O PROCESSO",                    "NÃO INFORMADO",     1L,        2,      1,
    "NAO RECONHECE O RECURSO",                    "NÃO INFORMADO",     1L,        2,      1,
    "REJEITAR EMBARGOS",                    "NÃO INFORMADO",     1L,        2,      1,
    # ------------------------------------------------------------------------------------------------------
    "NEGAR PROVIMENTO",                "RECURSO DE OFICIO",  2047L,        1,      0,
    "DAR PARCIAL PROVIMENTO",                "RECURSO DE OFICIO",   715L,        3,      0,
    "NAO RECONHECE O RECURSO",                "RECURSO DE OFICIO",   342L,        1,      1,
    "DAR PROVIMENTO",                "RECURSO DE OFICIO",   177L,        0,      0,
    "CONVERTIDO DILIGENCIA",                "RECURSO DE OFICIO",   146L,        1,      0,
    "classificar",                "RECURSO DE OFICIO",    69L,        2,      1,
    "SOBRESTAR O JULGAMENTO",                "RECURSO DE OFICIO",    14L,        1,      1,
    "ANULAR DECISAO DE PRIMEIRA ISNTANCIA",                "RECURSO DE OFICIO",    13L,        0,      0,
    "DECLINAR COMPETENCIA",                "RECURSO DE OFICIO",     9L,        1,      1,
    "ACOLHER EMBARGOS",                "RECURSO DE OFICIO",     6L,        0,      1,
    "ANULAR LANCAMENTO",                "RECURSO DE OFICIO",     6L,        2,      1,
    "ANULAR O PROCESSO",                "RECURSO DE OFICIO",     5L,        1,      1,
    "REJEITAR EMBARGOS",                "RECURSO DE OFICIO",     3L,        1,      1,
    "COMECA COM ASSUNTO",                "RECURSO DE OFICIO",     2L,        2,      1,
    "CONHECER PARCIALMENTE",                "RECURSO DE OFICIO",     1L,        2,      1,
    "DECLARA DECADENCIA",                "RECURSO DE OFICIO",     1L,        1,      0,
    "JULGAR EM CONJUNTO COM OUTRO",                "RECURSO DE OFICIO",     1L,        2,      1,
    "RECONHECER NULIDADE LANCAMENTO",                "RECURSO DE OFICIO",     1L,        2,      1,
    "VAZIO OU NULO",                "RECURSO DE OFICIO",     1L,        2,      1,
    # ------------------------------------------------------------------------------------------------------
    "NEGAR PROVIMENTO", "RECURSO ESPECIAL DO CONTRIBUINTE",  1766L,        0,      0,
    "DAR PARCIAL PROVIMENTO", "RECURSO ESPECIAL DO CONTRIBUINTE",  1659L,        3,      0,
    "DAR PROVIMENTO", "RECURSO ESPECIAL DO CONTRIBUINTE",   895L,        1,      0,
    "NAO RECONHECE O RECURSO", "RECURSO ESPECIAL DO CONTRIBUINTE",   667L,        0,      1,
    "classificar", "RECURSO ESPECIAL DO CONTRIBUINTE",    58L,        2,      1,
    "SOBRESTAR O JULGAMENTO", "RECURSO ESPECIAL DO CONTRIBUINTE",    35L,        0,      1,
    "CONVERTIDO DILIGENCIA", "RECURSO ESPECIAL DO CONTRIBUINTE",    29L,        0,      1,
    "ACOLHER EMBARGOS", "RECURSO ESPECIAL DO CONTRIBUINTE",     8L,        1,      1,
    "REJEITAR EMBARGOS", "RECURSO ESPECIAL DO CONTRIBUINTE",     8L,        0,      1,
    "ANULAR O PROCESSO", "RECURSO ESPECIAL DO CONTRIBUINTE",     2L,        0,      1,
    "COMECA COM ASSUNTO", "RECURSO ESPECIAL DO CONTRIBUINTE",     2L,        2,      1,
    "RECONHECER NULIDADE LANCAMENTO", "RECURSO ESPECIAL DO CONTRIBUINTE",     2L,        2,      1,
    "AFASTAR DECADENCIA", "RECURSO ESPECIAL DO CONTRIBUINTE",     1L,        1,      1,
    "CONHECER PARCIALMENTE", "RECURSO ESPECIAL DO CONTRIBUINTE",     1L,        2,      1,
    "NAO RECONHECE EMBARGOS", "RECURSO ESPECIAL DO CONTRIBUINTE",     1L,        0,      1,
    "REJEITAR PRELIMINAR NULIDADE LANCAMENTO", "RECURSO ESPECIAL DO CONTRIBUINTE",     1L,        0,      1,
    # ------------------------------------------------------------------------------------------------------
    "NEGAR PROVIMENTO",   "RECURSO ESPECIAL DO PROCURADOR",  2881L,        1,      0,
    "DAR PROVIMENTO",   "RECURSO ESPECIAL DO PROCURADOR",  2767L,        0,      0,
    "DAR PARCIAL PROVIMENTO",   "RECURSO ESPECIAL DO PROCURADOR",  2416L,        3,      0,
    "NAO RECONHECE O RECURSO",   "RECURSO ESPECIAL DO PROCURADOR",  1088L,        1,      1,
    "classificar",   "RECURSO ESPECIAL DO PROCURADOR",   111L,        2,      1,
    "CONVERTIDO DILIGENCIA",   "RECURSO ESPECIAL DO PROCURADOR",    39L,        1,      0,
    "ACOLHER EMBARGOS",   "RECURSO ESPECIAL DO PROCURADOR",    17L,        0,      1,
    "SOBRESTAR O JULGAMENTO",   "RECURSO ESPECIAL DO PROCURADOR",    15L,        1,      1,
    "ACATAR PRELIMINAR DE DECADENCIA",   "RECURSO ESPECIAL DO PROCURADOR",     9L,        1,      1,
    "DECLARA DECADENCIA",   "RECURSO ESPECIAL DO PROCURADOR",     8L,        1,      1,
    "COMECA COM ASSUNTO",   "RECURSO ESPECIAL DO PROCURADOR",     7L,        2,      1,
    "ANULAR O PROCESSO",   "RECURSO ESPECIAL DO PROCURADOR",     6L,        1,      1,
    "REJEITAR EMBARGOS",   "RECURSO ESPECIAL DO PROCURADOR",     6L,        1,      1,
    "AFASTAR DECADENCIA",   "RECURSO ESPECIAL DO PROCURADOR",     4L,        0,      1,
    "ANULAR AUTO DE INFRACAO",   "RECURSO ESPECIAL DO PROCURADOR",     2L,        1,      1,
    "ANULAR DECISAO",   "RECURSO ESPECIAL DO PROCURADOR",     2L,        1,      1,
    "CONHECER PARCIALMENTE",   "RECURSO ESPECIAL DO PROCURADOR",     2L,        2,      1,
    "ANULAR DECISAO DE PRIMEIRA ISNTANCIA",   "RECURSO ESPECIAL DO PROCURADOR",     1L,        1,      1,
    "ANULAR LANCAMENTO",   "RECURSO ESPECIAL DO PROCURADOR",     1L,        2,      1,
    "DECLINAR COMPETENCIA",   "RECURSO ESPECIAL DO PROCURADOR",     1L,        1,      1,
    "JULGAR EM CONJUNTO COM OUTRO",   "RECURSO ESPECIAL DO PROCURADOR",     1L,        2,      1,
    "RECONHECER NULIDADE LANCAMENTO",   "RECURSO ESPECIAL DO PROCURADOR",     1L,        0,      1,
    "REJEITAR PRELIMINAR NULIDADE LANCAMENTO",   "RECURSO ESPECIAL DO PROCURADOR",     1L,        1,      1,
    # ------------------------------------------------------------------------------------------------------
    "NEGAR PROVIMENTO",               "RECURSO VOLUNTARIO", 44478L,        0,      0,
    "DAR PARCIAL PROVIMENTO",               "RECURSO VOLUNTARIO", 20123L,        3,      0,
    "DAR PROVIMENTO",               "RECURSO VOLUNTARIO", 17074L,        1,      0,
    "CONVERTIDO DILIGENCIA",               "RECURSO VOLUNTARIO",  6874L,        0,      1,
    "NAO RECONHECE O RECURSO",               "RECURSO VOLUNTARIO",  5769L,        0,      1,
    "classificar",               "RECURSO VOLUNTARIO",  2086L,        2,      1,
    "SOBRESTAR O JULGAMENTO",               "RECURSO VOLUNTARIO",   623L,        0,      1,
    "ANULAR DECISAO DE PRIMEIRA ISNTANCIA",               "RECURSO VOLUNTARIO",   436L,        1,      1,
    "DECLARA DECADENCIA",               "RECURSO VOLUNTARIO",   410L,        0,      1,
    "DECLINAR COMPETENCIA",               "RECURSO VOLUNTARIO",   201L,        0,      1,
    "ANULAR LANCAMENTO",               "RECURSO VOLUNTARIO",   194L,        0,      1,
    "ANULAR DECISAO",               "RECURSO VOLUNTARIO",   132L,        1,      1,
    "ANULAR O PROCESSO",               "RECURSO VOLUNTARIO",   117L,        0,      1,
    "ACOLHER EMBARGOS",               "RECURSO VOLUNTARIO",   108L,        1,      1,
    "VAZIO OU NULO",               "RECURSO VOLUNTARIO",   100L,        2,      1,
    "COMECA COM ASSUNTO",               "RECURSO VOLUNTARIO",    85L,        2,      1,
    "REJEITAR EMBARGOS",               "RECURSO VOLUNTARIO",    71L,        0,      1,
    "ACATAR PRELIMINAR DE DECADENCIA",               "RECURSO VOLUNTARIO",    61L,        1,      1,
    "RECONHECER NULIDADE LANCAMENTO",               "RECURSO VOLUNTARIO",    48L,        1,      1,
    "CONHECER PARCIALMENTE",               "RECURSO VOLUNTARIO",    35L,        2,      1,
    "ANULAR AUTO DE INFRACAO",               "RECURSO VOLUNTARIO",    27L,        1,      1,
    "DECLARADA DECISAO NULA",               "RECURSO VOLUNTARIO",    16L,        1,      1,
    "NAO RECONHECE EMBARGOS",               "RECURSO VOLUNTARIO",    13L,        0,      1,
    "AFASTAR DECADENCIA",               "RECURSO VOLUNTARIO",    12L,        1,      1,
    "JULGAR EM CONJUNTO COM OUTRO",               "RECURSO VOLUNTARIO",     6L,        2,      1,
    "REJEITAR PRELIMINAR NULIDADE LANCAMENTO",               "RECURSO VOLUNTARIO",     5L,        0,      1,
    "CONTRIUICAO EXTINTA",               "RECURSO VOLUNTARIO",     1L,        2,      1,
    "EXCLUIR CONTRIBUICOES",               "RECURSO VOLUNTARIO",     1L,        2,      1,
    # ------------------------------------------------------------------------------------------------------
    "NEGAR PROVIMENTO",                            "VAZIO",   455L,        2,      1,
    "DAR PROVIMENTO",                            "VAZIO",   198L,        2,      1,
    "DAR PARCIAL PROVIMENTO",                            "VAZIO",   131L,        3,      1,
    "CONVERTIDO DILIGENCIA",                            "VAZIO",    77L,        2,      1,
    "NAO RECONHECE O RECURSO",                            "VAZIO",    64L,        2,      1,
    "classificar",                            "VAZIO",    24L,        2,      1,
    "ACOLHER EMBARGOS",                            "VAZIO",    11L,        2,      1,
    "REJEITAR EMBARGOS",                            "VAZIO",     9L,        2,      1,
    "ANULAR O PROCESSO",                            "VAZIO",     7L,        2,      1,
    "ANULAR LANCAMENTO",                            "VAZIO",     6L,        2,      1,
    "RECONHECER NULIDADE LANCAMENTO",                            "VAZIO",     4L,        2,      1,
    "DECLARADA DECISAO NULA",                            "VAZIO",     3L,        2,      1,
    "ANULAR DECISAO",                            "VAZIO",     2L,        2,      1,
    "DECLARA DECADENCIA",                            "VAZIO",     2L,        2,      1,
    "ANULAR DECISAO DE PRIMEIRA ISNTANCIA",                            "VAZIO",     1L,        2,      1,
    "COMECA COM ASSUNTO",                            "VAZIO",     1L,        2,      1,
    "NAO RECONHECE EMBARGOS",                            "VAZIO",     1L,        2,      1
    # ------------------------------------------------------------------------------------------------------
  )
  
  legenda <- purrr::cross_df(list(resultado_num = 0:3, tirar = 0:1)) %>% 
    dplyr::mutate(
      resultado = dplyr::case_when(
        resultado_num == 0 ~ "Desfavoravel",
        resultado_num == 1 ~ "Favoravel",
        resultado_num == 2 ~ NA_character_,
        resultado_num == 3 ~ "Parcialmente Favoravel",
      ),
      tirar_lab = dplyr::if_else(tirar == 0, "Não", "Sim")
    )
  
  
  result <- resp %>%
    left_join(classificacoes, by = c('decisao'='decisao', 'tipo_recurso' = 'tipo_recurso')) %>%
    left_join(legenda %>% select(resultado_num, resultado), c('resultado_num'='resultado_num')) %>%
    distinct()
  
  classificar <- resp %>%
    count(decisao) %>%
    mutate(prop = n/sum(n)) %>%
    filter(decisao == 'classificar') %>%
    select(prop) %>%
    pull
  
  message <- case_when(
    classificar > 0.2 ~ 'Taxa de nao-classificacao > 20%',
    classificar > 0.15 ~ 'Taxa de nao-classificacao entre 15% e 20%',
    classificar > 0.1 ~ 'Taxa de nao-classificacao entre 10% e 15%',
    classificar > 0.05 ~ 'Taxa de nao-classificacao entre 05% e 10%',
    T ~ 'ok'
  )
  
  if(message != 'ok') warning(message)  
    
  return(result)
}

