#' Download images form Herbário Virtual Reflora
#'
#'
#' @param fam A character string with the family name
#' @param gen A character string with the genus name
#' @param epi A character string with the specific epithet name
#'
#' @import dplyr
#' @import httr
#' @import xml2
#' @import rvest
#' @seealso \link{'http://reflora.jbrj.gov.br/reflora/herbarioVirtual'}
#' @return A rasterStack with the minimun, maximum, median, mean and standard
#' @examples 
#' fam = "Melastomataceae"
#' gen = "Tibouchina"
#' epi = "clavata"
#' image_hv(fam,gen,epi)
#' 
#' @export
image_hv = function(fam, gen, epi, tx = 1){
  url = "http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?modoConsulta=LISTAGEM&quantidadeResultado=1000&nomeCientifico=" 

  url_sp = paste0(url, fam,"+", gen,"+", epi)
  
  cdg_html <- url_sp %>%
    httr::GET() %>%
    httr::content('text', encoding = 'UTF-8') %>%
    xml2::read_html() %>% 
    html_nodes('img') %>% 
    html_attr('src') 
  
  cdg_html = grep(pattern = "width=", cdg_html, value=T)
  cdg_html
  
  h = 150
  w = 70
  pb <- txtProgressBar(min = 1,
                       max = length(cdg_html),
                       style = 3)
  for(i in length(cdg_html)){
    setTxtProgressBar(pb, i)
    nomes = unlist(strsplit(cdg_html[i], split = "&"))
    url1 = paste0(nomes[1:2], collapse = "&")
    url_image = paste(url1,"&", "width=", w*tx, "&", "height=", h*tx, sep = "")
    download.file(url = url_image, destfile = paste(fam,gen,epi, "_",i,".jpg", sep = " "), mode = "wb", quiet = TRUE)
  }
}