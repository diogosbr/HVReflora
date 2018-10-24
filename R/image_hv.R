#' Download images form Herbário Virtual Reflora
#'
#'
#' @param scientific.name A character string with the scientific name.
#' @param tx Multiplication rate of standard image size  (70 x 150 px).
#' @param destfolder A character string with the folder where the images will be saved
#' @param municipality A character string with the municipality name. If is NULL (default) download images of all available municipalities.
#'
#' @import dplyr
#' @import httr
#' @import xml2
#' @import rvest
#' @import textclean
#' @importFrom utils download.file 
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @seealso (<http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/ConsultaPublicoHVUC.do>)
#' @return A rasterStack with the minimun, maximum, median, mean and standard
#' @examples
#' image_hv("Prepusa montana")
#'
#' @export
image_hv = function(scientific.name,
                    tx = 1,
                    destfolder = "images",
                    municipality = NULL) {
  url = "http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?modoConsulta=LISTAGEM&quantidadeResultado=1000"
  
  if (!is.null(scientific.name)) {
    name = scientific.name %>% 
      strsplit(., " ") %>% 
      unlist(.)
    gen = name[1]
    epi = name[2]
    url_sp = paste0(url, "&nomeCientifico=", paste0(gen, "+", epi))
    if(!is.null(municipality)){
      mun  = municipality %>% 
        replace_non_ascii(.) %>% 
        strsplit(., " ") %>% 
        unlist(.) %>% 
        paste(., collapse = "+")
      url_sp = paste0(url, "&nomeCientifico=", paste0(gen, "+", epi), "&municipio=", mun)
    }
  } else{
    stop("Please provide search name")
  }
  
  cdg_html <- url_sp %>%
    httr::GET(.) %>%
    httr::content(.,'text', encoding = 'UTF-8') %>%
    xml2::read_html(.) %>%
    html_nodes(.,'img') %>%
    html_attr(.,'src')
  
  if(length(cdg_html)==86){stop("Please check if the taxon name is correct.")}
    
  cdg_html = grep(pattern = "width=", cdg_html, value = T)
  
  if (!dir.exists(destfolder)) {
    dir.create(destfolder)
  } else{
    warning(
      "The folder you have chosen already exists. Be careful because you can overwrite the files contained in it."
    )
  }
  
  pb <- txtProgressBar(min = 1,
                       max = length(cdg_html),
                       style = 3)
  for (i in 1:length(cdg_html)) {
    setTxtProgressBar(pb, i)
    #nomes = unlist(strsplit(cdg_html[i], split = "&"))
    nomes = cdg_html[i] %>% 
      strsplit(., split = "&") %>% 
      unlist(.)
    
    w = as.numeric(nomes[3:4] %>% strsplit(., split="=") %>% unlist(.) %>% .[2])
    h = as.numeric(nomes[3:4] %>% strsplit(., split="=") %>% unlist(.) %>% .[4])
    
    url1 = paste0(nomes[1:2], collapse = "&")
    url_image = paste(url1, "&", "width=", w * tx, "&", "height=", h * tx, sep = "")
    download.file(
      url = url_image,
      destfile = paste(
        "./",
        destfolder,
        "/",
        scientific.name,
        "_",
        i,
        ".jpg",
        sep = ""
      ),
      mode = "wb",
      quiet = TRUE
    )
  }
}
