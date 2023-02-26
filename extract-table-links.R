library(tidyverse)
library(rvest)
library(xml2)

# Get links in the same dimension table as the original table 
extract_table_links <- function(x, trim, header = NA, convert=NA){  
  #x= raw_page_html %>% html_node("table") 
  
  # Get table contents without links
  ns <- xml2::xml_ns(x)
  rows <- xml2::xml_find_all(x, ".//tr", ns = ns)
  cells <- lapply(rows, xml2::xml_find_all, ".//td|.//th", 
                  ns = ns)
  if (length(cells) == 0) {
    return(tibble::tibble())
  }
  
  values <- lapply(lapply(cells, html_node, "a"), html_attr, name = "href")
  values[[1]] <- str_squish(html_text(cells[[1]]))
  
  out <- do.call(rbind.data.frame, values)

  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  } else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }
  colnames(out) <- col_names
  df <- tibble::as_tibble(out, .name_repair = "minimal")
  
  if (isTRUE(convert)) {
    df[] <- lapply(df, function(x) {
      utils::type.convert(x, as.is = TRUE, dec = dec, na.strings = na.strings)
    })
  }
  df
}
