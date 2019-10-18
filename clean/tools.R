add_break <- function(label) {
  n_elements <- 1:length(label)
  desc <- map(n_elements, function(i) {
    desc_i <- unlist(strsplit(label[i], ' '))
    desc_i
  }) 
  map(1:length(desc), function(l) {
    n_letters <- nchar(desc[l] %>% unlist())
    string <- desc[l]
    
    temp_string <- map(string, function(s) { 
      nL <- nchar(s)
      map(1:length(nL), function(l){
        if(nchar(s[l]) > 9 ) {
          s_t <- paste0(substr(s[l], 1, n_letters/2-1), '-\n',substr(s[l], n_letters/2, n_letters))
        } else {
          s_t <- s[l]
        }
      })
    }) %>% unlist()
    temp_string <- paste(temp_string, collapse = " ")
    n_words <- ngram::wordcount(temp_string)
    if(sum(grepl('\n', temp_string)) < 3 & n_words > 2) {
      pattern <- " "
      index <- as.data.frame(str_locate_all(temp_string, pattern))           # find all positions of pattern
      numobs <- round(n_words/2)
      index <- index[seq(numobs, nrow(index), by=numobs),]$start   # filter to every fifth instance of pattern
      str_sub(temp_string, index, index) <- "\n"    # arbitrary symbol to split on
      temp_string <- temp_string[1]
      #temp_string <-  'pensar esta bien'
    } else {
      temp_string <- temp_string
    }
    temp_string
  }) %>% unlist()
}


as_number <- function(x, decimal = "."){
  regex <- paste0("[^0-9",decimal,"]+")
  as.numeric(gsub(regex,"",x))
}

