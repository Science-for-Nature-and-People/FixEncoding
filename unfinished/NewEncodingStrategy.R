# http://www.fileformat.info/info/unicode/utf8.htm
# 00 to 7F hex (0 to 127): first and only byte of a sequence.
# 80 to BF hex (128 to 191): continuing byte in a multi-byte sequence.
# C2 to DF hex (194 to 223): first byte of a two-byte sequence.
# E0 to EF hex (224 to 239): first byte of a three-byte sequence.
# F0 to FF hex (240 to 255): first byte of a four-byte sequence.

# But within these there are non-character and control-character code point.

# Do some data mining on http://www.utf8-chartable.de/unicode-utf8-table.pl. Limit the display options and choose "for Perl string literals" for the code point. 


byte_generator <- function(datapath) {
  require(magrittr)
  
  load(datapath) # data on path was created by commented code below
  # start1024URL <- "http://www.utf8-chartable.de/unicode-utf8-table.pl?start=1024&number=1024&utf8=string-literal"
  # sub_num <- seq(1024, 194560, 1024)
  # sub("[[:digit:]]{4,}", sub_num[2], start1024URL)
  # allURLs <- sapply(sub_num, function(x) sub("[[:digit:]]{4,}", x, start1024URL)) %>%
  #   c("http://www.utf8-chartable.de/unicode-utf8-table.pl?number=1024&utf8=string-literal",
  #     .)
  # data <- sapply(allURLs, readLines)
  
  invalidbytes <- function(website) {
    
    out <- tryCatch(
      
      {
        notutf8line <- c(grep("class=\"name\"><", website, useBytes = TRUE, value = TRUE),
                         grep("charnodisplay", website, useBytes = TRUE, value = TRUE))
        
        mypattern <- "<td class=\"utf8\">([^<]*)</td>"
        
        if (length(notutf8line) != 0) {
          notutf8char <- notutf8line %>%
            strsplit("<td class=\"utf8\">", useBytes = TRUE, fixed = TRUE) %>%
            sapply(extract, 2) %>% 
            strsplit("</td>") %>% 
            sapply(extract, 1)
          # Can't use the following because of lack of useBytes argument
          # mypattern = '<td class="row-text">([^<]*)</td>' # 
          # datalines = grep(mypattern,thepage[536:length(thepage)],value=TRUE) # value = TRUE
          # getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
          # gg = gregexpr(mypattern,datalines)
          # matches = mapply(getexpr,datalines,gg)
          # Tempresult = gsub(mypattern,'\\1',matches)
        } else notutf8char = NULL
      },
      
      condition = function(c) {
        message(c)
      }
      
    )
    return(out)
    
  }
  
  utf8.chartable.de <- sapply(data, invalidbytes) %>% unlist %>% unname
  
  # Additional characters not captured above
  utf8.chartable.de %<>% c(.,
                           "\\xed\\xa0\\x80", # Non Private Use High Surrogate, First
                           "\\xed\\xad\\xbf", # Non Private Use High Surrogate, Last
                           "\\xed\\xae\\x80", # Private Use High Surrogate, First
                           "\\xed\\xaf\\xbf", # Private Use High Surrogate, Last
                           "\\xed\\xb0\\x80", # Low Surrogate, First
                           "\\xed\\xbf\\xbf", # Low Surrogate, Last
                           "\\xee\\x80\\x80" # , # Private Use, First
                           # as.hexmode(195102:1048575) %>% 
                           #   paste0("\\U000",.), # U+2FA1E to ...
                           # as.hexmode(1048576:1114111) %>% 
                           #   paste0("\\U00", .) # ... U+10FFFF. Note that this range of non-characters shows up in R, not as Perl literals, but as Unicode regex. For example, the Perl literal \xf0\xaf\xa8\x9e (U+2FA1E) yields "\U0002fa1e" when printed in the console. Note also that this range corresponds to all Unicode beyond CJK.
  ) 
}




