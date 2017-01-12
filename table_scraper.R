#pangaea
#chart scraper
library(rvest)
library(data.table)

URLs = list(Anglosphere = list(),
            Europe = list(),
            `East Asia & Islands` = 
              list(country = c('S. Korea', 'Japan'),
                   URL = c('http://gaonchart.co.kr/main/section/chart/online.gaon',
                           'http://www.oricon.co.jp/rank/js/d/')),
            `South Asia` = list(),
            `Africa & Middle East` = list(),
            `Caribbean & Latin America` = list())
            
get_chart = function(country, URL)
  switch(country,
         'S. Korea' = get_korea,
         'Japan' = get_japan)(url)

get_korea = function(URL) {
  tbl.css = '#wrap > div.chart > table'
  chart = read_html(URL) %>% html_node(tbl.css) %>%
    html_table %>% setDT(check.names = TRUE)
  chart[ , c('Ranking.1', 'Title...Artist', 'Production',
           'Share', 'Play') := NULL]
  setnames(chart, c('rank', 'title_artist'))
  chart[ , c('title', 'artist', 'release') := 
           tstrsplit(title_artist, split = "[\r\n\t|]+")]
  chart[ , title_artist := NULL]
  chart
}

get_japan = function(URL) {
  #not in tabular format; list is of <div> elements
  #  with class "inner", also followed by
  #  links of the form /prof/...
  cell.xp = '//div[@class="inner" and ./a[contains(@href, "prof")]]'
  top10 = read_html(URL) %>% html_nodes(xpath = cell.xp)
  title = top10 %>% html_nodes('h2') %>% html_text
  artist = top10 %>% html_nodes(xpath = '//p[@class="name"]') %>% html_text
  data.table(rank = seq_len(length(title)), title, artist)
}
