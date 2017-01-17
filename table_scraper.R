#pangaea
#chart scraper
library(rvest)
library(data.table)

URLs = list(Anglosphere = list(),
            Europe = 
              list(country = 'Portugal'),
            `East Asia & Islands` = 
              list(country = c('S. Korea', 'Japan', 'India')),
            `South Asia` = list(),
            `Africa & Middle East` =
              list(country = 'Nigeria'),
            `Caribbean & Latin America` =
              list(country = 'Mexico'))
            
trim_white = function(x) gsub("^\\s+|\\s+$", "", x)

get_chart = function(country)
  switch(country,
         'S. Korea' = get_korea,
         'Japan' = get_japan,
         'India' = get_india,
         'Nigeria' = get_nigeria,
         'Mexico' = get_mexico,
         'Portugal' = get_portugal)()

get_korea = function(...) {
  URL = 'http://gaonchart.co.kr/main/section/chart/online.gaon'
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

get_japan = function(...) {
  URLs = paste0('http://www.oricon.co.jp/rank/js/w/',
                Sys.Date(), "/p/", 1L:5L, "/")
  rbindlist(lapply(URLs, function(URL) {
    nm1 = nchar(URL) - 1L
    pg = as.integer(substr(URL, nm1, nm1))
    #10 per page;
    #  see these Qs for extensive work that went 
    #  into figuring out the encoding:
    #  http://ja.stackoverflow.com/q/31894/18598
    #  http://stackoverflow.com/q/41684799/3576984
    page = read_html(URL, encoding = 'Shift_JISX0213') 
    title = page %>% 
      html_nodes(xpath = '//h2[@class="title"]') %>% 
      # Was using xpath based on hyperlinked title, but some
      #   items were missing such a link. Relevant xpath was:
      #   '//div[@class="inner" and ./a[contains(@href, "prof")]]'
      #   Instead, going straight to the <h2 class="title"> nodes,
      #     but this feels less robust (currently, there are
      #     more than 10 -- e.g., 16 -- such nodes on a page;
      #     current logic relies on the ranking titles
      #     being the first 10 of these
      html_text %>% `[`(1L:10L)
    artist = page %>% 
      html_nodes(xpath = '//p[@class="name"]') %>% html_text
    #Potential failsafe for the above
    if (length(artist) != 10 || length(title) != 10) 
      message(URL, " produced a problem -- did not find ", 
              "exactly 10 title/artist entries.")
    data.table(rank = 10L*(pg - 1L) + seq_len(length(title)), title, artist)
  }))
}

get_india = function(...) {
  URL = 'http://www.radiomirchi.com/more/mirchi-top-20/'
  top_20 = read_html(URL) %>% 
    html_nodes(xpath = '//article[@class="top01"]')
  title = top_20 %>% html_nodes('h2') %>% html_text
  artist = top_20 %>% html_nodes('h3') %>% html_text %>% 
    #First line appears to be the movie title?
    # then second line starts with white space
    strsplit(split = "\n") %>% sapply(., `[`, 2L) %>% gsub("^\\s+", "", .)
  data.table(rank = seq_len(length(title)), title, artist)
}

get_nigeria = function(...) {
  URL = 'http://africacharts.com/official-top-50-songs-nigeria/'
  top_50 = read_html(URL) %>% html_nodes(xpath = '//td[@class="column-3"]')
  title = top_50 %>% html_nodes('strong') %>% html_text
  artist = top_50 %>% html_nodes('h5') %>% html_text
  #picked up some empty strings & trim whitespace
  artist = trim_white(artist[artist != ""])
  if (length(artist) != length(title))
    message("Mismatched title/artist pull")
  data.table(rank = seq_len(length(title)), title, artist)
}

get_mexico = function(...) {
  URL = 'http://www.billboard.com/charts/regional-mexican-songs'
  page = read_html(URL) 
  title = page %>%
    html_nodes(xpath = '//h2[@class="chart-row__song"]') %>% html_text
  artist = page %>% 
    html_nodes(xpath = '//*[@class="chart-row__artist"]') %>% html_text
  artist = trim_white(artist)
  data.table(rank = seq_len(length(title)), title, artist)
}

get_portugal = function(...) {
  URL = 'http://euro200.net/Portugal-Top50.htm'
  #ragged table
  top_50 = read_html(URL) %>% 
    html_nodes('table') %>% html_table(fill = TRUE) %>% `[[`(1L)
  #exclude irrelevant header/footer rows
  top_50 = setDT(top_50)[-c(1L:grep('this week', X2),
                            (grep('25', X2) + 1L):.N)]
  top_50 = rbind(top_50[ , .(title_artist = X5)],
                 top_50[ , .(title_artist = X11)])
  top_50[ , c("artist", "title") := tstrsplit(title_artist, split = "\\s-\\s")]
  top_50[ , artist := trim_white(gsub("\n", "", artist))]
  top_50[ , artist := gsub("(.*),\\s(THE)", "\\2 \\1", artist)][]
  top_50[ , title := trim_white(gsub("\n", "", title))][]
}
