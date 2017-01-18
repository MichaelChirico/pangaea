#pangaea
#chart scraper
library(rvest)
library(data.table)

URLs = list(Anglosphere = list(),
            Europe = 
              list(country = c('Portugal', 'Spain', 'Belgium',
                               'France', 'Netherlands')),
            `East/South Asia & Islands` = 
              list(country = c('S. Korea', 'Japan', 'India')),
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
         'Portugal' = get_portugal,
         'Spain' = get_spain,
         'Belgium' = get_belgium,
         'France' = get_france,
         'Netherlands' = get_netherlands)()

# East/South Asia & Islands ####
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

# Africa & Middle East ####
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

# Caribbean & Latin America ####
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

# Europe ####
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

get_spain = function(...) {
  URL = 'http://www.elportaldemusica.es/canciones.php'
  page = read_html(URL) 
  #credit to mnel for helping construct this xpath
  #  http://stackoverflow.com/a/41709268/3576984
  #need to prefix with div[@id..] because RHS
  #  top-10 list also has this info.
  title.td = '//div[@id="contLoUltimo"]//td[@class="nombreContenido_inferior"]'
  title.xp = paste0(title.td, '//span/@title|', title.td, '[not(.//span)]')
  title = page %>% 
    html_nodes(xpath = title.xp) %>% html_text() %>% trim_white %>%
    gsub("&amp;", "&", ., fixed = TRUE)
  artist.td = '//div[@id="contLoUltimo"]//td[@class="nombreArtista"]'
  artist.xp = paste0(artist.td, '//span/@title|', artist.td, '[not(.//span)]')
  artist = page %>% 
    html_nodes(xpath = artist.xp) %>% html_text() %>% trim_white %>%
    gsub("&amp;", "&", ., fixed = TRUE)
  data.table(rank = seq_len(length(title)), title, artist)
}

get_belgium = function(...) {
  URL = 'http://www.ultratop.be/nl/ultratop50'
  page = read_html(URL)
  core.xp = '//table[@id="chartList"]//td/a/'
  #core structure is:
  # <a><b>Artist</b><br>Title</a>
  artist = page %>% 
    html_nodes(xpath = paste0(core.xp, 'b')) %>% html_text
  title = page %>% 
    #not sure why text() here doesn't capture the <b> text too...
    html_nodes(xpath = paste0(core.xp, 'text()')) %>% 
    #captured a bunch of blanks, exclude here
    html_text %>% grep('[^ ]', ., value = TRUE)
  if (length(artist) != length(title))
    message("Artist/title length mismatch")
  data.table(rank = seq_len(length(title)), title, artist)
}

get_france = function(...) {
  URL = 'http://www.snepmusique.com/tops-semaine/top-singles-streaming/'
  track_column = read_html(URL) %>% 
    html_nodes(xpath = '//td[@class="atl"]')
  title = track_column %>% 
    html_nodes(xpath = '//p[@class="title"]') %>% html_text
  artist = track_column %>%
    html_nodes(xpath = '//strong[@class="artist"]') %>% html_text
  data.table(rank = seq_len(length(title)), title, artist)
}

get_netherlands = function(...) {
  URL = 'https://www.top40.nl/top40'
  #appears older songs still stored in the document?
  #  under <li class="no-longer-listed"...>
  this_week = read_html(URL) %>% 
    html_nodes(xpath = '//li[@class!="no-longer-listed"]')
  title = this_week %>% 
    #completely obscure why //span[@class="title"] isn't sufficient --
    #  somehow still picks up the things that should have
    #  been clipped by li[@class!="no-longer-listed"]
    html_nodes(xpath = 'div/div/a/span[@class="title"]') %>% html_text
  artist = this_week %>% 
    html_nodes(xpath = 'div/div/a/span[@class="credit"]') %>% html_text
  data.table(rank = seq_len(length(title)), title, artist)
}
