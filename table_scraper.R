#pangaea
#chart scraper
library(rvest)
library(data.table)

trim_white = function(x) gsub("^\\s+|\\s+$", "", x)

charts = list(Anglosphere = 
                list(country = c('USA', 'Canada', 'UK',
                                 'Ireland', 'Australia',
                                 'New Zealand')),
              Europe = 
                list(country = c('Portugal', 'Spain', 'Belgium',
                                 'France', 'Netherlands',
                                 'Germany', 'Switzerland',
                                 'Italy', 'Denmark', 'Norway',
                                 'Sweden', 'Finland', 'Russia',
                                 'Poland', 'Belarus', 'Ukraine',
                                 'Austria', 'Croatia', 'Greece',
                                 'Czech Republic', 'Slovakia',
                                 'Hungary', 'Slovenia', 'Romania',
                                 'Lithuania', 'Bulgaria')),
              `East/South Asia & Islands` = 
                list(country = c('South Korea', 'Japan', 'India',
                                 'Vietnam', 'Philippines',
                                 'Malaysia', 'Singapore', 'Thailand',
                                 'Cambodia', 'Hong Kong',
                                 'Indonesia', 'China')),
              `Africa & Middle East` =
                list(country = c('Nigeria', 'Israel', 'Lebanon',
                                 'Kenya', 'Uganda', 'Malawi')),
              `Caribbean & Latin America` =
                list(country = c('Mexico', 'Brazil', 'Colombia',
                                 'Venezuela', 'Guatemala')))

get_chart = function(country)
  switch(country,
         'South Korea' = get_south_korea,
         'Japan' = get_japan,
         'India' = get_india,
         'Nigeria' = get_nigeria,
         'Mexico' = get_mexico,
         'Portugal' = get_portugal,
         'Spain' = get_spain,
         'Belgium' = get_belgium,
         'France' = get_france,
         'Netherlands' = get_netherlands,
         'Germany' = get_germany,
         'Switzerland' = get_switzerland,
         'Italy' = get_italy,
         'Denmark' = get_denmark,
         'Norway' = get_norway,
         'Sweden' = get_sweden,
         'Finland' = get_finland,
         'Russia' = get_russia,
         'USA' = get_usa,
         'Canada' = get_canada,
         'UK' = get_uk,
         'Ireland' = get_ireland,
         'Australia' = get_australia,
         'New Zealand' = get_new_zealand,
         'Poland' = get_poland,
         'Belarus' = get_belarus,
         'Ukraine' = get_ukraine,
         'Austira' = get_austria,
         'Croatia' = get_croatia,
         'Greece' = get_greece,
         'Czech Republic' = get_czech_republic,
         'Slovakia' = get_slovakia,
         'Hungary' = get_hungary,
         'Slovenia' = get_slovenia,
         'Romania' = get_romania,
         'Lithuania' = get_lithuania,
         'Bulgaria' = get_bulgaria,
         'Vietnam' = get_vietnam,
         'Philippines' = get_philippines,
         'Malaysia' = get_malaysia,
         'Singapore' = get_singapore,
         'Thailand' = get_thailand,
         'Cambodia' = get_cambodia,
         'Hong Kong' = get_hong_kong,
         'Indonesia' = get_indonesia,
         'China' = get_china,
         'Israel' = get_israel,
         'Lebanon' = get_lebanon,
         'Kenya' = get_kenya,
         'Uganda' = get_uganda,
         'Malawi' = get_malawi,
         'Brazil' = get_brazil,
         'Colombia' = get_colombia,
         'Venezuela' = get_venezuela,
         'Guatemala' = get_guatemala)()

# East/South Asia & Islands ####
get_south_korea = function(...) {
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
  chart[]
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
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_vietnam = function(...) {
  URL = 'http://musicweekly.asia/charts/top-30-singles-vietnam'
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//table//tr/td[3]/strong') %>% html_text
  artist.xp = '//table//tr/td[3]/node()[not(self::strong)]'
  artist = pg %>% html_nodes(xpath = artist.xp) %>% html_text
  #not sure where the blanks are coming from?
  artist = artist[nzchar(artist)]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_philippines = function(...) {
  #all of musicweekly.asia charts identical, see Vietnam
  URL = 'http://musicweekly.asia/charts/top-30-singles-philippines' 
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//table//tr/td[3]/strong') %>% html_text
  artist.xp = '//table//tr/td[3]/node()[not(self::strong)]'
  artist = pg %>% html_nodes(xpath = artist.xp) %>% html_text
  artist = artist[nzchar(artist)]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_malaysia = function(...) {
  #all of musicweekly.asia charts identical, see Vietnam
  URL = 'http://musicweekly.asia/charts/top-30-singles-chart-malaysia' 
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//table//tr/td[3]/strong') %>% html_text
  artist.xp = '//table//tr/td[3]/node()[not(self::strong)]'
  artist = pg %>% html_nodes(xpath = artist.xp) %>% html_text
  artist = artist[nzchar(artist)]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_singapore = function(...) {
  #all of musicweekly.asia charts identical, see Vietnam
  URL = 'http://musicweekly.asia/charts/top-30-singles-singapore' 
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//table//tr/td[3]/strong') %>% html_text
  artist.xp = '//table//tr/td[3]/node()[not(self::strong)]'
  artist = pg %>% html_nodes(xpath = artist.xp) %>% html_text
  artist = artist[nzchar(artist)]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_thailand = function(...) {
  #all of musicweekly.asia charts identical, see Vietnam
  #  **TO DO: investigate http://deungdutjai.com/top-charts/
  URL = 'http://musicweekly.asia/charts/top-30-singles-thailand' 
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//table//tr/td[3]/strong') %>% html_text
  artist.xp = '//table//tr/td[3]/node()[not(self::strong)]'
  artist = pg %>% html_nodes(xpath = artist.xp) %>% html_text
  artist = artist[nzchar(artist)]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_thailand = function(...) {
  #all of musicweekly.asia charts identical, see Vietnam
  URL = 'http://musicweekly.asia/charts/top-30-singles-cambodia' 
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//table//tr/td[3]/strong') %>% html_text
  artist.xp = '//table//tr/td[3]/node()[not(self::strong)]'
  artist = pg %>% html_nodes(xpath = artist.xp) %>% html_text
  artist = artist[nzchar(artist)]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_hong_kong = function(...) {
  #all of musicweekly.asia charts identical, see Vietnam
  URL = 'http://musicweekly.asia/charts/top-30-singles-hong-kong' 
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//table//tr/td[3]/strong') %>% html_text
  artist.xp = '//table//tr/td[3]/node()[not(self::strong)]'
  artist = pg %>% html_nodes(xpath = artist.xp) %>% html_text
  artist = artist[nzchar(artist)]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_indonesia = function(...) {
  idx_url = paste0('http://creativedisc.com/category/',
                   'top-charts/creative-disc-top-50-chart/')
  URL = read_html(idx_url) %>% 
    html_node(xpath = '//article[@class="latestPost excerpt  "]/a') %>% 
    html_attr('href')
  body.xp = '//div[@itemprop="articleBody"]/p[2]'
  tbl = read_html(URL) %>% html_node(xpath = body.xp) %>% 
    #page display is horrible, but thankfully the blanks are tabs!
    html_text %>% read.table(text = ., sep = '\t', header = TRUE) %>%
    setDT
  tbl[ , title := gsub('\\s+\\[.*', '', Song.Title..Album.Title...Label.)]
  setnames(tbl, 'Artist', 'artist')
  tbl[ , .(rank, title, artist)]
}

# China = dynamic
# get_china = function(...) {
#   URL = 'https://y.qq.com/portal/toplist/4.html'
#   pg = read_html(URL)
#   pg %>% html_nodes(xpath = '/html/body/div[2]/div[2]/div[3]/ul[2]/li[1]/div/div[4]/span/a[2]')
# }

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

# dynamic
# get_israel = function(...) {
#   URL = 'http://mediaforest.biz/WeeklyCharts/'
#   pg = read_html(URL)
#   pg %>% html_node(xpath = '//*[@id="local_tab1"]/div/table/tbody/tr[2]/td[5]')
# }

get_lebanon = function(...) {
  URL = 'http://www.olt20.com/Charts'
  pg = read_html(URL)
  title = pg %>% 
    html_nodes(xpath = '//span[@class="song-title"]') %>% html_text
  artist = pg %>% 
    html_nodes(xpath = '//span[@class="artist"]') %>% 
    html_text %>% trim_white
  data.table(rank = seq_len(length(title)), title, artist)
}

get_ghana = function(...) {
  URL = 'http://www.yfmghana.com/y-top-20/'
  song.xp = '//div[@class="ox-eef4d49dcb-gmail_default"]/ol/li'
  songs = read_html(URL) %>% 
    html_nodes(xpath = song.xp) %>% html_text
  tbl = setDT(transpose(strsplit(songs, '\\s+–\\s+')))
  setnames(tbl, c('artist', 'title'))
  tbl[ , rank := .I]
  setcolorder(tbl, 3:1)
  #reference case: 17. JOEL TWERKI
  #  (unsure song title, just assign as artist)
  tbl[is.na(title), title := artist][]
}

get_kenya = function(...) {
  URL = 'http://www.kenyatop40.com/'
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//h2[@class="title"]') %>% html_text
  artist = pg %>% html_nodes(xpath = '//h3[@class="artist"]') %>% html_text
  data.table(rank = seq_len(length(title)), title, artist)
}

get_uganda = function(...) {
  URL_stub = 'http://www.ugmusic.net/'
  mainURL = paste0(URL_stub, 'charts.php')
  trackURLs = read_html(mainURL) %>%
    #all four tables on page identical;
    #  doing full xpath with html_node was
    #  including other tables (despite [1] force attempt)
    html_node(xpath = '//table//table[1]') %>%
    html_nodes(xpath = './/td/a[contains(@href, "song")]') %>%
    html_attr('href') %>% paste0(URL_stub, .)
  tbl = setDT(transpose(lapply(trackURLS, function(uu) {
    title_artist = read_html(uu) %>% html_node('h2') %>% 
      html_text %>% strsplit('- ') %>% el
  })))
  setnames(tbl, c('title', 'artist'))
  tbl[ , rank := .I]
  setcolorder(tbl, c(3, 1, 2))[]
}

get_malawi = function(...) {
  URL = 'http://www.malawi-music.com/music-charts'
  top_list = read_html(URL) %>% 
    html_nodes(xpath = '//div/span[@class="item_name"]') %>% html_text
  #replace FIRST hyphen with tripled hyphen to
  #  distinguish from subsequent hyphens
  top_list = sub('-', '---', top_list)
  tbl = setDT(tstrsplit(top_list, '(?<=[0-9])\\.\\s+|\\s+---\\s+', 
                        perl = TRUE))
  setnames(tbl, c('rank', 'artist', 'title'))
  tbl[ , rank := as.integer(rank)]
  setcolorder(tbl, c(1, 3, 2))[]
}

# Caribbean & Latin America ####
get_mexico = function(...) {
  URL = 'http://www.billboard.com/charts/regional-mexican-songs'
  pg = read_html(URL) 
  title = page %>%
    html_nodes(xpath = '//h2[@class="chart-row__song"]') %>% html_text
  artist = page %>% 
    html_nodes(xpath = '//*[@class="chart-row__artist"]') %>% html_text
  artist = trim_white(artist)
  data.table(rank = seq_len(length(title)), title, artist)
}

# seems to block connection from rvest?
# get_brazil = function(...) {
#   URL = 'http://www.billboard.com.br/rankings'
#   pg = read_html(URL) 
#   title = page %>%
#     html_nodes(xpath = '//h2[@class="chart-row__song"]') %>% html_text
#   artist = page %>% 
#     html_nodes(xpath = '//*[@class="chart-row__artist"]') %>% html_text
#   artist = trim_white(artist)
#   data.table(rank = seq_len(length(title)), title, artist)
# }

get_colombia = function(...) {
  URL = 'http://www.national-report.com/top-100-colombia/'
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//h2[@class="chart-row__song"]') %>% 
    #not sure where it's coming from, but the list is doubled
    html_text %>% `[`(seq_len(100))
  artist = pg %>% html_nodes(xpath = '//h3[@class="chart-row__artist"]') %>% 
    html_text %>% `[`(seq_len(100))
  data.table(rank = seq_len(length(title)), title, artist)
}

get_venezuela = function(...) {
  URL = 'http://www.recordreport.com.ve/publico/top100/'
  pg = read_html(URL)
  title = pg %>% html_nodes(xpath = '//td/i') %>% 
    html_text %>% gsub('"', '', .)
  artist = pg %>% html_nodes(xpath = '//td/strong') %>% html_text
  data.table(rank = seq_len(length(title)), title, artist)
}

get_guatemala = function(...) {
  URL = 'http://charts.monitorlatino.com/top20/Guatemala/General'
  pg = read_html(URL) 
  title = pg %>% 
    html_nodes(xpath = '//td/label[@class="Font_Gmedia"]') %>% html_text
  artist = pg %>% 
    html_nodes(xpath = '//td/label[@class="iArt"]') %>% html_text
  data.table(rank = seq_len(length(title)), title, artist)
}

# Europe ####
get_portugal = function(...) {
  URL = 'http://euro200.net/Portugal-Top50.htm'
  #ragged table
  tbl = read_html(URL) %>% 
    html_node('table') %>% html_table(fill = TRUE) %>% setDT
  #exclude irrelevant header/footer rows
  tbl = tbl[-c(1L:grep('this week', X2), (grep('25', X2) + 1L):.N)]
  #all relevant info in these two columns
  tbl = rbind(tbl[ , .(title_artist = X5)],
              tbl[ , .(title_artist = X11)])
  tbl[ , c("artist", "title") := tstrsplit(title_artist, split = "\\s-\\s")]
  tbl[ , artist := trim_white(gsub("\n", "", artist))]
  tbl[ , artist := gsub("(.*),\\s(THE)", "\\2 \\1", artist)]
  tbl[ , title := trim_white(gsub("\n", "", title))]
  tbl[ , title_artist := NULL]
  tbl[ , rank := .I][]
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
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
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
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_france = function(...) {
  URL = 'http://www.snepmusique.com/tops-semaine/top-singles-streaming/'
  track_column = read_html(URL) %>% 
    html_nodes(xpath = '//td[@class="atl"]')
  title = track_column %>% 
    html_nodes(xpath = '//p[@class="title"]') %>% html_text
  artist = track_column %>%
    html_nodes(xpath = '//strong[@class="artist"]') %>% html_text
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
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
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_germany = function(...) {
  URL = 'https://www.offiziellecharts.de/charts'
  track_column = read_html(URL) %>% 
    html_nodes(xpath = '//td[@class="ch-info"]')
  title = track_column %>% 
    html_nodes(xpath = '//span[@class="info-title"]') %>% html_text
  artist = track_column %>% 
    html_nodes(xpath = '//span[@class="info-artist"]') %>% html_text
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_switzerland = function(...) {
  #near identical to Belgium
  URL = 'http://hitparade.ch/charts/singles'
  page = read_html(URL)
  core.xp = '//table[@class="table650"]//td/a/'
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
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

# #page appears dynamic? perhaps need RSelenium-type approach
# get_italy = function(...) {
#   URL = 'http://www.fimi.it/classifiche#/category:digital/'
#   page = read_html(URL) %>% html_nodes(xpath = '//td[@class="chart-titolo"]')
#   title = tbl %>% html_nodes(xpath = '//div[@class="title"]') %>% html_text
#   artist = tbl %>% html_nodes(xpath = '//div[@class="subtitle"]') %>% html_text
# }

get_denmark = function(...) { 
  URL = 'http://hitlisten.nu/default.asp?list=t40'
  page = read_html(URL)
  title = page %>% html_nodes(xpath = '//div[@id="titel"]') %>%
    html_text %>% trim_white
  artist = page %>% html_nodes(xpath = '//div[@id="artistnavn"]') %>%
    html_text %>% trim_white
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_norway = function(...) {
  URL = 'http://lista.vg.no/'
  page = read_html(URL) 
  title = page %>% html_nodes(xpath = '//a[@class="album"]') %>% html_text
  artist = page %>% html_nodes(xpath = '//a[@class="artist"]') %>% html_text
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

# #page appears dynamic? perhaps need RSelenium-type approach
# get_sweden = function(...) {
#   URL = 'http://www.sverigetopplistan.se/'
#   page = read_html(URL)
#   title = page %>% html_nodes(xpath = '//span[@class="title"]') %>% html_text
# }

get_finland = function(...) {
  #first, grab most recent week from directory page
  site.stem = 'http://www.ifpi.fi'
  this.year = year(Sys.Date())
  dir.URL = paste0(site.stem, '/tilastot/virallinen-lista/singlet/')
  dir.xp = paste0('//h3[text()="', this.year, 
                  '"]/following-sibling::*/li[1]/a')
  URL = read_html(dir.URL) %>% html_node(xpath = dir.xp) %>% 
    html_attr("href") %>% paste0(site.stem, .)
  track_column = read_html(URL) %>%
    html_nodes(xpath = '//div[@class="album-info"]')
  title = track_column %>% 
    html_nodes(xpath = '//a[@class="title"]') %>% html_text()
  artist = track_column %>% 
    html_nodes(xpath = '//a[@class="artist"]') %>% html_text()
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

# #page appears dynamic? perhaps need RSelenium-type approach
# get_russia = function(...) {
#   URL = 'https://www.tophit.ru/en/chart/common/week'
#   tbl = read_html(URL) %>% html_node(xpath = '//td')
# }

get_poland = function(...) {
  URL = 'http://bestsellery.zpav.pl/airplays/top.php'
  read_html(URL) %>%
    html_node(xpath = '//table[@class="airplaytable"]') %>%
    #4th column is record label
    html_table %>% `[`( , -4L) %>% setDT %>% 
    setnames(c('rank', 'artist', 'title')) %>% 
    setcolorder(c(1L, 3L, 2L)) %>% return
}

get_belarus = function(...) {
  URL = 'http://unistar.by/music/top20/'
  #must prefix by table[@class...] because there's
  #  a second table (of near-misses?) underneath with same div logic
  table.xp = '//table[@class="music-top20"]//div[@class="txt-plist__info"]'
  track.list = read_html(URL) %>% 
    html_nodes(xpath = table.xp) %>% html_text %>%
    #format is basically <div><span>ARTIST</span>TITLE</div>;
    #  couldn't figure out how to get TITLE & exclude ARTIST,
    #  so just getting them together and strsplitting them apart;
    #  not water-tight, but seems to work
    strsplit('[\t\n]') %>% lapply(function(x) x[nchar(x) > 0]) %>% 
    transpose %>% setDF %>% setDT(keep.rownames = TRUE) %>%
    setnames(c('rank', 'arist', 'title')) %>% return
}

# #page appears dynamic? perhaps need RSelenium-type approach
# get_ukraine = function(...) {
#   URL = 'http://fdr.com.ua/charts.php'
#   read_html(URL) %>% html_node(xpath = '//table[@id="teblechart"]') %>%
#     html_table
# }

get_austria = function(...) {
  URL = 'http://www.austriancharts.at/charts/singles'
  track_column.xp = '//td[@style = "padding-top:6px;cursor:pointer;"]/a'
  track_column = read_html(URL) %>% html_nodes(xpath = track_column.xp)
  #structure: <td><a><b>Artist</b>Title</a></td>
  #  so artist is easy, but need to exclude <b>
  #  to get title
  artist = track_column %>% html_nodes('b') %>% html_text
  #see http://stackoverflow.com/a/7122258/3576984
  title.xp = '//td/a/node()[not(self::b)]'
  title = track_column %>% html_nodes(xpath = title.xp) %>% html_text
  #some blank nodes were included somehow; delete now
  title = title[nzchar(trim_white(title))]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_croatia = function(...) {
  URL = 'http://radio.hrt.hr/aod/arc-top-40/194069/'
  tbl = read_html(URL) %>% html_node('table') %>% html_table %>% setDT
  head.idx = grep("^artist$", tbl$X2)
  tbl = tbl[-seq_len(head.idx)]
  #artist/title text is neighbored to some
  #  extraneous info (websites, album titles);
  #  rather than dealing with that (annoying),
  #  just use regex to split the wheat from the chaff
  tbl[ , X2 := gsub('^([^a-z]+)[a-z].*', '\\1', X2)]
  tbl[ , X3 := gsub('^([^a-z]+)[a-z].*', '\\1', X3)]
  setnames(tbl, c('rank', 'artist', 'title'))
  setcolorder(tbl, c(1L, 3L, 2L))[]
}

get_greece = function(...) {
  URL = 'http://www.ifpi.gr/airplay_en.html'
  tbl = read_html(URL) %>% html_node('table') %>% 
    html_table(header = TRUE) %>% setDT
  setnames(tbl, c('Rank', 'Artist', 'Title'),
           c('rank', 'artist', 'title'))
  tbl[ , c('Company', 'Week-1', 'Status') := NULL]
  setcolorder(tbl, c(1L, 3L, 2L))[]
}

# both CR & Slovakia read_html fail
# get_czech_republic = function(...) {
#   URL = 'http://www.ifpicr.cz/hitparada/'
# }
# 
# get_slovakia = function(...) {
#   URL = 'http://www.ifpicr.cz/hitparada/'
# }

get_hungary = function(...) {
  URL = 'http://zene.slagerlistak.hu/radios-top-40-jatszasi-lista'
  track_column = read_html(URL) %>%
    html_nodes(xpath = '//td[@class="lemez_sor"]')
  artist = track_column %>% 
    html_nodes(xpath = '//span[@class="eloado"]') %>% html_text
  title.xp = '//td[@class="lemez_sor"]/node()[not(self::span)]'
  title = track_column %>% html_nodes(xpath = title.xp) %>% 
    html_text %>% trim_white
  title = title[nzchar(title)]
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

# #page appears dynamic? perhaps need RSelenium-type approach
# get_slovenia = function(...) {
#   URL = 'http://www.slotop50.si/Lestvice/Tedenske-lestvice'
#   pg = read_html(URL)
#   artist = pg %>% html_nodes(xpath = '//div[@class="lw-artist"]') %>% html_text
# }

# #page appears dynamic? perhaps need RSelenium-type approach
# get_romania = function(...) {
#   URL = 'http://www.mediaforest.ro/WeeklyCharts/HistoryWeeklyCharts.aspx'
#   tbl = read_html(URL) %>% 
#     html_node(xpath = '//tbody[@id="tb_WeeklyChartRadioLocal"]')
# }

get_lithuania = function(...) {
  URL = 'http://rc.lt/top15/'
  track_column = read_html(URL) %>% 
    html_nodes(xpath = '//div[@class="post_title"]') %>%
    html_text %>% trim_white
  title = gsub('.*–\\s+', '', track_column)
  artist = gsub('\\s+–.*', '', track_column)
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_bulgaria = function(...) {
  #very similar to Portugal (see there for comments)
  URL = 'http://euro200.net/Bulgarije-top-40.htm'
  tbl = read_html(URL) %>% 
    html_node('table') %>% html_table(fill = TRUE) %>% setDT
  tbl = tbl[-c(1L:grep('this week', X2), (grep('^20$', X2) + 1L):.N)]
  tbl = rbind(tbl[ , .(title_artist = X5)],
              tbl[ , .(title_artist = X11)])
  tbl[ , c("artist", "title") := tstrsplit(title_artist, split = "\\s-\\s")]
  tbl[ , artist := trim_white(gsub("\n", "", artist))]
  tbl[ , artist := gsub("(.*),\\s(THE)", "\\2 \\1", artist)]
  tbl[ , title := trim_white(gsub("\n", "", title))]
  tbl[ , title_artist := NULL]
  tbl[ , rank := .I][]
}

# Anglosphere ####
get_usa = function(...) {
  #identical to Mexico
  URL = 'http://www.billboard.com/charts/hot-100'
  page = read_html(URL) 
  title = page %>%
    html_nodes(xpath = '//h2[@class="chart-row__song"]') %>% html_text
  artist = page %>% 
    html_nodes(xpath = '//*[@class="chart-row__artist"]') %>% html_text
  artist = trim_white(artist)
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_canada = function(...) {
  #identical to Mexico & USA
  URL = 'http://www.billboard.com/charts/canadian-hot-100'
  page = read_html(URL) 
  title = page %>%
    html_nodes(xpath = '//h2[@class="chart-row__song"]') %>% html_text
  artist = page %>% 
    html_nodes(xpath = '//*[@class="chart-row__artist"]') %>% html_text
  artist = trim_white(artist)
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

get_uk = function(...) {
  URL = 'http://www.officialcharts.com/charts/singles-chart/'
  track_column = read_html(URL) %>%
    html_nodes(xpath = '//div[@class="title-artist"]')
  title = track_column %>% 
    html_nodes(xpath = '//div[@class="title"]') %>% 
    html_text %>% trim_white
  artist = track_column %>% 
    html_nodes(xpath = '//div[@class="artist"]') %>% 
    html_text %>% trim_white
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}

# #page appears dynamic? perhaps need RSelenium-type approach
# get_ireland = function(...) {
#   URL = 'http://www.irma.ie/#chartTab1'
#   page = read_html(URL) %>% html_nodes("table")
# }

# #page appears dynamic? perhaps need RSelenium-type approach
# get_australia = function(...) {
#   URL = 'http://www.ariacharts.com.au/charts/singles-chart'
#   page = read_html(URL)
#   title = page %>% html_nodes(xpath = '//div[@class="item-title"]') %>% html_text
# }

get_new_zealand = function(...) {
  URL = 'http://nztop40.co.nz/'
  page = read_html(URL)
  title = page %>% 
    html_nodes(xpath = '//article//*[@class="title"]') %>% html_text
  artist = page %>% 
    html_nodes(xpath = '//article//*[@class="artist"]') %>% html_text
  setDT(data.frame(title, artist), keep.rownames = "rank")[]
}
