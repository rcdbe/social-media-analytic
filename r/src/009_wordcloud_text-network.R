# Import Library
library(readr)
library(e1071)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(igraph)

# Import Data
df <- read_csv("data/processed/berita-batubara.csv")

# Create a Wordcloud Function
wc <- function(df) {
  corpus <- Corpus(VectorSource(df$text))
  corpus <- tm_map(corpus, removeWords, c(
    "sdh", "the", "krn", "nya", "udh", "gua", "ttg",
    "liat", "gimana", "cari", "iya", "tolong", "isi", "deh", "gara", "tujuan", "makan",
    "bbrp", "dengan", "yuk", "kau", "dgn", "makanan", "kqf", "cek", "gak", "via",
    "nak", "dpt", "mknn", "with", "pic", "cod", "dri", "hub", "samping", "pin", "ready", "dijamin", "terbukti",
    "best", "hrga", "isu", "bahas", "menemukan", "rilis", "kto", "sms", "our", "kat", "hai",
    "wajahmu", "takyah", "relay", "fda", "komedo", "bcc", "tga", "chitin", "atasi", "bitly",
    "pengen", "diikuti", "jiexpo", "sedia", "arealampung", "areajakarta", "googlovku",
    "idealkanlah", "bihalal", "diada", "bitlyjfysvi", "bitlylkwtt", "bitlyjfchou",
    "bitlylbvun", "bitlyyxqwio", "partymsh", "soon", "salamcoming", "and", "sent", "booth", "areasumut",
    "gemilangmalangorgberitabupati", "gemilangmalangorgberitahalalb", "hadirihalalbihalalpgri ",
    "ihalalkeluargabesardinkes", "hadirihalalbihalalkmbkecamatankepanjen", "hadirisilaturahmidanhalalbihalaldismpsmkislamdonomulyo",
    "ihalalhutrikesimbolsemangatnasionalisme", "pencarian", "dilirik", "ajak", "smswa", "terpercaya",
    "termurah", "apli", "pincb", "fdatga", "aee", "pinbf", "berminyak", " pinaaa", "callsms", "pinff",
    "mengencangkan", "peninggibadanherbalwebid", "flickrcomphotos", "obatpeninggibadanasia", "pindbb ", "sis", "ampuh", "berbpom", "abc", "loh", "only",
    "jgn", "pinbdefb", "pictwittercomcvcggdi ", "pictwittercomaqnqxkvpd ", "pinecb", "bitlyphpwtc", "bed", "hadiahnya", "jepangkoreajepangtaiwanchinese",
    "alhamdulillah", "worry", "promosikan", "menghilangkan ", "more", "busui", "tinyurlcomonj", "bitlywoik", "bitlywojoe", "luncurkan", "apnih", "dtozym",
    "gaesaman", "solusinya", "idealkan", "pinfbbd", "dicari", "mimin", "agen", "oncomcontentviewarticleidamoscozyhotelconventionhalljakartagelarhalalbihalalcatiditemid oncomcontentviewarticleidamoscozyhotelconventionhalljakartagelarhalalbihalalcatiditemid
travelxposecomindexphpopti", "sertifikasi", "bitlybsplx", "oncomcontentviewarticleidamoscozyhotelconventionhalljakartagelarhalalbihalalcatiditemid   35
travelxposecomindexphpopti", "oncomcontentviewarticleidamoscozyhotelconventionhalljakartagelarhalalbihalalcatiditemid", "travelxposecomindexphpopti", "pilih", "detikcom", "republikacoidberitagayahi", "lifevivacoidnewsread", "twittercomberitasatusta",
    "gosumbarcomberitabaca", "pustakalewinetmodberitaid", "bitlyrbwlb", "astroawanicomberitamalaysi", "khazanahrepublikacoidberitaduniai", "goriaucomberitapemerin", "republikacoidberitaekonomi", "antaranewscomberita", "bitlyxsnjl", "republikacoidberitaduniai",
    "bitlylnth", "jtan", "all", "butuh", "pictwittercomndbndtzx ",
    "bilang", "trmsk", "inc", "pictwittercomndbnvur", "pictwittercomwglqgxquc", "pictwittercomwglqgprma",
    "maksimalkan", "pinde", "mengganggu", "pictwittercomaqnqxkvpd", "pictwittercomndbndtzx", "pictwittercomcvcggdi", "stp", "info", "klik", "friendly", "gritc", "for", "tinyurlcomybxccqb", "republikaonline", "wisatacehcomalasan", "kotabandaacehjadiwisatahtml", "bitlyspkok", "villadilembangcoid",
    "nasionalnewsvivacoidnewsread", "sikit", "raya", "pustakalewicommodberitaid", "feedproxygooglecomrislamposmed", "pictwittercombbcqckl", "memintamintahrbukhari", "twittercomnasibakarjkt", "pictwittercomprhxrewy"
  ))
  
  DTM <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
  DTM <- removeSparseTerms(DTM, 0.98)
  
  mat <- as.matrix(DTM)
  f <- sort(rowSums(mat), decreasing = TRUE)
  dat <- data.frame(word = names(f), freq = f)
  wc <- wordcloud(
    words = dat$word,
    freq = dat$freq,
    random.order = FALSE,
    rot.per = 0.30,
    colors = brewer.pal(8, "Dark2")
  )
  
  q <- mat %*% t(mat)
  g <- graph.adjacency(q, weighted = T, mode = "undirected")
  g <- simplify(g)
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  layout <- layout_in_circle(g)
  plot(g, layout = layout)
}

# Apply to Dataset
wc(df)
