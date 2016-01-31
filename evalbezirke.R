# Visualisierung der Wahlbeteiligung nach Wahllokalen
# bei der OB-Wahl 2015 in Wuppertal
#
# OK Lab Wuppertal
# Christian Oswald (info@coswald.eu)
# [25.10.2015]

# Wahlergebnisse laden
ergebnisse <- read.csv('ob_wahl2015.csv', comment.char = '#')
#ergebnisse <- read.csv('ob_stichwahl2015.csv', comment.char = '#')

# Addiere jeweils die Anzahl der Wahlberechtigten und der 
# abgegebenen Stimmen fuer die einzelnen Wahllokale. 
# (Einem Wahllokal können mehrere Stimmbezirke zugeordnet sein.)
ergebnisse <- aggregate(cbind(waehler,stimmen) ~ adresse, data=ergebnisse, FUN=sum)

# Entferne die Briefwahlbezirke aus der Tabelle
ergebnisse <- ergebnisse[ergebnisse$waehler!=0,]

# Berechne die Wahlbeteiligung je Wahllokal
ergebnisse$beteiligung <- ergebnisse$stimmen / ergebnisse$waehler

# Lade die Koordinaten der Wahllokale
koordinaten <- read.csv('stimmbezirke.csv', comment.char = '#')

# Entferne doppelte Eintraege aus der Koordianten-Tabelle 
# (gleiches Wahllokal fuer mehrere Stimmbezirke)
koordinaten <- unique(koordinaten)

# Ordne den Wahllokalen die Koordinaten zu
ergebnisse <- merge(koordinaten, ergebnisse, by='adresse')

# Visualisierung der Wahlbeteiligung mit Leaflet und OpenStreetMap
library(leaflet)
m <- leaflet()
m <- addTiles(m)

# Definiere Farbpalette für Wahlbeteiligung von 10% bis 60% in Schritten von 5%
pal <- colorBin(palette=c("#000000", "#FF0000", "#FF8400"), domain = c(0.1,0.6), bins=10, pretty=FALSE)

# Füge der Karte die Wahllokale als Punkte hinzu
# Die Farbe der Punkte wird durch die Wahlbeteiligung bestimmt.
m <- addCircleMarkers(m, data=ergebnisse, radius = 10, color = ~pal(beteiligung), stroke = FALSE, fillOpacity = 0.8)

# Füge der Karte eine Legende hinzu
m <- addLegend(m, position="topleft", pal = pal, values=ergebnisse$beteiligung,opacity=0.8,title="Wahlbeteiligung",labFormat = labelFormat(suffix = '%', transform = function(x) 100 * x))

# Speichere Karte als HTML
library(htmlwidgets)
saveWidget(m, file="output.html")