arbeidswet1919 <- data.frame(politician = c(
  "A.P. Staalman"," de Monté ver Loren"," van Wijnbergen"," Schokking"," Heemskerk"," Treub","
  Bakker"," Schaper"," Ketelaar"," Rugge","J.E.W. Duijs"," Groeneweg","
  Helsdingen"," Kleerekoper"," van der Waerden"," Ossendorp"," Bomans"," Duymaer van Twist"," Troelstra"," Teenstra","
  Swane"," van der Voort van Zijp"," Kolkman"," Snoeck Henkemans"," de Geer"," Oud","
  Bongaerts"," Dresselhuys"," van Rijzewijk"," van de Laar"," Niemeijer","A.G.Æ. ridder van Rappard","P.J. Reymer","
  Koolen"," de Jonge"," Beumer"," Kuiper"," Oudegeest"," van den Tempel"," van de Bilt"," L. M. Hermans"," de Zeeuw"," Heijkoop","
  Smeenk"," Deckers"," Lely"," de Wilde"," van Dijk"," Wintermans","
  van Schaik"," Haazevoet"," Nolens"," van Zadelhoff"," Poels"," Ter Hall"," de Savornin Lohman","
  de Groot"," H. Hermans","
  Schouten"," van Veen"," Engels"," Zijlstra"," Fleskens"," Bulten","
  Rutgers"," van Ryckevorsel"," J. ter Laan"," A. Staalman"," Fock"

)) %>%
  mutate(politician = stringr::str_trim(politician), vote = rep(1, length(politician)))

politician <- c("van Ravesteijn", "Wijnkoop", "Kolthek")
vote <- rep(0, length(politician))

arbeidswet1919 <- rbind(arbeidswet1919, cbind(politician, vote)) %>%
  mutate(law = "Arbeidswet 1919", date = "1919-07-11", house = "Tweede Kamer")

arbeidswet1919