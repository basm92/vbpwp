successiewet1878 <- data.frame(politician = c("K.H. de Jong"," de Beaufort","
                                              Idzerda"," Verniers van der Loeff"," Bredius jr "," Patijn"," 
                                              Rombach"," Hingst"," de Vos van Steenwijk "," Blussé van oud Alblas"," van der Kaay",
                                              "Sandberg","Stieltjes"," Mees"," Viruly Verbrugge "," van de Werk ",
                                              " van Stolk"," van der Feltz", "van Harinxma thoe Slooten",
                                              "Oldenhuis gratama"," Sickesz"," Bastert"," Gevers Deynoot",
                                              "Luyben"," Moens"," Bredius"," van Delden"," Schepel",
                                              "Lenting"," Godefroi"," de Meijier"," Goeman Borgesius"," Bergsma",
                                              "de Ruiter Zylker"," van Tienlioven"," Dijckmeester"," Roell",
                                              " Vening Meinesz"," van Houten"," Cremers"," Mirandolle",
                                              " Fransen van de Putte"," de Bruyn Kops"," Wybenga"," van Heukelom",
                                              " Holtzman","D. van Eck"," Dullert"
)) %>%
  mutate(politician = stringr::str_trim(politician), vote = rep(1, length(politician)))


politician <- c("Cremer van den Berch van Heemstede"," Schimmelpenninck van der Oye"," Bichon van IJsselmonde"," 
                Reekers"," van Asch van Wijck"," Heydenrijck","
                de Casembroot","G.E.F.X.M. Kerens de Wylré"," van Kerkwijk "," 
                de Bieberstein Rogalla Zawadsky"," van Nispen tot Sevenaer"," 
                Corver Hooft"," Begram"," van der Schrieck"," Teding van Berkhout"," Haffmans"," 
                van Baar"," Borret"," Verheijen"," Mackay"," Lambrechts"," 
                van Zinnicq Bergmann"," Saaymans Vader"," Insinger"," 
                Schagen van Leeuwen"," de Jonge"," 
                Schimmelpenninck","H.A. Amorie Hoeven"," Arnoldts"," 
                Brouwers"," van Wassenaer van Catwijck"," Wintgens") %>%
  stringr::str_trim()

vote <- rep(0, length(politician))

successiewet1878 <- rbind(successiewet1878, cbind(politician, vote)) %>%
  mutate(law = "Successiewet 1878", date = "1878-05-24", house = "Tweede Kamer")

