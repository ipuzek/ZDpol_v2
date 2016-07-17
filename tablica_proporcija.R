# osnovna tablica proporcija ----
## osnovne proporcije - postoci retka 
sk.prop <- select(data.2011, akt__zaposleni:akt__ostali_neaktivni) %>% 
  as.matrix %>% prop.table(margin = 1) %>% as.data.frame()

## agregati po dijelovima za ukupni podatak
dijelovi <- select(data.2011, ZD.dijelovi, brojstan__15plus, akt__zaposleni : akt__ostali_neaktivni) %>%
  group_by(ZD.dijelovi) %>%
  summarise_each(funs(sum), 
                 starts_with("akt__"), brojstan__15plus)

## postoci retka za agregate
dij.prop <- select(dijelovi,
                   akt__zaposleni : akt__ostali_neaktivni) %>% 
  as.matrix %>% prop.table(margin = 1) %>% as.data.frame()

## dodaj (vrati) identifikatore
sk.prop$SK <- data.2011$SK
sk.prop$ZD.dijelovi <- data.2011$ZD.dijelovi

dij.prop$SK <- dijelovi$ZD.dijelovi # ružno i zbunjujuće, ali radi

## dodaj i broj stanovnika
sk.prop$brojstan__15plus <- data.2011$brojstan__15plus
dij.prop$brojstan__15plus <- dijelovi$brojstan__15plus

sk.prop.fin <- filter(sk.prop, ZD.dijelovi == "Poluotok") %>%
  select(-ZD.dijelovi)

props <- rbind(dij.prop, sk.prop.fin) %>%
  filter(SK != "non_ZD") %>%
  select(SK, akt__zaposleni:akt__ostali_neaktivni, brojstan__15plus)

## mali pimp labela
names(props) <- stringr::str_replace(names(props), "akt__", "")

## zapiši
# write.csv2(props, "aktivnost_postoci.csv")

### ----