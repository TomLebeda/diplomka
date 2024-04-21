# Dědičnost
 - objekty můžou mít hierarchii (i multi-parentální) => nabízí se možnost dědičnosti vlastností a tripletů
 - např: pták sedí na větvi + větev je součástí stromu => pták sedí na stromě
 - problém: nelze zobecnit
```
list 1 ──┬── větev 1 ──╮
list 2 ──╯             ├── strom 1 ──╮
list 3 ──┬── větev 2 ──╯             ├── les
list 4 ──╯                           │
list 5 ──┬── větev 3 ───── strom 2 ──╯
list 6 ──╯

veverka sedí na větvi 1   -> lze dědit jednou směrem nahoru
veverka sedí na stromě 1   -> mohl bych dědit jednou dolů, ale nevím do kterého potomka
veverka sedí na stromě 2   -> lze dědit jednou směrem dolů
veverka je ve stromě 2     -> lze dědit jednou směrem nahoru
=> pro různé predikáty a různé hierarchie lze napsat pravidla dědičnosti, ale ne obecně
```
 - možnost 1: dědičnost nedělat
 - možnost 2: dědičnost udělat parametricky (bylo by potřeba specifikovat co a kde lze dědit)
 -! možnost 3: napsat jako potenciální rozšíření (+ vysvětlit na ukázkách)

# Detekce objektů
- současný stav: detekce objektů nezávisle, bez číslování
- potenciální rozšíření: detekce objektů z tripletů a atributů, umožnilo by číslování (ne všude)
- zatím není třeba

# Zájmena
- současný stav: zájmena neřeším
- potenciální rozšíření: v místě zájmena předpokládat nejbližší předchozí osobu/zvíře s odpovídajícím rodem (m/ž/s)
```
vlevo sedí žena na lehátku čte si knihu pod slunečníkem
vedle sebe má rozprostřenou piknikovou deku s košíkem a s jídlem na piknik
```

# Pozice a velikosti objektů
- současný stav: mám informace o bboxu v obrázku, ale zatím je nepoužívám
- hlavní problém: kompletní automatizace obtížná kvůli perspektivě a subjektivnímu hodnocení (malý/velký, před/za, pod/nad) 
- možnost 1: nechat jako potenciální rozšíření do budoucna
- možnost 2: udělat matice, které by definovaly vzájemné pozice mezi všemi objekty
-! možnost 3: ručně doplnit triplety zohledňující pozici/velikost tam, kde to má nějaký význam
+ snaha navrhovat obrázky i gramatiky tak, aby nevznikaly pokud možno nejednoznačnosti

# Vlastní SRGS-like jazyk/formát:
- název + zkratka?:
    -> Semantic Parsing Grammar Format (SPGF)
- formální definice jako příloha nebo ("přímo" do pdf)?

# Hodnocení přes ztrátové funkce:
- současný stav: infrastruktura je na to připravená (tagy + filtry), ale není implementováno
- problém: potřeba definovat ztrátové funkce (matice?)
- možnost 1: nechat jako potenciální rozšíření s tím, že na to je systém připraven
- možnost 2: udělat jen nějakou ukázku, ale nezabývat se "plným řešením"
- možnost 3: implementovat plnou verzi včetně ztrátových funkcí (nutná expertní znalost?)
+ implementovat "dummy" hodnoty

# Vyhodnocení
- mám udělat pouze jednotlivé "malé" ukázky funkčnosti s vysvětlením, nebo se snažit i o nějaký statistické vyhodnocení úspěšnosti?
+ ukázka/srovnání zdravého a nezdravého popisu
- v případě statistického vyhodnocení: podle čeho hodnotit úspěšnost?
    - možnost 1: s ručně vytvořenými anotacemi (to bych ale vytvářel já sám, tak nevím jak moc to bude vypovídající)
    - možnost 2: subjektivní hodnocení (ideálně expertů?)
- obrázky: mám shánět nějaké další + anotovat + sepisovat gramatiky?
- extrakce tripletů z textu: snažit se nasbírat data, nebo jen zmínit jako možné alternativy?
    - srovnání s AI výstupem?
    - srovnání s latentním prostorem?

# Osnova práce (orientační):
1. Úvod
2. Teoretická část
    - sémantika
    - porozumění řeči
    - existující (podobné) metody
3. Praktická část
    - návrh celkového systému, koncept
    - popis scény
    - sémantické zpracování textu
        - nejdříve existující SRGS
        - potom vlastní implementace
4. Vyhodnocení
    - současná kvalita/funkčnost
    - současná omezení
    - potenciální použití
    - možná budoucí rozšíření
