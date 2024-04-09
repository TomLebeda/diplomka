# Formát výstupu (csv):
`[type],[catch],[src]`
kde:
- `[type]` je jedna z hodnot `objekt` nebo `attribute` nebo `triplet`
- `[src]` je původní věta, ze které byl záznam získán
- `[catch]` je samotná reprezentace zachyceného objektu nebo atributu nebo tripletu ve formátech:
    - objekt: `[object_name]` 
    - atribut: `[object_name]:[attribute_name]=[attribute_value]`
    - triplet: `[from_object] [predicate] [target_object]`

# Výstupní sémantické příznaky:
- Scéna: 
    - absolutní hodnoty:
        - počet objektů na scéně
        - počet atributů (přes všechny objekty) na scéně
        - počet tripletů na scéně
    - statistiky:
        - průměrný počet atributů na objekt
        - medián počtu tripletů na objekt
        - [?] histogram počtu atributů na objekt (kolik objektů má `n` atributů)
    - výčty:
        - list unikátních jmen objektů [s/bez číslování?]
        - list typů atributů
        - list kategorií objektů (tagy)
        - `pro každou kategorii:` počet příslušejících objektů
- Text:
    - absolutní hodnoty:
        - kolik objektů bylo řečeno [kolikrát opakování?]
        - kolik atributů bylo řečeno
        - kolik tripletů bylo řečeno
    - statistiky:
        - kolik atributů bylo řečeno průměrně na jeden objekt
        - jaká byla variance v počtu atributů řečených na objekt
        - jaký typ atributu byl řečen nejčastěji
    - výčty:
        - list řečených objektů [chronologicky, včetně opakování?]
        - list řečených atributů [chronologicky, včetně opakování?]
        - list řečených tripletů [chronologicky, včetně opakování?]
- [?] Delta-příznaky:
    - absolutní hodnoty:
        - kolik objektů chybělo celkem
        - kolik atributů chybělo celkem
        - kolik tripletů chybělo celkem
        - kolik bylo atributů řečeno s chybnou hodnotou
    - statistiky:
        - histogram počtu zapomenutých objektů podle tagů
        - histogram počtu chybných hodnot podle typů atributů

