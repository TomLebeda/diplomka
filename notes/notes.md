# Vizualizace:
- Software/engine na vizualizace:
	- Graphviz -> nejpopulárnější, největší, největší možnosti nastavení
	- D2 -> relativně nový, menší možnost nastavení
	- zkusil jsem oba, Graphviz vypadal slibněji, ale ani jedna konfigurace/engine/layout nebyly moc dobré
		- grafy byly velmi dlouhé nebo úzké nebo zamotané
	- možnost řešení 1: nechat to prozatím být, udělat na předváděčku nějaký jeden jednodušší
	- možnost řešení 2: použít generovaný graf jako základ, importovat do Draw.io, ručně upravit layout, exportovat SVG, které by pak šlo dynamicky měnit (zapínat/vypínat jeho části)
- Forma vizualizace: 
	- více různých grafů/sítí/tabulek pro různé "úrovně": hierarchie objektů (stromy), síť dynamických vazeb, objekty + vlastnosti (UML-like tabulky), kombinace předchozích
	- prozatím "jen offline" -> exportují statické SVG (nebo PNG)
	-> je to takhle ok? 

# Gramatiky: 
- mám implementovaný a otestovaný vlastní jazyk (zjednodušená verze SRGS-ABNF)
- během testování jsem zjistil, že původní greedy matching nebude stačit
	- např text `hnědý pes honí oranžovou malou veverku` a pravidlo na chytání barev `$color $GARBAGE<0-> $object` chytilo "brown squirrel" a "orange squirrel"
		- greedy matching přes $GARBAGE zachytil nejvzdálenější objekt, což je vždy veverka
	=> musel jsem přidat i lazy matching => vypadalo slibně, ale to zase pro změnu vezme vždy nejbližší možnost -> pro případy kde je v jedné větě více možností vezme ale jen tu nejkratší
- jinými slovy: pro různé případy jsou vhodné různé matching strategie, ale ani tyto dvě nepokryjí vše
	- mám nápad na matching strategii která by měla pokrýt všechny možnosti, ale má dva problémy:
		1. kombinatorická exploze pro delší pravidla/věty (pro ručně psané pravidla pravděpodobně ještě zvládnutelné)
		2. agresivní hledání tvarů bude téměř jistě vracet falešné/nesmyslné triplety, obzvláště pro delší věty
- pročetl jsem si přepisy popisů plného obrázku a očekávám že bude problém napsat gramatiky tak, aby spolehlivě postihly alespoň většinu formulací
	- z anotovaných dat by šlo automatizovat generování pravidel
	- experimentálně jsem zkoušel namátkou předhodit větu + triplet ChatGPT s otázkou ve stylu "je v té větě obsažen ten triplet" a výsledky vypadaly slibně
	- lemmatizace by mohla pomoct (zjednodušení a zrychlení)

# Využití AI pro generaci dat:
- ChatGPT -> výsledky většinou jakž-takž, nicméně obsluha problematická
- Copilot -> výsledky srovnatelné s ChatGPT, nicméně obrázkový vstup mi nefunguje (opakovaně, konzistentně)
- Gemini (bývalý Bard) -> katastrofa, většina halucinací
- nápad: zkusit vlastní model (Llama-based?) přes framework Burn

# Ztrátové funkce/tabulky: 
- mám nějaké skutečně zkusit sestavit nebo jen připravit systém aby je dokázal přijmout a pak "narychlo" sestavit nějaké dummy hodnoty?
- možnost hodnotit podle ID, ne jen podle tagů

# Forma/styl/prezentace výsledků:
- myslím že je pomalu čas začít dokončovat -> jak by měl vypadat výsledek/ukázka? Co do něj zahrnout?

# Navazující studium
- jen se ujistit, že jsou všichni "na stejné notě"
