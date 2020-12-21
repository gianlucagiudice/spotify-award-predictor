# Disco d'oro predictor

Dataset:

https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks

Il dataset integrato è il file "data/integrated_data.csv"

## Cose da scrivere nel file latex
- Abbiamo preso il dataset di spotify
- Per etichettare una canzone come di successo potevamno guardare la distribuzione e considerare una canzone di succeso se aveva il valoe di popolarità oltre 2 volte la sd
- Abbiamo deciso di optare per una canzone di successo avendo una metrica oggettiva, overro i premi venti: Dischi di platino, dischi d'oro, ecc....
- A questo scopo (punto precedente) abbiamo creato uno scraper che da wikipedia scarica i dischi d'oro e di platino per diversi paesi:
    - Italia
    - usa
    - uk
    - australia
    - namiarca
- Screen bello bello dello scraper e della tabella di wikipedia
- Abbiamo unito le istanze in un dataframe unico indicando il titolo vinto e lo stato in cui è stato vinto il premipo
- Abbiamo considerato solo le istanze univoce dal momento in cui una canzone può vincere lo un premio in diversi stati 
    - Abbiamo ottenuto un totale di 5720 istanze univoce
- Abbiamo pulito il dataset utilizzando come strumento openRefine
    - Screen bello bello del tool utilizzato
    - Cosa si intende per aver puito?
        - Pulito le date con una regex dal momento in cui potevano esserci alcune piccole imperfezioni nella tabella scrapata da wiikipedia
            - Abbiamo utilizzato delle regex
        - Convertito in lowercase
    - Pulito il dataset anche per quanto riguarda i dati di spotify
        - Tolto le istanze uguali considerandolo come uguali se avevano stesso titolo e stessi autotri
- Record linkage
    - Abbiamo caricato tutte le istanze in un database documentale (mongoDB)
        - Utile un documentale perchè per modellare una canzone del dataset di spotifuy artista abbiamo utilizzato una lista di artisti
    - Abbiamo sostitutio tutti i caratteri accentati degli artisti in carattere "plain" (Es. è -> e; ù -> u; ö -> o)
    - Abbiamo fatto una join su titolo uguale e un artista che coincide nella lista degli artisti
- Abbiamo trasformato la lista degli artisti in una stringa
- Abbiamo fato il dump del tabase in formato csv
