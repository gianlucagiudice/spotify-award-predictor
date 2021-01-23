# Disco d'oro predictor
Dataset:

https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks

Il dataset integrato è il file "data/songs.csv"

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
- **Data enrichment** -  Record linkage
    - Abbiamo caricato tutte le istanze in un database documentale (mongoDB)
        - Utile un documentale perchè per modellare una canzone del dataset di spotifuy artista abbiamo utilizzato una lista di artisti
    - Abbiamo sostitutio tutti i caratteri accentati degli artisti in carattere "plain" (Es. è -> e; ù -> u; ö -> o)
    - Abbiamo fatto una join su titolo uguale e un artista che coincide nella lista degli artisti
- Abbiamo trasformato la lista degli artisti in una stringa
- Abbiamo fato il dump del tabase in formato csv

## Osservazioni
- Threshold = 2 in quanto vogliamo escludere gli artisti che hanno fatto una sola canzone (Anche perchè abbiamo fatto undersampling) 
- Feature extraction con PCA

- Migliorare la PCA
  - Seguire laboratorio fersini
  - Non visualizziamo in 2 dimensioni in quanto la varianza spiegata è pochissima e quindi inutile

- La frequency degli artisti deve essere >= 0 perchè è importante quelli che non vincono
- Dal pairplot risulta che esiste correlazione tra alcune variabili (es loudness:energy)
    - Questo è ancora più chiaro dalla matrice di covarianze
- Dal pairplot possiamo osservare anche la distribuzione dei valori delle features
    - Emerge che alcune features sono inutili: instrumentalness
- Far notare che chiaramente non useremo la feature popularity per il training del modellos
- Far notare che facendo training DOPO aver proiettato gli individui sulle componeneti principali, per previsioni di futuri individui sarà necessario proiettare gli individui nel nuovo spazio usando i primi k autovettori ordinati in ordine decrescente in base ai relativi autovalori associati, dove gli autovalori e autovettori sono il risultato della singular value decomposition della matrice di covarianza.
- Variabile categorica key viene trasformata in intero e non viene utilizzata una rappresentazione hot-encoding in quanto per le chiavi ddi un'ottava è possibile consideraree una relalzione d'ordine totale (e.g. sol < la < si < ...) 
- Model selection: 10-fold cross validtion per stimare gamma, c


## Domande
- Può essere un buon metodo fare un samplimng per motivi di data visualization avendo un numero di osservazioni molto ampio?
- Dotplot metric ROC
- Grid search