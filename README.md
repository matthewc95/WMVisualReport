# SAP WM Visual Monitoring Dashboard

Un report ABAP completo per il monitoraggio visuale delle attività di magazzino SAP WM (Warehouse Management) su SAP ECC EHP7.

## Caratteristiche

### Dashboard Panoramica
- **KPI Real-time**: Visualizzazione immediata delle metriche principali
- **Indicatori di stato**: Semafori colorati per identificare situazioni critiche
- **Occupazione ubicazioni**: Percentuali e grafici a barre ASCII
- **Stato ordini di trasferimento**: Conteggio aperti, confermati, in ritardo

### Monitoraggio Ubicazioni
- Lista completa ubicazioni con stato occupazione
- Filtro per magazzino, tipo deposito, ubicazione
- Conteggio quant per ubicazione
- Indicatore ubicazioni bloccate
- Colorazione righe in base allo stato

### Gestione Ordini di Trasferimento (OT)
- Lista dettagliata con tutti i campi rilevanti
- Calcolo automatico tempo di attesa (ore dalla creazione)
- Stato con icone: Confermato, Aperto, Warning, Critical
- Filtro per tipo movimento, materiale, date
- Double-click per dettagli (espandibile)

### KPI e Statistiche
- **KPI per tipo movimento**: Conteggio, tempo medio conferma, min/max
- **Statistiche giornaliere**: TOs creati/confermati per giorno, peak hours
- **Aging Analysis**: Distribuzione OT aperti per bucket temporali
- **Material Flow**: Flussi inbound/outbound/internal per materiale

### Simulazione Movimenti
- Timeline cronologica dei movimenti
- Rappresentazione visuale del flusso (origine → destinazione)
- Riepilogo flussi per materiale

### Analisi Carichi di Lavoro
- **Performance utenti**: TOs confermati, efficienza relativa
- **Heatmap oraria**: Distribuzione attività nelle 24 ore
- Identificazione peak hours

## Requisiti Tecnici

- SAP ECC EHP7 o superiore
- ABAP 7.40 o superiore
- Autorizzazioni per tabelle WM (LAGP, LQUA, LTAP, LTAK, T300, T301, T333T)

## Installazione via ABAPGit

1. Installare ABAPGit nel sistema SAP (se non già presente)
2. Creare un nuovo repository online in ABAPGit
3. Inserire l'URL di questo repository GitHub
4. Selezionare un package di destinazione (es. ZWMREPORT)
5. Eseguire il Pull
6. Attivare tutti gli oggetti

## Struttura Files

```
src/
├── zwm_visual_report.prog.abap      # Programma principale
├── zwm_visual_report.prog.xml       # Metadati programma
├── zwm_visual_report_top.prog.abap  # Definizioni globali, tipi, costanti
├── zwm_visual_report_cls.prog.abap  # Classi locali (estrattori, controller)
├── zwm_visual_report_scr.prog.abap  # Selection screen
├── zwm_visual_report_pbo.prog.abap  # Moduli PBO
├── zwm_visual_report_pai.prog.abap  # Moduli PAI
├── zwm_visual_report_frm.prog.abap  # Subroutine e helper
├── zwm_visual_report.prog.texts.en.txt  # Testi inglese
├── zwm_visual_report.prog.texts.it.txt  # Testi italiano
└── package.devc.xml                 # Definizione package
```

## Utilizzo

1. Eseguire transazione SE38 → ZWM_VISUAL_REPORT
2. Inserire almeno un numero magazzino (obbligatorio)
3. Opzionalmente filtrare per:
   - Tipo deposito
   - Ubicazione
   - Materiale
   - Tipo movimento
   - Range date (default: ultimi 30 giorni)
4. Selezionare la modalità di visualizzazione:
   - **Overview Dashboard**: Vista panoramica con KPI
   - **Storage Bins**: Lista ubicazioni
   - **Transfer Orders**: Lista OT
   - **KPI Statistics**: Dashboard statistiche
   - **Movement Simulation**: Timeline movimenti
   - **Workload Analysis**: Analisi carichi

## Funzionalità ABAP 7.40

Il report utilizza le seguenti funzionalità moderne di ABAP:

- **Inline declarations**: `DATA(lv_var) = ...`
- **Constructor expressions**: `VALUE #()`, `NEW #()`, `CONV #()`, `COND #()`, `SWITCH #()`
- **String templates**: `|text { variable } more text|`
- **Table expressions**: `itab[ key = value ]`
- **FOR expressions**: `VALUE #( FOR wa IN itab ... )`
- **REDUCE**: Per aggregazioni inline
- **GROUP BY**: Per raggruppamento dati nelle loop

## Classi Principali

### lcl_utilities
Metodi statici di utilità:
- `get_status_icon()`: Converte stato in icona
- `calculate_hours_diff()`: Calcola differenza ore tra timestamp
- `generate_bar_graph()`: Genera barre ASCII per visualizzazione
- `calc_percentage()`: Calcolo percentuali
- `get_day_name()`: Nome giorno da data

### lcl_data_extractor
Estrazione dati da tabelle WM:
- `extract_storage_bins()`: Ubicazioni con occupazione
- `extract_transfer_orders()`: OT con calcolo tempi
- `get_storage_type_summary()`: Riepilogo per tipo deposito
- `get_movement_kpis()`: KPI per tipo movimento
- `get_workload_analysis()`: Analisi workload oraria
- `get_user_workload()`: Performance utenti
- `get_daily_statistics()`: Statistiche giornaliere
- `get_aging_analysis()`: Aging OT aperti
- `get_material_flow()`: Flussi materiali

### lcl_html_dashboard
Generazione HTML per dashboard visuale:
- `generate_overview_html()`: Dashboard completa HTML
- `generate_kpi_card()`: Card singolo KPI
- `generate_progress_bar()`: Barre di progresso CSS

### lcl_alv_handler
Gestione visualizzazioni ALV:
- Display methods per ogni tipo dato
- Gestione colori righe
- Gestione eventi (double-click)

### lcl_controller
Controller principale:
- `initialize()`: Inizializzazione con parametri selezione
- `load_all_data()`: Caricamento tutti i dati
- `calculate_global_kpis()`: Calcolo KPI globali
- `display_overview()`: Visualizzazione dashboard
- `display_by_tab()`: Visualizzazione per tab

## Tabelle SAP WM Utilizzate

| Tabella | Descrizione |
|---------|-------------|
| LAGP | Master ubicazioni |
| LQUA | Quants (stock per ubicazione) |
| LTAP | Posizioni ordini di trasferimento |
| LTAK | Testata ordini di trasferimento |
| T300 | Numeri magazzino |
| T301 | Tipi deposito |
| T301T | Testi tipi deposito |
| T333T | Testi tipi movimento |
| MAKT | Testi materiale |

## Personalizzazione

### Soglie Indicatori
Nel file `zwm_visual_report_top.prog.abap`:
```abap
gc_occupancy_high TYPE p VALUE '85.00',  " Soglia occupazione alta
gc_occupancy_med  TYPE p VALUE '60.00',  " Soglia occupazione media
gc_to_time_good   TYPE i VALUE 4,        " Ore per stato "buono"
gc_to_time_warn   TYPE i VALUE 8,        " Ore per stato "warning"
```

### Limiti Record
```abap
gc_max_records TYPE i VALUE 100000,      " Max record per query
```

## Licenza

Questo software è fornito "as is" per uso interno SAP.

## Autore

Sviluppato con Claude Code (Anthropic) per SAP ECC EHP7 / ABAP 7.40

## Versione

1.0.0 - Release iniziale
