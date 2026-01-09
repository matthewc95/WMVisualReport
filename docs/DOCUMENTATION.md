# SAP WM Visual Monitoring Dashboard - Documentazione Tecnica

## Indice

1. [Panoramica](#panoramica)
2. [Architettura](#architettura)
3. [Guida Utente](#guida-utente)
4. [Dettaglio Funzionalità](#dettaglio-funzionalità)
5. [Modello Dati](#modello-dati)
6. [Classi e Metodi](#classi-e-metodi)
7. [Configurazione e Personalizzazione](#configurazione-e-personalizzazione)
8. [Performance](#performance)
9. [Troubleshooting](#troubleshooting)
10. [Changelog](#changelog)

---

## Panoramica

### Scopo
Il report **ZWM_VISUAL_REPORT** fornisce una dashboard completa per il monitoraggio delle attività di magazzino SAP WM. È progettato per offrire visibilità immediata su:

- Stato delle ubicazioni di magazzino
- Ordini di trasferimento (creazione, conferma, backlog)
- KPI operativi e di performance
- Analisi dei carichi di lavoro

### Target Users
- Responsabili di magazzino
- Warehouse operators / Team leaders
- Logistic managers
- Controller operations

### Requisiti Sistema
| Componente | Versione Minima |
|------------|-----------------|
| SAP ECC | EHP7 |
| SAP Basis | 7.40 |
| SAP GUI | 7.30+ |
| Modulo WM | Attivo |

### Autorizzazioni Richieste
```
Oggetto Autorizzazione: S_TABU_DIS
Gruppo Tabelle: WM (Warehouse Management)

Tabelle in lettura:
- T300, T300T (Warehouse Numbers)
- T301, T301T (Storage Types)
- LAGP (Storage Bins)
- LQUA (Quants)
- LTAP (Transfer Order Items)
- LTAK (Transfer Order Headers)
- T333, T333T (Movement Types)
- MAKT (Material Texts)
```

---

## Architettura

### Design Pattern
Il report utilizza un'architettura **MVC-like** con separazione delle responsabilità:

```
┌─────────────────────────────────────────────────────────────────┐
│                        PRESENTATION LAYER                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐  │
│  │   WRITE     │  │  CL_SALV_   │  │  CL_GUI_HTML_VIEWER     │  │
│  │  Statements │  │   TABLE     │  │  (HTML Dashboard)       │  │
│  └─────────────┘  └─────────────┘  └─────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                        CONTROLLER LAYER                          │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                    lcl_controller                            ││
│  │  - initialize()      - load_all_data()                      ││
│  │  - display_overview() - display_by_tab()                    ││
│  │  - calculate_global_kpis()                                  ││
│  └─────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                          MODEL LAYER                             │
│  ┌─────────────────────┐  ┌─────────────────────────────────┐  │
│  │  lcl_data_extractor │  │       lcl_utilities             │  │
│  │  - extract_*()      │  │  - calculate_hours_diff()       │  │
│  │  - get_*()          │  │  - generate_bar_graph()         │  │
│  └─────────────────────┘  └─────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                        DATABASE LAYER                            │
│         LAGP    LQUA    LTAP    LTAK    T300    T301            │
└─────────────────────────────────────────────────────────────────┘
```

### Struttura Files

```
ZWM_VISUAL_REPORT
│
├── zwm_visual_report.prog.abap          [MAIN]
│   └── Entry point, INCLUDE statements, event blocks
│
├── zwm_visual_report_top.prog.abap      [TOP]
│   ├── TYPE-POOLS
│   ├── TABLES declarations
│   ├── CONSTANTS
│   ├── TYPES definitions (12 strutture)
│   └── DATA declarations (global variables)
│
├── zwm_visual_report_cls.prog.abap      [CLS]
│   ├── lcl_utilities (metodi statici)
│   ├── lcl_data_extractor (estrazione dati)
│   ├── lcl_html_dashboard (generazione HTML)
│   ├── lcl_alv_handler (gestione ALV)
│   └── lcl_controller (orchestrazione)
│
├── zwm_visual_report_scr.prog.abap      [SCR]
│   └── SELECTION-SCREEN definitions
│
├── zwm_visual_report_pbo.prog.abap      [PBO]
│   └── MODULE definitions for PBO
│
├── zwm_visual_report_pai.prog.abap      [PAI]
│   └── MODULE definitions for PAI
│
└── zwm_visual_report_frm.prog.abap      [FRM]
    ├── FORM initialization
    ├── FORM f4_* (value helps)
    ├── FORM start_of_selection
    ├── FORM end_of_selection
    └── FORM display_* (output routines)
```

---

## Guida Utente

### Avvio del Report

1. Transazione **SE38** → Inserire `ZWM_VISUAL_REPORT` → Eseguire (F8)
2. Oppure creare una transazione custom (SE93)

### Selection Screen

```
┌─────────────────────────────────────────────────────────────────┐
│  SAP WM Visual Monitoring Dashboard                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─ Warehouse Selection ──────────────────────────────────────┐ │
│  │  Warehouse Number    [____] to [____]  (obbligatorio)      │ │
│  │  Storage Type        [____] to [____]                      │ │
│  │  Storage Bin         [__________] to [__________]          │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  ┌─ Date Range for Transfer Orders ───────────────────────────┐ │
│  │  Date From           [01.12.2025]                          │ │
│  │  Date To             [09.01.2026]                          │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  ┌─ Material and Movement Filter ─────────────────────────────┐ │
│  │  Material            [__________________] to [_______]     │ │
│  │  Movement Type       [___] to [___]                        │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  ┌─ Display Mode ─────────────────────────────────────────────┐ │
│  │  (●) Overview Dashboard    ( ) Storage Bins                │ │
│  │  ( ) Transfer Orders       ( ) KPI Statistics              │ │
│  │  ( ) Movement Simulation   ( ) Workload Analysis           │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  ┌─ Additional Options ───────────────────────────────────────┐ │
│  │  Max Records         [50000]                               │ │
│  │  [ ] HTML Dashboard Mode                                   │ │
│  │  [ ] Generate Test Data                                    │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  [Esegui (F8)]  [Varianti]  [Info]                              │
└─────────────────────────────────────────────────────────────────┘
```

### Parametri di Selezione

| Parametro | Descrizione | Obbligatorio | Default |
|-----------|-------------|--------------|---------|
| `S_LGNUM` | Numero magazzino | Sì | - |
| `S_LGTYP` | Tipo deposito | No | Tutti |
| `S_LGPLA` | Ubicazione | No | Tutte |
| `P_DATFR` | Data inizio | No | Oggi - 30gg |
| `P_DATTO` | Data fine | No | Oggi |
| `S_MATNR` | Materiale | No | Tutti |
| `S_BWLVS` | Tipo movimento WM | No | Tutti |
| `P_MAX` | Max record estratti | No | 50.000 |

---

## Dettaglio Funzionalità

### 1. Overview Dashboard

La vista panoramica mostra tutti i KPI principali in un'unica schermata.

**Output Esempio:**
```
════════════════════════════════════════════════════════════════════════════════
                    SAP WM VISUAL MONITORING DASHBOARD
════════════════════════════════════════════════════════════════════════════════

┌─────────────────────────────────────────────────────────────────────────────┐
│                           STORAGE BIN OVERVIEW                              │
└─────────────────────────────────────────────────────────────────────────────┘
📦 Total Bins:           1.250
✓  Occupied Bins:          847
○  Empty Bins:             389
🔒 Blocked Bins:            14
📈 Occupancy Rate:       67,76%

┌─────────────────────────────────────────────────────────────────────────────┐
│                         TRANSFER ORDER OVERVIEW                             │
└─────────────────────────────────────────────────────────────────────────────┘
📋 Total Transfer Orders:    3.456
⏳ Open TOs (pending):         234
✓  Confirmed TOs:            3.222
⏱  Avg Confirmation Time:    2,45 hours

┌─────────────────────────────────────────────────────────────────────────────┐
│                            STATUS INDICATORS                                │
└─────────────────────────────────────────────────────────────────────────────┘
🟢 Workload Status: NORMAL - All systems go
🟢 Processing Speed: GOOD
```

### 2. Storage Bins View

Lista dettagliata di tutte le ubicazioni con indicatori di occupazione.

**Colonne ALV:**
| Campo | Descrizione | Colorazione |
|-------|-------------|-------------|
| LGNUM | Magazzino | - |
| LGTYP | Tipo deposito | - |
| LGPLA | Ubicazione | - |
| OCCUPANCY | % Occupazione | Verde <60%, Giallo 60-85%, Rosso >85% |
| STATUS_ICON | Semaforo | 🟢🟡🔴 |
| QUANT_COUNT | Numero quants | - |
| MAT_COUNT | Materiali diversi | - |
| VERME | Quantità disponibile | - |
| BLOCKED | Bloccata | Rosso se X |

### 3. Transfer Orders View

Monitoraggio completo degli ordini di trasferimento.

**Colonne ALV:**
| Campo | Descrizione | Note |
|-------|-------------|------|
| TANUM | Numero OT | Click per dettaglio |
| TAPOS | Posizione | - |
| BWLVS | Tipo movimento | - |
| MATNR | Materiale | - |
| MAKTX | Descrizione | - |
| VSOLM | Quantità | - |
| VLTYP/VLPLA | Origine | Tipo/Ubicazione |
| NLTYP/NLPLA | Destinazione | Tipo/Ubicazione |
| BDATU/BUPTS | Creazione | Data/Ora |
| KDATU/KUPTS | Conferma | Data/Ora (se confermato) |
| WAIT_HOURS | Ore attesa | Calcolato automaticamente |
| STATUS | Stato | Open/Warning/Critical/Confirmed |

**Logica Stati:**
```
IF confermato:
    STATUS = "Confirmed" (verde)
ELSE:
    IF wait_hours > 8:
        STATUS = "Critical" (rosso)
    ELSE IF wait_hours > 4:
        STATUS = "Warning" (giallo)
    ELSE:
        STATUS = "Open" (neutro)
```

### 4. KPI Statistics

Dashboard con tre sezioni di analisi.

#### 4.1 Movement Type KPIs
Statistiche aggregate per tipo movimento WM.

| Metrica | Descrizione |
|---------|-------------|
| TO_COUNT | Totale OT per tipo movimento |
| TO_CONFIRMED | OT confermati |
| TO_OPEN | OT ancora aperti |
| AVG_TIME_HOURS | Media ore per conferma |
| MIN_TIME_HOURS | Tempo minimo conferma |
| MAX_TIME_HOURS | Tempo massimo conferma |
| TOTAL_QTY | Quantità totale movimentata |

#### 4.2 Daily Statistics
Andamento giornaliero delle operazioni.

```
Date       | Day       | Created | Confirmed | Avg Time | Peak Hour | Graph
-----------|-----------|---------|-----------|----------|-----------|----------------
09.01.2026 | Thursday  |     145 |       142 |     1,8h |        10 | [████████░░] 80%
08.01.2026 | Wednesday |     132 |       132 |     2,1h |        14 | [███████░░░] 72%
07.01.2026 | Tuesday   |     158 |       155 |     1,5h |        09 | [█████████░] 86%
...
```

#### 4.3 Aging Analysis
Distribuzione degli OT aperti per "età".

```
Aging Bucket  | Count | Percentage | Distribution
--------------|-------|------------|----------------------------------
< 4 hours     |    89 |     38,0%  | [████████████░░░░░░░░] 38%
4-8 hours     |    67 |     28,6%  | [█████████░░░░░░░░░░░] 29%
8-24 hours    |    45 |     19,2%  | [██████░░░░░░░░░░░░░░] 19%
1-2 days      |    23 |      9,8%  | [███░░░░░░░░░░░░░░░░░] 10%
> 2 days      |    10 |      4,3%  | [█░░░░░░░░░░░░░░░░░░░]  4%
```

### 5. Movement Simulation

Visualizzazione cronologica dei movimenti di magazzino.

**Material Flow Summary:**
| Material | Description | Movements | Total Qty | Inbound | Outbound | Internal |
|----------|-------------|-----------|-----------|---------|----------|----------|
| MAT001 | Widget A | 234 | 12.500 PC | 89 | 78 | 67 |
| MAT002 | Widget B | 189 | 8.900 PC | 45 | 92 | 52 |

**Movement Timeline:**
```
────────────── 09.01.2026 ──────────────
08:15  🚚  MAT001    001/A-01-01 → 002/B-02-03     125,00 PC
08:23  🚚  MAT002    001/A-01-02 → 003/C-01-01      50,00 PC
09:01  🚚  MAT001    002/B-02-03 → 910/SHIP-01    125,00 PC
09:15  🚚  MAT003    901/RECV-01 → 001/A-03-02    200,00 KG
...

────────────── 08.01.2026 ──────────────
...
```

### 6. Workload Analysis

#### 6.1 User Performance
Analisi performance per operatore.

| User | TOs Confirmed | Items Processed | Avg Time | Efficiency |
|------|---------------|-----------------|----------|------------|
| USER01 | 456 | 23.400 | 1,2h | 100,0% |
| USER02 | 423 | 21.800 | 1,4h | 85,7% |
| USER03 | 389 | 19.200 | 1,8h | 66,7% |

#### 6.2 Hourly Heatmap
Distribuzione attività nelle 24 ore.

```
Hour: 00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|
TOs:   2| 1| 0| 0| 1| 3|12|45|89|95|87|78|34|67|92|88|76|54|23|12| 5| 3| 2| 1|
      ░░ ░░ ░░ ░░ ░░ ░░ ▒▒ ██ ██ ██ ██ ██ ▒▒ ██ ██ ██ ██ ▒▒ ░░ ░░ ░░ ░░ ░░ ░░

Legend: ░ Low   ▒ Medium   █ High (Peak)
```

---

## Modello Dati

### Strutture Principali

#### gty_storage_bin
```abap
BEGIN OF gty_storage_bin,
  lgnum       TYPE lgnum,         " Warehouse Number
  lgtyp       TYPE lgtyp,         " Storage Type
  lgpla       TYPE lgpla,         " Storage Bin
  lgber       TYPE lgber,         " Storage Section
  lptyp       TYPE lptyp,         " Bin Type
  maxgew      TYPE lmaxg,         " Maximum Weight
  maxle       TYPE lmaxl,         " Maximum Capacity (LE)
  anzle       TYPE lanzl,         " Current LE count
  verme       TYPE lqua-verme,    " Available quantity
  gesme       TYPE lqua-gesme,    " Total quantity
  einme       TYPE lqua-einme,    " Unit of entry
  occupancy   TYPE p LENGTH 5 DECIMALS 2,  " Occupancy %
  status      TYPE char1,         " Status indicator (1/2/3)
  status_icon TYPE icon_d,        " Icon for display
  matnr       TYPE matnr,         " Material (if single)
  mat_count   TYPE i,             " Count of different materials
  quant_count TYPE i,             " Count of quants
  blocked     TYPE char1,         " Blocked indicator
  color       TYPE char4,         " Row color (ALV)
END OF gty_storage_bin
```

#### gty_transfer_order
```abap
BEGIN OF gty_transfer_order,
  lgnum       TYPE lgnum,         " Warehouse Number
  tanum       TYPE tanum,         " Transfer Order Number
  tapos       TYPE tapos,         " Item Number
  bwlvs       TYPE bwlvs,         " Movement Type
  betyp       TYPE ltap-betyp,    " Source Document Type
  benum       TYPE ltap-benum,    " Source Document
  nltyp       TYPE nltyp,         " Destination Storage Type
  nlpla       TYPE nlpla,         " Destination Storage Bin
  vltyp       TYPE vltyp,         " Source Storage Type
  vlpla       TYPE vlpla,         " Source Storage Bin
  matnr       TYPE matnr,         " Material
  werks       TYPE werks_d,       " Plant
  maktx       TYPE maktx,         " Material Description
  vsolm       TYPE ltap-vsolm,    " Source Quantity
  meins       TYPE meins,         " Unit of Measure
  bdatu       TYPE ltap-bdatu,    " Creation Date
  bupts       TYPE ltap-bupts,    " Creation Time
  kdatu       TYPE ltap-kdatu,    " Confirmation Date
  kupts       TYPE ltap-kupts,    " Confirmation Time
  pession     TYPE ltap-pession,  " Confirmed by (User)
  status      TYPE char10,        " Status text
  status_icon TYPE icon_d,        " Status icon
  wait_hours  TYPE p LENGTH 7 DECIMALS 2,  " Hours waiting
  color       TYPE char4,         " Row color
  confirmed   TYPE char1,         " X = confirmed
END OF gty_transfer_order
```

### Entity Relationship

```
┌─────────┐       ┌─────────┐       ┌─────────┐
│  T300   │──1:N──│  T301   │──1:N──│  LAGP   │
│ (Whse)  │       │ (StTyp) │       │ (Bins)  │
└─────────┘       └─────────┘       └────┬────┘
                                         │
                                        1:N
                                         │
                                    ┌────▼────┐
                                    │  LQUA   │
                                    │(Quants) │
                                    └─────────┘

┌─────────┐       ┌─────────┐
│  LTAK   │──1:N──│  LTAP   │
│(TO Hdr) │       │(TO Item)│
└─────────┘       └─────────┘
```

---

## Classi e Metodi

### lcl_utilities

Classe di utilità con metodi statici.

| Metodo | Parametri | Return | Descrizione |
|--------|-----------|--------|-------------|
| `get_status_icon` | iv_status (1/2/3) | icon_d | Converte codice stato in icona LED |
| `get_trend_icon` | iv_trend (U/D/S) | icon_d | Icona trend (up/down/stable) |
| `get_color_code` | iv_status | char4 | Codice colore ALV (C510/C310/C610) |
| `calculate_hours_diff` | iv_date1, iv_time1, iv_date2, iv_time2 | decimal | Differenza in ore tra due timestamp |
| `generate_bar_graph` | iv_value, iv_max, iv_width | string | Barra ASCII `[████░░░░] 45%` |
| `calc_percentage` | iv_part, iv_total | decimal | Calcolo percentuale |
| `get_day_name` | iv_date | char10 | Nome giorno (Monday, Tuesday...) |

### lcl_data_extractor

Classe per estrazione dati da database.

| Metodo | Return Type | Descrizione |
|--------|-------------|-------------|
| `extract_storage_bins` | gty_storage_bins | Ubicazioni con occupancy calcolata |
| `extract_transfer_orders` | gty_transfer_orders | OT con wait_hours calcolato |
| `extract_quants` | lqua_t | Quants raw |
| `get_storage_type_summary` | gty_storage_type_sums | Riepilogo per tipo deposito |
| `get_movement_kpis` | gty_movement_kpis | KPI per tipo movimento |
| `get_workload_analysis` | gty_workloads | Analisi oraria |
| `get_user_workload` | gty_user_workloads | Performance utenti |
| `get_daily_statistics` | gty_daily_stats | Statistiche giornaliere |
| `get_aging_analysis` | gty_agings | Aging OT aperti |
| `get_material_flow` | gty_material_flows | Flussi per materiale |
| `get_movement_simulation` | gty_movement_sims | Timeline movimenti |

### lcl_html_dashboard

Generazione HTML per visualizzazione grafica.

| Metodo | Descrizione |
|--------|-------------|
| `generate_overview_html` | HTML completo dashboard con CSS inline |
| `generate_kpi_card` | Singola card KPI con icona e colore |
| `generate_progress_bar` | Barra di progresso CSS |
| `generate_chart_container` | Container per grafici |
| `get_css_styles` | Foglio stile CSS completo |

### lcl_alv_handler

Gestione visualizzazioni ALV (Singleton pattern).

| Metodo | Descrizione |
|--------|-------------|
| `get_instance` | Ritorna istanza singleton |
| `display_storage_bins` | ALV ubicazioni |
| `display_transfer_orders` | ALV ordini trasferimento |
| `display_storage_type_summary` | ALV riepilogo tipi deposito |
| `display_movement_kpis` | ALV KPI movimenti |
| `display_daily_statistics` | ALV statistiche giornaliere |
| `display_aging_analysis` | ALV aging |
| `display_user_workload` | ALV workload utenti |
| `display_material_flow` | ALV flussi materiali |
| `display_movement_simulation` | ALV simulazione |
| `on_user_command` | Event handler toolbar |
| `on_double_click` | Event handler double-click |

### lcl_controller

Controller principale (Singleton pattern).

| Metodo | Descrizione |
|--------|-------------|
| `get_instance` | Ritorna istanza singleton |
| `initialize` | Inizializza con parametri selezione |
| `load_all_data` | Carica tutti i dati tramite extractor |
| `calculate_global_kpis` | Calcola KPI globali |
| `display_overview` | Visualizza dashboard panoramica |
| `display_by_tab` | Visualizza in base a tab selezionato |
| `get_dashboard_html` | Genera HTML dashboard |
| `refresh_data` | Ricarica dati |

---

## Configurazione e Personalizzazione

### Soglie Configurabili

Nel file `zwm_visual_report_top.prog.abap`:

```abap
CONSTANTS:
  " Soglie occupazione ubicazioni
  gc_occupancy_high TYPE p DECIMALS 2 VALUE '85.00',  " >= 85% = Warning
  gc_occupancy_med  TYPE p DECIMALS 2 VALUE '60.00',  " >= 60% = Medium

  " Soglie tempo conferma OT (ore)
  gc_to_time_good   TYPE i VALUE 4,   " <= 4h = Good (verde)
  gc_to_time_warn   TYPE i VALUE 8,   " <= 8h = Warning (giallo), > 8h = Critical (rosso)

  " Limiti performance
  gc_max_records    TYPE i VALUE 100000.  " Max record per query
```

### Estensione Tipi Movimento

Per personalizzare la classificazione inbound/outbound/internal, modificare il metodo `get_material_flow` in `lcl_data_extractor`:

```abap
" Logica attuale (basata su source/destination type)
IF ls_member-vltyp IS INITIAL.
  " No source = incoming
  ls_flow-inbound = ls_flow-inbound + 1.
ELSEIF ls_member-nltyp IS INITIAL.
  " No destination = outgoing
  ls_flow-outbound = ls_flow-outbound + 1.
ELSE.
  " Both = internal
  ls_flow-internal = ls_flow-internal + 1.
ENDIF.

" Alternativa: basarsi sul movimento type
" IF ls_member-bwlvs BETWEEN '100' AND '199'. " Goods Receipt
" IF ls_member-bwlvs BETWEEN '200' AND '299'. " Goods Issue
```

### Aggiunta Nuove Viste

Per aggiungere una nuova vista:

1. Aggiungere costante tab in `_TOP`:
```abap
gc_tab_newview TYPE i VALUE 7,
```

2. Aggiungere radio button in `_SCR`:
```abap
rb_new RADIOBUTTON GROUP rb1.
```

3. Aggiungere case in `FORM end_of_selection`:
```abap
ELSEIF rb_new = abap_true.
  PERFORM display_new_view.
```

4. Implementare `FORM display_new_view` in `_FRM`

---

## Performance

### Query Optimization

Le query sono ottimizzate con:

- `UP TO n ROWS` per limitare i record
- `FOR ALL ENTRIES` invece di join su tabelle grandi
- Indici utilizzati: chiavi primarie delle tabelle WM

### Raccomandazioni

| Scenario | Records | Tempo Atteso |
|----------|---------|--------------|
| Piccolo (1 magazzino, 1 settimana) | < 5.000 | < 5 sec |
| Medio (1 magazzino, 1 mese) | 5.000 - 20.000 | 5-15 sec |
| Grande (multi-magazzino, 3 mesi) | 20.000 - 100.000 | 15-60 sec |

### Memory Management

- Le tabelle interne sono processate con `GROUP BY` per ridurre allocazioni
- I dati sono caricati una volta e riutilizzati per tutte le viste
- Gli oggetti GUI sono puliti in `FORM cleanup_gui_objects`

---

## Troubleshooting

### Errori Comuni

| Errore | Causa | Soluzione |
|--------|-------|-----------|
| "No data found" | Filtri troppo restrittivi | Ampliare range selezione |
| "Max records exceeded" | Troppi dati | Aumentare P_MAX o restringere filtri |
| "Authorization error" | Mancano autorizzazioni | Verificare S_TABU_DIS per gruppo WM |
| ALV vuoto | Nessun dato per la vista | Verificare che esistano dati nel periodo |
| HTML non visualizzato | GUI limitation | Disabilitare flag P_HTML |

### Debug

Punti di break consigliati:

1. `lcl_data_extractor->extract_storage_bins` - Verifica estrazione ubicazioni
2. `lcl_data_extractor->extract_transfer_orders` - Verifica estrazione OT
3. `lcl_controller->calculate_global_kpis` - Verifica calcolo KPI
4. `FORM end_of_selection` - Verifica display

### Log

Per abilitare logging dettagliato, aggiungere in `_TOP`:
```abap
DATA: gv_debug TYPE abap_bool VALUE abap_true.
```

E utilizzare:
```abap
IF gv_debug = abap_true.
  WRITE: / 'DEBUG:', lv_variable.
ENDIF.
```

---

## Changelog

### Version 1.0.0 (2026-01-09)

**Initial Release**

- Dashboard panoramica con KPI real-time
- Monitoraggio ubicazioni con indicatori occupazione
- Gestione Transfer Orders con calcolo tempi attesa
- KPI per tipo movimento WM
- Statistiche giornaliere con grafici ASCII
- Aging analysis per OT aperti
- Simulazione timeline movimenti
- Analisi workload utenti
- Heatmap oraria attività
- Supporto multilingua (EN/IT)
- HTML dashboard opzionale
- Export ALV standard

---

## Contatti e Supporto

Per bug report o richieste di feature, aprire una Issue su GitHub.

---

*Documentazione generata per SAP ECC EHP7 / ABAP 7.40*
*Ultimo aggiornamento: 2026-01-09*
