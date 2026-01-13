# SAP WM Visual Monitoring Dashboard
# Dashboard SAP WM per Monitoraggio Visuale

Un report ABAP completo per il monitoraggio visuale delle attività di magazzino SAP WM (Warehouse Management) su SAP ECC EHP7.

A complete ABAP report for visual monitoring of SAP WM (Warehouse Management) warehouse activities on SAP ECC EHP7.

---

## Reports Disponibili / Available Reports

| Report | Descrizione / Description |
|--------|---------------------------|
| `ZWM_VISUAL_REPORT` | Versione base con visualizzazione ALV standard / Base version with standard ALV display |
| `ZWM_VISUAL_REPORT_GRAPH` | **NUOVO** Versione grafica avanzata con dashboard HTML e simulazione / **NEW** Enhanced graphical version with HTML dashboard and simulation |

---

## Architettura Versione Grafica / Graphical Version Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    ZWM_VISUAL_REPORT_GRAPH                  │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────┐   │
│  │                 Selection Screen                     │   │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────────────┐    │   │
│  │  │ Filters  │ │  Dates   │ │  Initial View    │    │   │
│  │  │ s_lgnum  │ │ p_datfr  │ │ ○ Bins ○ TOs    │    │   │
│  │  │ s_lgtyp  │ │ p_datto  │ │ ○ KPIs ○ Daily  │    │   │
│  │  │ s_matnr  │ │          │ │ ○ Users         │    │   │
│  │  └──────────┘ └──────────┘ └──────────────────┘    │   │
│  └─────────────────────────────────────────────────────┘   │
│                            ▼                                │
│  ┌─────────────────────────────────────────────────────┐   │
│  │               Screen 0100 (Dynpro)                   │   │
│  │  ┌─────────────────────────────────────────────┐    │   │
│  │  │         HTML Dashboard (35%)                 │    │   │
│  │  │  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐          │    │   │
│  │  │  │ KPI │ │ KPI │ │ KPI │ │ KPI │          │    │   │
│  │  │  │Card │ │Card │ │Card │ │Card │          │    │   │
│  │  │  └─────┘ └─────┘ └─────┘ └─────┘          │    │   │
│  │  │  ┌──────────────────────────────┐         │    │   │
│  │  │  │     Progress Rings           │         │    │   │
│  │  │  │  ◐ Occ  ◐ Conf  ◐ Speed    │         │    │   │
│  │  │  └──────────────────────────────┘         │    │   │
│  │  └─────────────────────────────────────────────┘    │   │
│  │  ┌─────────────────────────────────────────────┐    │   │
│  │  │           ALV Grid (65%)                     │    │   │
│  │  │  ┌────┬────┬────┬────┬────┬────┐          │    │   │
│  │  │  │Whse│Type│ Bin│ Qty│Occ%│Stat│ ← Colors │    │   │
│  │  │  ├────┼────┼────┼────┼────┼────┤          │    │   │
│  │  │  │ 100│ 001│AA01│ 150│ 85%│ ██ │ Red      │    │   │
│  │  │  │ 100│ 001│AA02│  50│ 40%│ ██ │ Green    │    │   │
│  │  │  │ 100│ 002│AB01│  80│ 65%│ ██ │ Yellow   │    │   │
│  │  │  └────┴────┴────┴────┴────┴────┘          │    │   │
│  │  └─────────────────────────────────────────────┘    │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

## Viste Disponibili / Available Views

### Vista 1: Storage Bins / Ubicazioni
Mostra tutte le ubicazioni con stato occupazione e codifica colori.
Shows all storage bins with occupancy status and color coding.

| Colore / Color | Significato / Meaning |
|----------------|----------------------|
| 🟢 Verde / Green | Vuoto o bassa occupazione / Empty or low occupancy |
| 🟡 Giallo / Yellow | Alta occupazione (>60%) / High occupancy (>60%) |
| 🔴 Rosso / Red | Bloccato o critico (>85%) / Blocked or critical (>85%) |

### Vista 2: Transfer Orders / Ordini di Trasferimento
Mostra ordini di trasferimento con tempo attesa e stato conferma.
Shows transfer orders with wait time and confirmation status.

| Colore / Color | Significato / Meaning |
|----------------|----------------------|
| 🟢 Verde / Green | Confermato / Confirmed |
| 🔵 Blu / Blue | Nuovo, entro SLA / New, within SLA |
| 🟡 Giallo / Yellow | Attesa > 4 ore / Waiting > 4 hours |
| 🔴 Rosso / Red | Attesa > 8 ore / Waiting > 8 hours |

### Vista 3: Movement KPIs / KPI Movimenti
Statistiche aggregate per tipo movimento.
Aggregated statistics by movement type.

### Vista 4: Storage Type Summary / Riepilogo Tipi Storage
Riepilogo per tipo storage con barre occupazione.
Summary by storage type with occupancy bars.

### Vista 5: Daily Statistics / Statistiche Giornaliere
Trend attività giornaliera per TO creati e confermati.
Daily activity trends for TOs created and confirmed.

### Vista 6: User Workload / Carico Lavoro Utenti
Metriche produttività per utente.
Productivity metrics per user.

### Vista 7: Movement Simulation / Simulazione Movimenti (NEW!)
Visualizzazione animata del flusso materiali nel magazzino.
Animated visualization of warehouse material flow.

```
┌─────────────────────────────────────────────────────────────┐
│                  Movement Simulation                         │
│                  Simulazione Movimenti                       │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│   ┌──────────┐         ┌──────────┐         ┌──────────┐   │
│   │  TYPE A  │ ──────► │  TYPE B  │ ──────► │  TYPE C  │   │
│   │  Stock:5 │         │  Stock:12│         │  Stock:3 │   │
│   └──────────┘         └──────────┘         └──────────┘   │
│                                                              │
│   Timeline: ████████████░░░░░░░░░░░░  Hour 8 of 24         │
│                                                              │
│   ┌─────────────────────────────────────────────────────┐   │
│   │ Storage Type Activity / Attività Tipi Storage       │   │
│   │ TYPE A: ↓5  ↑3  =2    TYPE B: ↓8  ↑4  =4           │   │
│   │ TYPE C: ↓2  ↑6  =-4   TYPE D: ↓1  ↑1  =0           │   │
│   └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

## Codici Funzione / Function Codes

| Codice | Testo Pulsante | Descrizione / Description |
|--------|----------------|--------------------------|
| BACK | Back | Torna a schermata selezione / Return to selection screen |
| EXIT | Exit | Esci dal report / Exit report |
| REFRESH | Refresh | Ricarica dati dal database / Reload data from database |
| VIEW_BINS | Bins | Mostra vista ubicazioni / Show storage bins view |
| VIEW_TO | TOs | Mostra vista TO / Show transfer orders view |
| VIEW_KPI | KPIs | Mostra vista KPI / Show movement KPIs view |
| VIEW_STSUM | Summary | Mostra riepilogo storage / Show storage type summary |
| VIEW_DAILY | Daily | Mostra statistiche giornaliere / Show daily statistics |
| VIEW_USERS | Users | Mostra carico utenti / Show user workload |
| VIEW_SIM | Simulation | Mostra simulazione / Show movement simulation |
| SIM_FWD | ► | Avanza nella simulazione / Step forward in simulation |
| SIM_BACK | ◄ | Torna indietro / Step backward |
| SIM_RESET | ⏮ | Reset all'inizio / Reset to start |

---

## Diagramma Classi / Class Diagram

```
┌──────────────────────────────────┐
│      lcl_data_extractor          │  (Base - from ZWM_VISUAL_REPORT)
├──────────────────────────────────┤
│ + extract_storage_bins()         │
│ + extract_transfer_orders()      │
│ + get_storage_type_summary()     │
│ + get_movement_kpis()            │
│ + get_daily_statistics()         │
│ + get_user_workload()            │
└──────────────────────────────────┘
              △
              │ uses / utilizza
┌──────────────────────────────────┐
│     lcl_controller_graph         │  (Singleton)
├──────────────────────────────────┤
│ - mo_extractor                   │
│ - mo_dashboard                   │
│ - mo_alv_handler                 │
│ - mo_simulator                   │
├──────────────────────────────────┤
│ + get_instance()                 │
│ + initialize()                   │
│ + load_all_data()                │
│ + get_dashboard_html()           │
│ + get_simulation_html()          │
│ + display_current_view()         │
│ + set_view() / get_current_view()│
│ + sim_step_forward/back/reset()  │
└──────────────────────────────────┘
              │
    ┌─────────┼─────────┐
    ▽         ▽         ▽
┌─────────┐ ┌─────────┐ ┌─────────────────┐
│ lcl_    │ │ lcl_    │ │ lcl_movement_   │
│ html_   │ │ alv_    │ │ simulator       │
│ dash    │ │ handler │ │                 │
│ board   │ │ _graph  │ │                 │
│ _modern │ │         │ │                 │
└─────────┘ └─────────┘ └─────────────────┘
```

---

## Requisiti Tecnici / Technical Requirements

- SAP ECC EHP7 o superiore / SAP ECC EHP7 or higher
- ABAP 7.40 o superiore / ABAP 7.40 or higher
- Autorizzazioni per tabelle WM / Authorization for WM tables (LAGP, LQUA, LTAP, LTAK, T300, T301, T333T)

## Tabelle SAP WM Utilizzate / SAP WM Tables Used

| Tabella / Table | Descrizione / Description |
|-----------------|---------------------------|
| LAGP | Anagrafica ubicazioni / Storage bins master data |
| LQUA | Quant (quantità stock) / Quants (stock quantities) |
| LTAK | Testata ordine di trasferimento / Transfer order header |
| LTAP | Posizioni ordine di trasferimento / Transfer order items |
| MAKT | Descrizioni materiale / Material descriptions |
| T301T | Descrizioni tipi storage / Storage type descriptions |

---

## Installazione / Installation

### Via ABAPGit

1. Installare ABAPGit nel sistema SAP (se non già presente)
   Install ABAPGit in SAP system (if not already present)
2. Creare un nuovo repository online in ABAPGit
   Create a new online repository in ABAPGit
3. Inserire l'URL di questo repository GitHub
   Enter this GitHub repository URL
4. Selezionare un package di destinazione (es. ZWMREPORT)
   Select a destination package (e.g. ZWMREPORT)
5. Eseguire il Pull
   Execute Pull

### Post-Import Steps for Graphical Version

1. **Creare dynpro 0100** manualmente in SE80:
   - Custom container: `CC_MAIN`
   - OK code field: (non necessario, usa sy-ucomm)

   **Create dynpro 0100** manually in SE80:
   - Custom container: `CC_MAIN`
   - OK code field: (not needed, uses sy-ucomm)

2. **Creare PF-STATUS** `STATUS_GRAPH` con i codici funzione elencati sopra
   **Create PF-STATUS** `STATUS_GRAPH` with the function codes listed above

3. **Creare TITLEBAR** `TITLE_GRAPH`
   **Create TITLEBAR** `TITLE_GRAPH`

---

## Struttura Files / File Structure

```
src/
├── zwm_visual_report.prog.abap          # Report base / Base report
├── zwm_visual_report_top.prog.abap      # Tipi base / Base types
├── zwm_visual_report_cls.prog.abap      # Classi base / Base classes
├── zwm_visual_report_scr.prog.abap      # Selection screen base
├── zwm_visual_report_pbo.prog.abap      # PBO base
├── zwm_visual_report_pai.prog.abap      # PAI base
├── zwm_visual_report_frm.prog.abap      # Form base
│
├── zwm_visual_report_graph.prog.abap    # Report grafico (main) / Graphical report
├── zwm_visual_report_graph_top.prog.abap # Tipi estesi / Extended types
├── zwm_visual_report_graph_cls.prog.abap # Classi grafiche / Graphical classes
├── zwm_visual_report_graph_scr.prog.abap # Selection screen grafico
├── zwm_visual_report_graph_pbo.prog.abap # Moduli PBO grafici / PBO modules
├── zwm_visual_report_graph_pai.prog.abap # Moduli PAI grafici / PAI modules
└── zwm_visual_report_graph_frm.prog.abap # Form routines grafiche
```

---

## Decisioni di Design Chiave / Key Design Decisions

1. **Pattern Singleton per Controller / Singleton Pattern for Controller**
   - Garantisce singola istanza in tutti i moduli
   - Mantiene lo stato tra chiamate PBO/PAI
   - Ensures single instance across all modules
   - Maintains state between PBO/PAI calls

2. **Variabili di Istanza per Dati ALV / Instance Variables for ALV Data**
   - Dati memorizzati in istanza classe, non variabili locali
   - Previene dump GETWA_NOT_ASSIGNED durante scrolling
   - Data stored in class instance, not local variables
   - Prevents GETWA_NOT_ASSIGNED dump on scrolling

3. **sy-ucomm per Codici Funzione / sy-ucomm for Function Codes**
   - Approccio standard SAP, più affidabile dei campi schermata
   - Standard SAP approach, more reliable than screen fields

4. **LVC_T_SCOL per Colori Riga / LVC_T_SCOL for Row Colors**
   - Tipo corretto per colorazione celle/righe ALV
   - Proper type for ALV cell/row coloring

---

## Soglie Personalizzabili / Customizable Thresholds

Nel file `zwm_visual_report_top.prog.abap`:
In file `zwm_visual_report_top.prog.abap`:

```abap
gc_occupancy_high TYPE p VALUE '85.00',  " Soglia occupazione alta / High occupancy threshold
gc_occupancy_med  TYPE p VALUE '60.00',  " Soglia occupazione media / Medium occupancy threshold
gc_to_time_good   TYPE i VALUE 4,        " Ore per stato "buono" / Hours for "good" status
gc_to_time_warn   TYPE i VALUE 8,        " Ore per stato "warning" / Hours for "warning" status
```

---

## Licenza / License

MIT License

## Autore / Author

Sviluppato con Claude AI (Anthropic) / Developed with Claude AI (Anthropic)

## Versione / Version

2.0.0 - Aggiunta versione grafica con simulazione / Added graphical version with simulation
