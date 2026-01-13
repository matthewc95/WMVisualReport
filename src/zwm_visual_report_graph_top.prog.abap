*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_TOP
*&---------------------------------------------------------------------*
*& Extended Data Definitions for Graphical Version
*& Definizioni Dati Estese per Versione Grafica
*&
*& ENGLISH:
*& This include contains all type definitions and global variables
*& for the enhanced graphical version of the WM Visual Report.
*& Key additions over base version:
*& - LVC_T_SCOL field in all structures for ALV row coloring
*& - GUI object references for splitter layout
*& - Simulation types for animated warehouse visualization
*&
*& ITALIANO:
*& Questo include contiene tutte le definizioni dei tipi e variabili
*& globali per la versione grafica avanzata del WM Visual Report.
*& Aggiunte chiave rispetto alla versione base:
*& - Campo LVC_T_SCOL in tutte le strutture per colorazione righe ALV
*& - Riferimenti oggetti GUI per layout splitter
*& - Tipi simulazione per visualizzazione animata del magazzino
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Constants for Graphical Version
* Costanti per Versione Grafica
*----------------------------------------------------------------------*
CONSTANTS:
  " Screen numbers / Numeri schermata
  " gc_dynnr_graph: Main dynpro for graphical display
  " gc_dynnr_graph: Dynpro principale per visualizzazione grafica
  gc_dynnr_graph    TYPE sy-dynnr VALUE '0100',

  " Container names for dynpro / Nomi container per dynpro
  gc_cont_top       TYPE char30 VALUE 'CC_TOP',
  gc_cont_bottom    TYPE char30 VALUE 'CC_BOTTOM',

  " Splitter ratios / Proporzioni splitter
  " Dashboard takes 35% of screen, ALV takes 65%
  " Dashboard occupa 35% dello schermo, ALV occupa 65%
  gc_split_top      TYPE i VALUE 35,
  gc_split_bottom   TYPE i VALUE 65,

  " Colors for ALV rows (from type-pool COL)
  " Colori per righe ALV (da type-pool COL)
  " Design choice: Use standard SAP colors for consistency
  " Scelta design: Usa colori standard SAP per coerenza
  gc_col_green      TYPE i VALUE 5,   " Green=Good / Verde=OK
  gc_col_yellow     TYPE i VALUE 3,   " Yellow=Warning / Giallo=Attenzione
  gc_col_red        TYPE i VALUE 6,   " Red=Critical / Rosso=Critico
  gc_col_blue       TYPE i VALUE 1,   " Blue=Info / Blu=Informazione
  gc_col_normal     TYPE i VALUE 0.   " Normal=Default / Normale=Predefinito

*----------------------------------------------------------------------*
* Types - Extended Storage Bin with ALV color support
* Tipi - Storage Bin Esteso con supporto colori ALV
*----------------------------------------------------------------------*
* Design rationale: Structure mirrors base type but adds CELLCOLOR
* for proper ALV row coloring using LVC_T_SCOL type
* Motivazione design: Struttura rispecchia tipo base ma aggiunge
* CELLCOLOR per colorazione righe ALV usando tipo LVC_T_SCOL
TYPES:
  BEGIN OF gty_storage_bin_graph,
    lgnum       TYPE lagp-lgnum,      " Warehouse number / Numero magazzino
    lgtyp       TYPE lagp-lgtyp,      " Storage type / Tipo ubicazione
    lgpla       TYPE lagp-lgpla,      " Storage bin / Ubicazione
    lgber       TYPE lagp-lgber,      " Storage section / Area stoccaggio
    lptyp       TYPE lagp-lptyp,      " Bin type / Tipo bin
    maxle       TYPE lagp-maxle,      " Max storage units / Unità max
    anzle       TYPE lagp-anzle,      " Current storage units / Unità attuali
    verme       TYPE lqua-verme,      " Available qty / Quantità disponibile
    gesme       TYPE lqua-gesme,      " Total qty / Quantità totale
    meins       TYPE lqua-meins,      " Unit of measure / Unità misura
    occupancy   TYPE p LENGTH 5 DECIMALS 2,  " Occupancy % / % Occupazione
    status      TYPE char1,           " Status code / Codice stato
    status_icon TYPE char4,           " Icon code / Codice icona
    matnr       TYPE lqua-matnr,      " Material / Materiale
    mat_count   TYPE i,               " Material count / Conteggio materiali
    quant_count TYPE i,               " Quant count / Conteggio quant
    blocked     TYPE char1,           " Blocked flag / Flag bloccato
    cellcolor   TYPE lvc_t_scol,      " ALV row color / Colore riga ALV
  END OF gty_storage_bin_graph,

  gty_storage_bins_graph TYPE STANDARD TABLE OF gty_storage_bin_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended Transfer Order with ALV color support
* Tipi - Transfer Order Esteso con supporto colori ALV
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_transfer_order_graph,
    lgnum       TYPE ltak-lgnum,      " Warehouse / Magazzino
    tanum       TYPE ltak-tanum,      " TO number / Numero TO
    tapos       TYPE ltap-tapos,      " TO item / Posizione TO
    bwlvs       TYPE ltak-bwlvs,      " Movement type / Tipo movimento
    refnr       TYPE ltak-refnr,      " Reference number / Numero riferimento
    nltyp       TYPE ltap-nltyp,      " Dest storage type / Tipo dest.
    nlpla       TYPE ltap-nlpla,      " Dest bin / Bin destinazione
    vltyp       TYPE ltap-vltyp,      " Source storage type / Tipo origine
    vlpla       TYPE ltap-vlpla,      " Source bin / Bin origine
    matnr       TYPE ltap-matnr,      " Material / Materiale
    werks       TYPE ltap-werks,      " Plant / Stabilimento
    maktx       TYPE ltap-maktx,      " Material description / Descrizione mat.
    vsolm       TYPE ltap-vsolm,      " Target qty / Quantità target
    meins       TYPE ltap-meins,      " Unit / Unità
    bdatu       TYPE ltak-bdatu,      " Creation date / Data creazione
    bzeit       TYPE ltak-bzeit,      " Creation time / Ora creazione
    qdatu       TYPE ltap-qdatu,      " Confirm date / Data conferma
    qzeit       TYPE ltap-qzeit,      " Confirm time / Ora conferma
    qname       TYPE ltap-qname,      " Confirmed by / Confermato da
    status      TYPE char10,          " Status text / Testo stato
    status_icon TYPE char4,           " Status icon / Icona stato
    wait_hours  TYPE p LENGTH 7 DECIMALS 2,  " Wait time / Tempo attesa
    confirmed   TYPE char1,           " Confirmed flag / Flag confermato
    cellcolor   TYPE lvc_t_scol,      " ALV row color / Colore riga ALV
  END OF gty_transfer_order_graph,

  gty_transfer_orders_graph TYPE STANDARD TABLE OF gty_transfer_order_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended Movement KPI with ALV color support
* Tipi - KPI Movimento Esteso con supporto colori ALV
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_movement_kpi_graph,
    lgnum           TYPE ltak-lgnum,  " Warehouse / Magazzino
    bwlvs           TYPE ltak-bwlvs,  " Movement type / Tipo movimento
    bwlvs_txt       TYPE char40,      " Movement type desc / Descr. tipo mov.
    to_count        TYPE i,           " Total TOs / Totale TO
    to_confirmed    TYPE i,           " Confirmed TOs / TO confermati
    to_open         TYPE i,           " Open TOs / TO aperti
    avg_time_hours  TYPE p LENGTH 7 DECIMALS 2,  " Avg time / Tempo medio
    min_time_hours  TYPE p LENGTH 7 DECIMALS 2,  " Min time / Tempo min
    max_time_hours  TYPE p LENGTH 7 DECIMALS 2,  " Max time / Tempo max
    total_qty       TYPE p LENGTH 15 DECIMALS 3, " Total qty / Quantità totale
    status          TYPE char1,       " Status / Stato
    status_icon     TYPE char4,       " Status icon / Icona stato
    cellcolor       TYPE lvc_t_scol,  " ALV row color / Colore riga ALV
  END OF gty_movement_kpi_graph,

  gty_movement_kpis_graph TYPE STANDARD TABLE OF gty_movement_kpi_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended Storage Type Summary with ALV color support
* Tipi - Riepilogo Tipo Storage Esteso con supporto colori ALV
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_storage_type_sum_graph,
    lgnum         TYPE lagp-lgnum,    " Warehouse / Magazzino
    lgtyp         TYPE lagp-lgtyp,    " Storage type / Tipo storage
    lgtyp_txt     TYPE char30,        " Description / Descrizione
    total_bins    TYPE i,             " Total bins / Totale bin
    occupied_bins TYPE i,             " Occupied / Occupati
    empty_bins    TYPE i,             " Empty / Vuoti
    blocked_bins  TYPE i,             " Blocked / Bloccati
    total_quants  TYPE i,             " Total quants / Totale quant
    occupancy_pct TYPE p LENGTH 5 DECIMALS 2,  " Occupancy % / % Occupazione
    status        TYPE char1,         " Status / Stato
    status_icon   TYPE char4,         " Icon / Icona
    bar_graph     TYPE char50,        " Visual bar / Barra visuale
    cellcolor     TYPE lvc_t_scol,    " ALV row color / Colore riga ALV
  END OF gty_storage_type_sum_graph,

  gty_storage_type_sums_graph TYPE STANDARD TABLE OF gty_storage_type_sum_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended Daily Statistics with ALV color support
* Tipi - Statistiche Giornaliere Estese con supporto colori ALV
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_daily_stat_graph,
    lgnum           TYPE ltak-lgnum,  " Warehouse / Magazzino
    date            TYPE ltak-bdatu,  " Date / Data
    day_name        TYPE char10,      " Weekday / Giorno settimana
    to_created      TYPE i,           " TOs created / TO creati
    to_confirmed    TYPE i,           " TOs confirmed / TO confermati
    avg_confirm_hrs TYPE p LENGTH 7 DECIMALS 2,  " Avg confirm time / Tempo medio conferma
    total_qty       TYPE p LENGTH 15 DECIMALS 3, " Total qty / Quantità totale
    peak_hour       TYPE i,           " Peak activity hour / Ora picco attività
    bar_created     TYPE char30,      " Created bar / Barra creati
    bar_confirmed   TYPE char30,      " Confirmed bar / Barra confermati
    cellcolor       TYPE lvc_t_scol,  " ALV row color / Colore riga ALV
  END OF gty_daily_stat_graph,

  gty_daily_stats_graph TYPE STANDARD TABLE OF gty_daily_stat_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended User Workload with ALV color support
* Tipi - Carico Utente Esteso con supporto colori ALV
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_user_workload_graph,
    lgnum           TYPE ltak-lgnum,  " Warehouse / Magazzino
    qname           TYPE ltap-qname,  " User ID / ID Utente
    user_name       TYPE char80,      " User name / Nome utente
    to_confirmed    TYPE i,           " TOs confirmed / TO confermati
    items_processed TYPE p LENGTH 15 DECIMALS 3, " Items / Articoli processati
    avg_time_hours  TYPE p LENGTH 7 DECIMALS 2,  " Avg time / Tempo medio
    efficiency      TYPE p LENGTH 5 DECIMALS 2,  " Efficiency % / % Efficienza
    cellcolor       TYPE lvc_t_scol,  " ALV row color / Colore riga ALV
  END OF gty_user_workload_graph,

  gty_user_workloads_graph TYPE STANDARD TABLE OF gty_user_workload_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Global Variables - Graphical Version Data Tables
* Variabili Globali - Tabelle Dati Versione Grafica
*----------------------------------------------------------------------*
* These tables hold converted data with color information
* Queste tabelle contengono dati convertiti con informazioni colore
DATA:
  gt_bins_graph     TYPE gty_storage_bins_graph,      " Bins with colors / Bin con colori
  gt_to_graph       TYPE gty_transfer_orders_graph,   " TOs with colors / TO con colori
  gt_kpi_graph      TYPE gty_movement_kpis_graph,     " KPIs with colors / KPI con colori
  gt_stsum_graph    TYPE gty_storage_type_sums_graph, " Storage sum with colors / Riepilogo con colori
  gt_daily_graph    TYPE gty_daily_stats_graph,       " Daily stats with colors / Stat. giorn. con colori
  gt_users_graph    TYPE gty_user_workloads_graph.    " Users with colors / Utenti con colori

*----------------------------------------------------------------------*
* Global Variables - GUI Objects for Splitter Layout
* Variabili Globali - Oggetti GUI per Layout Splitter
*----------------------------------------------------------------------*
* Design rationale: Use splitter to show dashboard and ALV simultaneously
* Motivazione design: Usa splitter per mostrare dashboard e ALV insieme
DATA:
  go_custom_container TYPE REF TO cl_gui_custom_container, " Main container / Container principale
  go_splitter_main    TYPE REF TO cl_gui_splitter_container, " Splitter / Divisore
  go_cont_dashboard   TYPE REF TO cl_gui_container,  " Dashboard container / Container dashboard
  go_cont_alv         TYPE REF TO cl_gui_container,  " ALV container / Container ALV
  go_html_dashboard   TYPE REF TO cl_gui_html_viewer, " HTML viewer / Visualizzatore HTML
  go_alv_graph        TYPE REF TO cl_salv_table,     " ALV table / Tabella ALV
  " Sub-splitter for ALV - can be destroyed and recreated when view changes
  " Sub-splitter per ALV - può essere distrutto e ricreato al cambio vista
  go_alv_splitter     TYPE REF TO cl_gui_splitter_container,
  go_alv_subcontainer TYPE REF TO cl_gui_container.

*----------------------------------------------------------------------*
* Global Variables - Screen Control
* Variabili Globali - Controllo Schermata
*----------------------------------------------------------------------*
DATA:
  gv_okcode_graph      TYPE sy-ucomm,   " OK code / Codice OK
  gv_graph_initialized TYPE abap_bool,  " Init flag / Flag inizializzazione
  gv_current_view      TYPE i VALUE 1,  " Current view (1-7) / Vista corrente (1-7)
  gv_last_view_pbo     TYPE i VALUE 0,  " Last view for PBO / Ultima vista per PBO
  gv_last_layout_mode  TYPE i VALUE 0.  " 0=top/bottom, 7=left/right / 0=sopra/sotto, 7=sinistra/destra

*----------------------------------------------------------------------*
* Types - Movement Simulation
* Tipi - Simulazione Movimenti
*----------------------------------------------------------------------*
* ENGLISH:
* These types support the animated warehouse visualization feature.
* The simulation shows material flow between storage types over time,
* allowing users to visualize warehouse activity hour by hour.
*
* ITALIANO:
* Questi tipi supportano la funzionalità di visualizzazione animata.
* La simulazione mostra il flusso materiali tra storage type nel tempo,
* permettendo agli utenti di visualizzare l'attività del magazzino ora per ora.
TYPES:
  " Single movement event for simulation timeline
  " Singolo evento movimento per timeline simulazione
  BEGIN OF gty_sim_movement,
    event_time    TYPE timestamp,     " Event timestamp / Timestamp evento
    event_date    TYPE sydatum,       " Event date / Data evento
    event_hour    TYPE i,             " Event hour (0-23) / Ora evento (0-23)
    event_minute  TYPE i,             " Event minute / Minuto evento
    tanum         TYPE ltak-tanum,    " TO number / Numero TO
    tapos         TYPE ltap-tapos,    " TO item / Posizione TO
    matnr         TYPE ltap-matnr,    " Material / Materiale
    maktx         TYPE makt-maktx,    " Material description / Descrizione
    quantity      TYPE ltap-vsolm,    " Quantity moved / Quantità movimentata
    meins         TYPE ltap-meins,    " Unit / Unità
    from_lgtyp    TYPE ltap-vltyp,    " Source storage type / Tipo origine
    from_lgpla    TYPE ltap-vlpla,    " Source bin / Bin origine
    to_lgtyp      TYPE ltap-nltyp,    " Dest storage type / Tipo destinazione
    to_lgpla      TYPE ltap-nlpla,    " Dest bin / Bin destinazione
    movement_type TYPE ltak-bwlvs,    " Movement type / Tipo movimento
    is_confirmed  TYPE abap_bool,     " Confirmed flag / Flag confermato
  END OF gty_sim_movement,

  gty_sim_movements TYPE STANDARD TABLE OF gty_sim_movement WITH DEFAULT KEY,

  " Storage type state for simulation display
  " Stato storage type per visualizzazione simulazione
  BEGIN OF gty_sim_storage_type,
    lgtyp         TYPE lagp-lgtyp,    " Storage type / Tipo storage
    lgtyp_txt     TYPE t301t-ltypt,   " Description / Descrizione
    total_bins    TYPE i,             " Total bins / Totale bin
    current_stock TYPE i,             " Current stock level / Livello stock attuale
    incoming      TYPE i,             " Incoming movements / Movimenti in entrata
    outgoing      TYPE i,             " Outgoing movements / Movimenti in uscita
    x_pos         TYPE i,             " X position for SVG / Posizione X per SVG
    y_pos         TYPE i,             " Y position for SVG / Posizione Y per SVG
  END OF gty_sim_storage_type,

  gty_sim_storage_types TYPE STANDARD TABLE OF gty_sim_storage_type WITH DEFAULT KEY,

  " Active movement for animation display
  " Movimento attivo per visualizzazione animazione
  BEGIN OF gty_sim_active_move,
    from_lgtyp    TYPE lagp-lgtyp,    " Source type / Tipo origine
    to_lgtyp      TYPE lagp-lgtyp,    " Dest type / Tipo destinazione
    matnr         TYPE ltap-matnr,    " Material / Materiale
    quantity      TYPE ltap-vsolm,    " Quantity / Quantità
    progress_pct  TYPE i,             " Animation progress 0-100 / Progresso 0-100
  END OF gty_sim_active_move,

  gty_sim_active_moves TYPE STANDARD TABLE OF gty_sim_active_move WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Global Variables - Simulation State
* Variabili Globali - Stato Simulazione
*----------------------------------------------------------------------*
DATA:
  gt_sim_movements     TYPE gty_sim_movements,       " All movements / Tutti i movimenti
  gt_sim_storage_types TYPE gty_sim_storage_types,   " Storage type states / Stati storage type
  gt_sim_active_moves  TYPE gty_sim_active_moves,    " Active animations / Animazioni attive
  gv_sim_current_time  TYPE timestamp,               " Current sim time / Ora sim corrente
  gv_sim_start_time    TYPE timestamp,               " Sim start / Inizio sim
  gv_sim_end_time      TYPE timestamp,               " Sim end / Fine sim
  gv_sim_speed         TYPE i VALUE 1,               " Speed multiplier / Moltiplicatore velocità
  gv_sim_playing       TYPE abap_bool,               " Playing flag / Flag in riproduzione
  gv_sim_current_hour  TYPE i,                       " Current hour index / Indice ora corrente
  gv_sim_current_idx   TYPE i VALUE 1.               " Current movement index / Indice movimento
