*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_FRM
*&---------------------------------------------------------------------*
*& Form Routines for Graphical Version
*& Routine Form per Versione Grafica
*&
*& ENGLISH:
*& This include contains FORM routines that act as bridges between
*& the procedural ABAP code (INITIALIZATION, START-OF-SELECTION) and
*& the object-oriented controller classes.
*&
*& ITALIANO:
*& Questo include contiene routine FORM che fungono da ponte tra
*& il codice ABAP procedurale (INITIALIZATION, START-OF-SELECTION) e
*& le classi controller orientate agli oggetti.
*&
*& Design rationale / Motivazione design:
*& Using FORM routines provides a clean separation between the report's
*& event blocks and the OO implementation. This makes the main program
*& more readable and allows the OO code to be tested independently.
*& L'uso di routine FORM fornisce una separazione pulita tra i blocchi
*& evento del report e l'implementazione OO. Questo rende il programma
*& principale più leggibile e permette di testare il codice OO indipendentemente.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_CONTROLLER
*& Form INITIALIZE_CONTROLLER
*&---------------------------------------------------------------------*
*& ENGLISH:
*& Initializes the controller singleton with selection screen parameters.
*& This must be called before load_data to set up the data extraction
*& filters (warehouse, storage type, material, date range, etc.).
*&
*& ITALIANO:
*& Inizializza il singleton del controller con i parametri della schermata
*& di selezione. Deve essere chiamato prima di load_data per impostare
*& i filtri di estrazione dati (magazzino, tipo storage, materiale, date, ecc.).
*&
*& Parameters passed / Parametri passati:
*& - s_lgnum[] : Warehouse number range / Range numero magazzino
*& - s_lgtyp[] : Storage type range / Range tipo storage
*& - s_lgpla[] : Storage bin range / Range ubicazione
*& - s_matnr[] : Material number range / Range numero materiale
*& - s_bwlvs[] : Movement type range / Range tipo movimento
*& - p_datfr   : Date from / Data da
*& - p_datto   : Date to / Data a
*&---------------------------------------------------------------------*
FORM initialize_controller.
  DATA: lo_ctrl_init TYPE REF TO lcl_controller_graph.

  " Get controller singleton instance
  " Ottieni istanza singleton del controller
  lo_ctrl_init = lcl_controller_graph=>get_instance( ).

  " Initialize with selection screen parameters
  " Inizializza con parametri schermata di selezione
  lo_ctrl_init->initialize(
    it_lgnum     = s_lgnum[]    " Warehouse filter / Filtro magazzino
    it_lgtyp     = s_lgtyp[]    " Storage type filter / Filtro tipo storage
    it_lgpla     = s_lgpla[]    " Storage bin filter / Filtro ubicazione
    it_matnr     = s_matnr[]    " Material filter / Filtro materiale
    it_bwlvs     = s_bwlvs[]    " Movement type filter / Filtro tipo movimento
    iv_date_from = p_datfr      " Start date / Data inizio
    iv_date_to   = p_datto ).   " End date / Data fine

  " Set initial view based on radio button selection
  " Imposta vista iniziale in base alla selezione radio button
  " Design: Radio buttons determine which ALV view shows first
  " Design: I radio button determinano quale vista ALV mostrare prima
  IF rb_bins = abap_true.
    " Storage Bins view / Vista Ubicazioni
    lo_ctrl_init->set_view( 1 ).
  ELSEIF rb_to = abap_true.
    " Transfer Orders view / Vista Ordini di Trasferimento
    lo_ctrl_init->set_view( 2 ).
  ELSEIF rb_kpi = abap_true.
    " Movement KPIs view / Vista KPI Movimenti
    lo_ctrl_init->set_view( 3 ).
  ELSEIF rb_stsum = abap_true.
    " Storage Type Summary view / Vista Riepilogo Tipi Storage
    lo_ctrl_init->set_view( 4 ).
  ELSEIF rb_daily = abap_true.
    " Daily Statistics view / Vista Statistiche Giornaliere
    lo_ctrl_init->set_view( 5 ).
  ELSEIF rb_users = abap_true.
    " User Workload view / Vista Carico Lavoro Utenti
    lo_ctrl_init->set_view( 6 ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_DATA
*& Form LOAD_DATA
*&---------------------------------------------------------------------*
*& ENGLISH:
*& Triggers the data loading process in the controller.
*& This reads all warehouse data from SAP tables (LAGP, LQUA, LTAK, LTAP)
*& and converts it to the graphical format with color information.
*&
*& ITALIANO:
*& Avvia il processo di caricamento dati nel controller.
*& Questo legge tutti i dati del magazzino dalle tabelle SAP (LAGP, LQUA, LTAK, LTAP)
*& e li converte nel formato grafico con informazioni sul colore.
*&
*& Data loaded / Dati caricati:
*& - Storage bins with occupancy and status / Ubicazioni con occupazione e stato
*& - Transfer orders with wait time / Ordini trasf. con tempo attesa
*& - Movement KPIs aggregated by type / KPI movimenti aggregati per tipo
*& - Storage type summary / Riepilogo tipi storage
*& - Daily statistics / Statistiche giornaliere
*& - User workload metrics / Metriche carico lavoro utenti
*& - Simulation data for movement animation / Dati simulazione per animazione
*&---------------------------------------------------------------------*
FORM load_data.
  DATA: lo_ctrl_load TYPE REF TO lcl_controller_graph.

  " Get controller singleton instance
  " Ottieni istanza singleton del controller
  lo_ctrl_load = lcl_controller_graph=>get_instance( ).

  " Load all warehouse data and convert to graphical format
  " Carica tutti i dati magazzino e converti in formato grafico
  " This also initializes the movement simulator
  " Questo inizializza anche il simulatore di movimenti
  lo_ctrl_load->load_all_data( ).
ENDFORM.
