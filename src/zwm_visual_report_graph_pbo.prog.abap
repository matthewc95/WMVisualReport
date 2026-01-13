*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_PBO
*&---------------------------------------------------------------------*
*& PBO Modules for Graphical Version - Splitter Layout
*& Moduli PBO per Versione Grafica - Layout Splitter
*&
*& ENGLISH:
*& Process Before Output modules handle screen initialization and display.
*& Key responsibilities:
*& - Create splitter container (dashboard + ALV)
*& - Initialize HTML viewer for dashboard
*& - Load appropriate HTML content based on current view
*& - Display ALV grid with color-coded data
*&
*& ITALIANO:
*& I moduli Process Before Output gestiscono inizializzazione e visualizzazione.
*& Responsabilità chiave:
*& - Creare container splitter (dashboard + ALV)
*& - Inizializzare visualizzatore HTML per dashboard
*& - Caricare contenuto HTML appropriato in base alla vista corrente
*& - Visualizzare griglia ALV con dati colorati
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*& Modulo STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*& Sets the GUI status (toolbar buttons) and title bar for screen 0100.
*& Imposta lo stato GUI (pulsanti toolbar) e barra titolo per schermata 0100.
MODULE status_0100 OUTPUT.
  " Set PF-STATUS with toolbar buttons (VIEW_BINS, VIEW_TO, etc.)
  " Imposta PF-STATUS con pulsanti toolbar (VIEW_BINS, VIEW_TO, ecc.)
  SET PF-STATUS 'STATUS_GRAPH'.

  " Set title bar text
  " Imposta testo barra titolo
  SET TITLEBAR 'TITLE_GRAPH'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module INIT_SCREEN OUTPUT
*& Modulo INIT_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*& ENGLISH:
*& Main initialization module that creates the splitter layout and
*& loads the HTML dashboard content. This module:
*& 1. Creates a custom container (CC_MAIN) to hold the splitter
*& 2. Creates a 2-row splitter (35% dashboard, 65% ALV)
*& 3. Creates an HTML viewer in the top container
*& 4. Loads dashboard or simulation HTML based on current view
*&
*& ITALIANO:
*& Modulo principale di inizializzazione che crea il layout splitter
*& e carica il contenuto HTML della dashboard. Questo modulo:
*& 1. Crea un container custom (CC_MAIN) per contenere lo splitter
*& 2. Crea uno splitter a 2 righe (35% dashboard, 65% ALV)
*& 3. Crea un visualizzatore HTML nel container superiore
*& 4. Carica HTML dashboard o simulazione in base alla vista corrente
*&
*& Design rationale / Motivazione design:
*& Using CL_GUI_SPLITTER_CONTAINER allows flexible layout that adapts
*& to different screen sizes. The 35/65 ratio provides enough space
*& for both the visual dashboard and the detailed ALV data.
*& L'uso di CL_GUI_SPLITTER_CONTAINER permette un layout flessibile che
*& si adatta a diverse dimensioni schermo. Il rapporto 35/65 fornisce
*& spazio sufficiente sia per la dashboard visuale che per i dati ALV.
MODULE init_screen OUTPUT.
  " Local variables for HTML processing
  " Variabili locali per elaborazione HTML
  DATA: lt_html      TYPE STANDARD TABLE OF w3html,  " HTML content table / Tabella contenuto HTML
        ls_html      TYPE w3html,                    " HTML line / Riga HTML
        lv_url       TYPE c LENGTH 255,              " Generated URL / URL generato
        lv_html_str  TYPE string,                    " HTML string / Stringa HTML
        lv_offset    TYPE i,                         " String offset / Offset stringa
        lv_len       TYPE i,                         " Length to copy / Lunghezza da copiare
        lv_remaining TYPE i.                         " Remaining length / Lunghezza rimanente

  DATA: lo_controller TYPE REF TO lcl_controller_graph,
        lv_new_layout TYPE i.

  lo_controller = lcl_controller_graph=>get_instance( ).

  " Determine required layout mode based on view
  " Determina modalità layout richiesta in base alla vista
  " View 7 (simulation) = left/right (columns), others = top/bottom (rows)
  " Vista 7 (simulazione) = sinistra/destra (colonne), altre = sopra/sotto (righe)
  lv_new_layout = COND #( WHEN lo_controller->get_current_view( ) = 7 THEN 7 ELSE 0 ).

  " Check if we need to recreate splitter due to layout change
  " Verifica se dobbiamo ricreare lo splitter per cambio layout
  IF gv_graph_initialized = abap_true AND gv_last_layout_mode <> lv_new_layout.
    " Layout mode changed - destroy splitter to recreate with new orientation
    " Modalità layout cambiata - distruggi splitter per ricreare con nuova orientazione
    IF go_html_dashboard IS BOUND.
      go_html_dashboard->free( ).
      FREE go_html_dashboard.
      CLEAR go_html_dashboard.
    ENDIF.
    IF go_alv_splitter IS BOUND.
      go_alv_splitter->free( ).
      FREE go_alv_splitter.
      CLEAR go_alv_splitter.
      CLEAR go_alv_subcontainer.
    ENDIF.
    IF go_splitter_main IS BOUND.
      go_splitter_main->free( ).
      FREE go_splitter_main.
      CLEAR go_splitter_main.
      CLEAR go_cont_dashboard.
      CLEAR go_cont_alv.
    ENDIF.
    cl_gui_cfw=>flush( ).
    gv_graph_initialized = abap_false.
  ENDIF.

  " Only initialize once per screen call (unless view changed)
  " Inizializza solo una volta per chiamata schermata (salvo cambio vista)
  IF gv_graph_initialized = abap_false.

    " Step 1: Create custom container for the splitter
    " Passo 1: Crea container custom per lo splitter
    " The container is linked to the CC_MAIN element in the dynpro
    " Il container è collegato all'elemento CC_MAIN nel dynpro
    IF go_custom_container IS INITIAL.
      CREATE OBJECT go_custom_container
        EXPORTING
          container_name = 'CC_MAIN'
        EXCEPTIONS
          OTHERS         = 1.
    ENDIF.

    " Step 2: Create splitter based on layout mode
    " Passo 2: Crea splitter in base alla modalità layout
    IF go_splitter_main IS INITIAL AND go_custom_container IS BOUND.
      IF lv_new_layout = 7.
        " SIMULATION VIEW: Left/Right layout (1 row, 2 columns - 60% HTML, 40% ALV)
        " VISTA SIMULAZIONE: Layout sinistra/destra (1 riga, 2 colonne - 60% HTML, 40% ALV)
        CREATE OBJECT go_splitter_main
          EXPORTING
            parent  = go_custom_container
            rows    = 1
            columns = 2
          EXCEPTIONS
            OTHERS  = 1.

        " Set column widths: 60% for HTML simulation, 40% for ALV detail
        " Imposta larghezze colonne: 60% per simulazione HTML, 40% per dettaglio ALV
        go_splitter_main->set_column_width(
          EXPORTING
            id    = 1
            width = 60 ).

        go_cont_dashboard = go_splitter_main->get_container(
          row    = 1
          column = 1 ).

        go_cont_alv = go_splitter_main->get_container(
          row    = 1
          column = 2 ).
      ELSE.
        " NORMAL VIEWS: Top/Bottom layout (2 rows, 1 column - 35% dashboard, 65% ALV)
        " VISTE NORMALI: Layout sopra/sotto (2 righe, 1 colonna - 35% dashboard, 65% ALV)
        CREATE OBJECT go_splitter_main
          EXPORTING
            parent  = go_custom_container
            rows    = 2
            columns = 1
          EXCEPTIONS
            OTHERS  = 1.

        " Set row heights: 35% for dashboard, 65% for ALV
        " Imposta altezze righe: 35% per dashboard, 65% per ALV
        go_splitter_main->set_row_height(
          EXPORTING
            id     = 1
            height = gc_split_top ).

        go_cont_dashboard = go_splitter_main->get_container(
          row    = 1
          column = 1 ).

        go_cont_alv = go_splitter_main->get_container(
          row    = 2
          column = 1 ).
      ENDIF.
    ENDIF.

    " Step 3: Create HTML viewer for dashboard display
    " Passo 3: Crea visualizzatore HTML per visualizzazione dashboard
    IF go_html_dashboard IS INITIAL AND go_cont_dashboard IS BOUND.
      CREATE OBJECT go_html_dashboard
        EXPORTING
          parent = go_cont_dashboard
        EXCEPTIONS
          OTHERS = 1.
    ENDIF.

    gv_graph_initialized = abap_true.
    gv_last_layout_mode = lv_new_layout.
  ENDIF.

  " Load dashboard or simulation HTML based on current view
  " Carica HTML dashboard o simulazione in base alla vista corrente
  IF go_html_dashboard IS BOUND.
    " Use simulation HTML for view 7, otherwise use dashboard
    " Usa HTML simulazione per vista 7, altrimenti usa dashboard
    IF lo_controller->get_current_view( ) = 7.
      lv_html_str = lo_controller->get_simulation_html( ).
    ELSE.
      lv_html_str = lo_controller->get_dashboard_html( ).
    ENDIF.

    " Convert HTML string to table format (w3html has max 255 chars per line)
    " Converti stringa HTML in formato tabella (w3html ha max 255 char per riga)
    CLEAR lt_html.
    lv_offset = 0.
    lv_remaining = strlen( lv_html_str ).

    WHILE lv_remaining > 0.
      IF lv_remaining > 255.
        lv_len = 255.
      ELSE.
        lv_len = lv_remaining.
      ENDIF.

      ls_html = lv_html_str+lv_offset(lv_len).
      APPEND ls_html TO lt_html.
      lv_offset = lv_offset + lv_len.
      lv_remaining = lv_remaining - lv_len.
    ENDWHILE.

    " Load HTML into viewer and display
    " Carica HTML nel visualizzatore e visualizza
    go_html_dashboard->load_data(
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_html ).

    go_html_dashboard->show_url( url = lv_url ).
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV OUTPUT
*& Modulo DISPLAY_ALV OUTPUT
*&---------------------------------------------------------------------*
*& ENGLISH:
*& Displays the ALV grid with the appropriate data based on current view.
*& Only recreates the ALV when the view changes to avoid flicker.
*& The ALV handler stores data in instance variables to support scrolling.
*&
*& ITALIANO:
*& Visualizza la griglia ALV con i dati appropriati in base alla vista corrente.
*& Ricrea l'ALV solo quando la vista cambia per evitare sfarfallio.
*& L'handler ALV memorizza i dati in variabili di istanza per supportare lo scrolling.
*&
*& View mapping / Mappatura viste:
*& 1 = Storage Bins / Ubicazioni
*& 2 = Transfer Orders / Ordini di Trasferimento
*& 3 = Movement KPIs / KPI Movimenti
*& 4 = Storage Type Summary / Riepilogo Tipi Storage
*& 5 = Daily Statistics / Statistiche Giornaliere
*& 6 = User Workload / Carico Lavoro Utenti
*& 7 = Movement Simulation / Simulazione Movimenti (no ALV, HTML only)
MODULE display_alv OUTPUT.
  DATA: lo_ctrl TYPE REF TO lcl_controller_graph.

  lo_ctrl = lcl_controller_graph=>get_instance( ).

  " Only recreate ALV if view changed or first time
  " Ricrea ALV solo se la vista è cambiata o è la prima volta
  IF gv_last_view_pbo <> lo_ctrl->get_current_view( ) OR
     lo_ctrl->mo_alv_handler->mo_salv IS NOT BOUND.

    " IMPORTANT: Destroy sub-splitter completely to allow new ALV
    " IMPORTANTE: Distruggi completamente sub-splitter per nuovo ALV
    " CL_SALV_TABLE cannot be reused in the same container, so we must
    " destroy the container itself and create a new one
    " CL_SALV_TABLE non può essere riutilizzato nello stesso container,
    " quindi dobbiamo distruggere il container stesso e crearne uno nuovo
    IF lo_ctrl->mo_alv_handler->mo_salv IS BOUND.
      FREE lo_ctrl->mo_alv_handler->mo_salv.
      CLEAR lo_ctrl->mo_alv_handler->mo_salv.
    ENDIF.

    IF go_alv_splitter IS BOUND.
      go_alv_splitter->free( ).
      FREE go_alv_splitter.
      CLEAR go_alv_splitter.
      CLEAR go_alv_subcontainer.
    ENDIF.

    " Flush to complete destruction
    " Flush per completare distruzione
    cl_gui_cfw=>flush( ).

    " Create new sub-splitter inside ALV container (1 row x 1 col)
    " Crea nuovo sub-splitter dentro container ALV (1 riga x 1 col)
    IF go_cont_alv IS BOUND.
      CREATE OBJECT go_alv_splitter
        EXPORTING
          parent  = go_cont_alv
          rows    = 1
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      IF go_alv_splitter IS BOUND.
        go_alv_subcontainer = go_alv_splitter->get_container(
          row    = 1
          column = 1 ).

        " Set the fresh container in the ALV handler
        " Imposta il nuovo container nell'handler ALV
        lo_ctrl->mo_alv_handler->set_container( go_alv_subcontainer ).
      ENDIF.
    ENDIF.

    " Display current view data in ALV
    " Visualizza dati vista corrente in ALV
    lo_ctrl->display_current_view( ).
    gv_last_view_pbo = lo_ctrl->get_current_view( ).
  ENDIF.
ENDMODULE.
