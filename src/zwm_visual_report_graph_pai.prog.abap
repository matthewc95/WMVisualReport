*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_PAI
*&---------------------------------------------------------------------*
*& PAI Modules for Graphical Version
*& Moduli PAI per Versione Grafica
*&
*& ENGLISH:
*& Process After Input modules handle user interactions and button clicks.
*& Key responsibilities:
*& - Handle navigation buttons (Back, Exit, Cancel)
*& - Handle view switching (VIEW_BINS, VIEW_TO, etc.)
*& - Handle simulation controls (SIM_FWD, SIM_BACK, SIM_RESET)
*& - Handle data refresh
*&
*& ITALIANO:
*& I moduli Process After Input gestiscono interazioni utente e click pulsanti.
*& Responsabilità chiave:
*& - Gestire pulsanti navigazione (Back, Exit, Cancel)
*& - Gestire cambio vista (VIEW_BINS, VIEW_TO, ecc.)
*& - Gestire controlli simulazione (SIM_FWD, SIM_BACK, SIM_RESET)
*& - Gestire refresh dati
*&
*& Function Code Reference / Riferimento Codici Funzione:
*& --------------------------------------------------------
*& BACK/EXIT/CANC - Exit screen / Esci dalla schermata
*& REFRESH        - Reload data / Ricarica dati
*& VIEW_BINS      - Show Storage Bins / Mostra Ubicazioni
*& VIEW_TO        - Show Transfer Orders / Mostra TO
*& VIEW_KPI       - Show Movement KPIs / Mostra KPI Movimenti
*& VIEW_STSUM     - Show Storage Type Summary / Mostra Riepilogo Storage
*& VIEW_DAILY     - Show Daily Statistics / Mostra Statistiche Giornaliere
*& VIEW_USERS     - Show User Workload / Mostra Carico Utenti
*& VIEW_SIM       - Show Movement Simulation / Mostra Simulazione Movimenti
*& SIM_FWD        - Simulation: Step Forward / Simulazione: Avanti
*& SIM_BACK       - Simulation: Step Backward / Simulazione: Indietro
*& SIM_RESET      - Simulation: Reset to Start / Simulazione: Reset
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*& Modulo USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
*& ENGLISH:
*& Main event handler for user actions on screen 0100.
*& Gets the function code from sy-ucomm (standard SAP approach)
*& and dispatches to appropriate handler.
*&
*& ITALIANO:
*& Gestore eventi principale per azioni utente sulla schermata 0100.
*& Ottiene il codice funzione da sy-ucomm (approccio standard SAP)
*& e indirizza al gestore appropriato.
*&
*& Design rationale / Motivazione design:
*& Using sy-ucomm instead of a screen field (like gv_okcode_graph)
*& is more reliable because it's the standard SAP mechanism.
*& We clear sy-ucomm after reading to prevent duplicate processing.
*& Usare sy-ucomm invece di un campo schermata (come gv_okcode_graph)
*& è più affidabile perché è il meccanismo standard SAP.
*& Puliamo sy-ucomm dopo la lettura per evitare elaborazioni duplicate.
MODULE user_command_0100 INPUT.
  DATA: lo_ctrl_pai TYPE REF TO lcl_controller_graph.
  DATA: lv_ucomm TYPE sy-ucomm.

  " Get function code from standard system variable
  " Ottieni codice funzione dalla variabile di sistema standard
  lv_ucomm = sy-ucomm.
  CLEAR sy-ucomm.  " Clear to prevent re-processing / Pulisci per evitare rielaborazione

  " Get controller singleton instance
  " Ottieni istanza singleton del controller
  lo_ctrl_pai = lcl_controller_graph=>get_instance( ).

  CASE lv_ucomm.
    "-------------------------------------------------------------------
    " Navigation buttons / Pulsanti navigazione
    "-------------------------------------------------------------------
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      " Clean up GUI objects before leaving screen
      " Pulisci oggetti GUI prima di uscire dalla schermata
      " This is important to avoid memory leaks and orphaned controls
      " Questo è importante per evitare memory leak e controlli orfani
      IF lo_ctrl_pai->mo_alv_handler->mo_salv IS BOUND.
        FREE lo_ctrl_pai->mo_alv_handler->mo_salv.
      ENDIF.
      IF go_alv_splitter IS BOUND.
        go_alv_splitter->free( ).
        FREE go_alv_splitter.
      ENDIF.
      IF go_html_dashboard IS BOUND.
        go_html_dashboard->free( ).
        FREE go_html_dashboard.
      ENDIF.
      IF go_splitter_main IS BOUND.
        go_splitter_main->free( ).
        FREE go_splitter_main.
      ENDIF.
      IF go_custom_container IS BOUND.
        go_custom_container->free( ).
        FREE go_custom_container.
      ENDIF.
      LEAVE TO SCREEN 0.  " Return to previous screen / Torna alla schermata precedente

    "-------------------------------------------------------------------
    " Data refresh / Aggiornamento dati
    "-------------------------------------------------------------------
    WHEN 'REFRESH'.
      " Reload all data from database
      " Ricarica tutti i dati dal database
      lo_ctrl_pai->refresh_data( ).
      gv_graph_initialized = abap_false.  " Force screen reinitialize / Forza reinizializzazione schermata

    "-------------------------------------------------------------------
    " View switching buttons / Pulsanti cambio vista
    "-------------------------------------------------------------------
    WHEN 'VIEW_BINS'.
      " Switch to Storage Bins view (View 1)
      " Passa alla vista Ubicazioni (Vista 1)
      lo_ctrl_pai->set_view( 1 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_TO'.
      " Switch to Transfer Orders view (View 2)
      " Passa alla vista Ordini di Trasferimento (Vista 2)
      lo_ctrl_pai->set_view( 2 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_KPI'.
      " Switch to Movement KPIs view (View 3)
      " Passa alla vista KPI Movimenti (Vista 3)
      lo_ctrl_pai->set_view( 3 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_STSUM'.
      " Switch to Storage Type Summary view (View 4)
      " Passa alla vista Riepilogo Tipi Storage (Vista 4)
      lo_ctrl_pai->set_view( 4 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_DAILY'.
      " Switch to Daily Statistics view (View 5)
      " Passa alla vista Statistiche Giornaliere (Vista 5)
      lo_ctrl_pai->set_view( 5 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_USERS'.
      " Switch to User Workload view (View 6)
      " Passa alla vista Carico Lavoro Utenti (Vista 6)
      lo_ctrl_pai->set_view( 6 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_SIM'.
      " Switch to Movement Simulation view (View 7)
      " Passa alla vista Simulazione Movimenti (Vista 7)
      " Note: This view shows only HTML, no ALV grid
      " Nota: Questa vista mostra solo HTML, nessuna griglia ALV
      lo_ctrl_pai->set_view( 7 ).
      gv_graph_initialized = abap_false.

    "-------------------------------------------------------------------
    " Simulation controls / Controlli simulazione
    "-------------------------------------------------------------------
    WHEN 'SIM_FWD'.
      " Step forward in simulation timeline
      " Avanza nella timeline della simulazione
      " Advances by simulation speed (default 1 hour)
      " Avanza della velocità simulazione (predefinito 1 ora)
      lo_ctrl_pai->sim_step_forward( ).
      gv_graph_initialized = abap_false.

    WHEN 'SIM_BACK'.
      " Step backward in simulation timeline
      " Torna indietro nella timeline della simulazione
      lo_ctrl_pai->sim_step_backward( ).
      gv_graph_initialized = abap_false.

    WHEN 'SIM_RESET'.
      " Reset simulation to beginning
      " Reset simulazione all'inizio
      lo_ctrl_pai->sim_reset( ).
      gv_graph_initialized = abap_false.

  ENDCASE.
ENDMODULE.
